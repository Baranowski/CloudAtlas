module Main where

import Parser
import System.IO
import System.Exit
import System.Environment
import System.Random
import qualified Data.Map as M
import Data.Word
import Data.List.Split
import Data.List
import Data.ConfigFile as C
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad as Mo
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Morph
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.ByteString as B

import qualified Hardcoded
import Zones
import QAT
import Interpreter
import Communication

panic msg = do
    hPutStrLn stderr msg
    exitFailure

debug = True

data Config = Config { c_port :: PortNumber
                     , c_path :: [String]
                     }
data Env = Env
    { e_zones :: TVar Zone
    , e_contacts :: TVar [Contact]
    , e_conf :: Config
    }

readConfig path = do
    rv <- runErrorT $ do
        cp <- Mo.join $ liftIO $ C.readfile C.emptyCP path
        port <- C.get cp "" "port"
        path <- C.get cp "" "zone"
        return $ Config { c_port = fromIntegral $ read port
                        , c_path = splitOn "/" path
                        }
    case rv of
        Left x -> do
            hPutStrLn stderr $ "readConfig: " ++ (snd x)
            exitFailure
        Right x -> return x

unknown = do
    prog <- getProgName
    hPutStrLn stderr $ "Usage:"
    hPutStrLn stderr $ "    " ++ prog ++ " [config file]"
    exitFailure

main = do
    args <- getArgs
    confPath <- case args of
        ["help"] -> unknown
        ['-':_] -> unknown
        [confPath] -> return confPath
        _ -> unknown
    h_zones <- zoneStoTvar Hardcoded.zones
    new_zones <- atomically $ newTVar h_zones
    contacts <- atomically $ newTVar []
    conf <- readConfig confPath
    let env = Env { e_zones = new_zones
                  , e_contacts = contacts
                  , e_conf = conf
                  }
    forkIO $ runReaderT gossiping env
    Main.listen env

reqAttr_stm aN z = do
    as <- myRead (z_attrs z)
    case M.lookup aN as of
        Nothing -> fail $ "Required attribute " ++ aN ++ " does not exist"
        Just x -> return x
    
reqTyped_stm aN aT z = do
    a <- reqAttr_stm aN z
    when (not $ sameType a aT) $ fail $ "Wrong type for attribute " ++ aN
    when (isNull a) $ fail $ "Attribute " ++ aN ++ " is null"
    return a

gossiping = do
    sock <- liftIO $ socket AF_INET Datagram 0
    loop
    where
      selectLevel = return 1 --TODO
      oneGossip :: ReaderT Env (ErrorT String IO) ()
      oneGossip = do
        i <- selectLevel
        conf <- asks e_conf
        let myself = c_path conf
        let shortPath = concat $ intersperse "/" (take i myself)
        g <- liftIO $ newStdGen
        cs <- embedSTM $ do
            z <- getByPath_stm shortPath
            kids <- filterM hasContacts (z_kids z)
            if (null kids)
                then return []
                else do
                    let (pos,_) = randomR (0, (length kids)-1) g
                    (Alist _ (Just l)) <- reqTyped_stm "contacts" (Alist 0 Nothing) (kids !! pos)
                    return l
        g <- liftIO $ newStdGen
        cs <- case cs of
            [] -> do
                csTv <- asks e_contacts
                atom $ readTVar csTv
            _ -> return (concatMap getC cs)
                 where
                   getC (Acontact (Just c)) = [c]
                   getC _ =  []
        when (null cs) $ fail "Contacts list is empty"
        let (ind,_) = randomR (0, (length cs)-1) g
        let sAddr = cs !! ind
        sendFreshness sAddr
      hasContacts z = do
        attrs <- myRead (z_attrs z)
        let cMbe = M.lookup "contacts" attrs
        case cMbe of
            Just (Alist _ (Just l)) -> return (length l > 0)
            _ -> return False
      loop :: ReaderT Env IO ()
      loop = do
        env <- ask
        res <- lift $ runErrorT $ runReaderT oneGossip env
        case res of
            Left err -> liftIO $ hPutStrLn stderr err
            Right _ -> return ()
        liftIO $ threadDelay 5000000 -- TODO magiczna stala
        loop

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet (c_port $ e_conf env) iNADDR_ANY)
    server env sock

maxLine = 1235

newThread env monad =
    forkIO $ do
        res <- runErrorT $ runReaderT monad env
        case res of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()

server env sock = do
    (mesg, client) <- recvFrom sock maxLine
    newThread env $ do
        when debug (liftIO $ putStrLn "New message received")
        handleMsg (B.unpack mesg) client
    server env sock

handleMsg ::  [Word8] -> SockAddr -> ReaderT Env (ErrorT String IO) ()
handleMsg mesg sender = do
    (msg, client) <- lift $ hoist generalizeId $ go mesg sender
    processMsg msg client
    where
      go mesg sender = do
        (hd, newMsg) <- readHeader mesg
        msg <- deserializeMsg newMsg
        newClient <- updateClient sender hd
        return (msg, newClient)

atom ::  STM a -> ReaderT Env (ErrorT String IO) a
atom = liftIO . atomically

reqAttr n z = embedSTM $ reqAttr_stm n z

reqTyped n a z = embedSTM $ reqTyped_stm n a z

myPath = do
    zTV <- asks e_zones
    conf <- asks e_conf
    let myself = c_path conf
    zones <- atom $ readTVar zTV
    go [] zones (tail myself)
    where
      go acc z [] = return $ reverse $ z:acc
      go acc z (n:ns) = do
        kids <- filterM (byName n) (z_kids z)
        when ((length kids) /= 1)
             (fail $ "Too many or too few children zones with name: " ++ n)
        go (z:acc) (head kids) ns
      byName n z = do
        a <- reqAttr "name" z
        return (a == Astr (Just n))

mkFreshness zones = do
    l <- mapM mkSingleFr zones
    return $ Freshness l
    where
      mkSingleFr z = do
        (Astr (Just n)) <- reqTyped "name" (Astr Nothing) z
        (Atime (Just f)) <- reqTyped "freshness" (Atime Nothing) z
        return (n, timeToTimestamp f)

sendFreshness client = do
    zones <- myPath
    myFr <- mkFreshness zones
    sendMsg client (FreshnessInit myFr)

sendMsg client msg = do
    conf <- asks e_conf
    let msgB = addHeader (c_port conf) $ serialize msg
    sock <- liftIO $ socket AF_INET Datagram 0
    sent <- liftIO $ sendTo sock (B.pack msgB) client
    liftIO $ sClose sock
    when debug (liftIO $ putStrLn $ "New message sent " ++ (show client))
    when (sent < (length msgB))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length msgB))

processMsg (FreshnessInit fr) client = do
    zones <- myPath
    myFr <- mkFreshness zones
    sendMsg client (FreshnessResponse myFr)
    sendUpdate client zones fr
processMsg (FreshnessResponse fr) client = do
    zones <- myPath
    sendUpdate client zones fr
processMsg (RmiReq reqId req) client = do
    resp <- rmiPerform req
    sendMsg client (RmiResp reqId resp)

rmiPerform GetBagOfZones = do
    zsTvar <- asks e_zones
    res <- embedSTM $ do
        root <- lift $ lift $ readTVar zsTvar
        go "" root
    return $ RmiBagOfZones res
    where
      go prevN z = do
        attrs <- lift $ lift $ readTVar (z_attrs z)
        let nMbe = M.lookup "name" attrs
        myName <- case nMbe of
                    Just (Astr (Just s)) -> return $ prevN ++ s ++ "/"
                    Just (Astr Nothing) -> return "/"
                    _ -> fail "Required attribute 'name' is missing or is of an invalid type"
        kidsRess <- mapM (go myName) (z_kids z)
        return $ myName:(concat kidsRess)
rmiPerform (GetZoneAttrs path) = do
    res <- embedSTM $ do
        z <- getByPath_stm path
        attrs <- myRead $ z_attrs z
        return $ M.toList attrs
    return $ RmiZoneInfo res
rmiPerform (SetZoneAttrs path attrs) = do
    embedSTM $ do
        z <- getByPath_stm path
        oldAttrs <- myRead (z_attrs z)
        myWrite (z_attrs z) ((M.fromList attrs) `M.union` oldAttrs)
    return RmiOk
rmiPerform (SetContacts cs) = do
    csTvar <- asks e_contacts
    embedSTM $ myWrite csTvar cs
    return RmiOk

getByPath_stm path = do
    zsTvar <- asks e_zones
    let pathList = splitOn "/" path
    root <- myRead zsTvar
    go (tail pathList) root
    where
      go [] z = return z
      go (n2:ns) z = do
        kid <- filterM (byName n2) (z_kids z)
        when ((length kid) /= 1) (fail $ "Error looking up kids with name " ++ n2)
        go (ns) (head kid)
      byName n z = do
        attrs <- lift $ lift $ readTVar (z_attrs z)
        let nMbe = M.lookup "name" attrs
        case nMbe of
            Just (Astr (Just s)) -> return (n==s)
            _ -> return False

embedSTM = hoist $ hoist atomically
myRead = lift . lift . readTVar
myWrite tv val= lift $ lift $ writeTVar tv val

sendUpdate client zones (Freshness fr) = do
    go "" zones fr
    where
      go _ [] [] = return ()
      go p (z:zs) (f:fs) = do
        (Atime (Just lf)) <- reqTyped "freshness" (Atime Nothing) z
        let (fName, fTimestamp) = f
        let ft = timeToTimestamp lf
        nameA <- reqAttr "name" z
        (newPath, matching) <- case nameA of
                    Astr (Just n) -> do
                        return (p ++ n ++ "/", fName == n)
                    Astr (Nothing) -> return ("/", True)
                    _ -> fail "Unexpected value type for attribute name"
        when (ft > fTimestamp || not matching)
             (sendZone client newPath z)
        when (matching)
             (go newPath zs fs)

sendZone client p z = do
    attrs <- atom $ readTVar (z_attrs z)
    sendMsg client (ZInfo p (M.toList attrs))
