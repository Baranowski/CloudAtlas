module Main where

import Parser
import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Word
import Data.List.Split
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
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

myself = ["uw", "violet07"]

installQueries qList z@(ZoneS attribs children) myself = 
    case myself of
        h:x:xs -> ZoneS newAttribs newChildren
        h:xs -> ZoneS newAttribs children
        _ -> z
    where
    newAttribs = attribs `M.union` (M.fromList (queriesToAttribs qList))
    newChildren = map (installInChosen qList myself) children
    queriesToAttribs :: [(String, QAT)] -> [(String, Attribute)]
    queriesToAttribs = map (\(name, qat) -> (name, Aquery (Just qat)))
    installInChosen qList myself@(h:x:xs) z@(ZoneS attribs _) =
        installQueries qList z (x:xs)

installAndPerform qList zones myself =
    performQueries (installQueries qList zones myself)

my_port = 12344
debug = True

data Env = Env
    { e_zones :: TVar Zone
    }

main = do
    h_zones <- zoneStoTvar Hardcoded.zones
    new_zones <- atomically $ newTVar h_zones
    Main.listen Env{
        e_zones = new_zones
        }

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet my_port iNADDR_ANY)
    server env sock

maxLine = 1235

server env sock = do
    (mesg, client) <- recvFrom sock maxLine
    forkIO $ do
        when debug (putStrLn "New message received")
        res <- runErrorT $ runReaderT (handleMsg (B.unpack mesg) client) env
        case res of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()
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

reqAttr n z = do
    as <- atom $ readTVar (z_attrs z)
    case M.lookup n as of
        Nothing -> fail $ "Required attribute " ++ n ++ " does not exist"
        Just x -> return x

reqTyped n a z = do
    newA <- reqAttr n z
    when (not $ sameType newA a) $ fail $ "Wrong type for attribute " ++ n
    when (isNull newA) $ fail $ "Attribute " ++ n ++ " is null"
    return newA

myPath = do
    zTV <- asks e_zones
    zones <- atom $ readTVar zTV
    go [] zones myself
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

sendMsg client msg = do
    let msgB = addHeader my_port $ serialize msg
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
    res <- (hoist $ hoist atomically) $ do
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
    let pathList = splitOn "/" path
    zsTvar <- asks e_zones
    res <- (hoist $ hoist atomically) $ do
        root <- lift $ lift $ readTVar zsTvar
        go (tail pathList) root
    return $ RmiZoneInfo res
    where
      go [] z = do
        attrs <- lift $ lift $ readTVar (z_attrs z)
        return $ M.toList attrs
        --return [(n, Astr Nothing)]
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
