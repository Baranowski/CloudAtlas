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
import Data.Time.Clock
import Data.ConfigFile as C
import Data.Bits
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad as Mo
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Monad.Catch
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

data GossipStrategy = RoundRobin | ExpRR | Random | ExpRandom

data Config = Config { c_port :: PortNumber
                     , c_path :: [String]
                     , c_g_freq :: Int
                     , c_g_strategy :: GossipStrategy
                     , c_qu_fr :: Int
                     }

data Env = Env { e_zones :: TVar Zone
               , e_contacts :: TVar [Contact]
               , e_conf :: Config
               }

readConfig path = do
    rv <- runErrorT $ do
        cp <- Mo.join $ liftIO $ C.readfile C.emptyCP path
        port <- C.get cp "" "port"
        path <- C.get cp "" "zone"
        gfreq <- C.get cp "" "gossip_frequency"
        strt <- C.get cp "" "gossip_strategy"
        qufr <- C.get cp "" "query_frequency"
        strategy <- case strt of
            "round-robin" -> return RoundRobin
            "exp-round-robin" -> return ExpRR
            "random" -> return Random
            "exp-random" -> return ExpRandom
            _ -> fail $ "Unrecognized gossip strategy: " ++ strt
        return $ Config { c_port = fromIntegral $ read port
                        , c_path = splitOn "/" path
                        , c_g_freq = (read gfreq) * 1000 * 1000
                        , c_g_strategy = strategy
                        , c_qu_fr = (read qufr) * 1000 * 1000
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
    h_zones <- atomically $ zoneStoTvar Hardcoded.zones
    new_zones <- atomically $ newTVar h_zones
    contacts <- atomically $ newTVar []
    conf <- readConfig confPath
    let env = Env { e_zones = new_zones
                  , e_contacts = contacts
                  , e_conf = conf
                  }
    forkIO $ runReaderT gossiping env
    forkIO $ runReaderT queries env
    Main.listen env

queries = do
    zTv <- asks e_zones
    z <- hoist atomically $ do
        z <- lift $ readTVar zTv
        lift $ zoneTvarToS z
    g <- liftIO $ newStdGen
    now <- liftIO $ getCurrentTime
    let ((newZ, errs),_) = runIdentity $ runStateT (runWriterT $ performQueries z) (g,now)
    liftIO $ mapM (hPutStrLn stderr) errs
    hoist atomically $ do
        newZTv <- lift $ zoneStoTvar newZ
        lift $ writeTVar zTv newZTv
    delay <- asks $ c_qu_fr . e_conf
    liftIO $ threadDelay delay
    queries

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

reqName_stm z = do
    a <- reqAttr_stm "name" z
    case a of
        (Astr Nothing) -> return ""
        (Astr (Just s)) -> return s
        _ -> fail $ "Wrong type for attribute name"

data GossipSt = GossipSt { level_st :: Int
                         }
gossiping :: ReaderT Env IO ()
gossiping = do
    sock <- liftIO $ socket AF_INET Datagram 0
    g <- liftIO $ newStdGen
    runStateT loop GossipSt { level_st = 0
                            }
    return ()
    where
      selectLevel = do
        strt <- asks $ c_g_strategy . e_conf
        myself <- asks $ c_path . e_conf
        let levelBound = (length myself)-1
        case strt of
            RoundRobin -> do
                modify $ \x -> x{level_st = ((level_st x)+1) `mod` levelBound}
                gets level_st
            ExpRR -> do
                modify $ \x -> x{level_st = ((level_st x)+1)}
                i <- gets level_st
                let c = countZeroes i 
                if c >= levelBound 
                    then do
                        modify $ \x -> x{level_st = 0}
                        return 0
                    else return c
            Random -> do
                g <- liftIO $ newStdGen
                let (res,_) = randomR(0, levelBound-1) g
                return res
            ExpRandom -> do
                g <- liftIO $ newStdGen
                let (res,_) = randomR(0::Int, (1 `shiftL` levelBound)-1) g
                return $ countZeroes res
        where
            countZeroes i = if (1 .&. i > 0)
                then 0
                else 1 + (countZeroes (i `shiftR` 1))

      oneGossip :: StateT GossipSt (ReaderT Env (ErrorT String IO)) ()
      oneGossip = do
        i <- selectLevel
        liftIO $ putStrLn $ show i
        myself <- asks $ c_path . e_conf
        let shortPath = concat $ intersperse "/" (take (i+1) myself)
        g <- liftIO $ newStdGen
        cs <- lift $ embedSTM $ do
            z <- getByPath_stm shortPath
            siblings <- filterM (diffName $ myself !! (i+1)) (z_kids z)
            kids <- filterM hasContacts siblings
            if (null kids)
                then return []
                else do
                    let (pos,_) = randomR (0, (length kids)-1) g
                    (Aset _ (Just l)) <- reqTyped_stm "contacts" (Aset 0 Nothing) (kids !! pos)
                    return l
        g <- liftIO $ newStdGen
        cs <- case cs of
            [] -> do
                csTv <- asks e_contacts
                lift $ atom $ readTVar csTv
            _ -> return (concatMap getC cs)
                 where
                   getC (Acontact (Just c)) = [c]
                   getC _ =  []
        when (null cs) $ fail "Contacts list is empty"
        let (ind,_) = randomR (0, (length cs)-1) g
        let sAddr = cs !! ind
        initGossip sAddr
      hasContacts z = do
        attrs <- myRead (z_attrs z)
        let cMbe = M.lookup "contacts" attrs
        n <- reqName_stm z
        case cMbe of
            Just (Aset _ (Just l)) -> return (length l > 0)
            _ -> return False
      diffName n z = do
        kidN <- reqName_stm z
        return $ kidN /= n
      loop :: StateT GossipSt (ReaderT Env IO) ()
      loop = do
        myTry $ (hoist $ hoist runErrWithPrint) oneGossip
        delay <- asks $ c_g_freq . e_conf
        liftIO $ threadDelay delay
        loop

myTry :: (MonadCatch m) => m a -> m (Either IOError a)
myTry = try

runErrWithPrint :: ErrorT String IO a -> IO a
runErrWithPrint m = do
    res <- runErrorT m
    case res of
        Left err -> do
            liftIO $ hPutStrLn stderr err
            fail err
        Right x -> return x

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet (c_port $ e_conf env) iNADDR_ANY)
    server env sock

newThread env monad =
    forkIO $ do
        res <- runErrorT $ runReaderT monad env
        case res of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()

maxLine = 20000

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

mkFreshness = embedSTM $ do
    zTv <- asks e_zones 
    z <- myRead zTv
    l <- go [] z
    return $ Freshness l
    where
        go path z = do
            n <- reqName_stm z
            let newPath = path ++ [n]
            (Atime (Just f)) <- reqTyped_stm "freshness" (Atime Nothing) z
            res <- mapM (go newPath) (z_kids z)
            let sPath = concat $ map ("/"++) newPath
            return $ (sPath, timeToTimestamp f):(concat res)

sendMsg client msg = do
    conf <- asks e_conf
    let msgB = addHeader (c_port conf) $ serialize msg
    sock <- liftIO $ socket AF_INET Datagram 0
    sent <- liftIO $ sendTo sock (B.pack msgB) client
    liftIO $ sClose sock
    when debug (liftIO $ putStrLn $ "New message sent " ++ (show client))
    when (sent < (length msgB))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length msgB))

initGossip client = do
    now <- liftIO $ myCurrentTime
    sendMsg client (FreshnessPre now)
processMsg (FreshnessPre t1a) client = do
    t1b <- liftIO $ myCurrentTime
    myFr <- mkFreshness
    t2b <- liftIO $ myCurrentTime
    sendMsg client (FreshnessInit (t1a,t1b,t2b) myFr)
processMsg (FreshnessInit (t1a,t1b,t2b) fr) client = do
    t2a <- liftIO $ myCurrentTime
    myFr <- mkFreshness
    t3a <- liftIO $ myCurrentTime
    sendMsg client (FreshnessResponse (t2b,t2a,t3a) myFr)
    sendUpdate client fr (t1a,t1b,t2b,t2a)
processMsg (FreshnessResponse (t1a,t1b,t2b) fr) client = do
    t2a <- liftIO $ myCurrentTime
    sendUpdate client fr (t1a,t1b,t2b,t2a)
processMsg (ZInfo (t1a,t1b,t2b) p l) client = do
    t2a <- liftIO $ myCurrentTime
    embedSTM $ do
        z <- getByPath_stm p
        (Atime (Just f)) <- reqTyped_stm "freshness" (Atime Nothing) z
        let localTStamp = timeToTimestamp f
        f <- case lookup "freshness" l of
                Just (Atime (Just f)) -> return f
                Nothing -> fail $ "The zone received does not have the freshness attribute"
        let remoteTStamp = timeToTimestamp f
        let adjusted = frAdjust remoteTStamp (t1a,t1b,t2b,t2a)
        when (adjusted > localTStamp) $ do
            oldAttrs <- myRead (z_attrs z)
            myWrite (z_attrs z) ((M.fromList l) `M.union` oldAttrs)

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
rmiPerform (SetContacts cSs) = do
    cs <- mapM resolve cSs
    csTvar <- asks e_contacts
    embedSTM $ myWrite csTvar cs
    return RmiOk
    where
    resolve (hS,pS) = do
        servAddrs <- liftIO $ getAddrInfo Nothing (Just hS) (Just pS)
        when (null servAddrs) $ fail $ "Cannot find host or port: " ++ hS ++ ":" ++ pS
        return $ addrAddress $ head servAddrs
rmiPerform (InstallQuery path name query) = do
    case parseSingle query of
        Left err -> return $ RmiErr $ show err
        Right q -> actuallyInstall path name q
    where
    actuallyInstall path name q = embedSTM $ do
        zs <- matchingZones_stm path
        forM_ zs (go name q)
        return RmiOk
    go name q z = do
        when (null $ z_kids z) $ fail "Cannot install query in a lowest-level zone"
        attrs <- myRead (z_attrs z)
        myWrite (z_attrs z) (M.insert name (Aquery $ Just q) attrs)
rmiPerform (UninstallQuery path name) = embedSTM $ do
    zs <- matchingZones_stm path
    forM_ zs (go name)
    return RmiOk
    where
    go name z = do
        reqTyped_stm name (Aquery Nothing) z
        attrs <- myRead (z_attrs z)
        myWrite (z_attrs z) (M.delete name attrs)

-- For (un)install query; skip lowest-level zones
matchingZones_stm "*" = do
    myself <- asks $ c_path . e_conf
    mapM (getByPath_stm . (intercalate "/")) (inits $ init myself)

getByPath_stm path = do
    res <- lookupPath_stm path
    case res of
        Just z -> return z
        Nothing -> fail $ "Error looking up zone " ++ path

lookupPath_stm path = do
    zsTvar <- asks e_zones
    let pathList = splitOn "/" path
    root <- myRead zsTvar
    go (tail pathList) root
    where
      go [] z = return $ Just z
      go (n2:ns) z = do
        kid <- filterM (byName n2) (z_kids z)
        case (length kid) of
            0 -> return Nothing
            1 -> go ns (head kid)
            _ -> fail $ "Siblings sharing the same name"
      byName n z = do
        attrs <- lift $ lift $ readTVar (z_attrs z)
        let nMbe = M.lookup "name" attrs
        case nMbe of
            Just (Astr (Just s)) -> return (n==s)
            _ -> return False

embedSTM = hoist $ hoist atomically
myRead = lift . lift . readTVar
myWrite tv val= lift $ lift $ writeTVar tv val

sendUpdate client (Freshness fr) times@(t1a,t1b,t2b,t2a) = do
    fr `forM_` go
    where
        go (path, remoteTStamp) = do
            res <- embedSTM $ do
                zMbe <- lookupPath_stm path
                case zMbe of
                    Just z -> check remoteTStamp times z
                    Nothing -> return Nothing
            case res of
                Just attrs -> do
                    t3a <- liftIO $ myCurrentTime
                    sendMsg client (ZInfo (t2b,t2a,t3a) path (M.toList attrs))
                Nothing -> return ()
        check rTs times z = do
            (Atime (Just lf)) <- reqTyped_stm "freshness" (Atime Nothing) z
            let rt = frAdjust rTs times
            let lt = timeToTimestamp lf
            case (lt > rt) of
                True -> do
                    attrs <- myRead (z_attrs z)
                    return $ Just attrs
                False -> do
                    return Nothing

frAdjust tstamp times@(t1a,t1b,t2b,t2a) = 
  -- localTStamp: tstamp-(t2b-(t2a-delay))
  -- delay: 0.5 * (t1b + t2a - t1a - t2b)
  round $ (fromIntegral tstamp)-((fromIntegral t2b)-((fromIntegral t2a) - delay))
  where
    delay = 0.5 * (fromIntegral (t1b+t2a-t1a-t2b))

myCurrentTime = do
    t <- getCurrentTime
    return $ timeToTimestamp t
