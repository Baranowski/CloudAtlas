module Listener(listen) where

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Morph
import Control.Monad.Reader
import qualified Data.ByteString as B
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Time.Clock
import Data.Word
import Network.Socket hiding (recvFrom, sendTo, listen)
import Network.Socket.ByteString
import System.IO

import Communication
import Concurrency
import ServerConfig
import Network
import Utils
import Zones
import Parser

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet (c_port $ e_conf env) iNADDR_ANY)
    server env sock

server env sock = do
    (mesg, client) <- recvFrom sock maxLine
    newThread env $ do
        when debug (liftIO $ putStrLn "New message received")
        handleMsg (B.unpack mesg) client
    server env sock

newThread env monad =
    forkIO $ do
        res <- runErrorT $ runReaderT monad env
        case res of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()

maxLine = 20000

handleMsg ::  [Word8] -> SockAddr -> ReaderT Env (ErrorT String IO) ()
handleMsg mesg sender = do
    (msg, client) <- lift $ hoist generalizeId $ go mesg sender
    when debug (liftIO $ hPutStrLn stderr $ "Received msg: \n  " ++ (show msg) ++ "\n  from: " ++ (show client))
    processMsg msg client
    where
      go mesg sender = do
        (hd, newMsg) <- readHeader mesg
        msg <- deserializeMsg newMsg
        newClient <- updateClient sender hd
        return (msg, newClient)

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
processMsg rZ@(ZInfo (t1a,t1b,t2b) p l) client = do
    myPath <- asks $ (intercalate "/") . c_path . e_conf
    when (p == myPath) $ fail "Refuse to update myself by gossiping"
    t2a <- liftIO $ myCurrentTime
    embedSTM $ do
        zMbe <- lookupPath_stm p
        verifyFreshness t2a rZ zMbe
    where
    verifyFreshness t2a (ZInfo ts p l) (Just z) = do
        (Atime (Just f)) <- reqTyped_stm "freshness" (Atime Nothing) z
        let localTStamp = timeToTimestamp f
        (adjusted, newAttrs) <- adjAttrsFromList ts t2a l
        when (adjusted > localTStamp) $ do
            oldAttrs <- myRead (z_attrs z)
            let queries = M.filter (sameType (Aquery Nothing)) oldAttrs
            myWrite (z_attrs z) (newAttrs `M.union` queries)
    verifyFreshness t2a (ZInfo ts p l) Nothing = do
        myPath <- asks $ c_path . e_conf
        when ((init (splitOn "/" p)) `isPrefixOf` myPath) $ do
            (_,newAttrs) <- adjAttrsFromList ts t2a l
            attrsTV <- myNewVar newAttrs
            let newZ = Zone attrsTV []
            addZone_stm (splitOn "/" p) newZ
    adjAttrsFromList (t1a,t1b,t2b) t2a l = do
        f <- case lookup "freshness" l of
                Just (Atime (Just f)) -> return f
                Nothing -> fail $ "The zone received does not have the freshness attribute"
        let remoteTStamp = timeToTimestamp f
        let adjusted = frAdjust remoteTStamp (t1a,t1b,t2b,t2a)
        return (adjusted, (M.insert "freshness"
                (Atime $ Just $ timestampToTime adjusted)
                (M.fromList l) ) )
processMsg (RmiReq reqId req) client = do
    resp <- rmiPerform req `catchError` (\e -> return $ RmiErr e)
    sendMsg client (RmiResp reqId resp)

rmiPerform GetBagOfZones = do
    zsTvar <- asks e_zones
    res <- embedSTM $ do
        root <- myRead zsTvar
        go "" root
    return $ RmiBagOfZones res
    where
      go prevN z = do
        attrs <- myRead (z_attrs z)
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
rmiPerform (SetZoneAttrs attrs) = do
    t <- liftIO $ getCurrentTime
    embedSTM $ do
        path <- asks $ c_path . e_conf
        z <- getByPath_stm (intercalate "/" path)
        oldAttrs <- myRead (z_attrs z)
        let newAttrs = (M.fromList attrs) `M.union` oldAttrs
        let freshAttrs = M.insert "freshness" (Atime $ Just t) newAttrs
        myWrite (z_attrs z) freshAttrs
    return RmiOk
rmiPerform (SetContacts cSs) = do
    csTvar <- asks e_contacts
    embedSTM $ myWrite csTvar cSs
    return RmiOk
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

frAdjust tstamp times@(t1a,t1b,t2b,t2a) = 
  -- localTStamp: tstamp-(t2b-(t2a-delay))
  -- delay: 0.5 * (t1b + t2a - t1a - t2b)
  round $ (fromIntegral tstamp)-((fromIntegral t2b)-((fromIntegral t2a) - delay))
  where
    delay = 0.5 * (fromIntegral (t1b+t2a-t1a-t2b))

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
            let sPath = intercalate "/" newPath
            return $ (sPath, timeToTimestamp f):(concat res)

sendUpdate client (Freshness fr) times@(t1a,t1b,t2b,t2a) = do
    l1N <- mapM cmpFreshness fr
    let l1 = concat l1N 
    l2 <- missing (map fst fr)
    forM_ (l1++l2) sendInfo
    where
    sendInfo (p, l) = do
        t3a <- liftIO $ myCurrentTime
        sendMsg client (ZInfo (t2b,t2a,t3a) p l)
    cmpFreshness (path, remoteTStamp) = do
        embedSTM $ do
            zMbe <- lookupPath_stm path
            case zMbe of
                Just z -> check remoteTStamp times z path
                Nothing -> return []
    check rTs times z p = do
        (Atime (Just lf)) <- reqTyped_stm "freshness" (Atime Nothing) z
        let rt = frAdjust rTs times
        let lt = timeToTimestamp lf
        case (lt > rt) of
            True -> do
                r <- toSend p z
                return [r]
            False -> return []
    missing ps = embedSTM $ do
        zTv <- asks e_zones
        z <- myRead zTv
        mgo ps [] z
    mgo ps path z = do
        n <- reqName_stm z
        let nPath = path ++ [n]
        kidsResNest <- mapM (mgo ps nPath) (z_kids z)
        let kidsRes = concat kidsResNest
        if ( (all (/=(intercalate "/" nPath)) ps) &&
             (any ((==path) . init . (splitOn "/")) ps) )
            then do
                r <- toSend (intercalate "/" nPath) z
                return $ r:kidsRes
            else return kidsRes
    toSend path z = do
        attrs <- myRead $ z_attrs z
        return (path, M.toList $ M.filter
            (not . (sameType (Aquery Nothing)) )
            attrs)
