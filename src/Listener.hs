module Listener(listen) where

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Morph
import Control.Monad.Reader
import qualified Data.ByteString as B
import qualified Data.Map as M
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
            let sPath = concat $ map ("/"++) newPath
            return $ (sPath, timeToTimestamp f):(concat res)

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
