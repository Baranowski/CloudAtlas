module Listener(listen,rmiPerform) where

import Control.Concurrent
import Control.Monad.Error
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Applicative
import qualified Data.ByteString as B
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Data.Time.Clock
import Data.Word
import Data.Function
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
import Security
import Attributes
import SecData

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet (c_port $ e_conf env) iNADDR_ANY)
    server env sock

server env sock = do
    (mesg, client) <- recvFrom sock maxLine
    newThread env $ do
        handleMsg (B.unpack mesg) client
            `myTrace` "New message received"
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
    (do verifyMsg msg
            `myTrace`
            ("Received msg: \n  " ++ (show msg) ++ "\n  from: " ++ (show client))
        processMsg msg client)
        `addTrace`
        ("handleMsg msg=" ++ (show msg) ++ " client=" ++ (show client))
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
processMsg rZ@(ZInfo (t1a,t1b,t2b) p zi) client = do
    myPath <- asks $ (intercalate "/") . c_path . e_conf
    when (p `isPrefixOf` myPath) $ fail "Refuse to update myself by gossiping"
    t2a <- liftIO $ myCurrentTime
    embedSTM $ do
        zMbe <- lookupPath_stm p
        verifyFreshness t2a rZ zMbe
  `addTrace`
  ("ZInfo for " ++ p)
    where
    verifyFreshness t2a (ZInfo ts p zi) (Just z) = do
        (Atime (Just f)) <- reqTyped_stm "freshness" (Atime Nothing) z
        let localTStamp = timeToTimestamp f
        (adjusted, newAttrs) <- adjAttrsFromList ts t2a (zi_attrs zi)
        when (adjusted > localTStamp) $ do
            myWrite (z_info z) zi
    verifyFreshness t2a (ZInfo ts p zi) Nothing = do
        myPath <- asks $ c_path . e_conf
        when ((init (splitOn "/" p)) `isPrefixOf` myPath) $ do
            (_,newAttrs) <- adjAttrsFromList ts t2a (zi_attrs zi)
            attrsTV <- myNewVar zi{zi_attrs=newAttrs}
            let newZ = Zone attrsTV Nothing Nothing []
            addZone_stm (splitOn "/" p) newZ
    adjAttrsFromList (t1a,t1b,t2b) t2a m = do
        f <- case M.lookup "freshness" m of
                Just (Atime (Just f)) -> return f
                Nothing -> fail $ "The zone received does not have the freshness attribute"
        let remoteTStamp = timeToTimestamp f
        let adjusted = frAdjust remoteTStamp (t1a,t1b,t2b,t2a)
        return (adjusted, m)
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
        attrs <- zi_attrs <$> myRead (z_info z)
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
        attrs <- zi_attrs <$> (myRead $ z_info z)
        return $ M.toList attrs
    return $ RmiZoneInfo res
rmiPerform (SetZoneAttrs fc) = do
    t <- liftIO $ getCurrentTime
    embedSTM $ do
        path <- asks $ c_path . e_conf
        let myPath = intercalate "/" path
        z <- getByPath_stm myPath
        pk <- case z_kpriv z of
                Just x -> return x
                Nothing -> fail "Don't have private key"
        oldInfo <- myRead (z_info z)
        let oldAttrs = zi_attrs oldInfo
        let newAttrs = (M.fromList $ fc_attrs fc) `M.union` oldAttrs
        let freshAttrs = M.insert "freshness" (Atime $ Just t) newAttrs
        myWrite (z_info z) oldInfo{zi_attrs=freshAttrs
                                  ,zi_cert =Just $ signZMI
                                                pk
                                                myPath
                                                t
                                                freshAttrs
                                                myPath
                                  }
    return RmiOk
rmiPerform (SetContacts cSs) = do
    csTvar <- asks e_contacts
    embedSTM $ myWrite csTvar cSs
    return RmiOk
rmiPerform (InstallQuery qc) = do
    forM [(qc_minL qc)..(qc_maxL qc)] installAt
    return RmiOk
    where
    caPath = splitOn "/" $ cc_author $ qc_cc qc
    installAt lvl = do
        path <- asks $ (take (lvl+1)) . init . c_path . e_conf
        when (not $ caPath `isPrefixOf` path)
             (fail $ "Will not install query with CC from "
                  ++ (cc_author $ qc_cc qc)
                  ++ " at "
                  ++ (intercalate "/" path))
        embedSTM $ do
            z <- getByPath_stm $ intercalate "/" path
            oldInfo <- myRead $ z_info z
            let filtered = filter ((flip comesLate) qc)
                                  (zi_qcs oldInfo)
            when (not $ any (comesLate qc) filtered) $ do
                let newInfo = oldInfo{zi_qcs=qc:filtered}
                myWrite (z_info z) newInfo
      `catchError` (\_ -> return())
    qca `comesLate` qcb = (((==) `on` qc_name) qca qcb)
                       && (((>=) `on` (ct_create . qc_cert)) qca qcb)
rmiPerform (UninstallQuery path name) = embedSTM $ do
    zs <- matchingZones_stm path
    forM_ zs (go name)
    return RmiOk
    where
    go name z = do
        reqTyped_stm name (Aquery Nothing) z
        info <- myRead (z_info z)
        myWrite (z_info z) info{zi_attrs=M.delete name (zi_attrs info)}

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
            myPath <- asks $ c_path . e_conf
            let fr = if newPath `isPrefixOf` myPath
                    then eternity
                    else f
            let sPath = intercalate "/" newPath
            return $ (sPath, timeToTimestamp fr):(concat res)

sendUpdate client (Freshness fr) times@(t1a,t1b,t2b,t2a) = do
    l1N <- mapM cmpFreshness fr
    let l1 = concat l1N 
    l2 <- missing (map fst fr)
    forM_ (l1++l2) sendInfo
    where
    sendInfo (p, l) = do
        t3a <- liftIO $ myCurrentTime
        sendMsg client (ZInfo (t2b,t2a,t3a) p l)
    cmpFreshness ("", remoteTStamp) = return []
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
        info <- myRead $ z_info z
        return (path, info{zi_attrs=M.filter (not . (sameType (Aquery Nothing)) ) (zi_attrs info)})
