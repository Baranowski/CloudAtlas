{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Main where

import System.IO
import System.Exit
import System.Environment
import System.Random
import qualified Data.Map as M
import Data.Time.Clock
import Data.List
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Identity
import Control.Monad.Morph
import Control.Applicative

import Concurrency
import qualified Hardcoded
import Zones
import Interpreter
import Gossip
import ServerConfig
import Listener
import Security
import Utils

panic msg = do
    hPutStrLn stderr msg
    exitFailure

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
    conf <- readConfig confPath
    (conf,keys) <- case conf of
        Left x -> panic $ "readConfig: " ++ (snd x)
        Right x -> return x
    t <- getCurrentTime
    let myname = intercalate "/" (c_path conf)
    zones <- initZones myname t ((reverse $ c_path conf) `zip` keys)
    new_zones <- atomically $ newTVar zones
    contacts <- atomically $ newTVar []
    let env = Env { e_zones = new_zones
                  , e_contacts = contacts
                  , e_conf = conf
                  }
    runErrorT $ runReaderT setContact env
    forkIO $ runReaderT gossiping env
    forkIO $ runReaderT queries env
    forkIO $ (runErrorT $ runReaderT purger env) >> (return ())
    Listener.listen env

initZones myname t l = go l []
    where
    go ((n,(caK,prvK,zc)):xs) kids = do
        z_info <- atomically $ newTVar info
        let z = Zone{..}
        if null xs
            then return z
            else go xs [z]
        where
        z_kids = kids
        z_kca = caK
        z_kpriv = Just prvK
        info = ZoneInfo{..}
        zi_zc = zc
        zi_attrs = M.fromList
            [ ("name", if null xs
                            then Astr Nothing
                            else Astr $ Just n)
            , ("timestamp", Atime $ Just t)
            ]
        zi_cert = signZMI prvK myname t zi_attrs

purger = do
    delay <- asks $ c_p_freq . e_conf
    liftIO $ threadDelay delay
    singlePurge `catchError` (liftIO . (hPutStrLn stderr))
    purger
    where
    singlePurge = do
        zTv <- asks e_zones
        myself <- asks $ c_path . e_conf
        now <- liftIO $ getCurrentTime
        embedSTM $ do
            z <- myRead zTv
            newZ <- go now myself [] z
            myWrite zTv newZ
    go now myself p z = do
        n <- reqName_stm z
        let newP = p ++ [n]
        purgedKids <- filterM
                        (upToDate now myself newP)
                        (z_kids z)
        newKids <- mapM (go now myself newP) purgedKids
        return $ z{z_kids=newKids}
    upToDate now myself p z = do
        n <- reqName_stm z
        let newP = p ++ [n]
        case (newP `isPrefixOf` myself) of
            True -> return True
            False -> do
                (Atime (Just f)) <- reqTyped_stm "freshness" (Atime Nothing) z
                let secs :: Double = fromRational $ toRational $
                                         now `diffUTCTime` f
                pDelay <- asks $ (/(1000.0*1000.0))
                               . fromIntegral
                               . c_p_freq . e_conf
                return (pDelay > secs)

setContact = embedSTM $ do
    myself <- asks $ c_path . e_conf
    portS <- asks $ show . c_port . e_conf
    hostS <- asks $ c_host . e_conf
    let c = (hostS, portS)
    z <- getByPath_stm (intercalate "/" myself)
    info <- myRead (z_info z)
    myWrite (z_info z) info{zi_attrs=M.insert "contacts" (Aset 0 (Just [Acontact $ Just c])) (zi_attrs info)}

queries = do
    zTv <- asks e_zones
    myname <- asks $ (intercalate "/") . c_path . e_conf
    g <- liftIO $ newStdGen
    now <- liftIO $ getCurrentTime
    errs <- hoist atomically $ do
        z <- lift $ readTVar zTv
        zS <- lift $ zoneTvarToS z
        let ((newZS, errs),_) = runIdentity $ runStateT (runWriterT $ performQueries zS) (g,now)
        newZ <- lift $ updateZones myname now z newZS
        lift $ writeTVar zTv newZ
        return errs
    liftIO $ mapM (hPutStrLn stderr) errs
    delay <- asks $ c_qu_fr . e_conf
    liftIO $ threadDelay delay
    queries

updateZones :: String -> UTCTime -> Zone -> ZoneS -> STM Zone
updateZones issuer t oldZ zs = 
    case z_kpriv oldZ of
        Nothing -> return oldZ
        Just pk -> do
            newKids <- concatMapM (findMatchAndUpdate
                                        (zs_kids zs))
                                  (z_kids oldZ)
            oldInfo <- readTVar (z_info oldZ)
            let newInfo = oldInfo{ zi_attrs = newAttrs
                                 , zi_cert  = newCert pk
                                 }
            newInfoTV <- newTVar newInfo
            return oldZ{ z_kids = newKids
                       , z_info = newInfoTV
                       }
    where
    newAttrs = zs_attrs zs
    newCert pk = signZMI pk issuer t newAttrs
    findMatchAndUpdate :: [ZoneS] -> Zone -> STM [Zone]
    findMatchAndUpdate zss z = do
        attrs <- zi_attrs <$> (readTVar $ z_info z)
        let onlyzs = filter (matchName (M.lookup "name" attrs)) zss
        case onlyzs of
            [x] -> (:[]) <$> (updateZones issuer t z x)
            _ -> return []
    matchName :: Maybe Attribute -> ZoneS -> Bool
    matchName attrMb zs = (M.lookup "name" (zs_attrs zs))==attrMb

relevant now (n:ns) z =
    ZoneS
        (M.insert "freshness" (Atime $ Just now) (zs_attrs z))
        newKids
    where
    newKids = if (zN == n)
        then map (relevant now ns) (filter (nMatch ns) $ zs_kids z)
        else []
    zN = case (M.lookup "name" (zs_attrs z)) of
        Just (Astr (Just x)) -> x
        _ -> ""
    nMatch (n2:nss) z = zNameMbe == (Just (Astr (Just n2)))
        where
        zNameMbe = M.lookup "name" (zs_attrs z)
