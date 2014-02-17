{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Main where

import System.IO
import System.Exit
import System.Environment
import System.Random
import qualified Data.Map as M
import Data.Time.Clock
import Data.List
import Data.Maybe
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
import Zones
import Interpreter
import Gossip
import ServerConfig
import Listener
import Security
import Utils
import SecData
import Attributes
import Communication

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
    zones <- initZones myname t ((c_path conf) `zip` (reverse keys))
    new_zones <- atomically $ newTVar zones
    contacts <- atomically $ newTVar []
    let env = Env { e_zones = new_zones
                  , e_contacts = contacts
                  , e_conf = conf
                  }
    runErrorT $ runReaderT setContact env
    forkIO $ runReaderT gossiping env
    forkIO $ do
        runErrorT $ runReaderT queries env
        return ()
    forkIO $ (runErrorT $ runReaderT purger env) >> (return ())
    Listener.listen env

initZones :: String -> UTCTime -> [(String, (Maybe PubKey, Maybe PrivKey, Maybe ZoneCert))] -> IO Zone
initZones myname t l = go "" l
    where
    go :: String -> [(String,(Maybe PubKey, Maybe PrivKey, Maybe ZoneCert))] -> IO Zone
    go sp ((n,(caK,prvK,zc)):xs) = do
        let spath = sp
                 ++ (if (not $ null sp) && ((last sp) == '/')
                        then ""
                        else "/")
                 ++ n
        z_kids <- case xs of
            x:_ -> (:[]) <$> (go spath xs)
            _ -> return []
        let zi_cert = (\pk -> signZMI pk myname t zi_attrs spath) <$> prvK
        let info = ZoneInfo{..}
        z_info <- atomically $ newTVar info
        return Zone{..}
        where
        z_kca = caK
        z_kpriv = prvK
        zi_zc = zc
        zi_attrs = M.fromList
            [ ("name", if null n
                            then Astr Nothing
                            else Astr $ Just n)
            , ("timestamp", Atime $ Just t)
            , ("freshness", Atime $ Just t)
            , ("cardinality", Aint $ if null xs
                                        then Just 1
                                        else Nothing)
            ]

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

propagateQueries :: ReaderT Env (ErrorT String IO) ()
propagateQueries = do
    zTv <- asks e_zones
    qs <- embedSTM $ do
        z <- myRead zTv
        go z
    forM_ qs tryToInstall
    where
    go z = do
        kidsQs <- concatMapM go (z_kids z)
        info <- myRead $ z_info z
        return $ (zi_qcs info) ++ kidsQs
    tryToInstall qc = do
        verifyQC qc
        rmiPerform (InstallQuery qc)
        return ()
      `catchError` (\_ -> return ())

runQueries :: ReaderT Env (ErrorT String IO) ()
runQueries = do
    zTv <- asks e_zones
    myname <- asks $ (intercalate "/") . c_path . e_conf
    g <- liftIO $ newStdGen
    now <- liftIO $ getCurrentTime
    errs <- embedSTM $ do
        z <- myRead zTv
        zS <- lift $ lift $ zoneTvarToS z
        let ((newZS, errs),_) = runIdentity $ runStateT (runWriterT $ performQueries zS) (g,now)
        newZ <- lift $ lift $ updateZones "" myname now z newZS
        myWrite zTv newZ
        return errs
    liftIO $ mapM_ (hPutStrLn stderr) errs

queries :: ReaderT Env (ErrorT String IO) ()
queries = do
    do
        propagateQueries
        runQueries
      `catchError` (liftIO . (hPutStrLn stderr))
    delay <- asks $ c_qu_fr . e_conf
    liftIO $ threadDelay delay
    queries

updateZones :: String -> String -> UTCTime -> Zone -> ZoneS -> STM Zone
updateZones sp issuer t oldZ zs = 
    if (z_kpriv oldZ == Nothing) && (nameMb /= Nothing)
        then return oldZ
        else do
            newKids <- concatMapM (findMatchAndUpdate
                                        (zs_kids zs))
                                  (z_kids oldZ)
            oldInfo <- readTVar (z_info oldZ)
            let newInfo = oldInfo{ zi_attrs = newAttrs
                                 , zi_cert  = newCert <$> (z_kpriv oldZ)
                                 }
            newInfoTV <- newTVar newInfo
            return oldZ{ z_kids = newKids
                       , z_info = newInfoTV
                       }
    where
    newSP = sp
         ++ (if (not $ null sp) && ((last sp) == '/')
                then ""
                else "/")
         ++ case nameMb of
            Nothing -> ""
            Just x -> x
    newAttrs = M.insert "owner"
                        (Astr $ Just issuer)
                        (zs_attrs zs)
    Just (Astr nameMb) = M.lookup "name" newAttrs
    newCert pk = signZMI pk issuer t newAttrs newSP
    findMatchAndUpdate :: [ZoneS] -> Zone -> STM [Zone]
    findMatchAndUpdate zss z = do
        attrs <- zi_attrs <$> (readTVar $ z_info z)
        let onlyzs = filter (matchName (M.lookup "name" attrs)) zss
        case onlyzs of
            [x] -> (:[]) <$> (updateZones newSP issuer t z x)
            _ -> return []
    matchName :: Maybe Attribute -> ZoneS -> Bool
    matchName attrMb zs = (M.lookup "name" (zs_attrs zs))==attrMb
