{-# LANGUAGE ScopedTypeVariables #-}
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

import Concurrency
import qualified Hardcoded
import Zones
import Interpreter
import Gossip
import ServerConfig
import Listener

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
    conf <- case conf of
        Left x -> panic $ "readConfig: " ++ (snd x)
        Right x -> return x
    t <- getCurrentTime
    h_zones <- atomically $ zoneStoTvar
               (relevant t (c_path conf) Hardcoded.zones)
    new_zones <- atomically $ newTVar h_zones
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
                        (upToDate now myself p)
                        (z_kids z)
        newKids <- mapM (go now myself newP) purgedKids
        return $ Zone (z_attrs z) newKids
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
    attrs <- myRead (z_attrs z)
    myWrite (z_attrs z) (M.insert "contacts" (Aset 0 (Just [Acontact $ Just c])) attrs)

queries = do
    zTv <- asks e_zones
    g <- liftIO $ newStdGen
    now <- liftIO $ getCurrentTime
    errs <- hoist atomically $ do
        z <- lift $ readTVar zTv
        zS <- lift $ zoneTvarToS z
        let ((newZS, errs),_) = runIdentity $ runStateT (runWriterT $ performQueries zS) (g,now)
        newZ <- lift $ zoneStoTvar newZS
        lift $ writeTVar zTv newZ
        return errs
    liftIO $ mapM (hPutStrLn stderr) errs
    delay <- asks $ c_qu_fr . e_conf
    liftIO $ threadDelay delay
    queries

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
