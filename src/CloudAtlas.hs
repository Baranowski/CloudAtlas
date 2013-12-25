{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO
import System.Exit
import System.Environment
import System.Random
import qualified Data.Map as M
import Data.Word
import Data.Bits
import Data.Time.Clock
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
import QAT
import Interpreter
import Communication
import Gossip
import ServerConfig
import Parser
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
    h_zones <- atomically $ zoneStoTvar Hardcoded.zones
    new_zones <- atomically $ newTVar h_zones
    contacts <- atomically $ newTVar []
    conf <- readConfig confPath
    conf <- case conf of
        Left x -> panic $ "readConfig: " ++ (snd x)
        Right x -> return x
    let env = Env { e_zones = new_zones
                  , e_contacts = contacts
                  , e_conf = conf
                  }
    forkIO $ runReaderT gossiping env
    forkIO $ runReaderT queries env
    Listener.listen env

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
