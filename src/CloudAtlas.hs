module Main where

import Parser
import System.IO
import System.Exit
import qualified Data.Map as M
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString
import Control.Monad.Reader
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

installQueries qList z@(Zone attribs children) myself = 
    case myself of
        h:x:xs -> Zone newAttribs newChildren
        h:xs -> Zone newAttribs children
        _ -> z
    where
    newAttribs = attribs `M.union` (M.fromList (queriesToAttribs qList))
    newChildren = map (installInChosen qList myself) children
    queriesToAttribs :: [(String, QAT)] -> [(String, Attribute)]
    queriesToAttribs = map (\(name, qat) -> (name, Aquery (Just qat)))
    installInChosen qList myself@(h:x:xs) z@(Zone attribs _) =
        installQueries qList z (x:xs)

installAndPerform qList zones myself =
    performQueries (installQueries qList zones myself)

listenPort = 1235

data Env = Env
    { e_zones :: TVar Zone
    }

main = do
    new_zones <- atomically $ newTVar Hardcoded.zones
    Main.listen Env{
        e_zones = new_zones
        }

listen env = do
    sock <- socket AF_INET Datagram 0
    bindSocket sock (SockAddrInet listenPort iNADDR_ANY)
    server env sock

maxLine = 1235

server env sock = do
    (mesg, client) <- recvFrom sock maxLine
    forkIO $ runReaderT (handleMsg (B.unpack mesg) client) env
    server env sock

handleMsg mesg sender = do
    case go mesg sender of
        Left err -> lift $ hPutStrLn stderr (show err)
        Right (msg, client) -> processMsg msg client
    where
      go mesg sender = do
        (hd, newMsg) <- readHeader mesg
        msg <- deserializeMsg newMsg
        newClient <- updateClient sender hd
        return (msg, newClient)

atom = lift . atomically

myPath = do
    zTV <- asks e_zones
    zones <- atom $ readTVar zTV
    return zones  -- TODO
processMsg (FreshnessInit fr) client = do
    zones <- myPath
    return () -- TODO

