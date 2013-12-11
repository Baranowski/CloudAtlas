module Main where

import System.Environment
import System.IO
import Data.List.Split
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Error
import qualified Data.ByteString as B

import Communication

my_port = 12345

main = do
    [hostS, portS] <- getArgs
    (servAddr:_) <- getAddrInfo Nothing (Just hostS) (Just portS)
    sock <- socket (addrFamily servAddr) Datagram defaultProtocol
    res <- runErrorT (loop 1 (addrAddress servAddr) sock)
    case res of
        Left str -> do
            hPutStrLn stderr str
        Right _ -> return ()

loop :: Int -> SockAddr -> Socket -> (ErrorT String IO) ()
loop id serv sock = do
    liftIO $ putStr "> "
    l <- liftIO $ getLine
    let cmd = splitOn " " l
    newId <- process id serv sock cmd
    loop newId serv sock

process id serv sock ["get_zones"] = do
    sendMsg serv sock (RmiReq id GetBagOfZones) 
    (mesg, _) <- liftIO $ recvFrom sock my_port
    (hd, newMsg) <- hoist generalizeId $ readHeader (B.unpack mesg)
    msg <- hoist generalizeId $ deserializeMsg newMsg
    case msg of
        RmiResp _ (RmiBagOfZones zs) -> do
            liftIO $ putStrLn ""
            liftIO $ zs `forM_` putStrLn
        _ -> do
            fail "Unexpected response"
    return (id+1)

sendMsg serv sock msg = do
    let packet = addHeader my_port $ serialize msg
    sent <- liftIO $ sendTo sock (B.pack packet) serv
    when (sent < (length packet))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length packet))
