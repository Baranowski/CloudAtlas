module Network(sendMsg) where

import Control.Monad.Reader

import qualified Data.ByteString as B
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString

import Communication
import ServerConfig

sendMsg client msg = do
    conf <- asks e_conf
    let msgB = addHeader (c_port conf) $ serialize msg
    sock <- liftIO $ socket AF_INET Datagram 0
    sent <- liftIO $ sendTo sock (B.pack msgB) client
    liftIO $ sClose sock
    when debug (liftIO $ putStrLn $ "New message sent " ++ (show client))
    when (sent < (length msgB))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length msgB))

