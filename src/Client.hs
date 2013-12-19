module Main where

import System.Environment
import System.IO
import Data.List.Split
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import qualified Data.ByteString as B

import Communication
import Zones

my_port :: PortNumber
my_port = 12345

main = do
    args <- getArgs
    case args of
        [configPath] -> daemon configPath
        [hostS, portS] -> interactive hostS portS
        _ -> unknown

unknown = return () -- TODO: help msg

data Config = Config { hostS :: String
                     , portS :: String
                     , updateInterval :: Int
                     , avgInterval :: Int
                     }
readConfig path = return $ Config "localhost" "12345" 30000000 10000000 -- TODO

data MyState = MSt { sinceLastUpd :: Int
                   }


daemon configPath = do
    conf <- readConfig configPath
    let initState = MSt { sinceLastUpd = 0
                        }
    runStateT (runReaderT loop conf) initState
    return ()
    where
        loop = do
            embedError singleIter
            uI <- asks updateInterval
            aI <- asks avgInterval
            slu <- gets sinceLastUpd
            when (slu == uI)
                $ modify $ \x -> x{sinceLastUpd = 0}
            let nextDelay = min (uI - slu `mod` uI)
                                (aI - slu `mod` aI)
            liftIO $ threadDelay nextDelay
            modify $ \x -> x{sinceLastUpd = nextDelay + slu}
            loop
        embedError m = do --TODO (niski priorytet)
            conf <- ask
            s <- get
            res <- lift $ lift $ runErrorT $ runStateT (runReaderT m conf) s
            case res of 
                Left err -> do
                    liftIO $ hPutStrLn stderr err
                    return ()
                Right (_, s) -> put s
ioRunError :: ErrorT String IO () -> IO ()
ioRunError m = do
    res <- runErrorT m
    case res of
        Left msg -> do
            hPutStrLn stderr msg
            return ()
        Right x -> return x

singleIter :: ReaderT Config (StateT MyState (ErrorT String IO)) ()
singleIter = do
    uI <- asks updateInterval
    aI <- asks avgInterval
    slu <- gets sinceLastUpd
    when (0 == slu `mod` aI) newAvgs
    when (slu >= uI) sendUpdates

newAvgs = do

sendUpdates = return () -- TODO

    
interactive hostS portS = do
    (servAddr:_) <- getAddrInfo Nothing (Just hostS) (Just portS)
    sock <- socket (addrFamily servAddr) Datagram defaultProtocol
    bindSocket sock (SockAddrInet my_port iNADDR_ANY)
    res <- runErrorT (loop 1 (addrAddress servAddr) sock)
    case res of
        Left str -> do
            hPutStrLn stderr str
        Right _ -> return ()
    where
    loop :: Int -> SockAddr -> Socket -> (ErrorT String IO) ()
    loop id serv sock = do
        liftIO $ putStr "> "
        liftIO $ hFlush stdout
        l <- liftIO $ getLine
        let cmd = splitOn " " l
        newId <- process id serv sock cmd
        loop newId serv sock

process id serv sock ["get_zones"] = do
    sendMsg serv sock (RmiReq id GetBagOfZones) 
    (mesg, _) <- liftIO $ recvFrom sock 2000
    (hd, newMsg) <- hoist generalizeId $ readHeader (B.unpack mesg)
    msg <- hoist generalizeId $ deserializeMsg newMsg
    case msg of
        RmiResp _ (RmiBagOfZones zs) -> do
            liftIO $ zs `forM_` putStrLn
        _ -> do
            fail "Unexpected response"
    return (id+1)
process id serv sock ["zone", path] = do
    sendMsg serv sock (RmiReq id $ GetZoneAttrs path)
    (mesg, _) <- liftIO $ recvFrom sock 2000
    (hd, newMsg) <- hoist generalizeId $ readHeader (B.unpack mesg)
    msg <- hoist generalizeId $ deserializeMsg newMsg
    case msg of
        RmiResp _ (RmiZoneInfo l) -> do
            liftIO $ l `forM_` (putStrLn . showAttr)
        _ -> do
            fail "Unexpected response"
    return (id+1)
    where
        showAttr (name, attr) = name ++ ": " ++ (printAType attr) ++ " = " ++ (printAVal attr)

sendMsg serv sock msg = do
    let packet = addHeader my_port $ serialize msg
    sent <- liftIO $ sendTo sock (B.pack packet) serv
    when (sent < (length packet))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length packet))
