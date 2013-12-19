{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment
import System.IO
import System.Exit
import System.Process
import Data.List.Split
import qualified Data.ConfigFile as C
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad
import Control.Monad.Morph
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import qualified Data.ByteString as B
import Text.Regex.Posix

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

data Config = Config { host_name :: String
                     , port_name :: String
                     , updateInterval :: Int
                     , avgInterval :: Double
                     , zone_path :: String
                     }
readConfig path = do
    rv <- runErrorT $ do
        cp <- join $ liftIO $ C.readfile C.emptyCP path
        hostS <- C.get cp "Remote" "host"
        portS <- C.get cp "Remote" "port"
        path <- C.get cp "Remote" "zone"

        uIS <- C.get cp "Local" "updates"
        aIS <- C.get cp "Local" "averages"
        return $ Config { host_name = hostS
                        , port_name = portS
                        , updateInterval = uIS * 1000 * 1000
                        , avgInterval = aIS
                        , zone_path = path
                        }
    case rv of
        Left x -> do
            hPutStrLn stderr $ "readConfig: " ++ (snd x)
            exitFailure
        Right x -> return x

data MyState = MSt { s_req_id :: Int
                   , s_server :: SockAddr
                   , s_socket :: Socket
                   }


daemon configPath = do
    conf <- readConfig configPath
    (sAddr, sock) <- openSocket (host_name conf) (port_name conf) 
    let initState = MSt { s_req_id = 0
                        , s_server = sAddr
                        , s_socket = sock
                        }
    runStateT (runReaderT loop conf) initState
    return ()
    where
        loop = do
            embedError singleIter
            uI <- asks updateInterval
            liftIO $ threadDelay uI
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
    srv <- gets s_server
    sock <- gets s_socket
    zpath <- asks zone_path
    id <- gets s_req_id
    newAttrs <- mapM findAttr attr_names
    liftIO $ putStrLn $ show newAttrs -- FIXME: debug
    sendMsg srv sock $ RmiReq id $ SetZoneAttrs zpath (concat newAttrs)
    modify $ \x -> x{s_req_id = (id+1)}
    where
        attr_names = [ "cpu_load" 
                     , "disk"
                     , "ram" ]
                  {- , "swap"
                     , "num_processes"
                     , "num_cores"
                     , "kernel_ver"
                     , "logged_users"
                     , "dns_names"
                     ] -}
findAttr "cpu_load" = do
    delay <- asks avgInterval
    output <- liftIO $ readProcess "/usr/bin/top" ["-b", "-n", "2", "-d", show delay] ""
    let s :: [[String]] = output =~ "Cpu\\(s\\): [^\n]+ ([0-9]+.[0-9]*)%id"
    let idle :: Double = read $ (s !! 1) !! 1
    let load = (100.0 - idle) / 100.0
    return [("cpu_load", Afloat $ Just load)]
findAttr "disk" = do
    output <- liftIO $ readProcess "/bin/df" ["--total", "-l"] ""
    let s :: [[String]] = output =~ "total [^0-9]+([0-9]+)[^0-9]+[0-9]+[^0-9]+([0-9]+)[^0-9]"
    let total :: Int = read $ (s !! 0) !! 1
    let free :: Int = read $ (s !! 0) !! 2
    return [ ("total_disk", Aint $ Just $ 1024 * total)
           , ("free_disk", Aint $ Just $ 1024 * free)
           ]
findAttr "ram" = do
    output <- liftIO $ readFile "/proc/meminfo"
    let s :: [[String]] = output =~ "MemTotal:[^0-9]+([0-9]+) kB"
    let total_ram :: Int = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "MemFree:[^0-9]+([0-9]+) kB"
    let free_ram :: Int = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "SwapTotal:[^0-9]+([0-9]+) kB"
    let total_swap :: Int = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "SwapFree:[^0-9]+([0-9]+) kB"
    let free_swap :: Int = 1024 * (read $ (s !! 0) !! 1)
    return [ ("total_ram", Aint $ Just total_ram)
           , ("free_ram", Aint $ Just free_ram)
           , ("total_swap", Aint $ Just total_swap)
           , ("free_swap", Aint $ Just free_swap)
           ]

findAttr s = fail $ "getAttr: Unknown attribute name: " ++ s

openSocket hostS portS = do
    (servAddr:_) <- getAddrInfo Nothing (Just hostS) (Just portS)
    sock <- socket (addrFamily servAddr) Datagram defaultProtocol
    bindSocket sock (SockAddrInet my_port iNADDR_ANY)
    return (addrAddress servAddr, sock)

interactive hostS portS = do
    (servAddr, sock) <- openSocket hostS portS
    res <- runErrorT (loop 1 servAddr sock)
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
