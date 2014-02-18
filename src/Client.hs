{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}
module Main where

import System.Environment
import System.IO
import System.Exit
import System.Process
import Data.List
import Data.List.Split
import Data.List.Utils
import Data.Time.Clock
import qualified Data.List.Utils as L
import qualified Data.ConfigFile as C
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad
import qualified Control.Monad as M
import Control.Monad.Morph
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Applicative
import qualified Data.ByteString as B
import Text.Regex.Posix

import Communication
import Zones
import Utils
import Attributes
import SecData
import Security
import Parser

main = do
    args <- getArgs
    case args of
        ['-':_] -> unknown
        ["help"] -> unknown
        [certS,privS,s] -> do
            cert <- read <$> (readFile certS)
            priv <- read <$> (readFile privS)
            if ':' `elem` s
                then interactive cert priv s
                else daemon cert priv s
        _ -> unknown

unknown = do
    p <- getProgName
    hPutStrLn stderr "Usage: "
    hPutStrLn stderr $ "   " ++ p ++ " <client certificate> <private key> <config file>"
    hPutStrLn stderr $ "       For daemon mode"
    hPutStrLn stderr $ "   " ++ p ++ " <client certificate> <private key> <host name>:<port>"
    hPutStrLn stderr $ "       For interactive mode"
    exitFailure

data Config = Config { host_name :: String
                     , port_name :: String
                     , updateInterval :: Int
                     , avgInterval :: Double
                     }
readConfig path = do
    rv <- runErrorT $ do
        cp <- M.join $ liftIO $ C.readfile C.emptyCP path
        hostS <- C.get cp "Remote" "host"
        portS <- C.get cp "Remote" "port"

        uIS <- C.get cp "Local" "updates"
        aIS <- C.get cp "Local" "averages"
        return $ Config { host_name = hostS
                        , port_name = portS
                        , updateInterval = uIS * 1000 * 1000
                        , avgInterval = aIS
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


daemon :: ClientCert -> PrivKey -> String -> IO ()
daemon cert priv configPath = do
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
            embedError $ singleIter cert priv
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

singleIter :: ClientCert -> PrivKey -> ReaderT Config (StateT MyState (ErrorT String IO)) ()
singleIter cert priv = do
    srv <- gets s_server
    sock <- gets s_socket
    id <- gets s_req_id
    newAttrs <- mapM findAttr attr_names
    t <- liftIO $ getCurrentTime
    let fc_attrs = concat newAttrs
    let fc_cc = cert
    let fc_cert = signFC priv fc_attrs t
    let fc = FeedCert{..}
    sendMsg srv sock $ RmiReq id (SetZoneAttrs fc)
    modify $ \x -> x{s_req_id = (id+1)}
    where
        attr_names = [ "cpu_load" 
                     , "disk"
                     , "ram"
                     , "num_processes"
                     , "num_cores"
                     , "kernel_ver"
                     , "logged_users"
                     , "dns_names"
                     ]
findAttr "cpu_load" = do
    delay <- asks avgInterval
    output <- liftIO $ myReadProcess "top" ["-b", "-n", "2", "-d", show delay] ""
    let s :: [[String]] = output =~ pattern
    let idle = read $ ((s !! 1) !! 1) ++ "." ++ ((s !! 1) !! 2)
    let load = (100.0 - idle) / 100.0
    return [("cpu_load", Afloat $ Just load)]
    where
    --pattern = "Cpu\\(s\\): [^\n]+ ([0-9]+.[0-9]*)%id"
    pattern = "Cpu\\(s\\): [^\n]+ ([0-9]+)[^0-9]([0-9]*).id"
findAttr "disk" = do
    output <- liftIO $ myReadProcess "df" ["--total", "-l"] ""
    let s :: [[String]] = output =~ "total [^0-9]+([0-9]+)[^0-9]+[0-9]+[^0-9]+([0-9]+)[^0-9]"
    let total = read $ (s !! 0) !! 1
    let free = read $ (s !! 0) !! 2
    return [ ("total_disk", Aint $ Just $ 1024 * total)
           , ("free_disk", Aint $ Just $ 1024 * free)
           ]
findAttr "ram" = do
    output <- liftIO $ readFile "/proc/meminfo"
    let s :: [[String]] = output =~ "MemTotal:[^0-9]+([0-9]+) kB"
    let total_ram = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "MemFree:[^0-9]+([0-9]+) kB"
    let free_ram = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "SwapTotal:[^0-9]+([0-9]+) kB"
    let total_swap = 1024 * (read $ (s !! 0) !! 1)
    let s :: [[String]] = output =~ "SwapFree:[^0-9]+([0-9]+) kB"
    let free_swap = 1024 * (read $ (s !! 0) !! 1)
    return [ ("total_ram", Aint $ Just total_ram)
           , ("free_ram", Aint $ Just free_ram)
           , ("total_swap", Aint $ Just total_swap)
           , ("free_swap", Aint $ Just free_swap)
           ]
findAttr "num_processes" = do
    output <- liftIO $ myReadProcess "ps" ["aux"] ""
    return [("num_processes", Aint $ Just $ fromIntegral (countElem '\n' output))]
findAttr "num_cores" = do
    output <- liftIO $ readFile "/proc/cpuinfo"
    let s :: [[String]] = output =~ "model name"
    return [("num_cores", Aint $ Just $ fromIntegral $ length s)]
findAttr "kernel_ver" = do
    output <- liftIO $ myReadProcess "uname" ["-r", "-v"] ""
    return [("kernel_ver", Astr $ Just $ filter (/='\n') output)]
findAttr "logged_users" = do
    output <- liftIO $ myReadProcess "who" [] ""
    return [("logged_users", Aint $ Just $ fromIntegral $ countElem '\n' output)]
findAttr "dns_names" = do
    output <- liftIO $ myReadProcess "hostname" ["-A"] ""
    let hnames = take 3 $ nub $ words output
    return [("dns_names", Aset 3 $ Just $ map (Astr . Just) hnames)]
findAttr s = fail $ "getAttr: Unknown attribute name: " ++ s

myReadProcess prog args input = do
    (ext, output, err) <- liftIO $ readProcessWithExitCode prog args input
    case ext of
        ExitSuccess -> return output
        ExitFailure _ -> fail $ "myReadProcess: trying to run " ++ prog ++ ": " ++ err

panic msg = do
    liftIO $ hPutStrLn stderr msg
    exitFailure

openSocket hostS portS = do
    servAddrs <- getAddrInfo (Just $ defaultHints{addrFamily=AF_INET}) (Just hostS) (Just portS)
    when (null servAddrs) $ panic $ "Cannot find host or port: " ++ hostS ++ ":" ++ portS
    let servAddr = head servAddrs
    sock <- socket (addrFamily servAddr) Datagram defaultProtocol
    bindSocket sock (SockAddrInet 0 iNADDR_ANY)
    return (addrAddress servAddr, sock)

interactive cert priv s = do
    Right (hostS, portS) <- runErrorT $ (parseContact s) `catchError` (lift . panic)
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
        newId <- process id serv sock cmd (cert,priv)
        loop newId serv sock

getMsg sock = do
    (mesg, _) <- liftIO $ recvFrom sock 2000
    (hd, newMsg) <- hoist generalizeId $ readHeader (B.unpack mesg)
    hoist generalizeId $ deserializeMsg newMsg

process id serv sock ["get_zones"] _ = do
    sendMsg serv sock (RmiReq id GetBagOfZones) 
    msg <- getMsg sock
    case msg of
        RmiResp _ (RmiBagOfZones zs) -> do
            liftIO $ zs `forM_` putStrLn
        x -> reportRmiResponse x
    return (id+1)
process id serv sock ["zone", path] _ = do
    sendMsg serv sock (RmiReq id $ GetZoneAttrs path)
    msg <- getMsg sock
    case msg of
        RmiResp _ (RmiZoneInfo l) -> do
            liftIO $ l `forM_` (putStrLn . showAttr)
        x -> reportRmiResponse x
    return (id+1)
    where
        showAttr (name, attr) = name ++ ": " ++ (printAType attr) ++ " = " ++ (printAVal attr)
process id serv sock ("set_contacts":cs) _ = do
    splitCs <- mapM parseContact cs
    sendMsg serv sock (RmiReq id $ SetContacts splitCs)
    msg <- getMsg sock
    reportRmiResponse msg
    return (id+1)
process id serv sock ("install_query":attrName:minLS:maxLS:querys) (cert,priv) = do
    let qc_minL = read minLS
    let qc_maxL = read maxLS
    let query = intercalate " " querys
    case parseSingle query of
        Left pe -> liftIO $ hPutStrLn stderr (show pe)
        Right qc_code -> do
            let qc_name = attrName
            t <- liftIO $ getCurrentTime
            let qc_cert = signQC priv qc_code qc_name qc_minL qc_maxL t
            let qc_cc = cert
            let qc = QueryCert{..}
            sendMsg serv sock (RmiReq id $ InstallQuery qc)
            msg <- getMsg sock
            reportRmiResponse msg
    return (id+1)
process id serv sock ("uninstall_query":path:attrName:[]) _ = do
    sendMsg serv sock (RmiReq id $ UninstallQuery path attrName)
    msg <- getMsg sock
    reportRmiResponse msg
    return (id+1)
process id serv sock ["quit"] _ = do
    liftIO $ sClose sock
    liftIO $ exitSuccess
    return id
process id serv sock _ _ = do
    liftIO $ putStrLn "Unknown command. Use one of:"
    liftIO $ putStrLn " - get_zones   - to get a list of all zones"
    liftIO $ putStrLn " - zone <path> - to list all of zone's attributes"
    liftIO $ putStrLn " - set_contacts <host1:port1> [ <host2:port2> [ ... ] ]- to set fallback contacts for an agent"
    liftIO $ putStrLn ""
    liftIO $ putStrLn " - install_query <query_name> <min level> <max level> <query> - to install a query within spedified levels"
{-    liftIO $ putStrLn " - uninstall_query <zone> <query_name> - to uinstall a query from the specified zone"
    liftIO $ putStrLn "   In the two commands above, <zone> might be '*' to denote all zones the agent belongs to" -}
    liftIO $ putStrLn ""
    liftIO $ putStrLn " - quit"
    return id

reportRmiResponse resp =
    case resp of
        RmiResp _ RmiOk -> do
            liftIO $ putStrLn  "OK"
        RmiResp _ (RmiErr e) -> do
            liftIO $ putStrLn $ "Error reported by agent: " ++ e
        _ -> do
            liftIO $ putStrLn "Unexpected response"

sendMsg serv sock msg = do
    my_port <- liftIO $ socketPort sock
    let packet = addHeader my_port $ serialize msg
    sent <- liftIO $ sendTo sock (B.pack packet) serv
    when (sent < (length packet))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length packet))

parseContact s = do
    let l = splitOn ":" s
    when (length l /= 2) $ fail $ "There should be exactly one colon in every contact specification"
    let [h,p] = l
    return (h,p)
