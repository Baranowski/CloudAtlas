module Main where

import Parser
import System.IO
import System.Exit
import qualified Data.Map as M
import Data.Word
import Network.Socket hiding (recvFrom, sendTo)
import Network.Socket.ByteString
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Morph
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

installQueries qList z@(ZoneS attribs children) myself = 
    case myself of
        h:x:xs -> ZoneS newAttribs newChildren
        h:xs -> ZoneS newAttribs children
        _ -> z
    where
    newAttribs = attribs `M.union` (M.fromList (queriesToAttribs qList))
    newChildren = map (installInChosen qList myself) children
    queriesToAttribs :: [(String, QAT)] -> [(String, Attribute)]
    queriesToAttribs = map (\(name, qat) -> (name, Aquery (Just qat)))
    installInChosen qList myself@(h:x:xs) z@(ZoneS attribs _) =
        installQueries qList z (x:xs)

installAndPerform qList zones myself =
    performQueries (installQueries qList zones myself)

listenPort = 1235

data Env = Env
    { e_zones :: TVar Zone
    }

main = do
    h_zones <- zoneStoTvar Hardcoded.zones
    new_zones <- atomically $ newTVar h_zones
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
    forkIO $ do
        res <- runErrorT $ runReaderT (handleMsg (B.unpack mesg) client) env
        case res of
            Left err -> hPutStrLn stderr err
            Right _ -> return ()
    server env sock

handleMsg ::  [Word8] -> SockAddr -> ReaderT Env (ErrorT String IO) ()
handleMsg mesg sender = do
    (msg, client) <- lift $ hoist generalizeId $ go mesg sender
    processMsg msg client
    where
      go mesg sender = do
        (hd, newMsg) <- readHeader mesg
        msg <- deserializeMsg newMsg
        newClient <- updateClient sender hd
        return (msg, newClient)

atom ::  STM a -> ReaderT Env (ErrorT String IO) a
atom = liftIO . atomically

reqAttr n z = do
    as <- atom $ readTVar (z_attrs z)
    case M.lookup n as of
        Nothing -> fail $ "Required attribute " ++ n ++ " does not exist"
        Just x -> return x

reqTyped n a z = do
    newA <- reqAttr n z
    when (not $ sameType newA a) $ fail $ "Wrong type for attribute " ++ n
    when (isNull newA) $ fail $ "Attribute " ++ n ++ " is null"
    return newA

myPath = do
    zTV <- asks e_zones
    zones <- atom $ readTVar zTV
    go [] zones myself
    where
      go acc z [] = return $ reverse $ z:acc
      go acc z (n:ns) = do
        kids <- filterM (byName n) (z_kids z)
        when ((length kids) /= 1)
             (fail $ "Too many or too few children zones with name: " ++ n)
        go (z:acc) (head kids) ns
      byName n z = do
        a <- reqAttr "name" z
        return (a == Astr (Just n))

mkFreshness zones = do
    l <- mapM mkSingleFr zones
    return $ Freshness l
    where
      mkSingleFr z = do
        (Astr (Just n)) <- reqTyped "name" (Astr Nothing) z
        (Atime (Just f)) <- reqTyped "freshness" (Atime Nothing) z
        return (n, timeToTimestamp f)

sendMsg client msg = do
    let msgB = serialize msg
    sock <- liftIO $ socket AF_INET Datagram 0
    sent <- liftIO $ sendTo sock (B.pack msgB) client
    liftIO $ sClose sock
    when (sent < (length msgB))
         (fail $ "Sent " ++ (show sent) ++ " bytes instead of " ++ (show $ length msgB))

processMsg (FreshnessInit fr) client = do
    zones <- myPath
    myFr <- mkFreshness zones
    sendMsg client (FreshnessResponse myFr)
    sendUpdate client zones fr
processMsg (FreshnessResponse fr) client = do
    zones <- myPath
    sendUpdate client zones fr

-- TODO: porownywac nazwy stref
sendUpdate client zones (Freshness fr) = do
    when ((length zones) /= (length fr))
         $ fail $ "sendUpdate: freshness length and my path length do not match"
    go "" zones fr
    where
      go _ [] [] = return ()
      go p (z:zs) (f:fs) = do
        (Atime (Just f)) <- reqTyped "freshness" (Atime Nothing) z
        let ft = timeToTimestamp f
        nameA <- reqAttr "name" z
        newPath <- case nameA of
                    Astr (Just n) -> return $ p ++ n ++ "/"
                    Astr (Nothing) -> return $ "/"
                    _ -> fail "Unexpected value type for attribute name"
        when (ft > (timeToTimestamp f)) (sendZone client newPath z)
        go newPath zs fs

sendZone client p z = do
    attrs <- atom $ readTVar (z_attrs z)
    sendMsg client (ZInfo p (M.toList attrs))
