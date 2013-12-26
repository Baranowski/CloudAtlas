module Gossip(gossiping) where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.Error
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.List
import Data.Time.Clock
import qualified Data.Map as M
import System.IO
import System.Random

import Communication
import Concurrency
import Network
import ServerConfig
import Zones
import Utils

data GossipSt = GossipSt { level_st :: Int
                         }

gossiping :: ReaderT Env IO ()
gossiping = do
    g <- liftIO $ newStdGen
    runStateT loop GossipSt { level_st = 0
                            }
    return ()
    where
    loop :: StateT GossipSt (ReaderT Env IO) ()
    loop = do
        myTry $ (hoist $ hoist runErrWithPrint) oneGossip
        delay <- asks $ c_g_freq . e_conf
        liftIO $ threadDelay delay
        loop

myTry :: (MonadCatch m) => m a -> m (Either IOError a)
myTry = try

oneGossip :: StateT GossipSt (ReaderT Env (ErrorT String IO)) ()
oneGossip = do
    i <- selectLevel
    liftIO $ putStrLn $ show i
    myself <- asks $ c_path . e_conf
    let shortPath = concat $ intersperse "/" (take (i+1) myself)
    let myName = myself !! (i+1)
    cs <- lookupContacts myName shortPath
    g <- liftIO $ newStdGen
    cs <- fallbackContacts cs
    when (null cs) $ fail "Contacts list is empty"
    let (ind,_) = randomR (0, (length cs)-1) g
    let contact = cs !! ind
    initGossip contact 

lookupContacts myName shortPath = do
    g <- liftIO $ newStdGen
    lift $ embedSTM $ do
        z <- getByPath_stm shortPath
        siblings <- filterM (diffName_stm myName) (z_kids z)
        kids <- filterM hasContacts_stm siblings
        if (null kids)
            then return []
            else do
                let (pos,_) = randomR (0, (length kids)-1) g
                (Aset _ (Just l)) <- reqTyped_stm "contacts" (Aset 0 Nothing) (kids !! pos)
                return l

hasContacts_stm z = do
    attrs <- myRead (z_attrs z)
    let cMbe = M.lookup "contacts" attrs
    n <- reqName_stm z
    case cMbe of
        Just (Aset _ (Just l)) -> return (length l > 0)
        _ -> return False

diffName_stm n z = do
    kidN <- reqName_stm z
    return $ kidN /= n

fallbackContacts cs = case cs of
    [] -> do
        csTv <- asks e_contacts
        lift $ embedSTM $ myRead csTv
    _ -> return (concatMap getC cs)
         where
           getC (Acontact (Just c)) = [c]
           getC _ =  []

selectLevel = do
    strt <- asks $ c_g_strategy . e_conf
    myself <- asks $ c_path . e_conf
    let levelBound = (length myself)-1
    useStrategy strt levelBound

useStrategy RoundRobin levelBound =  do
            modify $ \x -> x{level_st = ((level_st x)+1) `mod` levelBound}
            gets level_st
useStrategy ExpRR levelBound = do
            modify $ \x -> x{level_st = ((level_st x)+1)}
            i <- gets level_st
            let c = countZeroes i 
            if c >= levelBound 
                then do
                    modify $ \x -> x{level_st = 0}
                    return 0
                else return c
useStrategy Random levelBound = do
            g <- liftIO $ newStdGen
            let (res,_) = randomR(0, levelBound-1) g
            return res
useStrategy ExpRandom levelBound = do
            g <- liftIO $ newStdGen
            let (res,_) = randomR(0::Int, (1 `shiftL` levelBound)-1) g
            return $ countZeroes res

countZeroes i = if (1 .&. i > 0)
    then 0
    else 1 + (countZeroes (i `shiftR` 1))

runErrWithPrint :: ErrorT String IO a -> IO a
runErrWithPrint m = do
    res <- runErrorT m
    case res of
        Left err -> do
            liftIO $ hPutStrLn stderr err
            fail err
        Right x -> return x

initGossip contact = do
    client <- resolveHost contact
    now <- liftIO $ myCurrentTime
    sendMsg client (FreshnessPre now)
