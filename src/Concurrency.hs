module Concurrency where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.Error
import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Map as M

import ServerConfig
import Zones

type StmStack m a = ReaderT Env (ErrorT String m) a
-- TODO: error w srodku powinien powodowac rollback
embedSTM :: StmStack STM a -> StmStack IO a
embedSTM = (hoist $ hoist atomically)

myRead :: TVar a -> StmStack STM a
myRead = lift . lift . readTVar

myWrite :: TVar a -> a -> StmStack STM ()
myWrite tv val= lift $ lift $ writeTVar tv val

myNewVar :: a -> StmStack STM (TVar a)
myNewVar = lift . lift . newTVar

reqAttr_stm aN z = do
    as <- zi_attrs <$> myRead (z_info z)
    let n = M.lookup "name" as
    case M.lookup aN as of
        Nothing -> fail $ "Required attribute " ++ aN ++ " does not exist" ++ " " ++ (show n)
        Just x -> return x
    
reqTyped_stm aN aT z = do
    a <- reqAttr_stm aN z
    when (not $ sameType a aT) $ fail $ "Wrong type for attribute " ++ aN
    when (isNull a) $ fail $ "Attribute " ++ aN ++ " is null"
    return a

reqName_stm z = do
    a <- reqAttr_stm "name" z
    case a of
        (Astr Nothing) -> return ""
        (Astr (Just s)) -> return s
        _ -> fail $ "Wrong type for attribute name"

-- For (un)install query; skip lowest-level zones
matchingZones_stm "*" = do
    myself <- asks $ c_path . e_conf
    mapM (getByPath_stm . (intercalate "/")) (tail $ inits $ init myself)
matchingZones_stm path = do
    z <- getByPath_stm path
    return [z]

getByPath_stm path = do
    res <- lookupPath_stm path
    case res of
        Just z -> return z
        Nothing -> fail $ "Error looking up zone " ++ path

lookupPath_stm "/" = lookupPath_stm ""
lookupPath_stm path = do
    zsTvar <- asks e_zones
    let pathList = splitOn "/" path
    root <- myRead zsTvar
    go (tail pathList) root
    where
      go [] z = return $ Just z
      go (n2:ns) z = do
        kid <- filterM (byName n2) (z_kids z)
        case (length kid) of
            0 -> return Nothing
            1 -> go ns (head kid)
            _ -> fail $ "Siblings sharing the same name"
      byName n z = do
        attrs <- lift $ lift $ zi_attrs <$> readTVar (z_info z)
        let nMbe = M.lookup "name" attrs
        case nMbe of
            Just (Astr (Just s)) -> return (n==s)
            _ -> return False

addZone_stm l newZ = do
    zTv <- asks e_zones
    z <- myRead zTv
    newRoot <- go newZ (init l) z
    myWrite zTv newRoot
    where
    go newZ [] z = fail $ "addZone_stm: empy path"
    go newZ (n:ns) z = do
        zN <- reqName_stm z
        if (zN == n)
            then do
                newKids <- case ns of
                    [] -> return $ (newZ:(z_kids z))
                    _ -> mapM (go newZ ns) (z_kids z)
                return $ z{z_kids=newKids}
            else return z

