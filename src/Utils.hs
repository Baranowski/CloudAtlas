{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Data.Time.Clock
import Control.Monad.Error
import Data.List
import Data.List.Split
import Control.Monad

import Zones

myCurrentTime = do
    t <- getCurrentTime
    return $ timeToTimestamp t

addTrace :: (MonadError String m) => m a -> String -> m a
addTrace m str = m `catchError` (\s -> throwError $ s ++ "\n    " ++ str)

parentOf p = intercalate "/" $ init $ splitOn "/" p

concatMapM f l = do
    newL <- mapM f l
    return $ concat newL
