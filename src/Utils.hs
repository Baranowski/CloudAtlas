{-# LANGUAGE FlexibleContexts #-}
module Utils where

import Data.Time.Clock
import Control.Monad.Error

import Zones

myCurrentTime = do
    t <- getCurrentTime
    return $ timeToTimestamp t

addTrace :: (MonadError String m) => m a -> String -> m a
addTrace m str = m `catchError` (\s -> throwError $ s ++ "\n    " ++ str)
