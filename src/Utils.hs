module Utils where

import Data.Time.Clock

import Zones

myCurrentTime = do
    t <- getCurrentTime
    return $ timeToTimestamp t
