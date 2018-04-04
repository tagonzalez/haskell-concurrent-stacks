module Backoff where

import Data.IORef
import System.Random
import Control.Concurrent
data Backoff = BCK {minDelay :: Int, maxDelay :: Int, limit :: IORef Int}

backoff b = do
    backoffLimit <- readIORef $ limit b
    delay <- randomRIO (1, backoffLimit)
    writeIORef (limit b) (min (maxDelay b) (2 * backoffLimit))
    threadDelay (delay)
