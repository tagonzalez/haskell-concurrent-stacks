module Common.Backoff where

import Data.IORef
import System.Random
import Control.Concurrent
data Backoff = BCK {minDelay :: Int, maxDelay :: Int, limit :: IORef Int}

newBackoff min max limit = do
    limRef <- newIORef limit
    let minDelay = min
    let maxDelay = max
    return $ BCK min max limRef

backoff b = do
  backoffLimit <- readIORef $ limit b
  delay <- randomRIO (1, backoffLimit)
  print delay
  writeIORef (limit b) (min (maxDelay b) (2 * backoffLimit))
  threadDelay (delay * 1000) -- threadDelay accepts amount of microseconds as an argument
