module Common.Backoff where

import Data.IORef
import System.Random
import Control.Concurrent
data Backoff = BCK {minDelay :: Int, maxDelay :: Int, limit :: IORef Int}

newBackoff :: Int -> Int -> IO Backoff
newBackoff min max = (newIORef min) >>= return.(BCK min max)

-- Shavit: Thread.sleep(millis) takes amount of milliseconds to sleep as an argument
-- threadDelay takes amount of microseconds to sleep as argument
-- 1 millisecond = 1000 microseconds
backoff :: Backoff -> IO ()
backoff b = do
  backoffLimit <- readIORef $ limit b
  delayInMilliseconds <- randomRIO (0, backoffLimit)
  writeIORef (limit b) (min (maxDelay b) (2 * backoffLimit))
  threadDelay (toMicroseconds delayInMilliseconds)
    where toMicroseconds = (*) 1000
