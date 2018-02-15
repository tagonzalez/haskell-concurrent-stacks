
module Backoff where

import Prelude hiding (break)
import Data.IORef
import System.Random
import qualified Control.Concurrent as C
data Backoff = Back {minDelay :: Int, maxDelay :: Int, limit :: IORef Int}

-- Test
-- g <- newStdGen
-- testMinDelay = 1
-- testMaxDelay = 10
-- testLimit = newIORef testMinDelay
-- testBackoff = Back testMinDelay testMaxDelay (testMinDelay)

backoff b = do {
    backoffLimit <- readIORef(limit b);
    delay <- randomRIO (1, backoffLimit);
    writeIORef (limit b) (min (maxDelay b) (2 * backoffLimit));
    C.threadDelay (delay);
}