module ExperimentUtils where

import System.Clock

timeExperiment :: IO a -> IO ()
timeExperiment action = do
  startTime <- (getTime Monotonic) >>= return.toNanoSecs
  action
  endTime <- (getTime Monotonic) >>= return.toNanoSecs
  let inSecs = (fromIntegral (endTime - startTime)) / (10 ** 9)
  print inSecs