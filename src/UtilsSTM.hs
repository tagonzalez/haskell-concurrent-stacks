module UtilsSTM where

import Common.NodeSTM
import Data.IORef
import System.Clock
import Control.Monad.STM
import Control.Concurrent.STM

repeatIO 0 _ = return ()
repeatIO n action = do
  action
  repeatIO (n - 1) action

nodesToListSTM node =
  case node of
    NdSTM v nxt -> do
      nextNode <- atomically $ readTVar nxt
      acc <- nodesToListSTM nextNode
      return $ v:acc
    Null -> return []

timeIt msg action = do
  startTime <- (getTime Monotonic) >>= return.toNanoSecs
  action
  endTime <- (getTime Monotonic) >>= return.toNanoSecs
  let inSecs = (fromIntegral (endTime - startTime)) / (10 ** 9)
  if msg == ""
    then print inSecs
    else putStrLn $ "[" ++ msg ++ "]" ++ ":" ++ " " ++ (show inSecs)