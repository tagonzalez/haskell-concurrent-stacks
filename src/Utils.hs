module Utils where

import Common.Node
import Data.IORef
import System.Clock
import Control.Monad.STM
import Control.Concurrent.STM

repeatIO 0 _ = return ()
repeatIO n action = do
  action
  repeatIO (n - 1) action

nodesToListIO node =
  case node of
    Nd v nxt -> do
      nextNode <- readIORef nxt
      acc <- nodesToListIO nextNode
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