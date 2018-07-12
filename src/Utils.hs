module Utils where

import Common.Node
import Data.IORef
import System.Clock

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
  startTime <- (getTime Realtime) >>= return.toNanoSecs
  action
  endTime <- (getTime Realtime) >>= return.toNanoSecs
  let inSecs = (fromIntegral (endTime - startTime)) / (10 ** 9)
  if msg == ""
    then print inSecs
    else putStrLn $ "[" ++ msg ++ "]" ++ ":" ++ " " ++ (show inSecs)