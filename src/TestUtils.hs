module TestUtils where

import Common.NodeIO
import Common.NodeSTM
import Data.IORef
import Control.Monad.STM
import Control.Concurrent.STM

repeatIO :: Int -> IO a -> IO ()
repeatIO 0 _ = return ()
repeatIO n action = do
  action
  repeatIO (n - 1) action

nodesToListIO :: NodeIO a -> IO [a]
nodesToListIO node =
  case node of
    NdIO v nxt -> do
      nextNode <- readIORef nxt
      acc <- nodesToListIO nextNode
      return $ v:acc
    otherwise -> return []

nodesToListSTM :: NodeSTM a -> IO [a]
nodesToListSTM node =
  case node of
    NdSTM v nxt -> do
      nextNode <- atomically $ readTVar nxt
      acc <- nodesToListSTM nextNode
      return $ v:acc
    otherwise -> return []