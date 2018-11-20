module StackSTM.StackSTM where

import Data.IORef
import Control.Concurrent.STM
import Common.Backoff
import Common.NodeSTM
import Control.Exception
import Common.Exceptions

data StackSTM a = ST { top :: TVar (NodeSTM a)}

newStackSTM :: IO (StackSTM a)
newStackSTM = atomically (newTVar Null) >>= return.ST

pushStackSTM:: StackSTM a -> a -> IO ()
pushStackSTM st value = do
  node <- newNodeSTM value
  atomically $ do
    oldTop <- readTVar $ top st
    writeTVar (next node) oldTop
    writeTVar (top st) node

popStackSTM:: Show a => StackSTM a -> IO a
popStackSTM st = atomically $ do
    resNode <- readTVar $ top st
    case resNode of
      Nd v nxt -> do
        newTop <- readTVar nxt
        writeTVar (top st) newTop
        return v
      Null -> throw EmptyException  
