module LockFreeStack.LockFreeStackSTM where

import Data.IORef
import Control.Concurrent.STM
import Common.Backoff
import Common.NodeSTM
import Control.Exception
import Common.Exceptions

data LockFreeStack a = LFS { top :: TVar (Node a)}

newLFS :: IO (LockFreeStack a)
newLFS = do
  null <- atomically $ newTVar Null
  return $ LFS null

tryPush:: LockFreeStack a -> Node a -> STM ()
tryPush lfs node = do
  oldTop <- readTVar $ top lfs
  writeTVar (next node) oldTop
  writeTVar (top lfs) node

pushLFS:: LockFreeStack a -> a -> IO ()
pushLFS lfs value = do
  node <- newNode value
  atomically $ tryPush lfs node

tryPop:: Show a => LockFreeStack a -> STM a
tryPop lfs = do
  resNode <- readTVar $ top lfs
  case resNode of
    Nd v nxt -> do
      newTop <- readTVar nxt
      writeTVar (top lfs) newTop
      return v
    Null -> throw EmptyException

popLFS:: Show a => LockFreeStack a -> IO a
popLFS lfs = atomically $ tryPop lfs


-- Testing
lfsNodesToListIO :: Show a => Node a -> IO [a]
lfsNodesToListIO node =
  case node of
    Nd v nxt -> do
      nextNode <- atomically $ readTVar nxt
      acc <- lfsNodesToListIO nextNode
      return $ v:acc
    Null -> return []

lfsToListIO :: Show a => LockFreeStack a -> IO [a]
lfsToListIO lfs = do
  topNode <- atomically $ readTVar (top lfs)
  lfsNodesToListIO topNode
