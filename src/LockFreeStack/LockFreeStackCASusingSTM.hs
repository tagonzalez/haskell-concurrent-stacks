module LockFreeStack.LockFreeStackCASusingSTM where

import Data.IORef
import Control.Concurrent.STM
import Common.Backoff
import Common.AtomCASusingSTM
import Common.NodeSTM
import Control.Exception
import Common.Exceptions

data LockFreeStack a = LFS { top :: TVar (Node a), backoffLFS :: Backoff }

newLFS :: Int -> Int -> IO (LockFreeStack a)
newLFS min max = do
  bck <- newBackoff min max
  nullRef <- atomically $ newTVar Null
  return $ LFS nullRef bck

tryPush :: Eq a => LockFreeStack a -> Node a -> IO Bool
tryPush lfs node = do
  oldTop <- atomically $ readTVar (top lfs)
  atomically $ writeTVar (next node) oldTop
  atomCAS (top lfs) oldTop node

pushLFS :: Eq a => LockFreeStack a -> a -> IO ()
pushLFS lfs value = do
  node <- newNode value
  loopPushLFS lfs node
  where loopPushLFS lfs node = do
          b <- tryPush lfs node
          if b
            then
              return ()
            else do
              backoff (backoffLFS lfs)
              loopPushLFS lfs node

tryPop :: Eq a => LockFreeStack a -> IO (Node a)
tryPop lfs = do
  oldTop <- atomically $ readTVar (top lfs)
  if oldTop == Null
  then
    throw EmptyException
  else do
    newTop <- atomically $ readTVar (next oldTop)
    b <- atomCAS (top lfs) oldTop newTop
    if b
      then return oldTop
      else return Null

popLFS :: Eq a => LockFreeStack a -> IO a
popLFS = loopPop
  where loopPop lfs = do
          returnNode <- tryPop lfs
          if returnNode /= Null
          then
            return (val returnNode)
          else do
            backoff (backoffLFS lfs)
            loopPop lfs

-- Testing
lfsNodesToListIO :: Node a -> IO [a]
lfsNodesToListIO node =
  case node of
    Nd v nxt -> do
      nextNode <- atomically $ readTVar nxt
      acc <- lfsNodesToListIO nextNode
      return $ v:acc
    Null -> return []

lfsToListIO :: LockFreeStack a -> IO [a]
lfsToListIO lfs = do
  topNode <- atomically $ readTVar (top lfs)
  lfsNodesToListIO topNode
