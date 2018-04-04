module LockFreeStackCASusingSTM where

import Data.IORef
import Control.Concurrent.STM
import Backoff
import AtomCASusingSTM
import NodeSTM

data LockFreeStack a = LFS { top :: TVar (Node a), backoffLFS :: Backoff }

createLFS :: Int -> Int -> Int -> IO (LockFreeStack a)
createLFS min max limit = do
    lim <- newIORef limit
    bck <- return $ BCK min max lim
    null <- atomically $ newTVar Null
    return $ LFS null bck

tryPush :: Eq a => LockFreeStack a -> Node a -> IO Bool
tryPush lfs node = do
  oldTop <- atomically $ readTVar (top lfs)
  atomically $ writeTVar (next node) oldTop
  atomCAS (top lfs) oldTop node

loopPushLFS :: Eq a => LockFreeStack a -> Node a -> IO ()
loopPushLFS lfs node = do
  b <- tryPush lfs node
  if b
    then
      return ()
    else do
      backoff (backoffLFS lfs)
      loopPushLFS lfs node


pushLFS :: Eq a => LockFreeStack a -> a -> IO ()
pushLFS lfs value = do
  node <- newNode value
  loopPushLFS lfs node

tryPop :: Eq a => LockFreeStack a -> IO (Node a)
tryPop lfs = do
  oldTop <- atomically $ readTVar (top lfs)
  if oldTop == Null
    then
      error "Empty!"
    else do
      newTop <- atomically $ readTVar (next oldTop)
      b <- atomCAS (top lfs) oldTop newTop
      if b
        then return oldTop
        else return Null

loopPop :: Eq a => LockFreeStack a -> IO a
loopPop lfs = do
  returnNode <- tryPop lfs
  if returnNode /= Null
    then
      return (val returnNode)
    else do
      backoff $ backoffLFS lfs
      loopPop lfs

popLFS :: Eq a => LockFreeStack a -> IO a
popLFS = loopPop

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

main = do
  lfs <- createLFS 1 1 1
  pushLFS lfs 1
  pushLFS lfs 2
  pushLFS lfs 3
  popLFS lfs
  res <- lfsToListIO lfs
  putStrLn $ show res
