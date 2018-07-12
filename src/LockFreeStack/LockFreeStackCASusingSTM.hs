module LockFreeStack.LockFreeStackCASusingSTM where

import Data.IORef
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Common.Backoff
import Common.AtomCASusingSTM
import Common.NodeSTM
import Control.Exception
import Common.Exceptions
import Control.Monad.Loops
import Data.Maybe

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
  ret <- newIORef True

  node <- newNode value
  whileM_ (readIORef ret) $ do
    b <- tryPush lfs node
    if b
      then writeIORef ret False
      else backoff $ backoffLFS lfs

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
popLFS lfs = do
  ret <- newIORef True
  res <- newIORef Nothing

  whileM_ (readIORef ret) $ do
    returnNode <- tryPop lfs
    if returnNode /= Null
      then do
        writeIORef res $ Just (val returnNode)
        writeIORef ret False
      else backoff (backoffLFS lfs)

  readIORef res >>= return.fromJust

