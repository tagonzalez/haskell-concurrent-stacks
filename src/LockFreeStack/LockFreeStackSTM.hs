module LockFreeStack.LockFreeStackSTM where

import Data.IORef
import Control.Monad.STM
import Control.Concurrent.STM.TVar
import Common.Backoff
import Common.AtomCASSTM
import Common.NodeSTM
import Control.Exception
import Common.Exceptions
import Control.Monad.Loops
import Data.Maybe

data LockFreeStackSTM a = LFSSTM { top :: TVar (NodeSTM a), backoffLFS :: Backoff }

newLFSSTM :: Int -> Int -> IO (LockFreeStackSTM a)
newLFSSTM min max = do
  bck <- newBackoff min max
  nullRef <- atomically $ newTVar Null
  return $ LFSSTM nullRef bck

tryPushSTM :: Eq a => LockFreeStackSTM a -> NodeSTM a -> IO Bool
tryPushSTM lfs node = do
  oldTop <- atomically $ readTVar (top lfs)
  atomically $ writeTVar (next node) oldTop
  atomCASSTM (top lfs) oldTop node

pushLFSSTM :: Eq a => LockFreeStackSTM a -> a -> IO ()
pushLFSSTM lfs value = do
  ret <- newIORef True

  node <- newNodeSTM value
  whileM_ (readIORef ret) $ do
    b <- tryPushSTM lfs node
    if b
      then writeIORef ret False
      else backoff $ backoffLFS lfs

tryPopSTM :: Eq a => LockFreeStackSTM a -> IO (NodeSTM a)
tryPopSTM lfs = do
  oldTop <- atomically $ readTVar (top lfs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- atomically $ readTVar (next oldTop)
      b <- atomCASSTM (top lfs) oldTop newTop
      if b
        then return oldTop
        else return Null

popLFSSTM :: Eq a => LockFreeStackSTM a -> IO a
popLFSSTM lfs = do
  ret <- newIORef True
  res <- newIORef Nothing

  whileM_ (readIORef ret) $ do
    returnNode <- tryPopSTM lfs
    if returnNode /= Null
      then do
        writeIORef res $ Just (val returnNode)
        writeIORef ret False
      else backoff (backoffLFS lfs)

  readIORef res >>= return.fromJust

