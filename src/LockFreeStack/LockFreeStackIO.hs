module LockFreeStack.LockFreeStackIO where

import Data.IORef
import Common.Backoff
import Common.AtomCASIO
import Common.NodeIO
import Control.Exception
import Common.Exceptions
import Control.Monad.Loops
import Data.Maybe
import Utils

data LockFreeStackIO a = LFS { top :: IORef (NodeIO a), backoffLFS :: Backoff }

newLFSIO :: Int -> Int -> IO (LockFreeStackIO a)
newLFSIO min max = do
  bck <- newBackoff min max
  nullRef <- newIORef Null
  return $ LFS nullRef bck

tryPushIO :: Eq a => LockFreeStackIO a -> NodeIO a -> IO Bool
tryPushIO lfs node = do
  oldTop <- readIORef (top lfs)
  writeIORef (next node) oldTop
  atomCASIO (top lfs) oldTop node

pushLFSIO :: Eq a => LockFreeStackIO a -> a -> IO ()
pushLFSIO lfs value = do
  ret <- newIORef True
  node <- newNodeIO value
  whileM_ (readIORef ret) $ do
    b <- tryPushIO lfs node
    if b
      then writeIORef ret False
      else backoff $ backoffLFS lfs

tryPopIO :: Eq a => LockFreeStackIO a -> IO (NodeIO a)
tryPopIO lfs = do
  oldTop <- readIORef (top lfs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- readIORef (next oldTop)
      b <- atomCASIO (top lfs) oldTop newTop
      if b
        then return oldTop
        else return Null

popLFSIO :: Eq a => LockFreeStackIO a -> IO a
popLFSIO lfs = do
  ret <- newIORef True
  res <- newIORef Nothing

  whileM_ (readIORef ret) $ do
    returnNode <- tryPopIO lfs
    if returnNode /= Null
      then do
        writeIORef res $ Just (val returnNode)
        writeIORef ret False
      else backoff (backoffLFS lfs)

  readIORef res >>= return.fromJust
