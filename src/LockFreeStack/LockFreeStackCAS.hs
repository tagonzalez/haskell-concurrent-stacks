module LockFreeStack.LockFreeStackCAS where

import Data.IORef
import Common.Backoff
import Common.AtomCAS
import Common.Node
import Control.Exception
import Common.Exceptions
import Control.Monad.Loops
import Data.Maybe
import Utils

data LockFreeStack a = LFS { top :: IORef (Node a), backoffLFS :: Backoff }

newLFS :: Int -> Int -> IO (LockFreeStack a)
newLFS min max = do
  bck <- newBackoff min max
  nullRef <- newIORef Null
  return $ LFS nullRef bck

tryPush :: Eq a => LockFreeStack a -> Node a -> IO Bool
tryPush lfs node = do
  oldTop <- readIORef (top lfs)
  writeIORef (next node) oldTop
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
  oldTop <- readIORef (top lfs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- readIORef (next oldTop)
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
