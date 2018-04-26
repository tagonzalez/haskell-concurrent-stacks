module LockFreeStack.LockFreeStackCAS where

import Data.IORef
import Common.Backoff
import Common.AtomCAS
import Common.Node
import Control.Exception
import Common.Exceptions

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
      nextNode <- readIORef nxt
      acc <- lfsNodesToListIO nextNode
      return $ v:acc
    Null -> return []

lfsToListIO :: LockFreeStack a -> IO [a]
lfsToListIO lfs = do
  topNode <- readIORef $ top lfs
  lfsNodesToListIO topNode
