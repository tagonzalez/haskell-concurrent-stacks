module LockFreeStackCAS where

import Data.IORef
import Control.Concurrent
import Backoff

data Node a = Nd { val :: a, next :: IORef (Node a) } | Null

instance Eq a => Eq (Node a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2

data LockFreeStack a = LFS { top :: IORef (Node a), backoffLFS :: Backoff }

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

loopTryPush :: Eq a => LockFreeStack a -> a -> IO ()
loopTryPush lfs e = do
  oldTop <- readIORef (top lfs)
  node <- return $ Nd e (top lfs)
  b <- atomCAS (top lfs) oldTop node
  if b
    then return ()
    else backoff (backoffLFS lfs); loopTryPush lfs e


pushLFS:: Eq a => LockFreeStack a -> a -> IO ()
pushLFS lfs e = do loopTryPush lfs e

-- throwNullException ptr = undefined

tryPop:: Eq a => LockFreeStack a -> IO (Node a)
tryPop lfs = do
  oldTop <- readIORef (top lfs)
  -- throwNullException oldTop
  newTop <- readIORef (next oldTop)
  b <- atomCAS (top lfs) oldTop newTop
  if b
    then return oldTop
    else return Null

loopTryPop:: Eq a => LockFreeStack a -> IO a
loopTryPop lfs = do
  returnNode <- tryPop lfs
  case returnNode of
    Nd v _ -> return v
    otherwise -> do { backoff (backoffLFS lfs); loopTryPop lfs }

popLFS:: Eq a => LockFreeStack a -> IO a
popLFS lfs = do loopTryPop lfs
