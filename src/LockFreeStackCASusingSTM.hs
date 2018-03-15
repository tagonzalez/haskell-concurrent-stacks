module LockFreeStackCASusingSTM where

import Control.Concurrent.STM
import System.IO.Unsafe
-- import Test.HUnit
import Backoff

-- Data structures
data Node a = Nd { val :: a, next :: TVar (Node a) } | Null
data LockFreeStack a = LFS { top :: TVar (Node a), backoffLFS :: Backoff }

-- Equality
instance Eq a => Eq (Node a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2
  Nd _ _ == Null = False
  Null == Null = True
  Null == Nd _ _ = False

-- Member functions
atomCAS :: Eq a => TVar a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomically $ do
       cur <- readTVar ptr
       if cur == old
        then do writeTVar ptr new
                return True
        else return False

loopTryPush :: Eq a => LockFreeStack a -> a -> IO ()
loopTryPush lfs e = do
  oldTop <- atomically $ readTVar (top lfs)
  node <- return $ Nd e (top lfs)
  b <- atomCAS (top lfs) oldTop node
  if b
    then return ()
    else do
      backoff (backoffLFS lfs)
      loopTryPush lfs e


pushLFS:: Eq a => LockFreeStack a -> a -> IO ()
pushLFS lfs e = do loopTryPush lfs e

tryPop:: Eq a => LockFreeStack a -> IO (Node a)
tryPop lfs = do
  oldTop <- atomically $ readTVar (top lfs)
  -- TODO: throwNullException oldTop
  newTop <- atomically $ readTVar (next oldTop)
  b <- atomCAS (top lfs) oldTop newTop
  if b
    then return oldTop
    else return Null

loopTryPop:: Eq a => LockFreeStack a -> IO a
loopTryPop lfs = do
  returnNode <- tryPop lfs
  case returnNode of
    Nd v _ -> return v
    otherwise -> do
      backoff (backoffLFS lfs)
      loopTryPop lfs

popLFS:: Eq a => LockFreeStack a -> IO a
popLFS lfs = do loopTryPop lfs
