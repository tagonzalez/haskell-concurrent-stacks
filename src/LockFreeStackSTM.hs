module LockFreeStackSTM where

import Data.IORef
import Control.Concurrent.STM
import Backoff
import NodeSTM

data LockFreeStack a = LFS { top :: TVar (Node a), backoffLFS :: Backoff }

createLFS :: Int -> Int -> Int -> IO (LockFreeStack a)
createLFS min max limit = do
    lim <- newIORef limit
    bck <- return $ BCK min max lim
    null <- atomically $ newTVar Null
    return $ LFS null bck

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
        Null -> error "Empty!"

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

main = do
  lfs <- createLFS 1 1 1
  pushLFS lfs 1
  pushLFS lfs 2
  thingy <- popLFS lfs
  putStrLn $ show thingy
  res <- lfsToListIO lfs
  putStrLn (show res)
