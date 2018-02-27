module LockFreeStackSTM where
import Control.Concurrent.STM
import Backoff

data Node a = Nd { val :: a, next :: TVar (Node a) } | Null

instance Eq a => Eq (Node a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2

data LockFreeStack a = LFS { top :: TVar (Node a), backoffLFS :: Backoff }

tryPush:: LockFreeStack a -> a -> STM ()
tryPush lfs e = do
    node <- return $ Nd e (top lfs)
    writeTVar (top lfs) node

pushLFS:: LockFreeStack a -> a -> IO ()
pushLFS lfs e = do atomically(tryPush lfs e)

tryPop:: LockFreeStack a -> STM a
tryPop lfs = do
    resNode <- readTVar (top lfs)
    case resNode of
        Nd v nxt -> do
            newTop <- readTVar nxt
            writeTVar (top lfs) newTop
            return v
        -- otherwise -> throwException

popLFS:: LockFreeStack a -> IO a
popLFS lfs = do atomically(tryPop lfs)