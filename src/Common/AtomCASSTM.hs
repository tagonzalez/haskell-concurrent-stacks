module Common.AtomCASSTM where
import Control.Concurrent.STM

atomCASSTM :: Eq a => TVar a -> a -> a -> IO Bool
atomCASSTM ptr old new = atomically $ do
  cur <- readTVar ptr
  if cur == old
  then do
    writeTVar ptr new
    return True
  else return False
