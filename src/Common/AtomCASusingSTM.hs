module Common.AtomCASusingSTM where
import Control.Concurrent.STM

atomCAS :: Eq a => TVar a -> a -> a -> IO Bool
atomCAS ptr old new = atomically $ do
  cur <- readTVar ptr
  if cur == old
  then do
    writeTVar ptr new
    return True
  else return False
