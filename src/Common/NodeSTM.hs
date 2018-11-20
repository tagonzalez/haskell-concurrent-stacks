module Common.NodeSTM where
import Control.Concurrent.STM

data NodeSTM a = NdSTM { val :: a, next :: TVar (NodeSTM a) } | Null

instance Eq a => Eq (NodeSTM a) where
  NdSTM v1 n1 == NdSTM v2 n2 = v1 == v2 && n1 == n2
  NdSTM _ _ == Null = False
  Null == Null = True
  Null == NdSTM _ _ = False

newNodeSTM :: a -> IO (NodeSTM a)
newNodeSTM val = do
  nullRef <- atomically $ newTVar Null
  return $ NdSTM val nullRef