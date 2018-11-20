module Common.NodeSTM where
import Control.Concurrent.STM

data NodeSTM a = Nd { val :: a, next :: TVar (NodeSTM a) } | Null

instance Eq a => Eq (NodeSTM a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2
  Nd _ _ == Null = False
  Null == Null = True
  Null == Nd _ _ = False

newNodeSTM :: a -> IO (NodeSTM a)
newNodeSTM val = do
  nullRef <- atomically $ newTVar Null
  return $ Nd val nullRef