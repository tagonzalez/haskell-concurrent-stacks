module Common.NodeSTM where
import Control.Concurrent.STM

data Node a = Nd { val :: a, next :: TVar (Node a) } | Null

instance Eq a => Eq (Node a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2
  Nd _ _ == Null = False
  Null == Null = True
  Null == Nd _ _ = False

newNode :: a -> IO (Node a)
newNode val = do
  nullRef <- atomically $ newTVar Null
  return $ Nd val nullRef