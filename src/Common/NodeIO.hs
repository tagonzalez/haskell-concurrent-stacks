module Common.NodeIO where
import Data.IORef

data NodeIO a = NdIO { val :: a, next :: IORef (NodeIO a) } | Null

instance Eq a => Eq (NodeIO a) where
  Nd v1 n1 == Nd v2 n2 = v1 == v2 && n1 == n2
  Nd _ _ == Null = False
  Null == Null = True
  Null == Nd _ _ = False

newNodeIO :: a -> IO (NodeIO a)
newNodeIO val = do
  nullRef <- newIORef Null
  return $ Nd val nullRef