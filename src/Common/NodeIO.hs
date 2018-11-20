module Common.NodeIO where
import Data.IORef

data NodeIO a = NdIO { val :: a, next :: IORef (NodeIO a) } | Null

instance Eq a => Eq (NodeIO a) where
  NdIO v1 n1 == NdIO v2 n2 = v1 == v2 && n1 == n2
  NdIO _ _ == Null = False
  Null == Null = True
  Null == NdIO _ _ = False

newNodeIO :: a -> IO (NodeIO a)
newNodeIO val = do
  nullRef <- newIORef Null
  return $ NdIO val nullRef