module AtomCAS where
import Data.IORef

atomCAS :: Eq a => IORef a -> a -> a -> IO Bool
atomCAS ptr old new =
   atomicModifyIORef ptr (\ cur -> if cur == old
                                   then (new, True)
                                   else (cur, False))

atomicReadIORef :: IORef a -> IO a
atomicReadIORef = (flip atomicModifyIORef) (\x -> (x,x))
