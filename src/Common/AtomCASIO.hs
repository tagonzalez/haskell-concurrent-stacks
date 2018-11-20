module Common.AtomCASIO where
import Data.IORef

atomCASIO :: Eq a => IORef a -> a -> a -> IO Bool
atomCASIO ptr old new =
  atomicModifyIORef ptr (\ cur -> if cur == old
                                  then (new, True)
                                  else (cur, False))
