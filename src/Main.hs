module Main where
import LockFreeStackCAS
import Backoff
import Data.IORef

main = do
    limit <- newIORef 6
    bck <- return $ BCK 100 100 limit
    null <- newIORef Null
    lfs <- return $ LFS null bck
    return ()