module Main where
import Backoff
import Data.IORef
import LockFreeStackCASusingSTM
import Control.Monad.Loops
import Control.Concurrent.Async
import Control.Concurrent.STM
import System.Random
import System.Environment
import Text.Printf
import System.TimeIt


callPushes lfs iterations =
    case iterations of
        0 -> do
            -- printf "Thread finished!\n"
            return ()
        otherwise -> do
            e <- randomIO :: IO Int
            pushLFS lfs e
            callPushes lfs (iterations - 1)

callPops lfs iterations =
    case iterations of
        0 -> do
            -- printf "Thread finished!\n"
            return ()
        otherwise -> do
            e <- randomIO :: IO Int
            pushLFS lfs e
            res <- popLFS lfs
            callPops lfs (iterations - 1)

randomPushesAndPops lfs iterations push =
    if push
        then callPushes lfs iterations
        else callPops lfs iterations

createThreads lfs threadCount iterations tids = do
    case threadCount of
        0 -> mapM_ wait tids
        otherwise -> do
            -- printf "Creating thread %s \n" (show threadCount)
            tid <- async (randomPushesAndPops lfs iterations (mod threadCount 2 == 0))
            createThreads lfs (threadCount - 1) iterations (tid:tids)

createLFS min max limit = do
    lim <- newIORef limit
    bck <- return $ BCK min max lim
    null <- atomically(newTVar Null)
    return $ LFS null bck

main = do
    -- Parse arguments
    args <- getArgs
    min <- readIO (args !! 0) :: IO Int
    max <- readIO (args !! 1) :: IO Int
    limit <- readIO (args !! 2) :: IO Int
    threadCount <- readIO (args !! 3) :: IO Int
    iterations <- readIO (args !! 4) :: IO Int
    -- Create shared LockFreeStack and threads
    lfs <- createLFS min max limit
    timeIt $ createThreads lfs threadCount iterations []
    return ()
