{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import Control.Concurrent.Async
import System.Random
import System.Environment
import Control.Exception
import Common.Exceptions
import ExperimentUtils

import LockFreeStack.LockFreeStackIO

callPushes stack operationCount = do
    if operationCount > 0
        then do
            (randomIO :: IO Int) >>= pushLFSIO stack
            callPushes stack (operationCount - 1)
        else do
            return ()

callPops stack operationCount = do
    if operationCount > 0
        then do
            _ <- catch (popLFSIO stack) (\(e :: EmptyException) -> return 1)
            callPops stack (operationCount - 1)
        else do
            return ()

threadAction isPushThread = if isPushThread then callPushes else callPops

createThreads stack threadCount pushThreadCount operationCount tids = do
    let isPushThread = if pushThreadCount > 0 then True else False
    if threadCount > 0
        then do
            tid <- async (threadAction isPushThread stack operationCount)
            createThreads stack (threadCount - 1) (pushThreadCount - 1) operationCount (tid : tids)
        else
            mapM_ wait tids

parseCommandLineArguments args = do
    -- Get arguments from command line
    min <- readIO (args !! 0) :: IO Int
    max <- readIO (args !! 1) :: IO Int
    operationCount <- readIO (args !! 2) :: IO Int
    pushPercentage <- readIO (args !! 3) :: IO Float
    threadCount <- readIO (args !! 4) :: IO Int
    distributeOperations <- readIO (args !! 5) :: IO Bool
    -- Distribute operationCount
    operationCount <- return $ if distributeOperations then quot operationCount threadCount else operationCount
    -- Create stack and calculate amount of writer/push threads
    stack <- newLFSIO min max
    let pushThreadCount = floor $ (fromIntegral threadCount :: Float) * pushPercentage
    return (stack, threadCount, operationCount, pushThreadCount)

main = do
    args <- getArgs
    (stack, threadCount, operationCount, pushThreadCount) <- parseCommandLineArguments args
    callPushes stack 1000000
    timeExperiment $ createThreads stack threadCount pushThreadCount operationCount []
