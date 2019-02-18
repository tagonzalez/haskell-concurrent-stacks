{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import LockFreeStack.LockFreeStackSTM
import Common.NodeSTM
import Control.Monad.STM
import System.Random
import Data.IORef
import Test.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Common.Exceptions
import TestUtils
import Control.Concurrent.STM

pushThreadAction lfs iterations = do
  if iterations > 0
    then do
      elem <- randomIO :: IO Int
      pushLFSSTM lfs elem
    else return ()

popThreadAction lfs iterations = do
  if iterations > 0
    then do
      _ <- popLFSSTM lfs
      return ()
    else return ()

callPushes stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            e <- randomIO :: IO Int
            pushLFSSTM stack e
            callPushes stack (iterations - 1)
        else do
            return ()

callPops stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            _ <- catch (popLFSSTM stack) (\(e :: EmptyException) -> do
              putStrLn "Exception!"
              return 1)
            callPops stack (iterations - 1)
        else do
            return ()

threadAction isPushThread = if isPushThread then callPushes else callPops

createThreads stack threadCount pushThreadCount iterations tids = do
    let isPushThread = if pushThreadCount > 0 then True else False
    if threadCount > 0
        then do
            tid <- async (threadAction isPushThread stack iterations)
            createThreads stack (threadCount - 1) (pushThreadCount - 1) iterations (tid : tids)
        else
            mapM_ wait tids

lfsFromList xs = do
  lfs <- newLFSSTM 100 100
  pushListElems lfs (reverse xs)
  return lfs
  where pushListElems lfs list =
          case list of
            [] -> return ()
            (x:xs) -> do
              pushLFSSTM lfs x
              pushListElems lfs xs

-- Tests
listToLFSAndBackTest = do
  lfs <- lfsFromList [1,2,3,4,5]
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  [1,2,3,4,5] @=? list

singleThreadTest = do
  lfs <- newLFSSTM 100 100

  pushLFSSTM lfs 5
  pushLFSSTM lfs 4
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  [4,5] @=? list

  pushLFSSTM lfs 9
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  [9,4,5] @=? list

  _ <-popLFSSTM lfs
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  [4,5] @=? list

  _ <-popLFSSTM lfs
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  [5] @=? list

multipleThreadPushTest = do
  lfs <- newLFSSTM 100 100
  createThreads lfs 8 8 100 []
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  800 @=? length list

multipleThreadPopTest = do
  lfs <- newLFSSTM 100 100
  repeatIO 1000 $ pushLFSSTM lfs 2
  createThreads lfs 8 0 100 []
  list <- (atomically $ readTVar (top lfs)) >>= nodesToListSTM
  200 @=? length list

-- Bootstrapping
allTests = test [
  "LFS from list and back" ~: listToLFSAndBackTest,
  "Single thread" ~: singleThreadTest,
  "Multiple thread pops" ~: multipleThreadPopTest,
  "Multiple thread push" ~: multipleThreadPushTest
  ]

main = do runTestTT allTests
