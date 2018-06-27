{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import LockFreeStack.LockFreeStackCAS
import Common.Node
import System.Random
import Data.IORef
import Test.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Exception
import Common.Exceptions
import Utils

pushThreadAction lfs iterations = do
  if iterations > 0
    then do
      elem <- randomIO :: IO Int
      pushLFS lfs elem
    else return ()

popThreadAction lfs iterations = do
  if iterations > 0
    then do
      _ <- popLFS lfs
      return ()
    else return ()

callPushes stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            e <- randomIO :: IO Int
            pushLFS stack e
            callPushes stack (iterations - 1)
        else do
            return ()

callPops stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            _ <- catch (popLFS stack) (\(e :: EmptyException) -> do
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
  lfs <- newLFS 100 100
  pushListElems lfs (reverse xs)
  return lfs
  where pushListElems lfs list =
          case list of
            [] -> return ()
            (x:xs) -> do
              pushLFS lfs x
              pushListElems lfs xs

-- Tests
listToLFSAndBackTest = do
  lfs <- lfsFromList [1,2,3,4,5]
  list <- readIORef (top lfs) >>= nodesToListIO
  [1,2,3,4,5] @=? list

singleThreadTest = do
  lfs <- newLFS 100 100

  pushLFS lfs 5
  pushLFS lfs 4
  list <- readIORef (top lfs) >>= nodesToListIO
  [4,5] @=? list

  pushLFS lfs 9
  list <- readIORef (top lfs) >>= nodesToListIO
  [9,4,5] @=? list

  _ <-popLFS lfs
  list <- readIORef (top lfs) >>= nodesToListIO
  [4,5] @=? list

  _ <-popLFS lfs
  list <- readIORef (top lfs) >>= nodesToListIO
  [5] @=? list

multipleThreadPushTest = do
  lfs <- newLFS 100 100
  createThreads lfs 8 8 100 []
  list <- readIORef (top lfs) >>= nodesToListIO
  800 @=? length list

multipleThreadPopTest = do
  lfs <- newLFS 100 100
  repeatIO 1000 $ pushLFS lfs 2
  createThreads lfs 8 0 100 []
  list <- readIORef (top lfs) >>= nodesToListIO
  200 @=? length list

-- Bootstrapping
allTests = test [
  "LFS from list and back" ~: listToLFSAndBackTest,
  "Single thread" ~: singleThreadTest,
  "Multiple thread pops" ~: multipleThreadPopTest,
  "Multiple thread push" ~: multipleThreadPushTest
  ]

main = do runTestTT allTests
