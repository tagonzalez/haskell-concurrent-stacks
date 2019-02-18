{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import StackSTM.StackSTM

import Common.NodeIO
import System.Random
import Data.IORef
import Test.HUnit
import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Common.Exceptions
import TestUtils

pushThreadAction stack iterations = do
  if iterations > 0
    then do
      elem <- randomIO :: IO Int
      pushStackSTM stack elem
    else return ()

popThreadAction stack iterations = do
  if iterations > 0
    then do
      _ <- popStackSTM stack
      return ()
    else return ()

callPushes stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            e <- randomIO :: IO Int
            pushStackSTM stack e
            callPushes stack (iterations - 1)
        else do
            return ()

callPops stack iterations = do
    mytid <- myThreadId
    if iterations > 0
        then do
            _ <- catch (popStackSTM stack) (\(e :: EmptyException) -> do
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

stackFromList xs = do
  stack <- newStackSTM
  pushListElems stack (reverse xs)
  return stack
  where pushListElems stack list =
          case list of
            [] -> return ()
            (x:xs) -> do
              pushStackSTM stack x
              pushListElems stack xs

-- Tests
listToStackAndBackTest = do
  stack <- stackFromList [1,2,3,4,5]
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  [1,2,3,4,5] @=? list

singleThreadTest = do
  stack <- newStackSTM

  pushStackSTM stack 5
  pushStackSTM stack 4
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  [4,5] @=? list

  pushStackSTM stack 9
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  [9,4,5] @=? list

  _ <-popStackSTM stack
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  [4,5] @=? list

  _ <-popStackSTM stack
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  [5] @=? list

multipleThreadPushTest = do
  stack <- newStackSTM
  createThreads stack 8 8 100 []
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  800 @=? length list

multipleThreadPopTest = do
  stack <- newStackSTM
  repeatIO 1000 $ pushStackSTM stack 2
  createThreads stack 8 0 100 []
  list <- (atomically (readTVar $ top stack)) >>= nodesToListSTM
  200 @=? length list

-- Bootstrapping
allTests = test [
  "stack from list and back" ~: listToStackAndBackTest,
  "Single thread" ~: singleThreadTest,
  "Multiple thread pops" ~: multipleThreadPopTest,
  "Multiple thread push" ~: multipleThreadPushTest
  ]

main = do runTestTT allTests
