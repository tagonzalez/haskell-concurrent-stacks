module Main where

import EliminationBackoffStack.EliminationBackoffStackIO
import Utils
import Test.HUnit
import Control.Concurrent.Async
import Control.Monad
import Data.List
import Data.IORef

singleThreadTest = do
    ebs <- newEBSIO 10 100
    pushEBSIO ebs 3
    pushEBSIO ebs 4
    pushEBSIO ebs 5
    popEBSIO ebs
    popEBSIO ebs
    res <- readIORef (top ebs) >>= nodesToListIO
    [3] @=? res

multipleThreadTest = do
    ebs <- newEBSIO 10 100
    tid1 <- async (thread1 ebs)
    tid2 <- async (thread2 ebs)
    poppedItems <- mapM wait [tid1, tid2]
    remainingItems <- readIORef (top ebs) >>= nodesToListIO
    True @=? elem (poppedItems ++ remainingItems) (permutations [2..7])

    where thread1 ebs = do
            pushEBSIO ebs (3::Int)
            pushEBSIO ebs 5
            res <- popEBSIO ebs
            pushEBSIO ebs 7
            return res
          thread2 ebs = do
            pushEBSIO ebs 2
            pushEBSIO ebs 4
            pushEBSIO ebs 6
            popEBSIO ebs >>= return


-- Bootstrapping
allTests = test [
  "single thread test" ~: singleThreadTest,
  "multiple thread push test" ~: multipleThreadTest
  ]

main = do runTestTT allTests
