module Main where

import EliminationBackoffStack.EliminationBackoffStackSTM
import TestUtils
import Test.HUnit
import Control.Concurrent.Async
import Control.Monad
import Data.List
import Data.IORef
import Control.Concurrent.STM

singleThreadTest = do
    ebs <- newEBSSTM 10 100
    pushEBSSTM ebs 3
    pushEBSSTM ebs 4
    pushEBSSTM ebs 5
    popEBSSTM ebs
    popEBSSTM ebs
    res <- (atomically $ readTVar (top ebs)) >>= nodesToListSTM
    [3] @=? res

multipleThreadTest = do
    ebs <- newEBSSTM 10 100
    tid1 <- async (thread1 ebs)
    tid2 <- async (thread2 ebs)
    poppedItems <- mapM wait [tid1, tid2]
    remainingItems <- (atomically $ readTVar (top ebs)) >>= nodesToListSTM
    True @=? elem (poppedItems ++ remainingItems) (permutations [2..7])

    where thread1 ebs = do
            pushEBSSTM ebs (3::Int)
            pushEBSSTM ebs 5
            res <- popEBSSTM ebs
            pushEBSSTM ebs 7
            return res
          thread2 ebs = do
            pushEBSSTM ebs 2
            pushEBSSTM ebs 4
            pushEBSSTM ebs 6
            popEBSSTM ebs >>= return


-- Bootstrapping
allTests = test [
  "single thread test" ~: singleThreadTest,
  "multiple thread push test" ~: multipleThreadTest
  ]

main = do runTestTT allTests
