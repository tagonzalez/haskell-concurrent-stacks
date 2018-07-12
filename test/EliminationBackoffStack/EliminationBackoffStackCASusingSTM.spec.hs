module Main where

import EliminationBackoffStack.EliminationBackoffStackCASusingSTM
import UtilsSTM
import Test.HUnit
import Control.Concurrent.Async
import Control.Monad
import Data.List
import Data.IORef
import Control.Concurrent.STM

singleThreadTest = do
    ebs <- newEBS 10 100
    pushEBS ebs 3
    pushEBS ebs 4
    pushEBS ebs 5
    popEBS ebs
    popEBS ebs
    res <- (atomically $ readTVar (top ebs)) >>= nodesToListSTM
    [3] @=? res

multipleThreadTest = do
    ebs <- newEBS 10 100
    tid1 <- async (thread1 ebs)
    tid2 <- async (thread2 ebs)
    poppedItems <- mapM wait [tid1, tid2]
    remainingItems <- (atomically $ readTVar (top ebs)) >>= nodesToListSTM
    True @=? elem (poppedItems ++ remainingItems) (permutations [2..7])

    where thread1 ebs = do
            pushEBS ebs (3::Int)
            pushEBS ebs 5
            res <- popEBS ebs
            pushEBS ebs 7
            return res
          thread2 ebs = do
            pushEBS ebs 2
            pushEBS ebs 4
            pushEBS ebs 6
            popEBS ebs >>= return


-- Bootstrapping
allTests = test [
  "single thread test" ~: singleThreadTest,
  "multiple thread push test" ~: multipleThreadTest
  ]

main = do runTestTT allTests
