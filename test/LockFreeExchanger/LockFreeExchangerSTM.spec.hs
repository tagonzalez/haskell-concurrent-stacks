{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import LockFreeExchanger.LockFreeExchangerSTM
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import Data.Maybe
import Test.HUnit
import Control.Concurrent
import System.CPUTime
import System.Random
import System.Clock
import Control.Exception
import Common.Exceptions

exchangeTest = do
    lfe <- newLockFreeExchangerSTM
    tid1 <- async (thread1 lfe)
    tid2 <- async (thread2 lfe)
    res <- mapM wait [tid1, tid2]
    2 @=? (res !! 0)
    3 @=? (res !! 1)

    where thread1 lfe = do
            myTid <- myThreadId
            res <- exchangeSTM lfe (Just (3 :: Int)) 10000
            return $ fromJust res

          thread2 lfe = do
            myTid <- myThreadId
            res <- exchangeSTM lfe (Just (2 :: Int)) 10000
            return $ fromJust res

timeoutTest = do
    lfe <- newLockFreeExchangerSTM
    randTimeout <- randomRIO (100,1000) :: IO Integer
    timeBefore <- (getTime Realtime) >>= return.toNanoSecs
    res <- catch (exchangeSTM lfe (Just (4 :: Int)) randTimeout) (\(e::TimeoutException)-> return $ Just 50)
    timeAfter <- (getTime Realtime) >>= return.toNanoSecs
    let execTimeInMillis = quot (timeAfter - timeBefore) (10 ^ 6)
    True @=? execTimeInMillis >= (randTimeout)
    Just 50 @=? res

-- Bootstrapping
allTests = test [
  "exchange test" ~: exchangeTest,
  "timeout test" ~: timeoutTest
  ]

main = do runTestTT allTests
