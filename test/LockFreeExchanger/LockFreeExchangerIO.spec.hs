{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module Main where

import LockFreeExchanger.LockFreeExchangerIO
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
    lfe <- newLockFreeExchangerIO
    tid1 <- async (thread1 lfe)
    tid2 <- async (thread2 lfe)
    res <- mapM wait [tid1, tid2]
    2 @=? (res !! 0)
    3 @=? (res !! 1)

    where thread1 lfe = do
            myTid <- myThreadId
            res <- exchangeIO lfe (Just (3 :: Int)) 10000
            return $ fromJust res

          thread2 lfe = do
            myTid <- myThreadId
            res <- exchangeIO lfe (Just (2 :: Int)) 10000
            return $ fromJust res

timeoutTest = do
    lfe <- newLockFreeExchangerIO
    randTimeout <- randomRIO (100,1000) :: IO Integer
    timeBefore <- (getTime Realtime) >>= return.toNanoSecs
    res <- catch (exchangeIO lfe (Just (4 :: Int)) randTimeout) (\(e::TimeoutException)-> return $ Just 50)
    timeAfter <- (getTime Realtime) >>= return.toNanoSecs
    let execTimeInMillis = quot (timeAfter - timeBefore) (10 ^ 6)
    True @=? execTimeInMillis >= (randTimeout)
    Just 50 @=? res

-- Bootstrapping
allTests = test [
  "exchangeIO test" ~: exchangeTest,
  "timeout test" ~: timeoutTest
  ]

main = do runTestTT allTests
