module LockFreeExchanger.LockFreeExchangerCASusingSTM where

import Control.Concurrent.STM
import System.Clock
import Data.Time.Units
import Common.AtomCASusingSTM
import Common.State
import Control.Exception
import Common.Exceptions

data LockFreeExchanger a = LFE {slot :: TVar (Maybe a, State)}

newLockFreeExchanger :: IO (LockFreeExchanger a)
newLockFreeExchanger = do
  value <- atomically $ newTVar (Nothing,EMPTY)
  return $ LFE value

emptyCaseLoop :: LockFreeExchanger a -> Integer -> State -> IO (Maybe a)
emptyCaseLoop lfe timeBound stampHolder = do
  sysTime <- getTime Realtime
  if (toNanoSecs sysTime) < timeBound
    then do
      (yrItem,stampHolder) <- atomically $ readTVar (slot lfe)
      if stampHolder == BUSY
        then do
          atomically $ writeTVar (slot lfe) (Nothing, EMPTY)
          return yrItem
        else
          emptyCaseLoop lfe timeBound stampHolder
      else return Nothing -- Irrelevant

exchangeLoop :: Eq a =>  LockFreeExchanger a -> Maybe a -> Integer -> State -> IO (Maybe a)
exchangeLoop lfe myItem timeBound stampHolder = do
  sysTime <- getTime Realtime
  if (toNanoSecs sysTime) > timeBound
    then
      throw TimeoutException
    else do
        (yrItem, stampHolder) <- atomically $ readTVar (slot lfe)
        stamp <- return stampHolder
        case stamp of
          EMPTY -> do
            b <- atomCAS (slot lfe) (yrItem, EMPTY) (myItem, WAITING)
            if b
              then do
                emptyCaseLoop lfe timeBound stampHolder
                b <- atomCAS (slot lfe) (myItem, WAITING) (Nothing, EMPTY)
                if b
                  then throw TimeoutException
                  else do
                    (yrItem, stampHolder) <- atomically $ readTVar (slot lfe)
                    atomically $ writeTVar (slot lfe) (Nothing, EMPTY)
                    return yrItem
              else exchangeLoop lfe myItem timeBound stampHolder
          WAITING -> do
            b <- atomCAS (slot lfe) (yrItem, WAITING) (myItem, BUSY)
            if b
              then return yrItem
              else exchangeLoop lfe myItem timeBound stampHolder
          BUSY -> exchangeLoop lfe myItem timeBound stampHolder

exchange :: (Eq a,TimeUnit b) => LockFreeExchanger a -> Maybe a -> b -> IO (Maybe a)
exchange lfe myItem timeout = do
  let nanos = fromIntegral ((convertUnit timeout) :: Nanosecond) :: Integer
  systemTime <- getTime Realtime
  let timeBound = (toNanoSecs (systemTime)) + nanos
  let stampHolder = EMPTY
  exchangeLoop lfe myItem timeBound stampHolder

test = do
  let stampHolder = 1
  let (algo,stampHolder) = (2,2)
  putStrLn (show stampHolder)
  putStrLn "Todo OK"