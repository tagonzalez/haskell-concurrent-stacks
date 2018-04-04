module LockFreeExchangerCAS where

import Data.IORef
import System.Clock
import Data.Time.Units
import AtomCAS
import State

data LockFreeExchanger a = LFE {slot :: IORef (Maybe a, State)}

newLockFreeExchanger :: IO (LockFreeExchanger a)
newLockFreeExchanger = do
    value <- newIORef (Nothing,EMPTY)
    return $ LFE value

emptyCaseLoop :: LockFreeExchanger a -> Integer -> State -> IO (Maybe a)
emptyCaseLoop lfe timeBound stampHolder = do
    sysTime <- getTime Realtime
    if (toNanoSecs sysTime) < timeBound
        then do
            (yrItem,stampHolder) <- atomicReadIORef $ slot lfe
            if stampHolder == BUSY
                then do
                    writeIORef (slot lfe) (Nothing, EMPTY)
                    return yrItem
                else
                    emptyCaseLoop lfe timeBound stampHolder
        else return Nothing -- Irrelevant

exchangeLoop :: Eq a =>  LockFreeExchanger a -> Maybe a -> Integer -> State -> IO (Maybe a)
exchangeLoop lfe myItem timeBound stampHolder = do
    sysTime <- getTime Realtime
    if (toNanoSecs sysTime) > timeBound
        then
            error "Timeout!"
        else do
            (yrItem, stampHolder) <- atomicReadIORef $ slot lfe
            stamp <- return stampHolder
            case stamp of
                EMPTY -> do
                    b <- atomCAS (slot lfe) (yrItem, EMPTY) (myItem, WAITING)
                    if b
                        then do
                            emptyCaseLoop lfe timeBound stampHolder
                            b <- atomCAS (slot lfe) (myItem, WAITING) (Nothing, EMPTY)
                            if b
                                then
                                    error "Timeout!"
                                else do
                                    (yrItem, stampHolder) <- atomicReadIORef $ slot lfe
                                    atomicWriteIORef (slot lfe) (Nothing, EMPTY)
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
    nanos <- return (fromIntegral ((convertUnit timeout) :: Nanosecond) :: Integer)
    systemTime <- getTime Realtime
    timeBound <- return $ (toNanoSecs (systemTime)) + nanos
    stampHolder <- return $ EMPTY
    exchangeLoop lfe myItem timeBound stampHolder

main = do
    stampHolder <- return 1
    (algo,stampHolder) <- return (2,2)
    putStrLn (show stampHolder)
    putStrLn "Todo OK"