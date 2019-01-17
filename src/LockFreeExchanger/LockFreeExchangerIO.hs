module LockFreeExchanger.LockFreeExchangerIO where

import Data.IORef
import System.Clock
import Common.AtomCASIO
import Common.State
import Common.Exceptions
import Control.Exception
import Control.Monad.Loops
import Control.Concurrent

data LockFreeExchangerIO a = LFEIO {slot :: IORef (Maybe a, State)}

newLockFreeExchangerIO :: IO (LockFreeExchangerIO a)
newLockFreeExchangerIO = (newIORef (Nothing, EMPTY)) >>= return.LFEIO

getSlotIO :: IORef (Maybe a, State) -> IORef State -> IO (Maybe a)
getSlotIO slot stampHolder = do
  (val, state) <- readIORef slot
  writeIORef stampHolder state
  return val


exchangeIO :: (Eq a) => LockFreeExchangerIO a -> Maybe a -> Integer -> IO (Maybe a)
exchangeIO lfe myItem timeout = do
  ret <- newIORef True
  res <- newIORef Nothing
  let nanos = timeout * (10 ^ 6) -- timeout unit is millisecs
  timeBound <- systemNanoTime >>= return.((+) nanos)
  stampHolder <- newIORef EMPTY
  whileM_ (readIORef ret) $ do
    timeoutDone <- systemNanoTime >>= return.((<) timeBound)
    if timeoutDone
      then do
        throw TimeoutException
      else do
        yrItem <- getSlotIO (slot lfe) stampHolder
        stamp <- readIORef stampHolder
        case stamp of
          EMPTY -> do
            b <- atomCASIO (slot lfe) (yrItem, EMPTY) (myItem, WAITING)
            if b
              then do
                whileM_ (emptyCaseLoopCondition ret timeBound) $ do
                  yrItem <- getSlotIO (slot lfe) stampHolder
                  stampBusy <- (readIORef stampHolder) >>= return.((==) BUSY)
                  if stampBusy
                    then do
                      writeIORef (slot lfe) (Nothing, EMPTY)
                      writeIORef ret False
                      writeIORef res yrItem
                    else
                      return ()
                breakFromWhile <- readIORef ret >>= return.not
                if breakFromWhile
                  then return ()
                  else do
                    b <- atomCASIO (slot lfe) (myItem, WAITING) (Nothing, EMPTY)
                    if b
                      then do
                        throw TimeoutException
                      else do
                        yrItem <- getSlotIO (slot lfe) stampHolder
                        writeIORef (slot lfe) (Nothing, EMPTY)
                        writeIORef res yrItem
              else do
                return ()
          WAITING -> do
            b <- atomCASIO (slot lfe) (yrItem, WAITING) (myItem, BUSY)
            if b
              then do
                writeIORef ret False
                writeIORef res yrItem
              else
                return ()
          BUSY -> do
            return ()
  readIORef res

  where emptyCaseLoopCondition ret timeBound = do
          timeoutNotDone <- systemNanoTime >>= return.((>) timeBound)
          (readIORef ret) >>= return.((&&) timeoutNotDone)

        systemNanoTime = (getTime Monotonic) >>= return.toNanoSecs
