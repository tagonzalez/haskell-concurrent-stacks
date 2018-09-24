module LockFreeExchanger.LockFreeExchangerCASusingSTM where

import Data.IORef
import System.Clock
import Data.Time.Units
import Common.AtomCASusingSTM
import Common.State
import Common.Exceptions
import Control.Exception
import Control.Monad.Loops
import Control.Concurrent.STM

data LockFreeExchanger a = LFE {slot :: TVar (Maybe a, State)}

getSlot :: TVar (Maybe a, State) -> IORef State -> IO (Maybe a)
getSlot slot stampHolder = do
  (val, state) <- atomically $ readTVar slot
  writeIORef stampHolder state
  return val

newLockFreeExchanger :: IO (LockFreeExchanger a)
newLockFreeExchanger = (atomically $ newTVar (Nothing,EMPTY)) >>= return.LFE

exchange :: (Eq a) => LockFreeExchanger a -> Maybe a -> Integer -> IO (Maybe a)
exchange lfe myItem timeout = do
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
        yrItem <- getSlot (slot lfe) stampHolder
        stamp <- readIORef stampHolder
        case stamp of
          EMPTY -> do
            b <- atomCAS (slot lfe) (yrItem, EMPTY) (myItem, WAITING)
            if b
              then do
                whileM_ (emptyCaseLoopCondition ret timeBound) $ do
                  yrItem <- getSlot (slot lfe) stampHolder
                  stampBusy <- (readIORef stampHolder) >>= return.((==) BUSY)
                  if stampBusy
                    then do
                      atomically $ writeTVar (slot lfe) (Nothing, EMPTY)
                      writeIORef ret False
                      writeIORef res yrItem
                    else
                      return ()
                breakFromWhile <- readIORef ret >>= return.not
                if breakFromWhile
                  then return ()
                  else do
                    b <- atomCAS (slot lfe) (myItem, WAITING) (Nothing, EMPTY)
                    if b
                      then do
                        throw TimeoutException
                      else do
                        yrItem <- getSlot (slot lfe) stampHolder
                        atomically $ writeTVar (slot lfe) (Nothing, EMPTY)
                        writeIORef res yrItem
              else do
                return ()
          WAITING -> do
            b <- atomCAS (slot lfe) (yrItem, WAITING) (myItem, BUSY)
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

        systemNanoTime = (getTime Realtime) >>= return.toNanoSecs
