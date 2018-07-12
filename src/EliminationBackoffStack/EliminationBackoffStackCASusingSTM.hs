{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module EliminationBackoffStack.EliminationBackoffStackCASusingSTM where

import EliminationArray.EliminationArrayCASusingSTM
import Common.RangePolicy
import Common.NodeSTM
import Common.AtomCASusingSTM
import Control.Exception
import Common.Exceptions
import Data.IORef
import Control.Monad.Loops
import Data.Maybe
-- import Utils
import Common.Backoff
import Data.TLS.GHC
import Control.Concurrent.STM

data EliminationBackoffStack a = EBS {top :: TVar (Node a), capacity :: Int, eliminationArray :: EliminationArray a, policy :: TLS RangePolicy}

newEBS :: Int -> Integer ->  IO (EliminationBackoffStack a)
newEBS capacity duration = do
  nullRef <- atomically $ newTVar Null
  let maxRange = capacity - 1 -- Last accessible position within the elimination array (0-based index)
  rgPcy <- mkTLS $ newRangePolicy maxRange -- range policy must be thread local according to Shavit
  elArr <- newEliminationArray capacity duration
  return $ EBS nullRef capacity elArr rgPcy

tryPush :: Eq a => EliminationBackoffStack a -> Node a -> IO Bool
tryPush ebs node = do
  oldTop <- atomically $ readTVar (top ebs)
  atomically $ writeTVar (next node) oldTop
  atomCAS (top ebs) oldTop node

pushEBS :: Eq a => EliminationBackoffStack a -> a -> IO ()
pushEBS ebs value = do
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  node <- newNode value
  whileM_ (readIORef ret) $ do
    b <- tryPush ebs node
    if b
      then writeIORef ret False
      else (catch (tryExchangePush ebs node value range ret rangePolicy) $ \( e :: TimeoutException) -> do
          recordEliminationTimeout rangePolicy)

  where tryExchangePush ebs node value range ret rangePolicy = do
          otherValue <- visit (eliminationArray ebs) (Just value) range
          if otherValue == Nothing
            then do
              recordEliminationSuccess rangePolicy
              writeIORef ret False
            else return ()

tryPop :: Eq a => EliminationBackoffStack a -> IO (Node a)
tryPop ebs = do
  oldTop <- atomically $ readTVar (top ebs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- atomically $ readTVar (next oldTop)
      b <- atomCAS (top ebs) oldTop newTop
      if b
        then return oldTop
        else return Null

popEBS :: Eq a => EliminationBackoffStack a -> IO a
popEBS ebs = do
  res <- newIORef Nothing
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  whileM_ (readIORef ret) $ do
    returnNode <- tryPop ebs
    if returnNode /= Null
      then do
        writeIORef res $ Just (val returnNode)
        writeIORef ret False
      else (catch (exchangePop ebs range ret res rangePolicy) $ \( e :: TimeoutException) -> do
          recordEliminationTimeout rangePolicy)
  readIORef res >>= return.fromJust

  where exchangePop ebs range ret res rangePolicy = do
          otherValue <- visit (eliminationArray ebs) Nothing range
          case otherValue of
            Just v -> do
              recordEliminationSuccess rangePolicy
              writeIORef res (Just v)
              writeIORef ret False
            otherwise -> return ()
