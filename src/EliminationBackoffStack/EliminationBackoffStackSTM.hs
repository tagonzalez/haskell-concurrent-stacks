{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module EliminationBackoffStack.EliminationBackoffStackSTM where

import EliminationArray.EliminationArraySTM
import Common.RangePolicy
import Common.NodeSTM
import Common.AtomCASSTM
import Control.Exception
import Common.Exceptions
import Data.IORef
import Control.Monad.Loops
import Data.Maybe
import Common.Backoff
import Data.TLS.GHC
import Control.Concurrent.STM

data EliminationBackoffStack a = EBS {top :: TVar (NodeSTM a), capacity :: Int, eliminationArray :: EliminationArraySTM a, policy :: TLS RangePolicy}

newEBSSTM :: Int -> Integer ->  IO (EliminationBackoffStack a)
newEBSSTM capacity duration = do
  nullRef <- atomically $ newTVar Null
  let maxRange = capacity - 1 -- Last accessible position within the elimination array (0-based index)
  rgPcy <- mkTLS $ newRangePolicy maxRange -- range policy must be thread local according to Shavit
  elArr <- newEliminationArraySTM capacity duration
  return $ EBS nullRef capacity elArr rgPcy

tryPushSTM :: Eq a => EliminationBackoffStack a -> NodeSTM a -> IO Bool
tryPushSTM ebs node = do
  oldTop <- atomically $ readTVar (top ebs)
  atomically $ writeTVar (next node) oldTop
  atomCASSTM (top ebs) oldTop node

pushEBSSTM :: Eq a => EliminationBackoffStack a -> a -> IO ()
pushEBSSTM ebs value = do
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  node <- newNodeSTM value
  whileM_ (readIORef ret) $ do
    b <- tryPushSTM ebs node
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

tryPopSTM :: Eq a => EliminationBackoffStack a -> IO (NodeSTM a)
tryPopSTM ebs = do
  oldTop <- atomically $ readTVar (top ebs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- atomically $ readTVar (next oldTop)
      b <- atomCASSTM (top ebs) oldTop newTop
      if b
        then return oldTop
        else return Null

popEBSSTM :: Eq a => EliminationBackoffStack a -> IO a
popEBSSTM ebs = do
  res <- newIORef Nothing
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  whileM_ (readIORef ret) $ do
    returnNode <- tryPopSTM ebs
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
