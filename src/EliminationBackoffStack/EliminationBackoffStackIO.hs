{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module EliminationBackoffStack.EliminationBackoffStackIO where

import EliminationArray.EliminationArrayIO
import Common.RangePolicy
import Common.NodeIO
import Common.AtomCASIO
import Control.Exception
import Common.Exceptions
import Data.IORef
import Control.Monad.Loops
import Data.Maybe
import Common.Backoff
import Data.TLS.GHC

data EliminationBackoffStackIO a = EBSIO {top :: IORef (NodeIO a), capacity :: Int, eliminationArray :: EliminationArrayIO a, policy :: TLS RangePolicy}

newEBSIO :: Int -> Integer ->  IO (EliminationBackoffStackIO a)
newEBSIO capacity duration = do
  nullRef <- newIORef Null
  let maxRange = capacity - 1 -- Last accessible position within the elimination array (0-based index)
  rgPcy <- mkTLS $ newRangePolicy maxRange -- range policy must be thread local according to Shavit
  elArr <- newEliminationArrayIO capacity duration
  return $ EBSIO nullRef capacity elArr rgPcy

tryPushIO :: Eq a => EliminationBackoffStackIO a -> NodeIO a -> IO Bool
tryPushIO ebs node = do
  oldTop <- readIORef (top ebs)
  writeIORef (next node) oldTop
  atomCASIO (top ebs) oldTop node

pushEBSIO :: Eq a => EliminationBackoffStackIO a -> a -> IO ()
pushEBSIO ebs value = do
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  node <- newNodeIO value
  whileM_ (readIORef ret) $ do
    b <- tryPushIO ebs node
    if b
      then writeIORef ret False
      else (catch (tryExchangePush ebs node value range ret rangePolicy) $ \( e :: TimeoutException) -> do
          recordEliminationTimeout rangePolicy)

  where tryExchangePush ebs node value range ret rangePolicy = do
          otherValue <- visitIO (eliminationArray ebs) (Just value) range
          if otherValue == Nothing
            then do
              recordEliminationSuccess rangePolicy
              writeIORef ret False
            else return ()

tryPopIO :: Eq a => EliminationBackoffStackIO a -> IO (NodeIO a)
tryPopIO ebs = do
  oldTop <- readIORef (top ebs)
  if oldTop == Null
    then
      throw EmptyException
    else do
      newTop <- readIORef (next oldTop)
      b <- atomCASIO (top ebs) oldTop newTop
      if b
        then return oldTop
        else return Null

popEBSIO :: Eq a => EliminationBackoffStackIO a -> IO a
popEBSIO ebs = do
  res <- newIORef Nothing
  ret <- newIORef True
  rangePolicy <- getTLS (policy ebs)

  range <- getRange rangePolicy
  whileM_ (readIORef ret) $ do
    returnNode <- tryPopIO ebs
    if returnNode /= Null
      then do
        writeIORef res $ Just (val returnNode)
        writeIORef ret False
      else (catch (exchangePop ebs range ret res rangePolicy) $ \( e :: TimeoutException) -> do
          recordEliminationTimeout rangePolicy)
  readIORef res >>= return.fromJust

  where exchangePop ebs range ret res rangePolicy = do
          otherValue <- visitIO (eliminationArray ebs) Nothing range
          case otherValue of
            Just v -> do
              recordEliminationSuccess rangePolicy
              writeIORef res (Just v)
              writeIORef ret False
            otherwise -> return ()
