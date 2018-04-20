{-# LANGUAGE ScopedTypeVariables #-} -- Necessary for handling exceptions
module EliminationBackoffStack.EliminationBackoffStackCAS where

import EliminationArray.EliminationArrayCAS
import LockFreeStack.LockFreeStackCAS
import Common.RangePolicy
import Common.Node
import Control.Exception
import Common.Exceptions

data EliminationBackoffStack a = EBS {lfs :: LockFreeStack a, capacity :: Int, eliminationArray :: EliminationArray a, policy :: RangePolicy}

newEBS :: Int -> Integer ->  IO (EliminationBackoffStack a)
newEBS capacity duration = do
  lfs <- newLFS 0 0 0 -- Backoff isn't used for EBS functions, so it's parameters don't matter
  rgPcy <- newRangePolicy capacity
  elArr <- newEliminationArray capacity duration
  return $ EBS lfs capacity elArr rgPcy

pushEBS :: Eq a => EliminationBackoffStack a -> a -> IO ()
pushEBS ebs value = do
  range <- getRange $ policy ebs
  node <- newNode value
  loopPushEBS ebs node value range

  where loopPushEBS ebs node value range = do
          b <- tryPush (lfs ebs) node
          if b
            then return ()
            else catch (exchangePush ebs node value range) $ \( e :: TimeoutException) -> do
              recordEliminationTimeout $ policy ebs
              loopPushEBS ebs node value range

        exchangePush ebs node value range = do
          otherValue <- visit (eliminationArray ebs) (Just value) range
          case otherValue of
            Nothing -> do
              recordEliminationSuccess $ policy ebs
              return ()
            otherwise -> loopPushEBS ebs node value range

popEBS :: Eq a => EliminationBackoffStack a -> IO (Maybe a)
popEBS ebs = do
  range <- getRange $ policy ebs
  loopPopEBS ebs range

  where loopPopEBS ebs range = do
          returnNode <- tryPop (lfs ebs)
          if returnNode /= Null
            then return $ Just (val returnNode)
            else catch (exchangePop ebs range) $ \( e :: TimeoutException) -> do
              recordEliminationTimeout $ policy ebs
              loopPopEBS ebs range

        exchangePop ebs range = do
          otherValue <- visit (eliminationArray ebs) Nothing range
          if otherValue /= Nothing
            then do
              recordEliminationSuccess $ policy ebs
              return otherValue
            else do
              loopPopEBS ebs range

test2 = do
  ebs <- newEBS 9 100
  pushEBS ebs 3
  pushEBS ebs 2
  pushEBS ebs 4
  res <- lfsToListIO (lfs ebs)
  putStrLn (show res)

test = do
  catch (throw TimeoutException) (\e -> putStrLn (show (e::TimeoutException)))