module EliminationBackoffStackCASusingSTM where

import EliminationArrayCASusingSTM
import NodeSTM
import LockFreeStackCASusingSTM
import RangePolicy

data EliminationBackoffStack a = EBS {lfs :: LockFreeStack a, capacity :: Int, elimArr :: EliminationArray a, policy :: RangePolicy}

createEBS :: Int -> Integer ->  IO (EliminationBackoffStack a)
createEBS capacity duration = do
    lfs <- createLFS 0 0 0 -- Backoff isn't used for EBS functions, so it's parameters don't matter
    rgPcy <- newRangePolicy capacity
    elArr <- newEliminationArray capacity duration
    return $ EBS lfs capacity elArr rgPcy

loopPushEBS :: Eq a => EliminationBackoffStack a -> Node a -> a -> Int -> IO ()
loopPushEBS ebs node value range = do
    b <- tryPush (lfs ebs) node
    if b
        then return ()
        else do
            otherValue <- visit (elimArr ebs) (Just value) range
            case otherValue of
                Nothing -> do
                    recordEliminationSuccess $ policy ebs
                    return ()
                otherwise -> loopPushEBS ebs node value range

pushEBS :: Eq a => EliminationBackoffStack a -> a -> IO ()
pushEBS ebs value = do
    range <- getRange $ policy ebs
    node <- newNode value
    loopPushEBS ebs node value range

loopPopEBS :: Eq a => EliminationBackoffStack a -> Int -> IO (Maybe a)
loopPopEBS ebs range = do
    returnNode <- tryPop (lfs ebs)
    if returnNode /= Null
        then return $ Just (val returnNode)
        else do
            otherValue <- visit (elimArr ebs) Nothing range
            if otherValue /= Nothing
                then do
                    recordEliminationSuccess $ policy ebs
                    return otherValue
                else do
                    -- try
                    loopPopEBS ebs range
                    -- catch timeout exception


popEBS :: Eq a => EliminationBackoffStack a -> IO (Maybe a)
popEBS ebs = do
    range <- getRange $ policy ebs
    loopPopEBS ebs range

