module EliminationArrayCAS where

import LockFreeExchangerCAS
import System.Random
import Data.Time.Units

data EliminationArray a = EA {exchanger :: [LockFreeExchanger a], duration :: Integer}

createExchangerList :: Int -> IO [LockFreeExchanger a]
createExchangerList capacity =
    case capacity of
        0 -> return []
        otherwise -> do
            newExchanger <- newLockFreeExchanger
            rest <- createExchangerList (capacity - 1)
            return $ newExchanger : rest

newEliminationArray :: Int -> Integer -> IO (EliminationArray a)
newEliminationArray capacity duration = do
    exchangers <- createExchangerList capacity
    return $ EA exchangers duration

visit :: Eq a => EliminationArray a -> Maybe a -> Int -> IO (Maybe a)
visit elimArr value range = do
    slot <- randomRIO (0, range)
    exchange ((exchanger elimArr) !! slot) value (fromIntegral (duration elimArr) :: Millisecond)
