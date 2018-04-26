module EliminationArray.EliminationArrayCAS where

import LockFreeExchanger.LockFreeExchangerCAS
import System.Random
import Data.Time.Units

data EliminationArray a = EA {exchanger :: [LockFreeExchanger a], duration :: Integer}

newEliminationArray :: Int -> Integer -> IO (EliminationArray a)
newEliminationArray capacity duration = do
  exchangers <- newExchangerList capacity
  return $ EA exchangers duration
  where newExchangerList capacity =
          case capacity of
            0 -> return []
            otherwise -> do
              newExchanger <- newLockFreeExchanger
              rest <- newExchangerList (capacity - 1)
              return $ newExchanger : rest

visit :: Eq a => EliminationArray a -> Maybe a -> Int -> IO (Maybe a)
visit elimArr value range = do
  slot <- randomRIO (0, range)
  exchange ((exchanger elimArr) !! slot) value (duration elimArr)
