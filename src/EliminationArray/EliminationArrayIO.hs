module EliminationArray.EliminationArrayIO where

import LockFreeExchanger.LockFreeExchangerIO
import System.Random

data EliminationArrayIO a = EAIO {exchanger :: [LockFreeExchangerIO a], duration :: Integer}

newEliminationArrayIO :: Int -> Integer -> IO (EliminationArrayIO a)
newEliminationArrayIO capacity duration = do
  exchangers <- newExchangerList capacity
  return $ EAIO exchangers duration
  where newExchangerList capacity =
          case capacity of
            0 -> return []
            otherwise -> do
              newExchanger <- newLockFreeExchangerIO
              rest <- newExchangerList (capacity - 1)
              return $ newExchanger : rest

visitIO :: Eq a => EliminationArrayIO a -> Maybe a -> Int -> IO (Maybe a)
visitIO elimArr value range = do
  slot <- randomRIO (0, range)
  exchangeIO ((exchanger elimArr) !! slot) value (duration elimArr)
