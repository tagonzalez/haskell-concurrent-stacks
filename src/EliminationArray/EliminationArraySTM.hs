module EliminationArray.EliminationArraySTM where

import LockFreeExchanger.LockFreeExchangerSTM
import System.Random

data EliminationArraySTM a = EA {exchanger :: [LockFreeExchangerSTM a], duration :: Integer}

newEliminationArraySTM :: Int -> Integer -> IO (EliminationArraySTM a)
newEliminationArraySTM capacity duration = do
  exchangers <- newExchangerList capacity
  return $ EA exchangers duration
  where newExchangerList capacity =
          case capacity of
            0 -> return []
            otherwise -> do
              newExchanger <- newLockFreeExchangerSTM
              rest <- newExchangerList (capacity - 1)
              return $ newExchanger : rest

visit :: Eq a => EliminationArraySTM a -> Maybe a -> Int -> IO (Maybe a)
visit elimArr value range = do
  slot <- randomRIO (0, range)
  exchangeSTM ((exchanger elimArr) !! slot) value (duration elimArr)
