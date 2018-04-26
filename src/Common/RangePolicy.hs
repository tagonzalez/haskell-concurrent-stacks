module Common.RangePolicy where
import Data.IORef

data RangePolicy = RgPlcy {maxRange :: IORef Int, currentRange :: IORef Int}

newRangePolicy :: Int -> IO RangePolicy
newRangePolicy maxRange = do
  maxRg <- newIORef maxRange
  currRg <- newIORef 0
  return $ RgPlcy maxRg currRg

recordEliminationSuccess :: RangePolicy -> IO ()
recordEliminationSuccess rp = do
  max <- readIORef $ maxRange rp
  curr <- readIORef $ currentRange rp
  if curr < max
      then writeIORef (currentRange rp) (curr + 1)
      else return ()

recordEliminationTimeout :: RangePolicy -> IO ()
recordEliminationTimeout rp = do
  curr <- readIORef $ currentRange rp
  if curr > 0
      then writeIORef (currentRange rp) (curr - 1)
      else return ()

getRange :: RangePolicy -> IO Int
getRange = readIORef.currentRange