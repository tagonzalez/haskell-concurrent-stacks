module Common.State where

data State = EMPTY | WAITING | BUSY
instance Eq State where
  WAITING == WAITING = True
  EMPTY == EMPTY = True
  BUSY == BUSY = True
