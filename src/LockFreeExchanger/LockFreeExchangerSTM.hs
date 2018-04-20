module LockFreeExchanger.LockFreeExchangerCASusingSTM where

import Control.Concurrent.STM
import System.Clock
import Data.Time.Units
import Common.AtomCASusingSTM
import Common.State

data LockFreeExchanger a = LFE {slot :: TVar (Maybe a, State)}

exchange :: (Eq a,TimeUnit b) => LockFreeExchanger a -> Maybe a -> b -> IO (Maybe a)
exchange = undefined
