module Common.Exceptions where

import Control.Exception

data TimeoutException = TimeoutException
  deriving Show

data EmptyException = EmptyException
  deriving Show

instance Exception TimeoutException
instance Exception EmptyException

