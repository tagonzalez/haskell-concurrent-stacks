module Main where
import Prelude hiding (break)
import System.IO
import Control.Break
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Text.Printf
import Data.IORef

data LockFreeStack a = Node { val :: a
                            , next :: IORef (LockFreeStack a)}
                     | Null

push st e =
    let node = Node e (IORef Null) in
        loop $ do
            


-- main =
--     loop $ do
--         lift $ printf "Hello World"
--         lift $ printf "Goodbye World"
--         break ()