module Main where
import System.IO
import Control.Monad.Loops
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Text.Printf
-- import Control.Monad.LoopWhile

main =
    let cond = False
    in do 
        whileM_ (do return cond) $ do printf "Hello World"