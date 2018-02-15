module LockFreeStackIO where

import Prelude hiding (break)
import Control.Break
import Control.Concurrent
import Backoff

data Node a = Nd {val :: a, next :: Node a} | Null
data LockFreeStack a = LFSIO {top :: Node a, backoffLFS :: Backoff}

tryPush:: LockFreeStack a -> Node a -> Bool
tryPush = (\lfs node -> True)

-- Probar loop recursivo
loopTryPush:: LockFreeStack a -> Node a -> IO ()
loopTryPush lfs node = if tryPush lfs node
                            then return ()
                            else do {
                                backoff (backoffLFS lfs);
                                loopTryPush lfs node;
                                }

pushLFS:: LockFreeStack a -> a -> IO ()
pushLFS lfs e = do{
    node <- return $ Nd e Null;
    loop $ do{
        success <- return $ tryPush lfs node;
        if success
            then break ()
            else lift $ backoff (backoffLFS lfs);
    };
}

