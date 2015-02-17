module FreeStream.Par

where

import FreeStream.Core
import FreeStream.Util

import Control.Monad.Trans.Class
import Control.Monad (forever, unless)
import System.IO (isEOF)

{-
 - Experiments with Orc primitives - broadcasting values to multiple tasks and
 - collecting the results into a new source
 -}

-- | A fun source
prompt :: Source String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

-- | A fun sink
print :: Sink String IO ()
print = forever $ do
    it <- await
    lift . putStrLn $ it

-- Some arbitrary tasks
insult :: Monad m => Task String String m ()
insult = forever $ do
    thing <- await
    yield $ thing ++ " sucks"

compliment :: Monad m => Task String String m ()
compliment = forever $ do
    thing <- await
    yield $ thing ++ " rocks"

