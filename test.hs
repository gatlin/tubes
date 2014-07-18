-- | Load this up in ghci to play around

{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding ( drop
                      , take
                      , takeWhile
                      , print
                      , map
                      , filter
                      )
import FreeStream
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans.Free
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

print = do
    str <- await
    x   <- lift $ try $ putStrLn str
    case x of
        Left e@(G.IOError { G.ioe_type = t }) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        Right () -> print

doubleUp = do
    str1 <- await
    str2 <- await
    return $ str1 ++ str2
