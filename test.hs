-- | Load this up in ghci to play around

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Prelude hiding ( drop
                      , take
                      , takeWhile
                      , print
                      , map
                      , filter
                      , foldl
                      , foldl'
                      , foldr
                      , foldr'
                      , iterate
                      )
import FreeStream
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans.Free
import Control.Applicative
import Control.Alternative.Free
import Control.Applicative.Free (runAp, retractAp)
import Data.Traversable hiding (for)
import Data.Foldable hiding (fold)
import Data.Monoid (mempty, (<>), Monoid)
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import Data.Maybe (fromMaybe)
import qualified GHC.IO.Exception as G

ex1 = do
    let people = [ "gatlin"
                 , "mike"
                 , "ian"
                 , "ryan"
                 ]

    for (each people |- (/= "mike") +> map (++ " sucks")) $ \p -> do
        putStrLn $ p

prompt :: Generator String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

print :: Sink (Stream String) IO ()
print = do
    str <- await
    case recv str of
        Nothing -> return ()
        Just v  -> do
            x   <- lift $ try $ putStrLn v
            case x of
                Left e@(G.IOError { G.ioe_type = t }) ->
                    lift $ unless (t == G.ResourceVanished) $ throwIO e
                Right () -> print

evenNumbers = for (each [1..10] |- isEven +> map show) putStrLn
    where isEven n = if n `mod` 2 == 0 then True else False

fizzbuzz n = fromMaybe (show n) $ [ "fizz" | n `rem` 3 == 0 ]
                               <> [ "buzz" | n `rem` 5 == 0 ]
                               <> [ "bazz" | n `rem` 7 == 0 ]

sumS :: Monad m => Sink (Stream Int) m Int
sumS = fold (+) 0

prodS :: Monad m => Sink (Stream Int) m Int
prodS = fold (*) 1

relay sink = sink >>= yield

