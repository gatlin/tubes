-- | Load this up in ghci to play around

{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes #-}

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
import Data.Maybe (fromMaybe, fromJust)

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
            lift . putStrLn $ v
            print

evenNumbers :: IO ()
evenNumbers = for (each [1..10] |- isEven +> map show) putStrLn
    where isEven n = if n `mod` 2 == 0 then True else False

fizzbuzz n = fromMaybe (show n) $ [ "fizz" | n `rem` 3 == 0 ]
                               <> [ "buzz" | n `rem` 5 == 0 ]
                               <> [ "bazz" | n `rem` 7 == 0 ]

sumS :: Monad m => Sink (Stream Integer) m Integer
sumS = fold (+) 0

prodS :: Monad m => Sink (Stream Integer) m Integer
prodS = fold (*) 1

relay sink = sink >>= yield

getwords :: Sink (Stream Char) IO [String]
getwords = loop "" [] where
    loop s acc = do
        d <- await
        case recv d of
            Nothing -> return (acc++[s])
            Just  c -> case c of
                ' ' -> loop "" (s:acc)
                _   -> loop (s ++ [c]) acc

