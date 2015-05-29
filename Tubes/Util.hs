{-
Module          : Tubes.Util
Description     : Optional stream processing utilities
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

{-# LANGUAGE Rank2Types #-}

module Tubes.Util

( cat
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.reduce
, Tubes.Util.every
, Tubes.Util.prompt
, Tubes.Util.display
, Tubes.Util.unyield
) where

import Prelude hiding (map)
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Monoid (Monoid, mappend, mempty)
import System.IO

import Tubes.Core

fix :: (a -> a) -> a
fix f = let x = f x in x

-- | Used in the case of a specialized 'Tube' type and we know for certain a
-- particular case will never actually be called.
diverge :: a
diverge = fix id

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Tube a a m r
cat = forever $ do
    x <- await
    yield x

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Tube a b m r
map f = for cat $ \x -> yield (f x)

-- | Refuses to yield the first @n@ values it receives.
drop :: Monad m => Int -> Tube a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Tube a a m r
filter pred = for cat $ \x -> when (pred x) (yield x)

-- | Terminates the stream upon receiving a value violating the predicate
takeWhile :: Monad m => (a -> Bool) -> Tube a a m ()
takeWhile pred = go
    where
        go = do
            a <- await
            if (pred a)
                then do
                    yield a
                    go
            else return ()

-- | Relay only the first @n@ elements of a stream.
take :: Monad m => Int -> Tube a a m ()
take n = do
    replicateM_ n $ do
        x <- await
        yield x

-- | Taps the next value from a source.
unyield :: Monad m => Source b m () -> m (Maybe b)
unyield tsk = do
    tsk' <- runFreeT tsk
    case tsk' of
        Pure _      -> return Nothing
        Free tsk''  -> do
            let res = runT tsk'' diverge (\(v, _) -> Just v)
            return res

-- | Strict left-fold of a stream
reduce :: Monad m
       => (x -> a -> x) -- ^ step function
       -> x             -- ^ initial value
       -> (x -> b)      -- ^ final transformation
       -> Source a m () -- ^ stream source
       -> m b
reduce step begin done p0 = runFreeT p0 >>= \p' -> loop p' begin where
    loop (Pure _) x = return (done x)
    loop (Free p) x = runT p diverge (\(v, k) ->
        runFreeT k >>= \k' -> loop k' $! step x v)

-- | Similar to 'each' except it explicitly marks the stream as exhausted
every :: (Foldable t, Monad m) => t b -> Tube a (Maybe b) m ()
every xs = (each xs >< map Just) >> yield Nothing

prompt :: Source String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

display :: Sink String IO ()
display = forever $ do
    it <- await
    lift . putStrLn $ it
