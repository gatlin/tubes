{-# LANGUAGE RankNTypes #-}

module FreeStream.Util

( cat
, FreeStream.Util.map
, FreeStream.Util.drop
, FreeStream.Util.take
, FreeStream.Util.takeWhile
, FreeStream.Util.filter
, FreeStream.Util.fold
, FreeStream.Util.accum
, (|-)
, (>+)
, stream
) where

import Prelude hiding (map, fold, iterate)
import Control.Monad (forever, unless, replicateM_, when)
import Data.Monoid ((<>), mempty, Monoid)

import FreeStream.Core
import FreeStream.Stream

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Process a a m r
cat = forever $ do
    x <- await
    yield x

-- | Transform all input values into Stream messages
stream :: Monad m => Generator a m () -> Generator (Stream a) m ()
stream src = do
    iterate src $ \x -> yield (chunk x)
    yield halt

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Process a b m r
map f = iterate cat $ \x -> yield (f x)

-- | Refuses to yield the first `n` values it receives.
drop :: Monad m => Int -> Process a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Process a a m r
filter pred = iterate cat $ \x -> when (pred x) (yield x)

-- | Terminates the stream upon receiving a value violating the predicate
takeWhile :: Monad m => (a -> Bool) -> Process a a m ()
takeWhile pred = go
    where
        go = do
            a <- await
            if (pred a)
                then do
                    yield a
                    go
            else return ()

-- | Relay only the first `n` elements of a stream.
take :: Monad m => Int -> Process a a m ()
take n = do
    replicateM_ n $ do
        x <- await
        yield x

-- | Shorthand for src +> filter pred
(|-) :: (Monad m)
     => Generator b m r
     -> (b -> Bool)
     -> Generator b m r
src |- pred = src +> FreeStream.Util.filter pred

-- | Shorthand for src +> map f
(>+) :: (Monad m)
     => Generator a m r
     -> (a -> b)
     -> Generator b m r
src >+ f = src +> FreeStream.Util.map f

-- | Fold a Stream of values
fold :: Monad m => (a -> a -> a) -> a -> Sink (Stream a) m a
fold step init = loop init where
    loop acc = do
        n <- await
        case recv n of
            Just v  -> loop (step acc v)
            Nothing -> return acc

accum :: (Monad m, Monoid a) => Sink (Stream a) m a
accum = fold (<>) mempty
