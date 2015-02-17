{-# LANGUAGE RankNTypes #-}

module FreeStream.Util

( cat
, FreeStream.Util.map
, FreeStream.Util.drop
, FreeStream.Util.take
, FreeStream.Util.takeWhile
, FreeStream.Util.filter
, FreeStream.Util.reduce
, FreeStream.Util.iterate
) where

import Prelude hiding (map, fold, iterate)
import Control.Monad (forever, unless, replicateM_, when)
import Data.Monoid ((<>), mempty, Monoid)
import Data.Foldable

import FreeStream.Core
import Control.Monad.Trans.Free

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Task a a m r
cat = forever $ do
    x <- await
    yield x


-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Task a b m r
map f = for cat $ \x -> yield (f x)

-- | Refuses to yield the first `n` values it receives.
drop :: Monad m => Int -> Task a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Task a a m r
filter pred = for cat $ \x -> when (pred x) (yield x)

-- | Terminates the stream upon receiving a value violating the predicate
takeWhile :: Monad m => (a -> Bool) -> Task a a m ()
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
take :: Monad m => Int -> Task a a m ()
take n = do
    replicateM_ n $ do
        x <- await
        yield x

-- | Fold a Stream of values
-- | Strict left fold of a Source
reduce :: Monad m => (x -> a -> x) -> x -> (x -> b) -> Source a m () -> m b
reduce step begin done p0 = runFreeT p0 >>= \p' -> loop p' begin where
    loop p x = case p of
        Free (Yield v k) -> runFreeT k >>= \k' -> loop k' $! step x v
        Pure _           -> return (done x)

-- | Similar to @each@ except it explicitly marks the stream as exhausted
iterate :: (Foldable t, Monad m) => t b -> Task a (Maybe b) m ()
iterate xs = (each xs >< map Just) >> yield Nothing
