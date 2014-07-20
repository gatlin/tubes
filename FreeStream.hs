{- |
 - Iteratee-inspired streaming library.
 -
 - (c) 2014, Gatlin Johnson <gatlin@niltag.net>
 -
 - This exists primarily for my own education. It is updated often as I try
 - things and is probably, at this moment, wrong.
 -
 - If you want to know more about iteratees:
 -
 -     http://okmij.org/ftp/Streams.html
 -
 - My goals were to (1) learn more about iteratees and (2) see how far I
 - could get using free monads.
 -}

module FreeStream

( ProcessF(..)
, Process(..)
, Generator(..)
, Sink(..)
, Action(..)
-- * Re-exports
, lift -- re-exported from Control.Monad.Trans.Free
, Free -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
-- * Core infrastructure
, await
, yield
, each
, FreeStream.Core.for
, (|>)
, (>|)
, (+>)
, run
, liftT
-- * Utilities
, cat
, FreeStream.map
, FreeStream.drop
, FreeStream.take
, FreeStream.takeWhile
, FreeStream.filter
, (|-)
) where

import Prelude hiding (map)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad (forever, unless, replicateM_, when)

import FreeStream.Core

{- | Useful utilities -}

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Process a a m r
cat = forever $ do
    x <- await
    yield x

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Process a b m r
map f = for cat $ \x -> yield (f x)

-- | Refuses to yield the first `n` values it receives.
drop :: Monad m => Int -> Process a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Process a a m r
filter pred = for cat $ \x -> when (pred x) (yield x)

-- | Terminates the stream upon receiving a value matching the predicate
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

(|-) :: (Monad m)
     => Generator b m r
     -> (b -> Bool)
     -> Generator b m r
src |- pred = src +> FreeStream.filter pred

