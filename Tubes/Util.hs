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
(
  Tubes.Util.cat
, Tubes.Util.for
, Tubes.Util.each
, Tubes.Util.every
, (~>)
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.unyield
, Tubes.Util.mapM
, Tubes.Util.sequence
) where

import Prelude hiding (map, mapM)
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid (Monoid, mappend, mempty)
import System.IO

import Tubes.Core

for
    :: Monad m
    => Tube a b m r
    -> (b -> Tube a c m s)
    -> Tube a c m r
for src body = liftT src >>= go where
    go (Pure x) = return x
    go (Free src') = runTubeF src'
        (\f -> wrap $ awaitF (\x -> liftT (f x) >>= go))
        (\(v,k) -> do
            body v
            liftT k >>= go)

(~>)
    :: Monad m
    => Tube a b m r
    -> (b -> Tube a c m s)
    -> Tube a c m r
(~>) = for
{-# INLINE (~>) #-}
infixl 3 ~>

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Tube a a m r
cat = forever $ do
    x <- await
    yield x

each :: (Monad m, Foldable t) => t b -> Tube () b m ()
each as = Data.Foldable.mapM_ yield as

every :: (Foldable t, Monad m) => t b -> Tube () (Maybe b) m ()
every xs = ((each xs) >< map Just) >> yield Nothing

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Tube a b m r
map f = cat ~> (\x -> yield (f x))

-- | Refuses to yield the first @n@ values it receives.
drop :: Monad m => Int -> Tube a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Tube a a m r
filter pred = cat ~> (\x -> when (pred x) (yield x))

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

-- | Taps the next value from a source, maybe.
unyield
    :: Monad m
    => FreeT (TubeF () b) m ()
    -> m (Maybe (b, FreeT (TubeF () b) m ()))
unyield tsk = do
    tsk' <- runFreeT tsk
    case tsk' of
        Pure _      -> return Nothing
        Free tsk''  -> do
            let res = runTubeF tsk'' diverge (\(v, k) -> Just (v, k))
            return res

-- | Similar to 'map' except it maps a monadic function instead of a pure one.
mapM :: Monad m => (a -> m b) -> Tube a b m r
mapM f = cat ~> (\a -> do
    b <- lift $ f a
    yield b)

-- | Evaluates and extracts a pure value from a monadic one.
sequence :: Monad m => Tube (m a) a m r
sequence = mapM id

