{-
Module          : Tubes.Util
Description     : Optional stream processing utilities
Copyright       : (c) 2014-2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

{-# LANGUAGE RankNTypes #-}

module Tubes.Util
(
  Tubes.Util.stop
, Tubes.Util.cat
, Tubes.Util.for
, Tubes.Util.each
, Tubes.Util.every
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.unyield
, Tubes.Util.pass
, Tubes.Util.mapM
, Tubes.Util.sequence
, Tubes.Util.lfold
) where

import Prelude hiding (map, mapM)
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid (Monoid, mappend, mempty)
import System.IO
import Data.Functor.Identity
import Tubes.Core

-- * Tube utilities

-- | Loops over a 'Tube' and gives each 'yield'ed value to the continuation.
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
{-# RULES "for t yield" forall t. for t yield = t #-}
        
-- | A default tube to end a series when no further processing is required.
stop :: Monad m => Tube a () m r
stop = map (const ())

-- | Continuously relays any values it receives.
cat :: Monad m => Tube a a m r
cat = forever $ do
    x <- await
    yield x

{-# RULES
    "cat >< t" forall t. cat >< t = t
  ; "t >< cat" forall t. t >< cat = t
  #-}

each :: (Monad m, Foldable t) => t b -> Tube () b m ()
each as = Data.Foldable.mapM_ yield as

every :: (Foldable t, Monad m) => t b -> Tube () (Maybe b) m ()
every xs = ((each xs) >< map Just) >> yield Nothing

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Tube a b m r
map f = for cat (\x -> yield (f x))

{-# RULES
    "t >< map f" forall t f . t >< map f = for t (\y -> yield (f y))
  ; "map f >< t" forall t f . map f >< t = (do
        a <- await
        return (f a) ) >< t
  #-}

-- | Refuses to yield the first @n@ values it receives.
drop :: Monad m => Int -> Tube a a m r
drop 0 = cat
drop n = await >> Tubes.Util.drop (n-1)
{-# INLINABLE Tubes.Util.drop #-}

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Tube a a m r
filter pred = for cat (\x -> when (pred x) (yield x))

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
take 0 = return ()
take n = do
  await >>= yield
  Tubes.Util.take (n-1)
{-# INLINABLE Tubes.Util.take #-}

-- | Taps the next value from a source, maybe.
unyield
    :: Monad m
    => Tube x b m ()
    -> m (Maybe (b, Tube x b m ()))
unyield tsk = do
    tsk' <- runFreeT tsk
    case tsk' of
        Pure _      -> return Nothing
        Free tsk''  -> do
            let res = runTubeF tsk'' diverge (\(v, k) -> Just (v, k))
            return res

-- | Similar to 'unyield' but it first sends a value through the tube.
pass :: Monad m => a -> Tube a b m () -> m (Maybe (b, Tube a b m ()))
pass arg tb = do
    mtb <- runFreeT tb
    case mtb of
        Free tb' -> do
            let k = runTubeF tb' (\ak -> ak arg) diverge
            unyield k
        Pure _ -> return Nothing

-- | Similar to 'map' except it maps a monadic function instead of a pure one.
mapM :: Monad m => (a -> m b) -> Tube a b m r
mapM f = for cat (\a -> do
    b <- lift $ f a
    yield b)

-- | Evaluates and extracts a pure value from a monadic one.
sequence :: Monad m => Tube (m a) a m r
sequence = mapM id

-- * Pump utilities

{- |
Constructs a resumable left fold. Example usage:

@
    summer :: Pump () Int Identity Int
    summer = lfold (+) (\x -> ((),x)) 0

    main :: IO ()
    main = do
        result <- stream const (duplicate summer) $ each [1..10]
        putStrLn . show . extract $ result -- "55"
        result2 <- stream const (duplicate result) $ each [11..20]
        putStrLn . show . extract $ result2 -- "210"
@

-}
lfold
    :: (x -> a -> x)
    -> (x -> (b, x))
    -> x
    -> Pump b a Identity x
lfold step done init = pumpT (Identity init)
    (\(Identity xs) x -> Identity (step xs x))
    (\(Identity xs)   -> Identity <$> done xs)

