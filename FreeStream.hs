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

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}

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
, FreeStream.for
, (~>)
, (>~)
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
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Monoid
import Control.Monad (forever, unless, replicateM_, when)

{- | Process
 -
 - A process (also called an iteratee) is a function folded over a branching
 - stream. A process exists in one of two states: awaiting a new value, or
 - yielding a result value.
 -
 - Processes are monad transformers, meaning you can construct processes which
 - fold effectful computations over streams of data.
 -}
data ProcessF a b k
    = Await (a -> k)
    | Yield (b, k)

deriving instance Functor (ProcessF a b)
type Process   a b m r = FreeT      (ProcessF a b) m r
type Generator   b m r = forall a.   Process  a b  m r
type Sink      a   m r = forall b.   Process  a b  m r
type Action        m r = forall a b. Process  a b  m r

run = runFreeT

{- | Basic Process infrastructure -}

-- | Command used by iteratees to receive an upstream value
await :: (Monad m) => Process a b m a
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream
yield :: Monad m => b -> Process a b m ()
yield x = liftF $ Yield (x,())

liftT = lift . runFreeT

-- | Convert a list to GeneratorT
each :: Monad m => [b] -> Process a b m ()
each as = mapM_ yield as

-- | Loop over the data from a generator, performing some action on each datum
for :: Monad m => Process a b m r1 -> (b -> Process a c m r2) -> Process a c m r1
for src body = liftT src >>= go where
    go (Free (Yield (v, k))) = do
        body v
        liftT k >>= go

    go (Free (Await f)) = do
        v <- await
        liftT (f v) >>= go

    go (Pure x) = return x

-- | Feed a monadic function into a sink
(~>) :: Monad m => Process x y m a -> Sink a m r -> m r
d ~> sink = runFreeT sink >>= go where
    go (Free (Await f)) = do
        Pure d' <- runFreeT d
        r  <- runFreeT $ f d'
        go r

    go (Pure x) = return x

-- | Compose sinks
(>~) :: Monad m
     => Sink a m d
     -> Sink d m r
     -> Sink a m r
s1 >~ s2 = liftT s2 >>= go where
    go (Free (Await f)) = do
        Pure d' <- liftT s1
        r <- liftT $ f d'
        go r

    go (Pure x) = return x

-- | Connect two processes into a new pull-based process
(+>) :: Monad m
     => Generator b m r
     -> Process b c m r
     -> Generator c m r
src +> sink = liftT sink >>= go src where
    go src (Free (Await f)) = do
        input <- liftT src
        case input of
            Pure x -> return x

            Free (Yield (v, k)) -> do
                output <- liftT $ f v
                go k output

    go src (Free (Yield (v, k))) = do
        yield v
        liftT k >>= go src

    go _ (Pure x)   = return x

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
