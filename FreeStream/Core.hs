{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}

module FreeStream.Core

( Process(..)
, ProcessF(..)
, Generator(..)
, Sink(..)
, Action(..)
, run
, await
, yield
, liftT
, each
, FreeStream.Core.iterate
, FreeStream.Core.for
, (|>)
, (>|)
, (+>)
, runProcess
) where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Foldable

import FreeStream.Stream

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
type Generator   b m r = forall x. Process x b m r
type Sink      a   m r = forall x. Process a x m r
type Action        m r = forall x. Process x x m r

run = runFreeT

runProcess (Pure x) = return x

{- | Basic Process infrastructure -}

-- | Command used by iteratees to receive an upstream value
await :: (Monad m) => Process a b m a
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream
yield :: Monad m => b -> Process a b m ()
yield x = liftF $ Yield (x,())

liftT = lift . runFreeT

-- | Convert a list to GeneratorT
each :: (Monad m, Foldable t) => t b -> Process a b m ()
each as = Data.Foldable.mapM_ yield as

-- | Loop over the data from a generator, performing some action on each datum
iterate :: Monad m => Process a b m r1 -> (b -> Process a c m r2) -> Process a c m r1
iterate src body = liftT src >>= go where
    go (Free (Yield (v, k))) = do
        body v
        liftT k >>= go

    go (Free (Await f)) = do
        v <- await
        liftT (f v) >>= go

    go (Pure x) = return x

-- | Like iterate, but usable in the base monad
for :: Monad m
    => Generator a m r
    -> (a -> m r)
    -> m r
for src body = runFreeT src >>= go where
    go (Free (Yield (v, k))) = do
        body v
        runFreeT k >>= go

    go (Free (Await f)) = do
        v <- runFreeT await
        runFreeT (f v) >>= go

    go (Pure x) = return x

-- | Feed a monadic function into a sink
(|>) :: Monad m => Generator b m r -> Sink b m s -> m s
d |> sink = runFreeT sink >>= go d where
    go src (Free (Await f)) = do
        val <- runFreeT src
        case val of
            Free (Yield (v, k)) -> do
                r  <- runFreeT $ f v
                go k r

            -- TODO need to deal with the pure case

    go _ (Pure x) = return x

-- | Compose sinks
(>|) :: Monad m
     => Sink a m d
     -> Sink d m r
     -> Sink a m r
s1 >| s2 = liftT s2 >>= go where
    go (Free (Await f)) = do
        Pure d' <- liftT s1
        r <- liftT $ f d'
        go r

    go (Pure x) = return x

-- | Connect two processes into a new pull-based process
(+>) :: Monad m
     => Process x b m r
     -> Process b c m r
     -> Process y c m r
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
