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
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module FreeStream

( Stream(..)
, ProcessF(..)
, ProcessT(..)
, await
, yield
, run
, liftT
, poll
, lift -- re-exporting this
, runFreeT
, feed
, par
, agg
, (+>)
, ($>)
, (+<)
, ($<)
, concatS
) where

import Prelude hiding (sequence, mapM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Traversable
import Data.Foldable (Foldable, fold)
import Control.Applicative
import Data.Monoid

{- | A stream of data.
 -
 - A chunk of data wrapped in some functor, or the End of the stream.
 -}
data Stream f a where
    End   :: forall a f. Functor f => Stream f a
    Chunk :: forall a f. Functor f => f a -> Stream f a

deriving instance Show (f a) => Show (Stream f a)

{- | Process
 -
 - A process (also called an iteratee) is a function folded over a branching
 - stream. A process exists in one of two states: awaiting a new value, or
 - yielding a result value.
 -
 - Processes are monad transformers, meaning you can construct processes which
 - fold effectful computations over streams of data.
 -}
data ProcessF a b where
    Await :: (a -> b) -> ProcessF a b

deriving instance Functor (ProcessF a)
type ProcessT m f a b = FreeT (ProcessF (Stream f a)) m (b,(Stream f a))

{- | Useful utilities -}

-- | Command used by iteratees to receive an upstream value
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream
yield :: (Monad m, Functor f) => b -> m (b, Stream f a)
yield x = return (x, End)

-- | A monadic tear-down function for ProcessT values
runProcess (Pure x) = return x
runProcess (Free (Await f)) = runFreeT (f End) >>= runProcess

-- | A function for users to get values out of processes
run p = runFreeT p >>= runProcess
liftT = lift . runFreeT


-- | Poll an iteratee for its most recently yielded value, or stimulate it to
-- yield a value by passing it the End of a stream
poll :: (Monad m, Functor f) => ProcessT m f a b -> m b
poll src = runFreeT src >>= go where
    go (Pure (v,k))     = return v
    go (Free (Await f)) = runFreeT (f End) >>= go

-- | Feed an iteratee some piece of a stream, discarding unused input

feed :: (Monad m, Functor f)
     => ProcessT m f a b
     -> Stream f a
     -> m b
feed k str = runFreeT k >>= go where
    go (Free (Await f)) = run (f str) >>= return . fst
    go (Pure (v, k))    = return v

($>) :: (Monad m, Functor f)
     => Stream f a
     -> ProcessT m f a b
     -> ProcessT m f c b
str $> k = liftT k >>= go False where
    go repeat (Free (Await f)) | repeat    = liftT (f End) >>= go True
                               | otherwise = liftT (f str) >>= go True
    go repeat (Pure (v, k))    = yield v

infixl $>

-- | Take any Traversable structure and map the stream over it
par ss = do
    chunk <- await
    rs <- mapM (\s -> lift (feed s chunk)) ss
    yield rs

-- | Aggregate all input until the end of the stream
agg :: (Monad m, Functor f, Monoid (f a)) => ProcessT m f a (f a)
agg = loop mempty where
    loop acc = await >>= go acc
    go acc (Chunk xs) = loop (acc <> xs)
    go acc _          = yield acc

concatS :: (Monad m, Traversable f, Functor f, Monoid a, Monoid (f a))
        => ProcessT m f a a
concatS = loop mempty where
    loop acc = await >>= go acc
    go acc (Chunk xs) = loop (acc <> xs)
    go acc _          = yield $ fold acc

(+>) :: (Monad m, Functor f)
     => ProcessT m f a (f b)
     -> ProcessT m f b (f c)
     -> ProcessT m f d (f c)
src +> sink = do
    sink' <- liftT sink
    src'  <- liftT src
    go False src' sink'
    where
    {-
        go repeat (Free (Await f)) sink' = do
            v <- await
            r <- liftT $ f v
            go repeat r sink'
    -}

        go repeat (Pure (v, k)) (Free (Await f)) = case repeat of
            True -> do
                r <- liftT $ f End
                go True (Pure (v, k)) r
            _    -> do
                r <- liftT $ f $ Chunk v
                go True (Pure (v, k)) r

        go repeat src'          (Pure (v, k)) = do
            yield v

-- | Map a source's output to a functor of sinks
src +< ss = src +> par ss

-- | Map a raw stream value to a functor of sinks
strm $< ss = strm $> agg +< ss
