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
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module FreeStream

( Stream(..)
, ProcessF(..)
, ProcessT(..)
, await
, yield
, lift -- re-exported from Control.Monad.Trans.Free
, Free -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
, feed
, (+>)
, run
, sequence -- re-exported from Data.Traversable
, parPull
, (*<)
, ($>)
, ($<)
) where

import Prelude hiding (sequence, mapM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Traversable
import Data.Foldable (Foldable, fold)
import Data.Monoid

data Stream f a where
    End   :: (Traversable f, Monoid (f a)) => Stream f a
    Chunk :: forall a f. (Traversable f, Monoid (f a)) => f a -> Stream f a

{- | Process
 -
 - A process (also called an iteratee) is a function folded over a branching
 - stream. A process exists in one of two states: awaiting a new value, or
 - yielding a result value.
 -
 - Processes are monad transformers, meaning you can construct processes which
 - fold effectful computations over streams of data.
 -}
data ProcessF a b k where
    Await :: (a -> k) -> ProcessF a b k
    Yield :: (b, k)   -> ProcessF a b k

deriving instance Functor (ProcessF a b)
type ProcessT a b m r = FreeT (ProcessF a b) m r

{- | Useful utilities -}

-- | Command used by iteratees to receive an upstream value
await :: (Monad m) => ProcessT a b m a
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream
yield :: Monad m => b -> ProcessT a b m ()
yield x = liftF $ Yield (x,())

liftT = lift . runFreeT

run src = runFreeT src >>= go where
    go (Free (Yield (v, k))) = return $ Just (v, k)
    go (Free (Await f))      = return Nothing
    go (Pure x)              = return Nothing

-- | Construct pull-based stream pipelines
src +> sink = do
    sink' <- liftT sink
    go src sink'
    where
        go src (Free (Await f)) = do
            input <- lift $ run src
            case input of
                Just (v, k) -> do
                    r <- liftT $ f $ Chunk v
                    go k r
                Nothing -> do
                    r <- liftT $ f End
                    go src r

        go src (Free (Yield (v, k))) = do
            yield v
            k' <- liftT k
            go src k'

        go _ (Pure x) = return x

-- | Feed a process a piece of stream and return the output.
feed k str = runFreeT k >>= go False where
    go secondLoop (Free (Await f)) | secondLoop = runFreeT (f End) >>= go True
                                   | otherwise  = runFreeT (f str) >>= go True
    go secondLoop (Free (Yield (v, k))) = return v

-- | Map the stream over any Traversable of process sinks (pull-based)
parPull ss = do
    chunk <- await
    rs <- mapM (\s -> lift (feed s chunk)) ss
    yield rs

-- | Convenience function for connecting a source to a Traversable of sinks in
-- a pull-based stream.
src *< ss = src +> (parPull ss)

-- | Construct a simple pull-based source out of raw stream data
d $> sink = (yield d) +> sink

-- | Analogous to +<
d $< sinkF = (yield d) *< sinkF
