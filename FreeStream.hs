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
, forList
, poll
, lift -- re-exporting this
, runFreeT
, feed
, (>|<)
, par
) where

import Prelude hiding (sequence, mapM)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Traversable
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
yield x = return (x, End)

-- | A monadic tear-down function for ProcessT values
runProcess (Pure x) = return x
runProcess (Free (Await f)) = runFreeT (f End) >>= runProcess

-- | A function for users to get values out of processes
run p = runFreeT p >>= runProcess
liftT = lift . runFreeT

-- | Enumerate the contents of a list to a ProcessT
forList lst s = runFreeT s >>= go lst where
    go []     (Free (Await f)) = runFreeT (f End) >>= go []
    go (x:xs) (Free (Await f)) = runFreeT (f (Chunk [x])) >>= go xs
    go []     (Pure v)         = return v
    go lst    (Pure (v,_))     = return (v, Chunk lst)

-- | Poll an iteratee for its most recently yielded value, or stimulate it to
-- yield a value by passing it the End of a stream
poll src = runFreeT src >>= go where
    go (Pure (v,k))     = return (v, k)
    go (Free (Await f)) = runFreeT (f End) >>= go

-- | Feed an iteratee some piece of a stream, discarding unused input

feed :: (Monad m, Functor f)
     => ProcessT m f a b
     -> Stream f a
     -> m b
feed k str = runFreeT k >>= go where
    go (Free (Await f)) = run (f str) >>= return . fst
    go (Pure (v, k))    = return v

-- | Given two iteratees, retrieve some input and apply both to it
s1 >|< s2 = do
    chunk <- await
    r1 <- lift $ feed s1 chunk
    r2 <- lift $ feed s2 chunk
    yield (r1, r2)

infixl >|<

par ss = do
    chunk <- await
    rs <- mapM (\s -> lift (feed s chunk)) ss
    yield rs
