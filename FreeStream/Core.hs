{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module FreeStream.Core

( Task(..)
, TaskF(..)
, Source(..)
, Sink(..)
, Action(..)
, run
, await
, yield
, liftT
, each
, FreeStream.Core.for
, (><)
, (>-)
, (~>)
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
import Data.Foldable

{- |
   @TaskF@ is the union of unary functions and binary products into a single
   type, here defined with a Boehm-Berarducci encoding. The value constructor
   functions are 'awaitF' and 'yieldF'.

   Rather than using a normal ADT, which would certainly make the code a bit
   easier to read and write, a value of this type is actually a control flow
   mechanism accepting two continuations and choosing one or the other.

   Client code should never actually have to deal with this.
 -}
newtype TaskF a b k = TaskF {
    runT :: forall r.
             ((a -> k) -> r)
         ->  ((b,   k) -> r)
         ->  r
} deriving (Functor)

-- | Constructor for sink computations
awaitF :: (a -> k) -> TaskF a b k
awaitF f = TaskF $ \a _ -> a f

-- | Constructor for source computations
yieldF :: b -> k -> TaskF a b k
yieldF x k = TaskF $ \_ y -> y (x, k)

-- | A @Task@ is the free monad transformer arising from @TaskF@.
type Task   a b m r = FreeT  (TaskF a b) m r

-- ** Type aliases

-- | A computation which only @yield@s and never @await@s
type Source   b m r = forall x. Task x b m r

-- | A computation which only @await@s and never @yield@s
type Sink   a   m r = forall x. Task a x m r

-- | A computation which neither @yield@s nor @await@s
type Action     m r = forall x. Task x x m r

-- | Simple utilities that make writing this library easier
liftT :: (MonadTrans t, Monad m)
      => FreeT f m a
      -> t m (FreeF f a (FreeT f m a))
liftT = lift . runFreeT

-- | 'run' is shorter than 'runFreeT' and who knows, maybe it'll change some
-- day
run :: FreeT f m a -> m (FreeF f a (FreeT f m a))
run = runFreeT

{- ** Basic Task infrastructure -}

-- | Command to wait for a new value upstream
await :: Monad m => Task a b m a
await = improveT $ liftF $ awaitF id

-- | Command to send a value downstream
yield :: Monad m => b -> Task a b m ()
yield x = improveT $ liftF $ yieldF x ()

-- | Connect a task to a continuation yielding another task; see '><'
(>-) :: Monad m
     => Task a b m r
     -> (b -> Task b c m r)
     -> Task a c m r
p >- f = liftT p >>= go where
    go (Pure x) = return x
    go (Free p') = runT p' (\f' -> wrap $ awaitF (\a -> (f' a) >- f))
                           (\(v, k) -> k >< f v)

-- | Compose two tasks in a pull-based stream
(><) :: Monad m
     => Task a b m r
     -> Task b c m r
     -> Task a c m r
a >< b = liftT b >>= go where
    go (Pure x) = return x
    go (Free b') = runT b' (\f -> a >- f)
                           (\(v, k) -> wrap $ yieldF v $ liftT k >>= go)

infixl 3 ><

-- | Enumerate @yield@ed values into a continuation, creating a new @Source@
for :: Monad m
    => Task a b m r
    -> (b -> Task a c m s)
    -> Task a c m r
for src body = liftT src >>= go where
        go (Pure x) = return x
        go (Free src') = runT src'
            (\f -> wrap $ awaitF (\x -> liftT (f x) >>= go))
            (\(v, k) -> do
                body v
                liftT k >>= go)

-- | Infix version of 'for'
(~>) :: Monad m
     => Task a b m r
     -> (b -> Task a c m s)
     -> Task a c m r
(~>) = for

-- | Convert a list to a 'Source'
each :: (Monad m, Foldable t) => t b -> Task a b m ()
each as = Data.Foldable.mapM_ yield as

