{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Monoid

data TaskF a b k
    = Await (a -> k)
    | Yield b k

instance Functor (TaskF a b) where
    fmap f (Await k) = Await $ f . k
    fmap f (Yield v k) = Yield v $ f k

type Task   a b m r = FreeT  (TaskF a b) m r
type Source   b m r = forall x. Task x b m r
type Sink   a   m r = forall x. Task a x m r
type Action     m r = forall x. Task x x m r

run = runFreeT

{- | Basic Task infrastructure -}

-- | Command used by iteratees to receive an upstream value
await :: MonadFree (TaskF a b) m => m a
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream
yield :: MonadFree (TaskF a b) m => b -> m ()
yield x = liftF $ Yield x ()

-- | Helper that probably should not be necessary but is anyway.
liftT = lift . runFreeT

-- | Convert a list to SourceT
each :: (Monad m, Foldable t) => t b -> Task a b m ()
each as = Data.Foldable.mapM_ yield as

src ~> body = FreeStream.Core.for src body

for :: Monad m
    => Task a b m r
    -> (b -> Task a c m s)
    -> Task a c m r
for src body = liftT src >>= go where
    go (Free (Await f)) = wrap $ Await (\x -> liftT (f x) >>= go)
    go (Free (Yield v k)) = do
        body v
        liftT k >>= go
    go (Pure x) = return x

-- | Feed a stream generator into a stream reducer
-- | Connect a task to a continuation yielding another task
(>-) :: Monad m
     => Task a b m r
     -> (b -> Task b c m r)
     -> Task a c m r
p >- f = liftT p >>= go where
    go (Pure x) = return x
    go (Free (Await f')) = wrap $ Await (\a -> (f' a) >- f)
    go (Free (Yield v k)) = k >< f v

-- | Compose two tasks in a pull-based stream
(><) :: Monad m
     => Task a b m r
     -> Task b c m r
     -> Task a c m r
a >< b = liftT b >>= go where
    go (Pure x) = return x
    go (Free (Yield v k)) = wrap $ Yield v $ liftT k >>= go
    go (Free (Await f)) = a >- f

infixl 3 ><

instance (Monad m, Monoid r) => Monoid (Task a b m r) where
    mempty = return mempty
    mappend p1 p2 = liftT p1 >>= go where
        go (Free (Await f))   = wrap $ Await (\a -> liftT (f a) >>= go)
        go (Free (Yield v k)) = wrap $ Yield v $ liftT k >>= go
        go (Pure r)           = fmap (mappend r) p2
