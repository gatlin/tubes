{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
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

{- |
   @TaskF@ is the union of unary functions and binary products into a single
   type. The value constructors may be misleading in this regard but they are
   suggestive of the roles these two types will play in stream processing.

   Free monads and free monad transformers may be derived from functions and
   tuples.
 -}
data TaskF a b k
    = Await (a -> k)
    | Yield b k

-- | These are just function and tuple @Functor@ instances, suitably tagged.
instance Functor (TaskF a b) where
    fmap f (Await k) = Await $ f . k
    fmap f (Yield v k) = Yield v $ f k

-- | A @Task@ is the free monad transformer arising from @TaskF@.
type Task   a b m r = FreeT  (TaskF a b) m r

-- | Type aliases for safety and clarity in client code.
type Source   b m r = forall x. Task x b m r
type Sink   a   m r = forall x. Task a x m r
type Action     m r = forall x. Task x x m r

-- | Convenience and readability alias.
run = runFreeT

{- ** Basic Task infrastructure -}

-- | Command used by iteratees to receive an upstream value.
await :: MonadFree (TaskF a b) m => m a
await = liftF $ Await id

-- | Command used by iteratees to yield a value downstream.
yield :: MonadFree (TaskF a b) m => b -> m ()
yield x = liftF $ Yield x ()

-- | Helper that probably should not be necessary but is anyway.
liftT = lift . runFreeT

-- | Convert a list to SourceT
each :: (Monad m, Foldable t) => t b -> Task a b m ()
each as = Data.Foldable.mapM_ yield as

-- | Enumerate @yield@ed values into a continuation, creating a new @Source@.
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

-- | Infix @for@.
src ~> body = FreeStream.Core.for src body

-- | Connect a task to a continuation yielding another task; see '><'
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

