{-
Module          : Tubes.Core
Description     : Fundamental types and operations.
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}

module Tubes.Core
(
-- * Basic definitions
  Tube(..)
, TubeF(..)
-- * Type aliases
, Source(..)
, Sink(..)
-- * Core commands
, run
, await
, yield
-- * Control mechanisms
, each
, Tubes.Core.for
, (><)
, (>-)
, (~>)
, (-<)
, (|>)
-- * @TubeF@ value constructors
, yieldF
, awaitF
-- * Miscellaneous
, liftT
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
import Data.Foldable
import Data.Functor.Identity

-- as in Util, these are here to trivially satisfy situations I provably won't
-- get in.

fix :: (a -> a) -> a
fix f = let x = f x in x

diverge :: a
diverge = fix id

{- |
'TubeF' is the union of unary functions and binary products into a single
type, here defined with a Boehm-Berarducci encoding.

This type is equivalent to the following:

    @
        data TubeF a b k
            = Await (a -> k) -- :: (a -> k) -> TubeF a b k
            | Yield (b  , k) -- :: (b  , k) -> TubeF a b k
    @

The type signatures for the two value constructors should bear a strong
resemblance to the actual type signature of 'runT'. Instead of encoding
tubes as structures which build up when composed, a 'TubeF' is a control
flow mechanism which picks one of two provided continuations.

People using this library should never have to contend with these details
but it is worth mentioning.
 -}
newtype TubeF a b k = TubeF {
    runT :: forall r.
             ((a -> k) -> r)
         ->  ((b,   k) -> r)
         ->  r
} deriving (Functor)

-- | Constructor for sink computations
awaitF :: (a -> k) -> TubeF a b k
awaitF f = TubeF $ \a _ -> a f

-- | Constructor for source computations
yieldF :: b -> k -> TubeF a b k
yieldF x k = TubeF $ \_ y -> y (x, k)

{- |
A 'Tube' is a computation which can

* 'yield' an intermediate value downstream and suspend execution; and

* 'await' a value from upstream, deferring execution until it is received.

Moreover, individual 'Tube's may be freely composed into larger ones, so long
as their types match. Thus, one may write small, reusable building blocks and
construct efficient stream process pipelines.

Since a much better engineered, more popular, and decidedly more mature
library already uses the term "pipes" I have opted instead to think of my work
as a series of tubes.
-}
type Tube   a b     = FreeT  (TubeF a b)

-- ** Type aliases

-- | A computation which only 'yield's and never 'await's
type Source   b m r = forall x. Tube x b m r

-- | A computation which only 'await's and never 'yield's.
type Sink   a   m r = forall x. Tube a x m r

{- |
This performs a neat trick: a 'Tube' with a return type @a@ will be
turned into a new 'Tube' containing the underlying 'TubeF' value.

In this way the '><' and '>-' functions can replace the @()@ return value with
a continuation and recursively traverse the computation until a final result
is reached.
-}
liftT :: (MonadTrans t, Monad m)
      => FreeT f m a
      -> t m (FreeF f a (FreeT f m a))
liftT = lift . runFreeT

-- | 'run' is shorter than 'runFreeT' and who knows, maybe it\'ll change some
-- day
run :: FreeT f m a -> m (FreeF f a (FreeT f m a))
run = runFreeT

-- | Command to wait for a new value upstream
await :: Monad m => Tube a b m a
await = improveT $ liftF $ awaitF id

-- | Command to send a value downstream
yield :: Monad m => b -> Tube a b m ()
yield x = improveT $ liftF $ yieldF x ()

-- | Connect a task to a continuation yielding another task; see '><'
(>-) :: Monad m
     => Tube a b m r
     -> (b -> Tube b c m r)
     -> Tube a c m r
p >- f = liftT p >>= go where
    go (Pure x) = return x
    go (Free p') = runT p' (\f' -> wrap $ awaitF (\a -> (f' a) >- f))
                           (\(v, k) -> k >< f v)

-- | Compose two tubes into a new tube.
(><) :: Monad m
     => Tube a b m r
     -> Tube b c m r
     -> Tube a c m r
a >< b = liftT b >>= go where
    go (Pure x) = return x
    go (Free b') = runT b' (\f -> a >- f)
                           (\(v, k) -> wrap $ yieldF v $ liftT k >>= go)

infixl 3 ><

-- | Enumerate 'yield'ed values into a continuation, creating a new 'Source'
for :: Monad m
    => Tube a b m r
    -> (b -> Tube a c m s)
    -> Tube a c m r
for src body = liftT src >>= go where
        go (Pure x) = return x
        go (Free src') = runT src'
            (\f -> wrap $ awaitF (\x -> liftT (f x) >>= go))
            (\(v, k) -> do
                body v
                liftT k >>= go)

-- | Infix version of 'for'
(~>) :: Monad m
     => Tube a b m r
     -> (b -> Tube a c m s)
     -> Tube a c m r
(~>) = for

-- | Convert a list to a 'Source'
each :: (Monad m, Foldable t) => t b -> Tube a b m ()
each as = Data.Foldable.mapM_ yield as

-- | Insert a value into a 'Sink'
(-<) :: Monad m
     => a
     -> Sink a m b
     -> Sink a m b
x -< snk = liftT snk >>= go where
    go (Pure y) = return y
    go (Free snk') = runT snk' (\f -> f x) diverge

-- | Implementation of '|>' but without the type constraints.
(\|>) :: Monad m
      => Tube a b m r
      -> Tube (Maybe b) c m s
      -> Tube (Maybe b) c m s
src \|> snk = liftT snk >>= goSnk where
    goSnk (Pure x) = return x
    goSnk (Free snk') = runT snk'
        (\fSnk -> (liftT src) >>= goSrc fSnk)
        diverge

    goSrc f (Pure _) = f Nothing
    goSrc f (Free snk') = runT snk' diverge $
        \(v,k) -> k \|> (f (Just v))

{- |
Connects a 'Source' to a 'Sink', finishing when either the 'Source' is
exhausted or the 'Sink' terminates.
-}
(|>) :: Monad m
     => Source b m ()
     -> Sink (Maybe b) m s
     -> Sink (Maybe b) m s
(|>) = (\|>)
