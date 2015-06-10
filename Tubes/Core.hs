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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Tubes.Core

( Tube(..)
, TubeF(..)
, Source(..)
, Sink(..)
, Action(..)
, run
, await
, yield
, yieldF
, awaitF
, liftT
, each
, Tubes.Core.for
, (><)
, (>-)
, (~>)
, Handler(..)
, HandlerF(..)
, Pairing(..)
, pairEffect
, handle
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Data.Foldable
import Data.Functor.Identity

{- |
   'TubeF' is the union of unary functions and binary products into a single
   type, here defined with a Boehm-Berarducci encoding.

   Rather than using a normal ADT, which would certainly make the code a bit
   easier to read and write, a value of this type is actually a control flow
   mechanism accepting two continuations and choosing one or the other.

   Client code should never actually have to deal with this.
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

-- | A 'Tube' is the free monad transformer arising from 'TubeF'.
type Tube   a b = FreeT  (TubeF a b)

-- ** Type aliases

-- | A computation which only 'yield's and never 'await's
type Source   b m r = forall x. Tube x b m r

-- | A computation which only 'await's and never 'yield's
type Sink   a   m r = forall x. Tube a x m r

-- | A computation which neither 'yield's nor 'await's
type Action     m r = forall x. Tube x x m r

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

{- ** Basic Tube infrastructure -}

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

-- | Compose two tasks in a pull-based stream
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

data HandlerF a b k = HandlerF
    { recv :: (a  , k)
    , send :: (b -> k)
    } deriving Functor

type Handler a b = CofreeT (HandlerF a b)

class (Functor f, Functor g) => Pairing f g | f -> g, g -> f where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
    pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
    pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
    pair p f g = p (snd f) (g (fst f))

pairEffect :: (Pairing f g, Comonad w, Monad m)
           => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
pairEffect p s c = do
    mb <- runFreeT c
    case mb of
        Pure x -> return $ p (extract s) x
        Free gs -> pair (pairEffect p) (unwrap s) gs

instance Pairing (HandlerF a b) (TubeF a b) where
    pair p (HandlerF ak bk) tb = runT tb (\ak' -> pair p ak ak')
                                         (\bk' -> pair p bk bk')

handle :: Comonad w
       => w a
       -> (w a -> (b, w a))
       -> (c   -> w a)
       -> Handler b c w a
handle x r s = coiterT cf x where
    cf wa = HandlerF (r wa) s
