{-
Module          : Tubes.Core
Description     : Fundamental types and operations.
Copyright       : (c) 2014-2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Tubes.Core
(
  Tube(..)
, TubeF(..)
, await
, yield
, (>-)
, (><)
, liftT
, diverge
, awaitF
, yieldF
, Pump(..)
, PumpF(..)
, pump
, send
, recv
, meta
, stream
, streamM
, runTube
)
where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church

import Control.Comonad
import Control.Comonad.Trans.Cofree

import Control.Applicative

import Data.Foldable
import Data.Functor.Identity

import Data.Void

-- * Tubes

newtype TubeF a b k = TubeF {
    runTubeF :: forall r.
                 ((a -> k) -> r)
              -> ((b ,  k) -> r)
              -> r
} deriving (Functor)

awaitF :: (a -> k) -> TubeF a b k
awaitF f = TubeF $ \a _ -> a f

yieldF :: b -> k -> TubeF a b k
yieldF x k = TubeF $ \_ y -> y (x, k)

type Tube a b = FreeT (TubeF a b)

liftT :: (MonadTrans t, Monad m)
      => FreeT f m a
      -> t m (FreeF f a (FreeT f m a))
liftT = lift . runFreeT

await :: Monad m => Tube a b m a
await = improveT $ liftF $ awaitF id

yield :: Monad m => b -> Tube a b m ()
yield x = improveT $ liftF $ yieldF x ()

(>-)
    :: Monad m
    => Tube a b m r
    -> (b -> Tube b c m r)
    -> Tube a c m r
p >- f = liftT p >>= go where
    go (Pure x) = return x
    go (Free p') = runTubeF p' (\f' -> wrap $ awaitF (\a -> (f' a) >- f))
                               (\(v,k) -> k >< f v)

infixl 3 >-

(><)
    :: Monad m
    => Tube a b m r
    -> Tube b c m r
    -> Tube a c m r
a >< b = liftT b >>= go where
    go (Pure x) = return x
    go (Free b') = runTubeF b' (\f -> a >- f)
                               (\(v,k) -> wrap $ yieldF v $ liftT k >>= go)

infixl 3 ><

fix :: (a -> a) -> a
fix f = let x = f x in x

diverge :: a
diverge = fix id

-- * Pumps

data PumpF a b k = PumpF
    { sendF :: (b -> k)
    , recvF :: (a  , k)
    } deriving (Functor)

type Pump a b = CofreeT (PumpF a b)

send :: Comonad w => b -> Pump a b w r -> Pump a b w r
send x p = (sendF (unwrap p)) x

recv :: Comonad w => Pump a b w r -> (a, Pump a b w r)
recv p = recvF . unwrap $ p

pump
    :: Comonad w
    => w r
    -> (w r -> (a, w r))
    -> (w r -> b -> w r)
    -> Pump a b w r
pump x s r = coiterT cf x where
    cf wa = PumpF (r wa) (s wa)

meta :: (x -> a -> x) -> (x -> (b, x)) -> x -> x
meta step done init = extract $ pump (Identity init)
    (\(Identity xs) -> Identity <$> done xs)
    (\(Identity xs) x -> Identity (step xs x))

-- * Tubes and Pumps are adjoint!
-- I considered using the @adjunctions@ package but I don't need all that
-- power.

class (Functor f, Functor g) => Adjoint f g | f -> g, g -> f where
    adj :: (a -> b -> r) -> f a -> g b -> r

instance Adjoint Identity Identity where
    adj f (Identity a) (Identity b) = f a b

instance Adjoint ((->) a) ((,) a) where
    adj p f = uncurry (p . f)

instance Adjoint ((,) a) ((->) a) where
    adj p f g = p (snd f) (g (fst f))

instance Adjoint (PumpF a b) (TubeF a b) where
    adj p (PumpF bk ak) tb = runTubeF tb
        (\ak' -> adj p ak ak')
        (\bk' -> adj p bk bk')

_stream
    :: (Adjoint f g, Comonad w, Monad m)
    => (a -> b -> r) -> CofreeT f w a -> FreeT g m b -> m r
_stream p s c = do
    mb <- runFreeT c
    case mb of
        Pure x -> return $ p (extract s) x
        Free gs -> adj (_stream p) (unwrap s) gs

_streamM
    :: (Adjoint f g, Comonad w, Monad m)
    => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
_streamM p s c = do
    a <- extract s
    mb <- runFreeT c
    case mb of
        Pure x -> return $ p a x
        Free gs -> adj (_streamM p) (unwrap s) gs

-- With specialized types
stream
    :: (Monad m, Comonad w)
    => (a -> b -> r)
    -> Pump c d w a
    -> Tube c d m b
    -> m r
stream = _stream

streamM
    :: (Monad m, Comonad w)
    => (a -> b -> r)
    -> Pump c d w (m a)
    -> Tube c d m    b
    -> m r
streamM = _streamM

runTube
    :: Monad m
    => Tube Void Void m r
    -> m r
runTube = stream (flip const)
                 (pump (Identity ())
                       (\i -> (diverge, i))
                       const)
