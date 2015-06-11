{-
Module          : Tubes.Pump
Description     : Comonadic structures to manipulate tubes ("pumps")
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental

-}

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Tubes.Pump

( Pump(..)
, PumpF(..)
, Pairing(..)
, pairEffect
, pump
, recv
, send
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Data.Functor.Identity

import Tubes.Core

data PumpF a b k = PumpF
    { recvF :: (a  , k)
    , sendF :: (b -> k)
    } deriving Functor

{- |
A 'Pump' is the dual to a 'Tube': where a 'Tube' is a computation manipulating
a stream of values, a 'Pump' can be situated on either end of a tube to both
insert values when requested and handle any yielded results.

This module is subject to change before I upload `0.2.0.0` to Hackage.
-}
type Pump a b = CofreeT (PumpF a b)

recv :: Comonad w => Pump a b w r -> (a, Pump a b w r)
recv p = recvF . unwrap $ p

send :: Comonad w => Pump a b w r -> b -> Pump a b w r
send p x = (sendF (unwrap p)) x

-- ** Pairing

{- |
Lovingly stolen from Dan Piponi and David Laing. This defines a poor man\'s
adjunction: it allows adjoint functors to essentially annihilate one another
and produce a final value.

If something equivalent comes along, I'll switch to it.
-}
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

instance Pairing (PumpF a b) (TubeF a b) where
    pair p (PumpF ak bk) tb = runT tb (\ak' -> pair p ak ak')
                                         (\bk' -> pair p bk bk')

pump :: Comonad w
       => w a
       -> (w a -> (b, w a))
       -> (c   -> w a)
       -> Pump b c w a
pump x r s = coiterT cf x where
    cf wa = PumpF (r wa) s

