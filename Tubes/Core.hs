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
, pumpT
, send
, recv
, stream
, streamM
, runTube
, runFreeT
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

-- * Tubes

{- |
@TubeF@ defines the language of 'Tube's - namely, 'yield' and 'await'.

This type is merely the CPS-encoded version of the following much friendlier
data type:

    @
        data TubeF a b k
            = Await (a -> k)
            | Yield (b  , k)
            deriving (Functor)
    @

This says: a tube computation is either paused awaiting upstream data, or
paused yielding data downstream. The free monad transformer fleshes out the
other cases, namely having finished with a final result value or wrapping a
lower monad.
-}
newtype TubeF a b k = TubeF {
    runTubeF :: forall r.
                 ((a -> k) -> r)
              -> ((b ,  k) -> r)
              -> r
} deriving (Functor)

-- | Value constructor for the first 'TubeF' case.
awaitF :: (a -> k) -> TubeF a b k
awaitF f = TubeF $ \a _ -> a f

-- | Value constructor for the second 'TubeF' case.
yieldF :: b -> k -> TubeF a b k
yieldF x k = TubeF $ \_ y -> y (x, k)

{- |
The central data type. @Tube@s stacked on top of the same base monad @m@ may be
composed in series, so long as their type arguments agree. 
-}
type Tube a b = FreeT (TubeF a b)

await :: Monad m => Tube a b m a
await = improveT $ liftF $ awaitF id

yield :: Monad m => b -> Tube a b m ()
yield x = improveT $ liftF $ yieldF x ()

liftT :: (MonadTrans t, Monad m)
      => FreeT f m a
      -> t m (FreeF f a (FreeT f m a))
liftT = lift . runFreeT

{- |
Compose a 'Tube' emitting values of type @b@ with a continuation producing a
suitable successor.

Used primarily to define '(><)'.
-}
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

{- |
Compose compatible tubes in series to produce a new 'Tube'.
-}
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

-- * Pumps

{- |
Pumps are the dual of 'Tube's. Where a 'Tube' may either be 'await'ing or
'yield'ing, a 'Pump' is always in a position to 'send' or 'recv' data. They are
the machines which run 'Tube's, essentially.

Pumps may be used to formulate infinite streams and folds.

TODO: more examples!

Note the type arguments are "backward" from the 'Tube' point of view: a
@Pump b a w r@ may be sent values of type @a@ and you may receive @b@ values
from it.
-}
type Pump b a = CofreeT (PumpF b a)

{- |
The basis for the 'Pump' comonad transformer. This says that a pump computation
can send and receive data.
-}
data PumpF b a k = PumpF
    { sendF :: (a -> k)
    , recvF :: (b  , k)
    } deriving (Functor)

-- | Send a 'Pump' a value, yielding a new 'Pump'.
send :: Comonad w => b -> Pump a b w r -> Pump a b w r
send x p = (sendF (unwrap p)) x

-- | Receive a value from a 'Pump', along with a new 'Pump' for the future.
recv :: Comonad w => Pump a b w r -> (a, Pump a b w r)
recv p = recvF . unwrap $ p

-- | Construct a 'Pump' based on an arbitrary comonad.
pumpT
    :: Comonad w
    => w r
    -> (w r -> b -> w r)
    -> (w r -> (a, w r))
    -> Pump a b w r
pumpT x s r = coiterT cf x where
    cf wa = PumpF (s wa) (r wa)

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

-- | Process a 'Tube' stream with a given 'Pump', and merge their results.
stream
    :: (Monad m, Comonad w)
    => (a -> b -> r)
    -> Pump c d w a
    -> Tube c d m b
    -> m r
stream = _stream

-- | Process a 'Tube' stream with an effectful 'Pump', and merge their results.
streamM
    :: (Monad m, Comonad w)
    => (a -> b -> r)
    -> Pump c d w (m a)
    -> Tube c d m    b
    -> m r
streamM = _streamM

-- | Run a self-contained 'Tube' computation.
runTube
    :: Monad m
    => Tube () () m r
    -> m r
runTube = stream (flip const)
                 (pumpT (Identity ())
                        const
                        (\i -> ((), i)))

fix :: (a -> a) -> a
fix f = let x = f x in x

-- | Used only in situations where a dummy value is needed. Actively working to
-- get rid of this.
diverge :: a
diverge = fix id

