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
(
-- * Basic definitions
  Tube(..)
, TubeF(..)
, Pump(..)
, PumpF(..)
-- * Type aliases
, Source(..)
, Sink(..)
-- * Core commands
, run
, await
, yield
, send
, recv
, pump
, pumpM
, mkPump
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
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Data.Foldable
import Data.Functor.Identity

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
A 'TubeT' is a computation which can

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

-- | Command to wait for a new value upstream
await :: Monad m => Tube a b m a
await = improveT $ liftF $ awaitF id

-- | Command to send a value downstream
yield :: Monad m => b -> Tube a b m ()
yield x = improveT $ liftF $ yieldF x ()

-- ** Pumps

type Pump a b = CofreeT (PumpF a b)

data PumpF a b k = PumpF
    { recvF :: (a  , k)
    , sendF :: (b -> k)
    } deriving Functor

instance Foldable (PumpF a b) where
    foldMap f (PumpF (_, k) _) = f k
    foldr f z (PumpF (_, k) _) = f k z

-- | Pull a value from a 'Pump', along with the rest of the 'Pump'.
recv :: Comonad w => Pump a b w r -> (a, Pump a b w r)
recv p = recvF . unwrap $ p

-- | Send a value into a 'Pump', effectively re-seeding the stream.
send :: Comonad w => b -> Pump a b w r -> Pump a b w r
send x p = (sendF (unwrap p)) x

{- |
Creates a 'Pump' for a 'Tube' using a comonadic seed value, a function to give
it more data upon request, and a function to handle any yielded results.

Values received from the 'Tube' may be altered and sent back into the tube,
hence this mechanism does act like something of a pump.
-}
mkPump :: Comonad w
       => w a
       -> (w a -> (b, w a))
       -> (w a -> c -> w a)
       -> Pump b c w a
mkPump x r s = coiterT cf x where
    cf wa = PumpF (r wa) (s wa)

{- |
Lovingly stolen from Dan Piponi and David Laing. This models a poor man\'s
adjunction: it allows adjoint functors to essentially annihilate one another
and produce a final value.

If this or something equivalent turns up in a separate package I will happily
switch to using that.
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

pairEffectM :: (Pairing f g, Comonad w, Monad m)
            => (a -> b -> r) -> CofreeT f w (m a) -> FreeT g m b -> m r
pairEffectM p s c = do
    a <- extract s
    mb <- runFreeT c
    case mb of
        Pure x -> return $ p a x
        Free gs -> pair (pairEffectM p) (unwrap s) gs

instance Pairing (PumpF a b) (TubeF a b) where
    pair p (PumpF ak bk) tb = runT tb (\ak' -> pair p ak ak')
                                      (\bk' -> pair p bk bk')

{-|
Given a suitably matching 'Tube' and 'Pump', you can use the latter to execute
the former.
-}
pump :: (Comonad w, Monad m)
     => (x -> y -> r) -> Pump a b w x -> Tube a b m y -> m r
pump = pairEffect

{-|
A variant of 'pump' which allows effects to be executed inside the pump as well.
-}
pumpM :: (Comonad w, Monad m)
      => (x -> y -> r) -> Pump a b w (m x) -> Tube a b m y -> m r
pumpM = pairEffectM

{- |
Runs a tube computation, producing a result value in the base monad.

Because of higher-rank polymorphism, tubes created using a 'Source' and '><'
will work with this function as well.

Similarly, any tube created using '|>' and a 'Sink' will work as well. This is
an improvement over the behavior of 'runFreeT' which gives back an unevaluated
'FreeT' tree.

An example (using @num_src@ and @src_snk@ defined previously in this
documentation):

    @
        num_src :: Source Int IO ()
        num_src = do
            forM_ [1..] $ \\n -> do
                lift . putStrLn $ "Yielding " ++ (show n)
                yield n

        sum_snk :: Sink (Maybe Int) IO Int
        sum_snk = do
            ns \<\- forM [1,2,3,4,5] $ \\_ -> do
                mn <- await
                case mn of
                    Just n -> return [n]
                    Nothing -> return []
            return $ sum . concat $ ns

        >>> run $ num_src |> sum_snk
        15
    @

@15@ is the return value from @sum_snk@. Both the source and the sink have the
ability to terminate the computation by 'return'ing, perhaps when the source is
exhausted or the sink is full.
-}
run :: Monad m => Tube (Maybe a) b m r -> m r
run tb = pump (flip const) p tb where
    p = mkPump (Identity ())
            (\i@(Identity ()) -> (Nothing, i))
            const
