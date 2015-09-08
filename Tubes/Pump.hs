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

(
  Pump(..)
, PumpF(..)
, mkPump
, recv
, send
, pump
, pumpM
, meta
, enumerator
, enumerate
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.Trans.Free.Church
import Control.Comonad
import Control.Comonad.Trans.Cofree
import Data.Functor.Identity
import Data.Foldable

import Tubes.Core

-- ** Definition

{- |
A 'Pump' is the dual to a 'Tube'. Intuitively, if a @Tube@ is a stream-
processing computation, then a @Pump@ is both a stream generator and reducer.

Examples may help!

One interesting use of a @Pump@ is as a data stream, which can be fed into a
@Tube@ or @Sink@.

    @
    import Data.Functor.Identity

    e :: Pump (Maybe Int) Int Identity Int
    e = mkPump (Identity 0)
            (\(Identity x) -> (Just x, Identity (x+1)))
            const

    ex1 :: IO ()
    ex1 = do
        run $ each e \>\< take 10 \>\< map show \>\< display
        -- displays 0-9 in the console
    @

A @Pump@ may also be used to fold a @Source@. Indeed, a @Pump@ may be thought
of as both a non-recursive left fold and a non-recursive unfold paired
together. (This is called a "metamorphism," hence the function "meta".)

    @
    num_src :: Source Int IO ()
    num_src = do
        forM_ [1..] $ \n -> do
            lift . putStrLn $ "Yielding " ++ (show n)
            yield n

    enum_ex :: IO ()
    enum_ex = do
        v \<\- reduce (flip send) (meta (+) 0 (\x -> (x,x))) extract $ num_src >< take 5
        putStrLn . show $ "v = " ++ (show v)
        -- v = 15
    @

The following is an example of a @Pump@ both accumulating values from a
@Source@ and then enumerating them into a @Sink@. This gives back both the
result of the computation and the unused input.

    @
    import Data.Functor.Identity

    -- a 'Sink' that stops after 5 loops, or when input is exhausted
    sum_snk :: Sink (Maybe Int) IO Int
    sum_snk = do
        ns \<\- forM [1,2,3,4,5] $ \_ -> do
            mn <- await
            case mn of
                Just n -> return [n]
                Nothing -> return []
        return $ sum . concat $ ns

    source_sink_ex :: IO ([Int], Int)
    source_sink_ex = do
        e \<\- reduce (flip send) (enumerator []) id $ num_src >< take 10
        (unused, total) <- pump (,) e sum_snk
        putStrLn $ "Total: " ++ (show total)
        putStrLn $ "Unused: " ++ (show unused)
        -- "Total: 15"
        -- "Unused: [6,7,8,9,10]"
    @

Note that when a @Pump@ and a @Tube@ are combined with 'pump', that the @Tube@
determines control flow. @Pump@s are comonads, not monads.

There are doubtless more and more interesting examples of combining @Tube@s
and @Pump@s. If you think of any, drop the author a line!
-}

type Pump a b = CofreeT (PumpF a b)

data PumpF a b k = PumpF
    { recvF :: (a  , k)
    , sendF :: (b -> k)
    } deriving Functor

instance Foldable (PumpF a b) where
    foldMap f (PumpF (_, k) _) = f k
    foldr f z (PumpF (_, k) _) = f k z

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

-- | Pull a value from a 'Pump', along with the rest of the 'Pump'.
recv :: Comonad w => Pump a b w r -> (a, Pump a b w r)
recv p = recvF . unwrap $ p

-- | Send a value into a 'Pump', effectively re-seeding the stream.
send :: Comonad w => b -> Pump a b w r -> Pump a b w r
send x p = (sendF (unwrap p)) x

-- ** Utilities

-- | Takes a fold function, an initial value, and an unfold to produce a
-- metamorphism. Can be used to change.
meta :: (x -> a -> x)
     -> x
     -> (x -> (b, x))
     -> Pump b a Identity x
meta step init done = mkPump (Identity init)
    (\(Identity xs) -> Identity <$> done xs)
    (\(Identity xs) x -> Identity (step xs x))

-- | Constructs an enumerator pump, which can buffer values and then enumerate
-- them to, say, a 'Sink' (see the examples above).
enumerator :: [a] -> Pump (Maybe a) a Identity [a]
enumerator inp = meta (\xs x -> xs ++ [x]) inp
    (\lst -> case lst of
        []      -> (Nothing, lst)
        (x:xs)  -> (Just x, xs))

-- | Transforms a 'Pump' into a corresponding 'Tube'.
enumerate :: (Monad m, Comonad w)
          => Pump (Maybe a) b w r
          -> Tube c a m ()
enumerate p = do
    let (mv, k) = recv p
    case mv of
        Nothing -> return ()
        Just v' -> yield v' >> enumerate k

-- ** Pairing

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
