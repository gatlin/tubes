{-
Module          : Tubes.Channel
Description     : Defines the Channel type.
Copyright       : (c) 2014-2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}
module Tubes.Channel
(
  Channel(..)
, tee
)
where

import Prelude hiding (map, (.), id)
import qualified Prelude as P
import Data.Profunctor
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Category
import Control.Arrow
import Control.Monad (forever)
import Control.Monad.Trans.Free

import Tubes.Core
import Tubes.Util
import Tubes.Sink

{- |
A @Channel m a b@ is a stream processor which converts values of type @a@ into
values of type @b@, while also performing side-effects in some monad @m@.

If a @Channel@ 'yield's exactly once after each time it 'await's then it may be
safely treated as an @Arrow@. For example:

@
    {&#45;\# LANGUAGE Arrows \#&#45;}

    import Tubes
    import Control.Arrow
    import Prelude hiding (map)

    -- A simple channel which accumulates a total
    total :: (Num a, Monad m) => Channel m a a
    total = Channel $ loop 0 where
        loop acc = do
            n <- await
            let acc' = n + acc
            yield acc'
            loop acc'

    -- A running average using two totals in parallel
    avg :: (Fractional a, Monad m) => Channel m a a
    avg = proc value -> do
        t <- total -< value
        n <- total -< 1
        returnA -< t / n

    main :: IO ()
    main = runTube $ each [0,10,7,8]
                  >< tune avg
                  >< map show
                  >< pour display

@

This program would output

@
    0.0
    5.0
    5.666666666666667
    6.25
@

This has interesting potential in FRP applications.

-}

newtype Channel m a b = Channel {
    tune :: Tube a b m ()
}

instance Monad m => Profunctor (Channel m) where
    lmap f ch = Channel $ map f >< tune ch
    rmap f ch = Channel $ tune ch >< map f

instance Monad m => Category (Channel m) where
    id  = Channel cat
    c_bc . c_ab = Channel $ forever (tune c_ab) >< forever (tune c_bc)

instance Monad m => Functor (Channel m a) where
    fmap = rmap

instance Monad m => Applicative (Channel m a) where
    pure x = Channel $ await >>= \_ -> yield x
    ch1 <*> ch2 = ch1 &&& ch2 >>> arr (\(f,x) -> f x)

-- Not sure how useful this is but whatever
instance Monad m => Monad (Channel m a) where
    return = pure
    ma >>= f = _join (fmap f ma) where
        _join (Channel tb) = Channel $ loop tb
        loop tb = do
            a <- await
            mChB <- lift $ pass a tb
            case mChB of
                Nothing -> halt
                Just ((Channel tbB), tb') -> do
                    mB <- lift $ pass a tbB
                    case mB of
                        Nothing -> halt
                        Just (b, _) -> do
                            yield b
                            loop tb'

instance (Monad m) => Arrow (Channel m) where
    arr f = Channel $ map f

    first ch = Channel $ loop (tune ch) where
        loop tb = do
            ~(b,d) <- await
            mr <- lift $ pass b tb
            maybe halt (\(c,k) -> (yield (c,d)) >> loop k) mr

    second ch = Channel $ loop (tune ch) where
        loop tb = do
            ~(d,b) <- await
            mr <- lift $ pass b tb
            maybe halt (\(c,k) -> (yield (d, c)) >> loop k) mr

instance (Monad m) => ArrowChoice (Channel m) where
    (Channel tb1) +++ (Channel tb2) = Channel $ loop tb1 tb2  where
        loop t1 t2 = await >>=
            either
            (\l -> (lift $ pass l t1) >>=
                maybe halt (\(out, t1') ->
                                (yield $ Left out) >> loop t1' t2))
            (\r -> (lift $ pass r t2) >>=
                maybe halt (\(out, t2') ->
                                (yield $ Right out) >> loop t1 t2'))

    left (Channel tb) = Channel $ loop tb where
        loop tb = await >>=
            either
            (\v -> (lift $ pass v tb) >>=
                maybe halt (\(out, tb') ->
                                (yield $ Left out) >> loop tb'))
            (\v -> (yield $ Right v) >> loop tb)

    right (Channel tb) = Channel $ loop tb where
        loop tb = await >>=
            either
            (\v -> (yield $ Left v) >> loop tb)
            (\v -> (lift $ pass v tb) >>=
                maybe halt (\(out, tb') ->
                                (yield $ Right out) >> loop tb'))

{- |
Convert a 'Sink m a' into a 'Channel m a a', re-forwarding values downstream.

Useful example:

@
    import Data.Semigroup

    writeToFile :: Sink IO String
    writeToFile = Sink $ do
        line <- await
        liftIO . putStrLn $ "Totally writing this to a file: " ++ line

    writeToConsole :: Sink IO String
    writeToConsole = Sink $ do
        line <- await
        liftIO . putStrLn $ "Console out: " ++ line

    writeOut :: Channel IO String String
    writeOut = tee $ writeToFile <> writeToConsole

    main :: IO ()
    main = runTube $ each ["a","b","c"] \>\< forever (tune writeOut) \>\< pour display
    --  Totally writing this to a file: a
    --  Console out: a
    --  a
    --  Totally writing this to a file: b
    --  Console out: b
    --  b
    --  Totally writing this to a file: c
    --  Console out: c
    --  c
@

This takes advantage of the divisible nature of 'Sink's to merge effectful
computations and then continue the process.
-}

tee
    :: Monad m
    => Sink m a
    -> Channel m a a
tee (Sink tube) = Channel $ forever $ do
    a <- await
    liftT $ yield a >< tube
    yield a
