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
, pass
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
A @Channel m a b@ is a one-way stream processor, transforming values of type
@a@ into values of type @b@ in some base monad @m@.

Channels may be thought of as 'Sink's followed immediately by 'Source's.

You may also write Arrow computations with them:

    @
        {-# LANGUAGE Arrows #-}

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

instance (Monad m) => Arrow (Channel m) where
    arr f = Channel $ map f

    first ch = Channel $ loop (tune ch) where
        loop tb = do
            ~(b,d) <- await
            (c,k) <- lift $ pass b tb
            yield (c, d)
            loop k

    second ch = Channel $ loop (tune ch) where
        loop tb = do
            ~(d,b) <- await
            (c,k) <- lift $ pass b tb
            yield (d, c)
            loop k


-- | This function assumes that a 'Tube' will alternate 'await'ing and
-- 'yield'ing in succession.
pass :: Monad m => a -> Tube a b m () -> m (b, Tube a b m ())
pass arg tb = do
    Free tb' <- runFreeT tb
    let k = runTubeF tb' (\ak -> ak arg) diverge
    Just r <- unyield k
    return r

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
        main = runTube $ each ["a","b","c"] >< forever (tune writeOut) >< pour display
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
tee (Sink tube) = Channel $ do
    a <- await
    liftT $ yield a >< tube
    yield a
