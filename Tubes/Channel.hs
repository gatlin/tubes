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
, arg
, pass
, fromSink
)
where

import Prelude hiding (map, (.), id)
import qualified Prelude as P
import Data.Profunctor
import Data.Functor.Identity
import Control.Monad.Trans.Class
import Control.Category
import Control.Arrow

import Tubes.Core
import Tubes.Util
import Tubes.Sink

{- |
A @Channel m a b@ is a one-way stream processor, transforming values of type
@a@ into values of type @b@ in some base monad @m@.

Channels may be thought of as 'Sink's followed immediately by 'Source's.
-}

newtype Channel m a b = Channel {
    tune :: Tube a b m ()
}

instance Monad m => Profunctor (Channel m) where
    lmap f ch = Channel $ map f >< tune ch
    rmap f ch = Channel $ tune ch >< map f

instance Monad m => Category (Channel m) where
    id  = Channel cat
    c_bc . c_ab = Channel $ tune c_ab >< tune c_bc

-- this and 'pass' are almost certainly wrong
arg :: a -> Pump a b Identity (Maybe b)
arg x = pumpT (Identity Nothing)
            (\_ x -> Identity (Just x))
            (\i@(Identity _) -> (x, i))

pass
    :: Monad m
    => Tube a b m ()
    -> a
    -> m b
pass tube x = (\(Just x) -> x) <$> stream const (arg x) tube

-- | This implementation almost certainly contains bugs. Here for exploration.
instance Monad m => Arrow (Channel m) where
    arr f = Channel $ map f

    first (Channel tube) = Channel $ do
        ~(b,d) <- await
        c <- lift $ pass tube b
        yield (c, d)

    second (Channel tube) = Channel $ do
        ~(d,b) <- await
        c <- lift $ pass tube b
        yield (d, c)

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
        writeOut = fromSink $ writeToFile <> writeToConsole

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

fromSink
    :: Monad m
    => Sink m a
    -> Channel m a a
fromSink (Sink tube) = Channel $ do
    a <- await
    liftT $ yield a >< tube
    yield a
