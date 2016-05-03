{-
Module          : Tubes.Sink
Description     : Defines the Sink type.
Copyright       : (c) 2014-2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}
{-# LANGUAGE RankNTypes #-}

module Tubes.Sink
(
  Sink(..)
)
where

import Prelude hiding (map)
import qualified Prelude as P

import Control.Monad.IO.Class

import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..))
import Data.Semigroup

import Tubes.Core
import Tubes.Util

{- |
A potentially full sink of values parameterized over a base monad. It never
'yield's.

A 'Sink' is a contravariant functor. Intuitively this means that it is a
consumer of some base type, and you may map transformations over its input
before it is consumed.

Example:

    @
        import Data.Functor.Contravariant

        add5 :: Sink IO Int
        add5 = Sink $ loop 0 5 where
            loop acc 0 = do
                liftIO $ putStrLn $ "Sum of five numbers: " ++ (show acc)
                halt
            loop acc count = do
                n <- await
                loop (acc + n) (count - 1)

        add5 :: Sink IO Int
        add5 = (*2) >$< add5

        main :: IO ()
        main = do
            runTube $ each [1..10] >< pour add5
            -- "Sum of five numbers: 15"

            runTube $ each [1..10] >< pour add5Times2
            -- "Sum of five numbers: 30"
    @

'Sink's may also be merged together, as they form a semigroup:

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

        writeOut :: Sink IO String
        writeOut = writeToFile <> writeToConsole

        main :: IO ()
        main = do
            runTube $ each [1..3] >< map show >< forever (pour writeOut)
            -- Totally writing this to a file: 1
            -- Console out: 1
            -- Totally writing this to a file: 2
            -- Console out: 2
            -- Totally writing this to a file: 3
            -- Console out: 3
    @

-}
newtype Sink m a = Sink {
    pour :: Tube a () m ()
}

instance Monad m => Contravariant (Sink m) where
    contramap f snk = Sink $ map f >< (pour snk)

instance Monad m => Divisible (Sink m) where
    divide f (Sink sa) (Sink sb) = Sink $ do
        (a,b) <- await >>= return . f
        yield a >< sa
        yield b >< sb

    conquer = Sink $ cat >< (pour conquer)

instance Monad m => Decidable (Sink m) where
    lose f = Sink $ await >>= return . f >> return ()

    choose f sa sb = Sink $ do
        x <- await >>= return . f
        case x of
            Left a  -> yield a ><  pour sa
            Right b -> yield b >< pour sb

instance Monad m => Semigroup (Sink m a) where
    s1 <> s2 = divide (\x -> (x,x)) s1 s2

