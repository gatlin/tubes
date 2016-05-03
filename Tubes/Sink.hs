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

import Tubes.Core
import Tubes.Util

{- |
A potentially full sink of values parameterized over a base monad.
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
