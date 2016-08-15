{-
Module          : Tubes.Source
Description     : Defines the Source monad.
Copyright       : (c) 2014-2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Tubes.Source
(
  Source(..)
, reduce
, merge
)
where

import Prelude hiding (map)
import qualified Prelude as P

import Control.Monad.IO.Class
import Control.Monad.Trans (MonadTrans(..), lift)
import Control.Monad.Trans.Free
import Control.Monad (MonadPlus(..))
import Control.Applicative (Applicative(..), Alternative(..), liftA2)
import Data.Semigroup

import System.IO

import Data.Foldable (Foldable(..))
import qualified Data.Foldable as F
import Data.Traversable (Traversable(..))
import qualified Data.Traversable as T

import Tubes.Core
import Tubes.Util

{- |
An exhaustible source of values parameterized over a base monad. It never
'await's, it only 'yield's.

'Source's are monad transformers in their own right, as they are possibly
finite. They may also be synchronously merged:

@

    src1 :: Source IO String
    src1 = Source $ each ["line A1", "line A2", "line A3"]

    src2 :: Source IO String
    src2 = Source $ each ["line B1", "line B2", "line B3", "line B4"]

    src3 :: Source IO String
    src3 = src1 `merge` src2

    main :: IO ()
    main = runTube $ sample src3 >< pour display
    -- line A1
    -- line B1
    -- line A2
    -- line B2
    -- line A3
    -- line B3
    -- line B4
@

If one source runs out, the other will continue until completion.
-}
newtype Source m a = Source {
    sample :: Tube () a m ()
}

instance Monad m => Functor (Source m) where
    fmap f src = Source $ (sample src) >< map f

instance Monad m => Applicative (Source m) where
    pure x = Source $ yield x

    srcF <*> srcA = Source $
        for (sample srcF) $ \f ->
            for (sample srcA) $ \a ->
                yield (f a)

instance (Monad m) => Monad (Source m) where
    return = pure
    ma >>= f = Source $ for (sample ma) $ \a -> sample (f a)
    fail _ = mzero

instance Monad m => Alternative (Source m) where
    empty = Source $ return ()

    (Source s1) <|> (Source s2) = Source $ loop s1 s2 where
        loop s1 s2 = do
            k <- lift $  unyield s1
            case k of
                Nothing -> s2
                Just (v, sk) -> yield v >> loop sk s2

instance MonadTrans Source where
    lift m = Source $ do
        a <- lift m
        yield a

instance (MonadIO m) => MonadIO (Source m) where
    liftIO m = lift $ liftIO m

instance (Monad m) => MonadPlus (Source m) where
    mzero = empty
    mplus = (<|>)

instance (Monad m) => Monoid (Source m a) where
    mempty = empty
    mappend = (<|>)

instance (Monad m) => Semigroup (Source m a) where
    (<>) = (<|>)

instance (Monad m, Num a) => Num (Source m a) where
    fromInteger n = pure $ fromInteger n
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum

    (+) = liftA2 (+)
    (*) = liftA2 (*)
    (-) = liftA2 (-)

instance (Monad m, Fractional a) => Fractional (Source m a) where
    fromRational n = pure $ fromRational n
    recip = fmap recip
    (/) = liftA2 (/)

instance (Monad m, Floating a) => Floating (Source m a) where
    pi = pure pi
    exp = fmap exp
    sqrt = fmap sqrt
    log = fmap log
    sin = fmap sin
    tan = fmap tan
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    tanh = fmap tanh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

    (**) = liftA2 (**)
    logBase = liftA2 logBase

{- |
Strict left-fold of a 'Source', using a 'Pump' internally.
-}
reduce
    :: Monad m
    => (b -> a -> b)
    -> b
    -> Tube () a m ()
    -> m b
reduce step begin src = stream const f src where
    f = lfold step (\x -> ((), x)) begin

{- |
Interleave the values of two 'Source's until both are exhausted.
-}
merge :: Monad m => Source m a -> Source m a -> Source m a
merge (Source s1) (Source s2) = Source $ loop s1 s2 where
    loop s1 s2 = do
        mR1 <- lift $ unyield s1
        case mR1 of
            Nothing -> s2
            Just (v1, s1') -> do
                yield v1
                mR2 <- lift $ unyield s2
                case mR2 of
                    Nothing -> s1'
                    Just (v2, s2') -> do
                        yield v2
                        loop s1' s2'
