{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tubes.Source
(
  Source(..)
, reduce
)
where

import Prelude hiding (map)
import qualified Prelude as P

import Control.Monad.IO.Class 
import Control.Monad.Trans (MonadTrans(..), lift)
import Control.Monad.Trans.Free
import Control.Monad (MonadPlus(..))
import Control.Applicative (Applicative(..), Alternative(..))

import System.IO

import qualified Data.Foldable as F

import Tubes.Core
import Tubes.Util

{- |
An exhaustible source of values parameterized over a base monad.
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

    s1 <|> s2 = Source $ do
        sample s1
        sample s2

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

{- |
Strict left-fold of a stream. Note that the actual return type of the source
is not relevant, only the intermediate yield type.
-}
reduce :: Monad m
       => (x -> a -> x) -- ^ step function
       -> x             -- ^ initial value
       -> (x -> b)      -- ^ final transformation
       -> Source m a
       -> m b
reduce step begin done p0 = runFreeT (sample p0) >>= \p' -> loop p' begin where
    loop (Pure _) x = return (done x)
    loop (Free p) x = runTubeF p diverge (\(v, k) ->
        runFreeT k >>= \k' -> loop k' $! step x v)
