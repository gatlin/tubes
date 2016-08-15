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
import Data.Semigroup

import System.IO

import qualified Data.Foldable as F

import Tubes.Core
import Tubes.Util

{- |
An exhaustible source of values parameterized over a base monad. It never
'await's, it only 'yield's.

'Source's are monad transformers in their own right, as they are possibly
finite. They may also be synchronously merged as monoids:

@
    import Data.Monoid

    src1 :: Source IO String
    src1 = Source $ each ["line A1", "line A2", "line A3"]

    src2 :: Source IO String
    src2 = Source $ each ["line B1", "line B2", "line B3", "line B4"]

    src3 :: Source IO String
    src3 = src1 <> src2

    main :: IO ()
    main = runTube $ sample (src1 <> src2) >< pour display
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
    (<>) = mappend

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
