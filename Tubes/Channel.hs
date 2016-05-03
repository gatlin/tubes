module Tubes.Channel
(
  Channel(..)
, arg
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

import Tubes.Core
import Tubes.Util

newtype Channel m a b = Channel {
    tune :: Tube a b m ()
}

instance Monad m => Profunctor (Channel m) where
    lmap f ch = Channel $ map f >< tune ch
    rmap f ch = Channel $ tune ch >< map f

instance Monad m => Category (Channel m) where
    id  = Channel cat
    c_bc . c_ab = Channel $ tune c_ab >< tune c_bc

arg :: a -> Pump a b Identity (Maybe b)
arg x = pump (Identity Nothing)
            (\i@(Identity _) -> (x, i))
            (\_ x -> Identity (Just x))

pass
    :: Monad m
    => Tube a b m ()
    -> a
    -> m b
pass tube x = (\(Just x) -> x) <$> stream const (arg x) tube

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

