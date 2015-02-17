module FreeStream.Par

( broadcast
, merge
, (*<)
, (>*)
)

where

import Prelude hiding (map)

import FreeStream.Core
import FreeStream.Util

import Data.Foldable
import Control.Monad.Trans.Class

broadcast :: (Functor f, Monad m)
          =>     Task a b m r
          -> f ( Task b c m r )
          -> f ( Task a c m r )
broadcast src tsks = fmap (\t -> src >< t) tsks

(*<) :: (Functor f, Monad m)
     =>     Task a b m r
     -> f ( Task b c m r )
     -> f ( Task a c m r )
(*<) = broadcast
infixl 9 *<

merge :: ( Foldable t, Monad m )
      => t ( Task a c m s )
      -> Task a c m ()
merge tasks = for (each tasks) (\t -> for t yield)

(>*) :: (Foldable t, Monad m)
     => t ( Task a b m s )
     ->     Task b c m ()
     ->     Task a c m ()
tasks >* k = (merge tasks) >< k
infixr 0 >*

