{-
Module          : FreeStream.Par
Description     : Stream-processing utilities related to parallelism
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

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

-- | Broadcast the values of a source 'Task' to 'Functor' of sink @Task@s
broadcast :: (Functor f, Monad m)
          =>     Task a b m r
          -> f ( Task b c m r )
          -> f ( Task a c m r )
broadcast src tsks = fmap (\t -> src >< t) tsks

-- | Infix synonym for 'broadcast'
(*<) :: (Functor f, Monad m)
     =>     Task a b m r
     -> f ( Task b c m r )
     -> f ( Task a c m r )
(*<) = broadcast
infixl 9 *<

-- | Naively merges the values of some 'Functor' of source 'Task's
merge :: ( Foldable t, Monad m )
      => t ( Task a c m s )
      -> Task a c m ()
merge tasks = for (each tasks) (\t -> for t yield)

-- | 'merge's a 'Functor' of source 'Task's and pipes ('><') to a sink @Task@
(>*) :: (Foldable t, Monad m)
     => t ( Task a b m s )
     ->     Task b c m ()
     ->     Task a c m ()
tasks >* k = (merge tasks) >< k
infixr 0 >*

