{-
Module          : Tubes.Par
Description     : Stream-processing utilities related to parallelism
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

module Tubes.Par

( broadcast
, merge
, (*<)
, (>*)
)

where

import Prelude hiding (map)

import Tubes.Core
import Tubes.Util

import Data.Foldable
import Control.Monad.Trans.Class

-- | Broadcast the values of a source 'Task' to 'Functor' of sink @Task@s
broadcast :: (Functor f, Monad m)
          =>     Tube a b m r
          -> f ( Tube b c m r )
          -> f ( Tube a c m r )
broadcast src tsks = fmap (\t -> src >< t) tsks

-- | Infix synonym for 'broadcast'
(*<) :: (Functor f, Monad m)
     =>     Tube a b m r
     -> f ( Tube b c m r )
     -> f ( Tube a c m r )
(*<) = broadcast
infixl 9 *<

-- | Naively merges the values of some 'Functor' of source 'Tube's
merge :: ( Foldable t, Monad m )
      => t ( Tube a c m s )
      -> Tube a c m ()
merge tasks = for (each tasks) (\t -> for t yield)

-- | 'merge's a 'Functor' of source 'Tube's and pipes ('><') to a sink @Tube@
(>*) :: (Foldable t, Monad m)
     => t ( Tube a b m s )
     ->     Tube b c m ()
     ->     Tube a c m ()
tasks >* k = (merge tasks) >< k
infixr 0 >*

