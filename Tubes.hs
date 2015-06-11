{- |
Module          : Tubes
Description     : All-encompassing module.
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental

Write effect-ful stream processing functions and compose them into a series of
tubes.

This exists primarily for my own education. It is updated often as I try
things and is probably, at this moment, wrong.

If you want to know more about efficient stream processing:

    http://okmij.org/ftp/Streams.html

My goals were to

* learn more about iteratees and

* explore the relationships between functions, pairs, sum types, and products.
-}

module Tubes
(
-- * Tubes
  Tube(..)
, TubeF(..)
, Source(..)
, Sink(..)
, Action(..)
-- * Core infrastructure
, run
, await
, yield
, each
, Tubes.Core.for
, (~>)
, (>-)
, (><)
, liftT
-- * Utilities
, cat
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.reduce
, Tubes.Util.every
, Tubes.Util.unyield
, Tubes.Util.prompt
, Tubes.Util.display
-- * Pump
, Pump(..)
, PumpF(..)
, pump
, send
, recv
, runPump
-- * Re-exports
, lift -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
) where

import Prelude hiding (map, fold, print, filter, take)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free

import Tubes.Core
import Tubes.Util
import Tubes.Pump

import Control.Comonad
import Data.Functor.Identity

p :: Pump Bool String Identity Int
p = pump (Identity 1) (\wa -> (even (extract wa), fmap (+1) wa))
                      (\wa str -> fmap (+(length str)) wa)
