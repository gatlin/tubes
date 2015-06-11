{- |
Module          : Tubes
Description     : All-encompassing module.
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental

This exists primarily for my own education. It is updated often as I try
things and is probably, at this moment, wrong.

If you want to know more about iteratees:

    http://okmij.org/ftp/Streams.html

My goals were to (1) learn more about iteratees and (2) see how far I
could get using free monads.
 -}

module Tubes

( TubeF(..)
, Tube(..)
, Source(..)
, Sink(..)
, Action(..)
-- * Re-exports
, lift -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
-- * Core infrastructure
, await
, yield
, awaitF
, yieldF
, each
, Tubes.Core.for
, (~>)
, (>-)
, (><)
, run
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
, PumpF(..)
, Pump(..)
, pump
, Pairing(..)
, pairEffect
, send
, recv
) where

import Prelude hiding (map, fold, print, filter, take)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free

import Tubes.Core
import Tubes.Util
import Tubes.Pump
