{- |
Iteratee-inspired streaming library.

(c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

This exists primarily for my own education. It is updated often as I try
things and is probably, at this moment, wrong.

If you want to know more about iteratees:

    http://okmij.org/ftp/Streams.html

My goals were to (1) learn more about iteratees and (2) see how far I
could get using free monads.
 -}

module FreeStream

( TaskF(..)
, Task(..)
, Source(..)
, Sink(..)
, Action(..)
-- * Re-exports
, lift -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
-- * Core infrastructure
, await
, yield
, each
, FreeStream.Core.for
, (~>)
, (>-)
, (><)
, run
, liftT
-- * Utilities
, cat
, FreeStream.Util.map
, FreeStream.Util.drop
, FreeStream.Util.take
, FreeStream.Util.takeWhile
, FreeStream.Util.filter
, FreeStream.Util.reduce
, FreeStream.Util.every
, FreeStream.Util.unyield
) where

import Prelude hiding (map, fold, print, filter)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free

import FreeStream.Core
import FreeStream.Util
import FreeStream.Par

import Control.Monad

