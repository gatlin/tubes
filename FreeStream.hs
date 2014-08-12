{- |
 - Iteratee-inspired streaming library.
 -
 - (c) 2014, Gatlin Johnson <gatlin@niltag.net>
 -
 - This exists primarily for my own education. It is updated often as I try
 - things and is probably, at this moment, wrong.
 -
 - If you want to know more about iteratees:
 -
 -     http://okmij.org/ftp/Streams.html
 -
 - My goals were to (1) learn more about iteratees and (2) see how far I
 - could get using free monads.
 -}

{-# LANGUAGE RankNTypes #-}

module FreeStream

( ProcessF(..)
, Process(..)
, Generator(..)
, Sink(..)
, Action(..)
-- * Re-exports
, lift -- re-exported from Control.Monad.Trans.Free
, runFreeT -- re-exported from Control.Monad.Trans.Free
-- * Core infrastructure
, await
, yield
, each
, FreeStream.Core.iterate
, FreeStream.Core.for
, (|>)
, (+>)
, (>+)
, (~>)
, run
, liftT
-- * Utilities
, cat
, FreeStream.Util.map
, FreeStream.Util.drop
, FreeStream.Util.take
, FreeStream.Util.takeWhile
, FreeStream.Util.filter
, FreeStream.Util.fold
, FreeStream.Util.accum
, (|-)
-- * Stream
, Stream(..)
, StreamF(..)
, chunk
, recv
, halt
, stream
) where

import Prelude hiding (map, fold, iterate)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad.Free
import Control.Monad (forever, unless, replicateM_, when)
import Data.Monoid ((<>), mempty, Monoid)

import FreeStream.Core
import FreeStream.Stream
import FreeStream.Util

{- | Useful utilities -}

