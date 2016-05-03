{-# LANGUAGE RankNTypes #-}


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
-}

module Tubes
(
-- $tubeintro
-- * Tubes
  Tube(..)
, runTube
, yield
, await
, Source(..)
, reduce
, Sink(..)
, Channel(..)
-- * Pumps
, Pump(..)
, send
, recv
, pump
, meta
-- * Utilities
, stream
, streamM
, Tubes.Util.cat
, Tubes.Util.for
, Tubes.Util.each
, Tubes.Util.every
, (~>)
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.unyield
, Tubes.Util.mapM
, Tubes.Util.sequence
-- * Miscellaneous
, prompt
, display
-- * Re-exports
, liftT
, runFreeT
)
where

import Tubes.Core
import Tubes.Util
import Tubes.Source
import Tubes.Sink
import Tubes.Channel

import System.IO
import Control.Monad.IO.Class
import Control.Monad (unless, forever)

{- $tubeintro
TODO: Write this
-}

-- | Source of 'String's from stdin. This is mostly for debugging / ghci example purposes.
prompt :: MonadIO m => Source m String
prompt = Source $ do
    liftIO . putStr $ "> "
    eof <- liftIO isEOF
    unless eof $ do
        str <- liftIO getLine
        yield str
        sample prompt

-- | Sink for 'String's to stdout. This is mostly for debugging / ghci example
-- purposes.
display :: MonadIO m => Sink m String
display = Sink $ forever $ do
    it <- await
    liftIO . putStrLn $ it

