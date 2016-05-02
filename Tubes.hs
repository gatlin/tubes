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
, contramap
, (>$<)
, divide
, conquer
, choose
, lose
)
where

import Tubes.Core
import Tubes.Util
import Tubes.Source
import Tubes.Sink

import System.IO
import Control.Monad.IO.Class
import Control.Monad (unless, forever)

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible

import Prelude hiding (map, filter, take, drop, takeWhile, sequence)
import qualified Prelude as P

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

insult :: MonadIO m => Sink m String
insult     = (++ " is a knob") >$< display

compliment :: MonadIO m => Sink m String
compliment = (++ " is totally rad") >$< display

output :: MonadIO m => Sink m String
output = divide (\x -> (x,x)) insult compliment
