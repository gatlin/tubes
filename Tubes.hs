{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Arrows #-}

{- |
Module          : Tubes
Description     : All-encompassing module.
Copyright       : (c) 2014, 2016 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental

Write effect-ful stream processing functions and compose them into a series of
tubes.
-}

module Tubes
(
-- * Tube
-- $tubeintro
  Tube(..)
, yield
, await
, (><)
, runTube
, halt
-- * Sources
, Source(..)
, reduce
-- * Sinks
, Sink(..)
-- * Channels
, Channel(..)
, tee
-- * Pump
, Pump(..)
, send
, recv
, pumpT
, lfold
-- * Utilities
, stream
, streamM
, Tubes.Util.cat
, Tubes.Util.for
, Tubes.Util.each
, Tubes.Util.every
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.unyield
, Tubes.Util.pass
, Tubes.Util.mapM
, Tubes.Util.sequence
, Tubes.Util.stop
-- * Miscellaneous
, prompt
, display
-- * Re-exports
, liftT
, runFreeT
-- * Example
-- $tubeexample
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
-}

{- $tubeexample

Code is worth a thousand words. This program ...

@
    import Prelude hiding (map)
    import qualified Prelude as P

    import Data.Semigroup
    import Control.Monad (forever)

    import Tubes

    srcA :: MonadIO m => Source m String
    srcA = Source $ each ["line A1", "line A2", "line A3"]

    srcB :: MonadIO m => Source m String
    srcB = Source $ each ["line B1", "line B2", "line B3", "line B4"]

    -- Synchronously merge input
    srcAB :: MonadIO m => Source m String
    srcAB = srcA `merge` srcB

    writeToFile :: MonadIO m => Sink m String
    writeToFile = Sink $ do
        line <- await
        liftIO . putStrLn $ "Totally writing this to a file: " ++ line

    writeToConsole :: MonadIO m => Sink m String
    writeToConsole = Sink $ do
        line <- await
        liftIO . putStrLn $ "Console out: " ++ line

    -- Merge outputs together
    writeOut :: MonadIO m => Sink m String
    writeOut = writeToFile <> writeToConsole

    -- And make outputs re-forward their input data
    writeOut' :: MonadIO m => Channel m String String
    writeOut' = tee writeOut

    main :: IO ()
    main = runTube $ sample srcAB
                  >< tune writeOut'
                  >< pour display
@

... gives this output:

@
    Totally writing this to a file: line A1
    Console out: line A1
    line A1
    Totally writing this to a file: line B1
    Console out: line B1
    line B1
    Totally writing this to a file: line A2
    Console out: line A2
    line A2
    Totally writing this to a file: line B2
    Console out: line B2
    line B2
    Totally writing this to a file: line A3
    Console out: line A3
    line A3
    Totally writing this to a file: line B3
    Console out: line B3
    line B3
    Totally writing this to a file: line B4
    Console out: line B4
    line B4
@
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
