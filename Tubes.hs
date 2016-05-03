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
, halt
, Source(..)
, reduce
, Sink(..)
, Channel(..)
, fromSink
-- * Pumps
-- $pumpintro
, Pump(..)
, send
, recv
, pumpT
, lfold
-- * Utilities
-- $utilintro
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

import Data.Semigroup

{- $tubeintro

Code is worth a thousand words. This program ...

    @
        import Prelude hiding (map)
        import qualified Prelude as P

        import Data.Semigroup
        import Control.Monad (forever)

        import Tubes

        srcA :: Source IO String
        srcA = Source $ each ["line A1", "line A2", "line A3"]

        srcB :: Source IO String
        srcB = Source $ each ["line B1", "line B2", "line B3", "line B4"]

        -- Synchronously merge input
        srcAB :: Source IO String
        srcAB = srcA <> srcB

        writeToFile :: Sink IO String
        writeToFile = Sink $ do
            line <- await
            liftIO . putStrLn $ "Totally writing this to a file: " ++ line

        writeToConsole :: Sink IO String
        writeToConsole = Sink $ do
            line <- await
            liftIO . putStrLn $ "Console out: " ++ line

        -- Merge outputs together
        writeOut :: Sink IO String
        writeOut = writeToFile <> writeToConsole

        -- And make outputs re-forward their input data
        writeOut' :: Channel IO String String
        writeOut' = fromSink writeOut

        main :: IO ()
        main = runTube $ sample srcAB
                      >< forever (tune writeOut')
                      >< pour display -- builtin
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

{- $utilintro
TODO: Write this
-}

{- $pumpintro
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

src1 :: Source IO String
src1 = Source $ each ["line A1", "line A2", "line A3"]

src2 :: Source IO String
src2 = Source $ each ["line B1", "line B2", "line B3", "line B4"]

-- Synchronously merge input
src3 :: Source IO String
src3 = src1 <> src2

writeToFile :: Sink IO String
writeToFile = Sink $ do
    line <- await
    liftIO . putStrLn $ "Totally writing this to a file: " ++ line

writeToConsole :: Sink IO String
writeToConsole = Sink $ do
    line <- await
    liftIO . putStrLn $ "Console out: " ++ line

-- Merge outputs together
writeOut :: Sink IO String
writeOut = writeToFile <> writeToConsole

-- And make outputs re-forward their input data
writeOut' :: Channel IO String String
writeOut' = fromSink writeOut
