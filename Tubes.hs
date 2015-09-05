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

My goals were to

* learn more about iteratees and stream processing; and

* explore the relationships between functions, pairs, sum types, and products.
-}

module Tubes
(
-- $tubeintro
-- * Tubes
  Tube(..)
, TubeF(..)
, Source(..)
, Sink(..)
-- * Core infrastructure
, run
, await
, yield
, each
, Tubes.Core.for
, (~>)
, (>-)
, (><)
, (|>)
, (-<)
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
, Tubes.Util.mapM
, Tubes.Util.sequence
, Tubes.Util.display
-- * Pump
, Pump(..)
, PumpF(..)
, mkPump
, send
, recv
, pump
, meta
, enumerator
, enumerate
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

{- $tubeintro
A 'Tube' is a computation that can yield multiple intermediate values or await
intermediate inputs before computing a final result. Any monadic function may
be turned into a 'Tube'.

'Tube's may be composed in different ways. For instance, in ghci:

    @
    >>> run $ for (each [1..4] >< map show) $ lift . putStrLn
    1
    2
    3
    4
    @

Here, 'each' converts a 'Foldable' into a 'Source' of values; 'for' performs a
computation with each value. Another example, using two built-in 'Tube's for
convenience:

    @
    >>> run $ prompt >\< filter (/= "Die Antwoord") >\< map (++ " is bad") >\< print
    > dubstep
    dubstep is bad
    > the sun
    the sun is bad
    > Die Antwoord
    > this example
    this example is bad
    @

A few stream processing combinators are provided for mapping, filtering,
taking, and other basic operations.

For those times when you want to 'reduce' a stream, you can like so:

    @
    >>> reduce (+) 0 id (each [1..10])
    55
    @

'><' is useful for combining 'Tube's which all have the same return value -
most often @()@ simply because every 'Source' will have that value.

There is more in the library not covered here, and you are encouraged to take
a look around.
-}
