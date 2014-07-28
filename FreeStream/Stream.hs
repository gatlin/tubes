{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- | Streams
 -
 - The goal is to model exhaustible streams for use with Processes.
 -}

module FreeStream.Stream

( StreamF(..)
, Stream(..)
, message
, recv
, halt
) where

import Data.Foldable
import Data.Traversable
import Control.Alternative.Free
import Control.Applicative

newtype StreamF a = Message {
    recvF :: Maybe a
} deriving (Show, Eq)

deriving instance Functor StreamF
type Stream = Alt StreamF

message :: a -> Stream a
message x = liftAlt $ Message . Just $ x

recv :: Stream a -> Maybe a
recv s = runAlt recvF s

halt :: Stream a
halt = empty
