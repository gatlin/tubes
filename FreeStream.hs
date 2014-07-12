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
 -
 - This code is licensed under the WTFPL: http://www.wtfpl.net/
 -}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

import Prelude hiding (sequence)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Control.Monad (forever)
import Data.Traversable
import Data.Monoid

{- | A stream of data.
 -
 - A chunk of data wrapped in some functor, or the End of the stream.
 -}
data Stream f a where
    End   :: forall a f. Functor f => Stream f a
    Chunk :: forall a f. Functor f => f a -> Stream f a

deriving instance Show (f a) => Show (Stream f a)

{- | Process
 -
 - A process (also called an iteratee) is a function folded over a branching
 - stream. A process exists in one of two states: awaiting a new value, or
 - yielding a result value.
 -
 - Processes are monad transformers, meaning you can construct processes which
 - fold effectful computations over streams of data.
 -}
data ProcessF a b where
    Await :: (a -> b) -> ProcessF a b

deriving instance Functor (ProcessF a)
type ProcessT m f a r = FreeT (ProcessF (Stream f a)) m ((Stream f r),(Stream f a))

{- | Useful utilities -}

await = liftF $ Await id

yield :: (Monad m, Functor f)
      => f b
      -> m (Stream f b, Stream f a)
yield x = return (Chunk x, End)

run = runFreeT
liftT = lift . runFreeT

-- | Enumerate the contents of a list to a ProcessT
forList lst s = run s >>= go lst where
    go []     (Free (Await f)) = run (f End) >>= go []
    go (x:xs) (Free (Await f)) = run (f (Chunk [x])) >>= go xs
    go []     (Pure v)         = return v
    go lst    (Pure (v,_))     = return (v, Chunk lst)

poll src = run src >>= go where
    go (Pure (v,k))     = return (v, k)
    go (Free (Await f)) = run (f End) >>= go

