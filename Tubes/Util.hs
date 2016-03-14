{-
Module          : Tubes.Util
Description     : Optional stream processing utilities
Copyright       : (c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

License         : GPL-3
Maintainer      : gatlin@niltag.net
Stability       : experimental
-}

{-# LANGUAGE Rank2Types #-}

module Tubes.Util

( cat
, Tubes.Util.map
, Tubes.Util.drop
, Tubes.Util.take
, Tubes.Util.takeWhile
, Tubes.Util.filter
, Tubes.Util.reduce
, Tubes.Util.every
, Tubes.Util.prompt
, Tubes.Util.display
, Tubes.Util.unyield
, Tubes.Util.mapM
, Tubes.Util.sequence
, Tubes.Util.meta
, Tubes.Util.enumerate
, Tubes.Util.enumerator
, (><)
, (>-)
, Tubes.Util.for
, Tubes.Util.each
, (~>)
, (-<)
, (|>)
) where

import Prelude hiding (map, mapM)
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans
import Control.Monad.Trans.Free
import Control.Monad.IO.Class
import Control.Comonad
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid (Monoid, mappend, mempty)
import System.IO

import Tubes.Core

-- | Connect a task to a continuation yielding another task; see '><'
(>-) :: Monad m
     => Tube a b m r
     -> (b -> Tube b c m r)
     -> Tube a c m r
p >- f = liftT p >>= go where
    go (Pure x) = return x
    go (Free p') = runT p' (\f' -> wrap $ awaitF (\a -> (f' a) >- f))
                           (\(v, k) -> k >< f v)

-- | Compose two tubes into a new tube.
(><) :: Monad m
     => Tube a b m r
     -> Tube b c m r
     -> Tube a c m r
a >< b = liftT b >>= go where
    go (Pure x) = return x
    go (Free b') = runT b' (\f -> a >- f)
                           (\(v, k) -> wrap $ yieldF v $ liftT k >>= go)

infixl 3 ><

-- | Enumerate 'yield'ed values into a continuation, creating a new 'Source'
for :: Monad m
    => Tube a b m r
    -> (b -> Tube a c m s)
    -> Tube a c m r
for src body = liftT src >>= go where
        go (Pure x) = return x
        go (Free src') = runT src'
            (\f -> wrap $ awaitF (\x -> liftT (f x) >>= go))
            (\(v, k) -> do
                body v
                liftT k >>= go)

-- | Infix version of 'for'
(~>) :: Monad m
     => Tube a b m r
     -> (b -> Tube a c m s)
     -> Tube a c m r
(~>) = for

-- | Convert a list to a 'Source'
each :: (Monad m, Foldable t) => t b -> Tube a b m ()
each as = Data.Foldable.mapM_ yield as

-- | Insert a value into a 'Sink'
(-<) :: Monad m
     => a
     -> Sink a m b
     -> Sink a m b
x -< snk = liftT snk >>= go where
    go (Pure y) = return y
    go (Free snk') = runT snk' (\f -> f x) diverge

-- | Implementation of '|>' but without the type constraints.
(\|>) :: Monad m
      => Tube a b m r
      -> Tube (Maybe b) c m s
      -> Tube (Maybe b) c m s
src \|> snk = liftT snk >>= goSnk where
    goSnk (Pure x) = return x
    goSnk (Free snk') = runT snk'
        (\fSnk -> (liftT src) >>= goSrc fSnk)
        diverge

    goSrc f (Pure _) = f Nothing
    goSrc f (Free snk') = runT snk' diverge $
        \(v,k) -> k \|> (f (Just v))

{- |
Connects a 'Source' to a 'Sink', finishing when either the 'Source' is
exhausted or the 'Sink' terminates.
-}
(|>) :: Monad m
     => Tube x b m r
     -> Sink (Maybe b) m s
     -> Sink (Maybe b) m s
(|>) = (\|>)

fix :: (a -> a) -> a
fix f = let x = f x in x

-- | Used in the case of a specialized 'Tube' type and we know for certain a
-- particular case will never actually be called.
diverge :: a
diverge = fix id

-- | Continuously relays any values it receives. Iteratee identity.
cat :: Monad m => Tube a a m r
cat = forever $ do
    x <- await
    yield x

-- | Transforms all incoming values according to some function.
map :: (Monad m) => (a -> b) -> Tube a b m r
map f = for cat $ \x -> yield (f x)

-- | Refuses to yield the first @n@ values it receives.
drop :: Monad m => Int -> Tube a a m r
drop n = do
    replicateM_ n await
    cat

-- | Yields only values satisfying some predicate.
filter :: Monad m => (a -> Bool) -> Tube a a m r
filter pred = for cat $ \x -> when (pred x) (yield x)

-- | Terminates the stream upon receiving a value violating the predicate
takeWhile :: Monad m => (a -> Bool) -> Tube a a m ()
takeWhile pred = go
    where
        go = do
            a <- await
            if (pred a)
                then do
                    yield a
                    go
            else return ()

-- | Relay only the first @n@ elements of a stream.
take :: Monad m => Int -> Tube a a m ()
take n = do
    replicateM_ n $ do
        x <- await
        yield x

-- | Taps the next value from a source, maybe.
unyield :: Monad m => FreeT (TubeF x b) m () -> m (Maybe (b, FreeT (TubeF x b) m ()))
unyield tsk = do
    tsk' <- runFreeT tsk
    case tsk' of
        Pure _      -> return Nothing
        Free tsk''  -> do
            let res = runT tsk'' diverge (\(v, k) -> Just (v, k))
            return res

{- |
Strict left-fold of a stream. Note that the actual return type of the source
is not relevant, only the intermediate yield type.
-}
reduce :: Monad m
       => (x -> a -> x) -- ^ step function
       -> x             -- ^ initial value
       -> (x -> b)      -- ^ final transformation
       -> Source a m () -- ^ stream source
       -> m b
reduce step begin done p0 = runFreeT p0 >>= \p' -> loop p' begin where
    loop (Pure _) x = return (done x)
    loop (Free p) x = runT p diverge (\(v, k) ->
        runFreeT k >>= \k' -> loop k' $! step x v)

-- | Similar to 'each' except it explicitly marks the stream as exhausted
every :: (Foldable t, Monad m) => t b -> Tube a (Maybe b) m ()
every xs = (each xs >< map Just) >> yield Nothing

-- | Similar to 'map' except it maps a monadic function instead of a pure one.
mapM :: Monad m => (a -> m b) -> Tube a b m r
mapM f = for cat $ \a -> do
    b <- lift $ f a
    yield b

-- | Evaluates and extracts a pure value from a monadic one.
sequence :: Monad m => Tube (m a) a m r
sequence = mapM id

-- | Source of 'String's from stdin. This is mostly for debugging / ghci example purposes.
prompt :: MonadIO m => Source String m ()
prompt = do
    liftIO . putStr $ "> "
    eof <- liftIO isEOF
    unless eof $ do
        str <- liftIO getLine
        yield str
        prompt

-- | Sink for 'String's to stdout. This is mostly for debugging / ghci example
-- purposes.
display :: MonadIO m => Sink String m ()
display = forever $ do
    it <- await
    liftIO . putStrLn $ it

{- |
Using the supplied functions, 'meta' will fold and then unfold a stream, hence
its name (which is short for @metamorphism@).
-}
meta :: (x -> a -> x)
     -> x
     -> (x -> (b, x))
     -> Pump b a Identity x
meta step init done = mkPump (Identity init)
    (\(Identity xs) -> Identity <$> done xs)
    (\(Identity xs) x -> Identity (step xs x))

-- | Constructs an enumerator pump, which can buffer values and then enumerate
-- them to, say, a 'Sink' (see the examples above).
enumerator :: [a] -> Pump (Maybe a) a Identity [a]
enumerator inp = meta (\xs x -> xs ++ [x]) inp
    (\lst -> case lst of
        []      -> (Nothing, lst)
        (x:xs)  -> (Just x, xs))

-- | Transforms a 'Pump' into a corresponding 'Tube'.
enumerate :: (Monad m, Comonad w)
          => Pump (Maybe a) b w r
          -> Tube c a m ()
enumerate p = do
    let (mv, k) = recv p
    case mv of
        Nothing -> return ()
        Just v' -> yield v' >> enumerate k
