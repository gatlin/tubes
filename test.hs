{- | A simple test file
 -
 - Load this up in ghci to play around with it. Note that any functor may be
 - used as a carrier type for stream chunks.
 -
 - Examples WHICH DON'T APPLY SINCE I CHANGED EVERYTHING IN THIS BRANCH:
 -
 -     ghci> poll $ Chunk "what is this" $> reverseS
 -     "siht si tahw"
 -
 -     ghci> poll $ Chunk [1..10] $> sumS
 -     55
 -
 -     ghci> poll $ Chunk [1..10] $< [ prodS , sumS ]
 -     [3628800,55]
 -
 -     ghci> poll $ prompt +< [ reverseS , insult ]
 -     > gatlin
 -     ["niltag","gatlin sucks"]
 -
 -     ghci> poll $ Chunk "gatlin" $> insult +> reverseS
 -     ("skcus niltag",End)
 -
 -     ghci> poll $ prompt +< [ reverseS , insult ] +> concatS +> printS
 -     > gatlin
 -     "niltaggatlin sucks"
 -
 -}

{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (foldr, foldr', foldl, foldl', sum, sequence)
import FreeStream
import Control.Monad.Trans.Free
import Control.Monad (forever)
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative (pure, Applicative)

prompt = do
    lift . putStr $ "> "
    line <- lift getLine
    yield line

promptMany = forever prompt

sumS = loop 0 where
    loop acc = await >>= go acc
    go acc (Chunk []) = yield acc >> loop 0
    go acc (Chunk ns) = loop $ acc + (sum ns)
    go acc _          = yield acc

getword = loop "" where
    loop acc = await >>= go acc
    go acc (Chunk [])  = yield acc
    go acc (Chunk [c]) | c /= ' ' = loop (acc ++ [c])
                       | otherwise = yield acc >> loop ""
    go acc (End)     = yield acc

printS = loop where
    loop = await >>= go
    go (Chunk xs) = do
        lift . putStrLn $ "Output: " ++ xs
        yield xs
        loop
    go _          = do
        loop

enumList [] = yield []
enumList (x:xs) = do
    yield [x]
    enumList xs

explode = await >>= go where
    go (Chunk xs) = enumList xs
    go _          = yield []

reverseS :: ProcessT (Stream [] Char) String IO ()
reverseS = loop "" where
    loop mem = await >>= go mem
    go mem (Chunk xs) = yield (reverse xs) >> loop (reverse xs)
    go mem _          = yield mem

insult = loop where
    loop = await >>= go
    go (Chunk xs) = yield (xs ++ " sucks") >> loop
    go _          = return ""

huh = reverseS +> reverseS

xs = [ reverseS , huh ]
