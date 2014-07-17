{- | A simple test file
 -
 - Load this up in ghci to play around with it. Note that any functor may be
 - used as a carrier type for stream chunks.
 -
 - Examples:
 -
 -     ghci> Just (v, k) <- run $ "what is this" $> reverseS +> printS
 -     Printing: siht si tahw
 -     ghci> v
 -     "siht si tahw"
 -
 -     ghci> Just (v, k) <- run $ prompt *< [ reverseS , insult ]
 -     > gatlin
 -     ghci> v
 -     ["niltag","gatlin sucks"]
 -
 -     ghci> Just (v, k) <- run $ [1..10] $> sumS
 -     ghci> v
 -     55
 -
 -     ghci> Just (v, k) <- run $ "no more spaces" $> (filterSpaces reverseS)
 -     ghci> v
 -     "secapseromon"
 -
 -}

{-# LANGUAGE NoMonomorphismRestriction #-}

import Prelude hiding (foldr, foldr', foldl, foldl', sum, product, sequence)
import FreeStream
import Control.Monad (forever, unless)
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Traversable
import System.IO (isEOF)

prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

printS = loop where
    loop = await >>= go
    go (Chunk xs) = do
        lift . putStrLn $ "Printing: " ++ xs
        yield xs
    go _          = return ()

sumS = loop 0 where
    loop acc = await >>= go acc
    go acc (Chunk []) = yield acc
    go acc (Chunk ns) = loop $ acc + (sum ns)
    go acc _          = yield acc

prodS = loop 1 where
    loop acc = await >>= go acc
    go acc (Chunk []) = yield acc >> loop 1
    go acc (Chunk ns) = loop $ acc * (product ns)
    go acc _          = yield acc

getword = loop "" where
    loop acc = await >>= go acc
    go acc (Chunk [])  = yield acc
    go acc (Chunk [c]) | c /= ' ' = loop (acc ++ [c])
                       | otherwise = yield acc >> loop ""
    go acc (End)     = yield acc

enumList [] = yield []
enumList (x:xs) = do
    yield [x]
    enumList xs

explode = await >>= go where
    go (Chunk xs) = enumList xs
    go _          = yield []

reverseS = loop "" where
    loop mem = await >>= go mem
    go mem (Chunk xs) = yield (reverse xs) >> loop (reverse xs)
    go mem _          = yield mem

insult = loop where
    loop = await >>= go
    go (Chunk xs) = yield ( xs ++ " sucks" ) >> loop
    go _     = return ()

aggList = loop [] where
    loop acc = await >>= go acc
    go acc (Chunk xs) = loop (acc ++ xs)
    go acc _          = yield acc

exclude target = forever $ do
    d <- await
    go d
    where go (Chunk xs) = yield $ filter (target /=) xs
          go _          = return ()

filterSpaces k = loop where
    loop = await >>= go
    go (Chunk xs) = do
        ys <- lift $ feed (k +> exclude ' ') $ Chunk xs
        yield ys >> loop
    go _          = return ()

filterInsult = filterSpaces insult
