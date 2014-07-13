{- | A simple test file
 -
 - Load this up in ghci to play around with it. Note that any functor may be
 - used as a carrier type for stream chunks.
 -
 - Examples:
 -
 -     ghci> feed reverseS $ Chunk "what is this"
 -     "siht si tahw"
 -
 -     ghci> feed sumS $ Chunk [1..10]
 -     55
 -
 -     ghci> feed prodS $ Chunk . Just $ 10
 -     10
 -
 -     ghci> feed (prodS >|< sumS) $ Chunk [1..10]
 -     (3628800,55)
 -
 -     ghci> feed (reverseS >|< insult) $ Chunk "gatlin"
 -     ("niltag","gatlin sucks")
 -
 -     ghci> (v, k) <- run prompt
 -     > wild and exciting user input
 -
 -     ghci> v
 -     "wild and exciting user input"
 -
 -     ghci> forList v getword
 -     ("wild", Chunk "and exciting user input")
 -}

import Prelude hiding (foldr, foldr', foldl, foldl', sum)
import FreeStream
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Traversable
import Data.Monoid

type LProcessT m a b = ProcessT m [] a b

-- | Read in space-delimited words and yield the words
getword :: (Monad m) => LProcessT m Char String
getword = loop mempty where
    loop acc = await >>= check acc
    check acc (Chunk [xs]) | xs /= ' ' = loop (acc <> [xs])
                           | otherwise  = yield acc
    check acc _            = yield acc

-- | Yield input from the user.
prompt :: LProcessT IO () String
prompt = do
    lift . putStr $ "> "
    line <- lift getLine
    yield line

sumS :: (Monad m, Traversable t, Num n) => ProcessT m t n n
sumS = loop 0 where
    loop acc = await >>= go acc
    go acc (Chunk ns) = loop (acc + (foldl (+) 0 ns))
    go acc _          = yield acc

prodS :: (Monad m, Traversable t, Num n) => ProcessT m t n n
prodS = loop 1 where
    loop acc = await >>= go acc
    go acc (Chunk ns) = loop (acc * (foldl (*) 1 ns))
    go acc _           = yield acc

reverseS :: Monad m => LProcessT m Char String
reverseS = loop mempty where
    loop acc = await >>= go acc
    go acc (Chunk xs) = loop ((reverse xs) <> acc)
    go acc _          = yield acc

insult :: Monad m => LProcessT m Char String
insult = loop "" where
    loop acc = await >>= go acc
    go acc (Chunk c) = loop (acc ++ c)
    go acc _         = yield $ acc ++ " sucks"
