{- | A simple test file
 -
 - Load this up in ghci to play around with it. Note that any functor may be
 - used as a carrier type for stream chunks.
 -
 - Examples:
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

import Prelude hiding (foldr, foldr', foldl, foldl', sum)
import FreeStream
import Control.Monad.Trans.Free
import Data.Foldable
import Data.Traversable
import Data.Monoid
import Control.Applicative (pure, Applicative)

type LProcessT m a b = ProcessT m [] a b

-- | Yield input from the user.
prompt :: LProcessT IO a String
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

times2 :: (Monad m, Traversable t, Num n, Monoid (t n)) => ProcessT m t n (t n)
times2 = loop mempty where
    loop acc = await >>= go acc
    go acc (Chunk ns) = loop (acc <> (fmapDefault (2 *) ns))
    go acc _          = yield acc

printS :: LProcessT IO Char String
printS = loop mempty where
    loop mem = await >>= go mem
    go _ (Chunk str) = do
        lift . putStrLn $ "Output: " ++ str
        loop str

    go mem _ = yield mem
