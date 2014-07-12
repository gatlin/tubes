import FreeStream
import Control.Applicative
import Data.Monoid

type LProcessT m a b = ProcessT m [] a b

-- | Read in space-delimited words and yield the words
getword :: Monad m => LProcessT m Char String
getword = loop "" where
    loop acc = await >>= check acc
    check acc (Chunk [xs]) | xs /= ' ' = loop (acc ++ [xs])
                           | otherwise  = yield acc
    check acc _            = yield acc

-- | Yield input from the user.
prompt :: LProcessT IO () String
prompt = do
    lift . putStr $ "> "
    line <- lift getLine
    yield line

sumS :: Monad m => LProcessT m Int Int
sumS = loop 0 where
    loop acc = await >>= go acc
    go acc (Chunk [n]) = loop (acc + n)
    go acc _           = yield acc

prodS :: Monad m => LProcessT m Integer Integer
prodS = loop 1 where
    loop acc = await >>= go acc
    go acc (Chunk [n]) = loop (acc * n)
    go acc _           = yield acc

reverseS :: Monad m => LProcessT m Char String
reverseS = loop "" where
    loop acc = await >>= go acc
    go acc (Chunk xs) = loop ((reverse xs)++acc)
    go acc _          = yield acc


