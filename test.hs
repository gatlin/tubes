-- | Load this up in ghci to play around

import Prelude hiding ( drop
                      , take
                      , takeWhile
                      , print
                      , map
                      , filter
                      )
import FreeStream
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans.Free
import Data.Traversable (Traversable, traverse, mapM, sequence)
import Data.Monoid (mempty, (<>), Monoid)
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import qualified GHC.IO.Exception as G

prompt :: Generator String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

print :: Sink String IO ()
print = do
    str <- await
    x   <- lift $ try $ putStrLn str
    case x of
        Left e@(G.IOError { G.ioe_type = t }) ->
            lift $ unless (t == G.ResourceVanished) $ throwIO e
        Right () -> print

doubleUp :: Sink String IO String
doubleUp = do
    str1 <- await
    str2 <- await
    return $ str1 ++ str2

insult :: Action IO ()
insult = prompt +> map (++ " sucks") +> print

evenNumbers = for (each [1..10] |- isEven) $ \n -> do
    lift $ putStrLn . show $ n

    where isEven x = if x `mod` 2 == 0 then True else False

sumS :: Monad m => Sink (Stream Int) m Int
sumS = loop 0 where
    loop acc = do
        d <- await
        case recv d of
            Just n  -> loop (acc + n)
            Nothing -> return acc

