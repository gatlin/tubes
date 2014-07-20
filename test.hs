-- | Load this up in ghci to play around

{-# LANGUAGE MonadComprehensions #-}

import Prelude hiding ( drop
                      , take
                      , takeWhile
                      , print
                      , map
                      , filter
                      , foldl
                      , foldl'
                      , foldr
                      , foldr'
                      )
import FreeStream
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans.Free
import Control.Applicative
import Control.Alternative.Free
import Control.Applicative.Free (runAp, retractAp)
import Data.Traversable hiding (for)
import Data.Foldable
import Data.Monoid (mempty, (<>), Monoid)
import System.IO (isEOF)
import Control.Exception (try, throwIO)
import Data.Maybe (fromMaybe)
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

handle :: Sink String IO String
handle = do
    str <- await
    return $ "Handling: " ++ str

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

fizzbuzz n = fromMaybe (show n) $ [ "fizz" | n `rem` 3 == 0 ]
                               <> [ "buzz" | n `rem` 5 == 0 ]
                               <> [ "bazz" | n `rem` 7 == 0 ]

sumS :: Monad m => Sink (Stream Int) m Int
sumS = loop 0 where
    loop acc = do
        n <- await
        case recv n of
            Just v -> loop (acc + v)
            Nothing -> return acc

relay sink = sink >>= yield
