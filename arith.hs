{-# LANGUAGE RankNTypes #-}

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
                      , iterate
                      )
import FreeStream
import Data.Foldable hiding (fold)
import System.IO (isEOF)
import Control.Monad (forever, unless, replicateM_, when)
import Control.Monad.Trans.Free
import Data.Maybe (fromJust)

data Arithmetic
    = Value Int
    | Add Arithmetic Arithmetic
    | Mul Arithmetic Arithmetic
    | Sub Arithmetic Arithmetic
    | Div Arithmetic Arithmetic
    deriving Show

doArith :: Arithmetic -> Int
doArith (Value v) = v
doArith (Add l r) = (doArith l) + (doArith r)
doArith (Mul l r) = (doArith l) * (doArith r)
doArith (Sub l r) = (doArith l) - (doArith r)
doArith (Div l r) = (doArith l) `div` (doArith r)

newtype Token a = Token { getToken :: a } deriving (Show, Eq)
type TokenStream = Stream (Token String)

parseArith :: Sink TokenStream IO (Maybe Arithmetic)
parseArith = loop [] where
    loop stack = do
        d <- await
        case recv d of
            Just token -> case getToken token of
                "+" -> buildBranch (Add) stack
                "*" -> buildBranch (Mul) stack
                "-" -> buildBranch (Sub) stack
                "/" -> buildBranch (Div) stack

                _   -> do
                    v <- return $ getToken token
                    loop $ (Value (read v)):stack
            Nothing -> return . Just . head $ stack

    buildBranch con stack = do
        if length stack < 2
            then return Nothing
            else do
                (l:r:rest) <- return stack
                r          <- return $ con l r
                loop $ r:rest

astring = "56 2 * 30 +"

prompt :: Generator String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

tokenize :: Sink (Stream Char) IO [Token String]
tokenize = loop "" [] where
    loop s acc = do
        d <- await
        case recv d of
            Nothing -> return (acc++[Token s])
            Just  c -> case c of
                ' ' -> loop "" (acc++[Token s])
                _   -> loop (s ++ [c]) acc

rpn :: String -> IO (Maybe Int)
rpn str = do
    tokens <- stream (each str) |> tokenize
    parsed <- stream (each tokens |- (/= Token "")) |> parseArith
    case parsed of
        Just p -> return . Just $ doArith p
        Nothing -> return Nothing

gen :: Monad m => b -> Generator b m ()
gen sink = yield sink >> return ()
