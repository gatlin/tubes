{-# LANGUAGE Rank2Types #-}

import Prelude hiding (iterate)
import FreeStream

data Arithmetic
    = Value Int
    | Add Arithmetic Arithmetic
    | Mul Arithmetic Arithmetic
    deriving (Show)

doArith :: Arithmetic -> Int
doArith (Value v)   = v
doArith (Add l r)   = (doArith l) + (doArith r)
doArith (Mul l r)   = (doArith l) * (doArith r)

type Token = Maybe String

tokenize :: Monad m => Task (Maybe Char) Token m ()
tokenize = loop "" where
    loop acc = do
        token <- await
        case token of
            Nothing -> yield (Just acc) >> yield Nothing
            Just  t -> case t of
                ' ' -> (yield (Just acc)) >> loop ""
                _   -> loop $ acc ++ [t]

parseArith :: Monad m => Source Token m () -> m Int
parseArith = reduce step [] (doArith . head) where
    step stack token = case token of
        Just t  -> case t of
            "+" -> buildBranch (Add) stack
            "*" -> buildBranch (Mul) stack
            ""  -> stack
            _   -> (Value (read t)):stack
        Nothing -> stack

    buildBranch con stack =
        if length stack < 2
            then stack
            else
                let (r:l:rest)  = stack
                    ret         = con l r
                in  ret:rest

compute :: Monad m => String -> m Int
compute x = parseArith $ iterate x >< tokenize

main :: IO ()
main = do
    putStrLn "Computing `24 5 + 2 *`"
    fifty_eight <- compute "24 5 + 2 *"
    putStrLn $ "Result: " ++ (show fifty_eight)
