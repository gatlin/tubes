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
type TokenStream = Stream (Token Char)

parseArith :: Sink TokenStream IO (Maybe Arithmetic)
parseArith = loop [] where
    loop stack = do
        d <- await
        case recv d of
            Just token -> case getToken token of
                '+' -> buildBranch (Add) stack
                '*' -> buildBranch (Mul) stack
                '-' -> buildBranch (Sub) stack
                '/' -> buildBranch (Div) stack

                _   -> do
                    v <- return $ getToken token
                    loop $ (Value (read [v])):stack
            Nothing -> return . Just . head $ stack
    buildBranch con stack = do
        if length stack < 2
            then return Nothing
            else do
                (l:r:rest) <- return stack
                r          <- return $ con l r
                loop $ r:rest

bullshit :: Sink TokenStream IO (Maybe Arithmetic)
bullshit = await >> return Nothing

astring = "5 6 * 2 3 * +"

rpn :: Foldable t => t Char -> IO (Maybe Int)
rpn str = do
    a <- stream (each str |- (/= ' ') >+ Token) |> parseArith
    case a of
        Just x  -> return . Just . doArith $ x
        _       -> return Nothing

