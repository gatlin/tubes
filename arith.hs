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
    deriving Show

doArith :: Arithmetic -> Int
doArith (Value v) = v
doArith (Add l r) = (doArith l) + (doArith r)
doArith (Mul l r) = (doArith l) * (doArith r)

newtype Token a = Token { getToken :: a } deriving (Show, Eq)
type TokenStream = Stream (Token Char)

parseArith :: Sink TokenStream IO (Maybe Arithmetic)
parseArith = loop [] where
    loop stack = do
        d <- await
        case recv d of
            Just token -> case getToken token of
                '+' -> do
                    if length stack < 2
                        then return Nothing
                        else do
                            (l:r:rest) <- return stack
                            r        <- return $ Add l r
                            loop $ r:rest
                '*' -> do
                    if length stack < 2
                        then return Nothing
                        else do
                            (l:r:rest) <- return stack
                            r        <- return $ Mul l r
                            loop $ r:rest

                _   -> do
                    v <- return $ getToken token
                    loop $ (Value (read [v])):stack
            Nothing -> return . Just . head $ stack


astring = "5 6 * 2 3 * +"

rpn :: Foldable t => t Char -> IO (Maybe Int)
rpn str = do
    a <- stream (each str |- (/= ' ') >+ Token) |> parseArith
    case a of
        Just x  -> return . Just . doArith $ x
        _       -> return Nothing
