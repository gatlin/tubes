Simple reverse-Polish notation calculator
===

Writing parsers can be confusing and painful. One technique is to view unparsed
text as a stream of characters, and the act of parsing as a stream processing
problem.

Here I have written a very simple reverse-Polish notation (RPN) calculator
which parses text and returns a result.

This post is literate Haskell so if you download the source and have
`FreeStream` installed you can totally run it.

Imports and language extensions
---

Nothing too wild and crazy. This library does require `Rank2Types` because of
the `Source` and `Sink` type aliases. Additionally, `FreeStream` provides its
own function called `iterate` which, while different to the one in `Prelude`,
nonetheless *does* iterate through a sequence of values.

> {-# LANGUAGE Rank2Types #-}

> import Prelude hiding (iterate)
> import FreeStream

Tabulation
---

First we want to define an abstract syntax tree for arithmetic. This is easy
enough:

> data Arithmetic
>     = Value Int
>     | Add Arithmetic Arithmetic
>     | Mul Arithmetic Arithmetic
>     deriving (Show)

Evaluating an `Arithmetic` expression is straightforward enough:

> doArith :: Arithmetic -> Int
> doArith (Value v)   = v
> doArith (Add l r)   = (doArith l) + (doArith r)
> doArith (Mul l r)   = (doArith l) * (doArith r)

The more interesting part of this example is parsing RPN into `Arithmetic`
values.

Parse-hole
---

Parsing will consist of two steps:

1. Tokenizing (or *lexing*); and
2. Actual "parsing," as it were.

Tokenizing in this case means grouping successive non-whitespace characters
together into *tokens*.

The `iterate` function emits `Just` values for each element of some input
structure (NB: it can be any `Foldable` value) and terminates the sequence with
`Nothing`.

We will adopt this practice throughout the example, of emitting `Nothing` when
the stream is exhausted. To that end, I'll say a `Token` is maybe a `String`
value:

> type Token = Maybe String

Now for the actual tokenizing. We will loop over the incoming stream elements,
accumulating characters into a token until a whitespace character is received.
When this happens, we emit the token and start over. Similarly, when the stream
is exhausted we emit the token and stop.

> tokenize :: Monad m => Task (Maybe Char) Token m ()
> tokenize = loop "" where
>     loop acc = do
>         token <- await
>         case token of
>             Nothing -> yield (Just acc) >> yield Nothing
>             Just  t -> case t of
>                 ' ' -> (yield (Just acc)) >> loop ""
>                 _   -> loop $ acc ++ [t]

Now we have grouped characters into `Token`s. We need to now somehow build an
`Arithmetic` expression.

RPN is essentially a stack manipulation language: numbers are pushed onto the
stack and operators pop some number of values off the stack, replacing them
with a single value.

Our stack will simply be a list, initially `[]`. We will use the `reduce`
function provided by the library to consume stream elements as they are
produced. `reduce` accepts as arguments a *step* function defining what to do
at each step of the reduction, an initial value, and a final function to
transform the result.

At the end of parsing the stack will contain a single `Arithmetic` value, so
our final transformation - `doArith . head` - will take this one value off the
stack and evaluate it.

`step` is the interesting part: given an existing stack and a token, we must
decide what to do with the stack with regard to the token. If the token is an
operator, we punt to a helper - `performOp` - to juggle the stack values.

Our `tokenize` function is rather naive and will spit out empty `String`s when
successive whitespace characters are encountered. The simplest way to handle
this is to ignore them and keep moving; hence, there is a case for `""` tokens.
If the token is not an operator or empty, then we `read` the number and push it
on the stack.

> parseArith :: Monad m => Source Token m () -> m Int
> parseArith = reduce step [] (doArith . head) where
>     step stack token = case token of
>         Just t  -> case t of
>             "+" -> performOp (Add) stack
>             "*" -> performOp (Mul) stack
>             ""  -> stack
>             _   -> (Value (read t)):stack
>         Nothing -> stack

`performOp` takes an operator and an operand stack. If the stack does not have
enough items in it, then the operation is ignored (which will likely lead to
failure). Otherwise, the top two items are popped and the operation is pushed
in their place.

>     performOp op stack =
>         if length stack < 2
>             then stack
>             else
>                 let (r:l:rest)  = stack
>                     ret         = op l r
>                 in  ret:rest

Putting it all together is a function `compute` which takes a string and runs
it through our pipeline. We `iterate` the characters, pipe them to `tokenize`
and the resulting `Source` is given to `parseArith` which returns our result.

> compute :: Monad m => String -> m Int
> compute x = parseArith $ iterate x >< tokenize

An example for those playing at home:

> main :: IO ()
> main = do
>     putStrLn "Computing `24 5 + 2 *`"
>     fifty_eight <- compute "24 5 + 2 *"
>     putStrLn $ "Result: " ++ (show fifty_eight)
