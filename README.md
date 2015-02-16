FreeStream
==========

[Iteratee][iteratees]-based streaming library from a free monad. This exists
solely for my own understanding of functions, pairs, and the interesting things
you can derive automatically by combining the two.

If this library also happens to be useful to anyone, even better.

(c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

Synopsis
---

The following are snippets run in `ghci`.

```haskell
ghci> run $ for (each [1..4] >< map show) (lift . putStrLn)
1
2
3
4
```

```haskell
ghci> reduce (+) 0 id (each [1..10])
55
```

We can define a `Source` and a `Sink` for `stdin` and `stdout`, respectively:

```haskell
prompt :: Source String IO ()
prompt = do
    lift . putStr $ "> "
    eof <- lift isEOF
    unless eof $ do
        str <- lift getLine
        yield str
        prompt

print :: Sink String IO ()
print = forever $ do
    it <- await
    lift . putStrLn $ it
```

And write interactive pipelines:

```haskell
ghci> let echo = prompt >< print
ghci> run echo
> ping
ping
> pong
pong
```

```haskell
ghci> run $ prompt >< filter (/= "Die Antwoord") >< map (++ " sucks") >< print
> dubstep
dubstep sucks
> the sun
the sun sucks
> Die Antwoord
> this program
this program sucks
```

Extended example: Arithmetic parsing
---

The following is an extended example of tokenizing, parsing, and evaluating
reverse-Polish notation (RPN) arithmetic expressions, eg

    24 5 + 2 *

First we want to define an abstract syntax tree for arithmetic. This is easy
enough:

```haskell
data Arithmetic
    = Value Int
    | Add Arithmetic Arithmetic
    | Mul Arithmetic Arithmetic
    deriving (Show)
```

Our example expression in this form is

```
fifty_eight = Mul (Add (Value 24) (Value 5)) (Value 2)
```

Evaluating such a tree is straightforward

```haskell
doArith :: Arithmetic -> Int
doArith (Value v) = v
doArith (Add l r) = (doArith l) + (doArith r)
doArith (Mul l r) = (doArith l) * (doArith r)
```

But how do we convert `24 5 + 2 *` into `Mul (Add (Value 24) (Value 5)) (Value
2)`?

First we should tokenize the input; in this case, break the string up by
spaces. A token is either a `String` of data or a null-value to signify the
end of the input. So let's make a convenient type alias:

```haskell
type Token = Maybe String
```

Our `tokenize` function will receive a stream of `Maybe Char` and will
periodically yield a `Token`.

```haskell
tokenize :: Monad m => Task (Maybe Char) Token m ()
tokenize = loop "" where
    loop acc = do
        token <- await
        case token of
            Nothing -> yield (Just acc) >> yield Nothing
            Just  t -> case t of
                ' ' -> (yield (Just acc)) >> loop ""
                _   -> loop $ acc ++ [t]
```

So now we can split up `24 5 + 2 *` into `"24" "5" "+" "2" "*"`. The next step
is producing an `Arithmetic` expression. This library already provides a
function, `reduce`, which reduces a stream of values into a single result.

```haskell
parseArith :: Monad m => Source Token m () -> m Int
parseArith = reduce step [] (doArith . head) where
    step stack token = case token of
        Just t  -> case t of
            "+" -> buildBranch (Add) stack
            "*" -> buildBranch (Mul) stack
            ""  -> stack
            _   -> (Value (read t)):stack
        Nothing -> stack

    buildBranch con stack = do
        if length stack < 2oken
            then stack
            else
                let (r:l:rest) = stack
                    ret        = con l r
                 in (ret:rest)
```

Note the cases for the different possible tokens. `"+"`, `"*"`, or an integer of
some kind all manipulate the expression stack in some way. An empty string,
though, leaves the stack completely untouched - in essence, it is ignored.

The final step is feeding an input string into this pipeline. This library also
provides `iterate` for breaking a `Foldable` of input into a series of `Maybe`
values.

```haskell
compute :: Monad m => String -> m Int
compute x = parseArith $ iterate x >< chunk
```

In `ghci`, then, I can use my fancy new RPN calculator:

```haskell
ghci> compute "24   5 + 2   *"
58
```

Explanation
---

The central type is the `Task`, defined as such:

```haskell
data TaskF a b k
    = Await (a -> k)
    | Yield b k
```

This type has two variants: a function (`Await`) and a product (`Yield`). Both
functions and products are `Functors`:

```haskell
fmap :: (a -> b) -> (r -> a) -> (r -> b)
fmap mapper f = mapper . f

fmap :: (a -> b) -> (r, a) -> (r, b)
fmap mapper (x, y) = (x, mapper y)
```

Because these types are `Functor`s, you can derive free monads and free monad
transformers from them.

The free monad based on functions is often called the `Reader` monad because it
permits access to some internal value, allowing you to "read" it while
computing the final return value. In `FreeStream` this behavior is captured by
the `await` function; computations which `await` are `Sink`s.

The free monad based on products is often called the `Writer` monad because it
permits appending values ("writing") to some internal log structure. In
`FreeStream` this behavior is captured by the `yield` function; computations
which `yield` are `Source`s.

`Source`s and `Sink`s are both special cases of a more general type, the
`Task`. All composition functions are written for `Task`s which allows
arbitrary composition of standalone computations - as long as the `yield`ed
type of one task matches the `await`ing type of the next one.

More thoughts
---

When a value is `yield`ed, it must either be `await`ed in another task or
captured by a function such as `reduce` and turned into the final result value.
Calling `yield` then returns an intermediate value requiring some consumer of
that value to produce the eventual result.

It may be interesting to look at the definition of `ContT`, the continuation
monad transformer, in this light:

```haskell
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }
```

A `ContT` computation is one in need of a continuation to complete the
computation. `Task` emulates this ability to suspend the computation and
intuitively this makes sense.

It may also be edifying to look at the definition of `StateT`, the monad
transformer giving a monad access to a mutable state:

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
```

A `StateT` computation is a function which returns a pair; a `Task` is also a
computation which `await`s a value (so it is a function) and then `yield`s
another value (so it returns a pair). In this way a `Task` is similar to
`StateT`: it reads stateful values from upstream and can transform those values
arbitrarily before sending them downstream. A single `Task` (on its own) does
not have mutable state per se but the pipeline itself may be thought of as a
stateful stream transformer.

The point ... ?
---

You can get very powerful behavior from concepts as simple as unary functions
and binary products. Simple compositions of the two result in side-effecting,
stateful, suspendable stream processing pipelines.

This library is an examination of these (and hopefully, eventually, other)
implications and discoveries.

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.

Some terms and ideas were stolen from Gabriel Gonzalez, author of the
[pipes][pipes] library.

[iteratees]: http://okmij.org/ftp/Streams.html
[pipes]: http://hackage.haskell.org/package/pipes
