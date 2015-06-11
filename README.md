Tubes
==========

Stream processing with a series of tubes.

This library is by no means unique; indeed it is heavily inspired by the
[pipes][pipes] library. However, the goal is not only to write a correct and
efficient stream processing library but also to explore the properties and
relationships of and among functions, pairs, products, and sums.

Haddock documentation and more information is available at
[http://niltag.net/tubes](http://niltag.net/tubes).

For pleasant reading on stream processing and iteratees, you might like
[Oleg's page on the subject][iteratees].

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

I have written a more elaborate [reverse-Polish notation (RPN) calculator
example][rpn] to showcase the utility of stream processing pipelines for
realistic tasks.

Some explanations
---

### Freedom may or may not be free, but monads are!

This library is built using a *free monad* construction. **Before you run away
screaming** just give this a shot:

Conceptually, a monad is always in one of two states: representing a pure value
(which is what `return` does) or a continuation function wrapping another
monadic value (this is related to what `>>=` does).[^1]

In practice, most monad instances take advantage of the properties of the type
being monad-ified and while they are equivalent to this conceptual
understanding, they're a bit more direct.

However, you can create a type which implements a very generic monad instance
and relies on another functor's `fmap` function:

```haskell
data Free f a = Pure a | Wrap (f (Free f a))
```

So if you have a functor with several different value constructors, then they
give you information to use when evaluating `Free` values.

Intuitively, a free monad constructs a command language out of any functor,
treating that functor's different variants as the grammar. It is then your
responsibility to define what these commands should actually *do* when invoked.

The `Tube` language, so-to-speak, is this:

```haskell
{-# LANGUAGE DeriveFunctor #-}
data TubeF a b k
    = Await (a -> k) -- corresponds to `await`
    | Yield (b,   k) -- corresponds to `yield`
    deriving (Functor)
```

A `Tube` can other be `await`ing a new value, `yield`ing a result, or be
a completed `Pure` value (given by `return`).

This library makes use of the [`free` package][free] which provides code very
similar to this, with some performance optimizations and more advanced options.
At its core though it is equivalent to my definition above.

### Lions, functions, and pairs, oh my !

Functions of type `(->) a` are functors, because I can write an `fmap` for
them:

```haskell
fmap :: (b -> c) -> (a -> b) -> (a -> c)
fmap mapper f = mapper . f
```

Pairs of the type `((,) a)` are also functors, for much the same reason:

```haskell
fmap :: (b -> c) -> (a, b) -> (a, c)
fmap mapper (x, y) = (x, mapper y)
```

Functions also form a monad, which in Haskell is called the `Reader` monad:

```haskell
newtype Reader r m a = Reader {
    runReader :: r -> a
} deriving Functor

-- ... monad instance ...

ask :: Reader a a
ask = Reader id
```

This monad lets you embed some read-only (hence the name) value into a monad,
and refer to it throughout your program. If you compose two `Reader`s the value
is threaded through both of them. You can alter the value for sub-computations,
but the modification will only persist until the end of the sub-computations.

```haskell
r1 :: Reader Int Bool
r1 = do
    n <- ask
    return (n `mod` 2 == 0)

r2 :: Bool -> Reader Int String
r2 isEven = do
    n <- ask
    let s = show n
    if isEven
        then return $ s ++ " is even!"
        else return $ s ++ " is odd!"

r3 :: Reader Int String
r3 = r1 >>= r2
```

Calling `runReader r3 3` results in the helpful message "3 is odd!".

Pairs also form a monad so long as their first element is a monoid; this is
referred to as the `Writer` monad:

```haskell
newtype Writer w m a = Writer {
    runWriter :: (a, w)
} deriving Functor

-- ... monad instance ...
```

This monad lets you embed a log in the monad, and write to it 

```haskell
tell :: Monoid m => m -> Writer m ()
tell msg = Writer ((), msg)

w1 :: Int -> Writer [String] Int
w1 n = do
    tell ["Given " ++ (show n)]
    return n

w2 :: Int -> Writer [String] Bool
w2 n = do
    let isEven = n `mod` 2 == 0
    tell ["n is even: " ++ (show isEven)]
    return isEven

w3 n = w1 n >>= w2
```

Invoking `runWriter (w3 2)` gives back `(True,["Given 2","n is even: True"])`.

The actual implementations are a little more sophisticated. When you compose
them, you get another well-known monad:

```haskell
newtype State s m a = State {
    runState :: s -> (a, s)
} deriving Functor

-- monad instance
```

Where `Reader` lets you thread a value through a computation, and `Writer` lets
you write more values, `State` allows you to thread and modify a value through
a computation.

With all this in mind, the utility of a `Tube` becomes more clear: a
composition of two tubes threads "hidden" stream values through both parts, and
grants the ability to modify the values and types of elements as they go
downstream. If you squint, this is similar to the power of `State`.

### Dropping the Boehm

Here is my actual definition of `TubeF`:

```haskell
{-# LANGUAGE DeriveFunctor, Rank2Types #-}
newtype TubeF a b k = TubeF {
    runT :: forall r.
            ((a -> k) -> r)
         -> ((b,   k) -> r)
         -> r
} deriving (Functor)

awaitF :: (a -> k) -> TubeF a b k
awaitF f = TubeF $ \a _ -> a f

yieldF :: (b, k) -> TubeF a b k
yieldF x k = TubeF $ \_ y -> y (x, k)
```

This may make more sense with a simpler example: home-made boolean types!
Compare:

```haskell
data NaiveBoolean = T | F

if' :: NaiveBoolean -> a -> a -> a
if' (T) then' _ = then'
if' (F) _ else' = else'

one = if' T 1 2 -- => 1
two = if' F 1 2 -- => 2
```

with

```haskell
{-# LANGUAGE Rank2Types #-}
newtype Boolean = Boolean {
    if' :: forall k. k -> k -> k
}

-- Picks the first option
_t :: Boolean
_t = Boolean $ \t _ -> t

-- Picks the second option
_f :: Boolean
_f = Boolean $ \_ f -> f

one = if' _t 1 2 -- == 1
two = if' _f 1 2 -- == 2
```

The first definitions effectively create structures that we can tear down
later; the second definition says that a Boolean is a value which takes two
arguments and chooses one or the other.

Data is code; code is data.

This is called a [Boehm-Berarducci encoding][bbencode]. Instead of building up
a mountain of `Yield`s and `Await`s wrapping each other, which can waste memory
and slow down execution, the state of the `Tube` dictates which of two
alternatives it will take.[^2]

### Are you pumped up?

I have also written the dual to a `Tube`: a `Pump`. Check out the `Tubes.Pump`
documentation for more information. I am still exploring the uses of pumps and
how they can usefully interact with tubes; if anyone ever actually reads this,
stay tuned!

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
with "Tubes" somewhere in the subject line.

Some terms and ideas were stolen from Gabriel Gonzalez, author of the
[pipes][pipes] library.

[iteratees]: http://okmij.org/ftp/Streams.html
[pipes]: http://hackage.haskell.org/package/pipes
[rpn]: https://gist.github.com/gatlin/aa11b4e08b5bfb52e011
[bbencode]: http://okmij.org/ftp/tagless-final/course/Boehm-Berarducci.html
[free]: http://github.com/ekmett/free/

[^1]: If this seems eerily like how you define a list - an empty
start state or a value paired with another list - then lookup the term
"monoidal".

[^2]: It is also sometimes called a "Church encoding" after Alonzo Church;
however, with all due respect to the man, he was dealing with an untyped
calculus, whereas Boehm and Berarducci were dealing with a typed lambda
calculus.
