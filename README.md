Tubes
==========

[Iteratee][iteratees]-based streaming library from a free monad. This exists
solely for my own understanding of functions, pairs, and the interesting things
you can derive automatically by combining the two.

If this library also happens to be useful to anyone, even better.

Haddock documentation is available at
[http://niltag.net/FreeStream](http://niltag.net/FreeStream).

(c) 2014, 2015 Gatlin Johnson <gatlin@niltag.net>

**I JUST CHANGED THE NAME OF THIS PACKAGE. EVERYTHING BUILDS but the
documentation and such might not be completely changed**

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

Parallelism and Concurrency
---

**This is incomplete but worth bringing up.**

For this example we have a source of `Int` values

```haskell
nums :: Monad m => Source Int m ()
nums = each [1,2,3]
```

and some tasks which transform `Int`s:

```haskell
doubleIt :: Monad m => Tube Int String m ()
doubleIt = forever $ do
    n <- await
    yield . show $ n * 2

squareIt :: Monad m => Tube Int String m ()
squareIt = forever $ do
    n <- await
    yield . show $ n * n
```

And, just to make things a little more interesting, some super-important final
task:

```haskell
obvious :: Monad m => Tube String String m ()
obvious = cat >< map (++ " is a number")
```

We can split and merge streams like so:

```haskell
ghci> run $ nums *< [ squareIt, doubleIt ] >* obvious >< print
1 is a number
4 is a number
9 is a number
2 is a number
4 is a number
6 is a number
```

This is primitive at the moment but it will work for any `Functor`, such as
those which `fmap` in parallel or concurrently. I think this is exciting.

Extended example: Arithmetic parsing
---

I have written a more elaborate [reverse-Polish notation (RPN) calculator
example][rpn] to showcase the utility of stream processing pipelines for
realistic tasks.

Explanation
---

### Free Monads

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
treating that functor's different variants as the grammar.

This library makes use of the [`free` package][free] which provides code very
similar to this, with some performance optimizations and more advanced options.
At the heart of it, though, is something very similar to my `Free` example.

The central type is `TubeF`, a functor representing the two states a streaming
pipeline can be in: yielding a value downstream, or awaiting a value from
upstream. My definition is a bit different but equivalent to the following:

```haskell
{-# LANGUAGE DeriveFunctor #-}
data TubeF a b k
    = Await (a -> k)
    | Yield (b,   k)
    deriving (Functor)
```

Then the `free` package is used to convert this into the `Tube` type. Thus, a
`Tube` can be in one of three states: `Await`-ing an upstream value;
`Yield`-ing a value downstream; or returning some `Pure` value.

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

This may not make much sense. Let's take a look at a simpler type: boolean
values. We can write our own like this:

```haskell
data NaiveBoolean = T | F

if' :: NaiveBoolean -> a -> a -> a
if' (T) then' _ = then'
if' (F) _ else' = else'
```

Depending on which `NaiveBoolean` value is received by `if'`, a choice is
selected from two options.

Really, `T` is a function which selects the first of two options, and `F`
selects the second. So let's define an alternate representation:

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

`if'` and `Boolean` are really just optimization hints to the compiler, and
conveniences for the programmer. They are optimized away; the second and third
arguments to `if'` are simply handed over to the value wrapped inside
`Boolean`.

Data is control; control is data.

This is called a [Boehm-Berarducci encoding][bbencode]. Instead of building up
a mountain of `Yield`s and `Await`s wrapping each other, which can waste memory
and slow down execution, the state of the `Tube` dictates which of two
alternatives it will take.[^2]

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
