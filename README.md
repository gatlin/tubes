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

    >>> run $ for (each [1..4] >< map show) (lift . putStrLn)
    1
    2
    3
    4

    >>> reduce (+) 0 id (each [1..10])
    55

    >>> let echo = prompt >< print
    >>> run echo
    > ping
    ping
    > pong
    pong

    >>> run $ prompt >< filter (/= "Die Antwoord") >< map (++ " sucks") >< print
    > dubstep
    dubstep sucks
    > the sun
    the sun sucks
    > Die Antwoord
    > this program
    this program sucks

    >>> reduce (++) [] id (each [1..10] >< map (\x -> [x]))
    [1,2,3,4,5,6,7,8,9,10]

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
