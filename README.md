FreeStream
==========

Iteratee-based streaming library from a free monad

(c) 2014 Gatlin Johnson <gatlin@niltag.net>

Synopsis
---

This exists mostly for my own education about 1) [iteratees][iteratees] and 2) free monads / monad transformers.
It will probably change often and at any given moment could be brilliant, horrifying, inane, or simply wrong.

Load `test.hs` for some sample iteratee usage. The following is inspired by a
[pipes tutorial][pipes] I found.

    ghci> for (each [1..10] +> map show) putStrLn
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10

    ghci> for (each [1..9] +> drop 7 >+ show) putStrLn
    8
    9

    ghci> stream (prompt |- (/= "Die Antwoord") >+ (++ " sucks")) |> print
    > dubstep
    dubstep sucks
    > normal music
    normal music sucks
    > Die Antwoord
    > rock
    rock sucks

    ghci> for prompt putStrLn
    > one
    one
    > two
    two

    ghci> let sumS = fold (+) 0 :: Sink (Stream Int) IO Int
    ghci> stream (each [1..10]) |> sumS
    55

    ghci> let maxInput n = prompt +> take n
    ghci> stream (maxInput 2) |> print
    > first
    first
    > second
    second

    ghci> stream (each [1..10] >+ (:[])) |> accum
    [1,2,3,4,5,6,7,8,9,10]

    ghci> stream (each (Just 1) >+ (:[])) |> accum
    [1]

Given:

    {-# LANGUAGE MonadComprehensions #-}
    fizzbuzz n = fromMaybe (show n) $ [ "fizz" | n `rem` 3 == 0 ]
                                   <> [ "buzz" | n `rem` 5 == 0 ]
                                   <> [ "bazz" | n `rem` 7 == 0 ]

then ...

    ghci> for (each [1..100] >+ fizzbuzz) putStrLn
    1
    2
    fizz
    4
    buzz
    fizz
    bazz
    8
    fizz
    buzz
    ...

[iteratees]: http://okmij.org/ftp/Streams.html
[pipes]: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/Pipes%20tutorial

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.
