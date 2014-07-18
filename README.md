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

    ghci> run $ for (each [1..10]) $ \n -> lift $ putStrLn . show $ n
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

    ghci> run $ for (each [1..9] +> drop 7) $ \n -> lift $ putStrLn . show $ n
    8
    9

    ghci> run $ for prompt $ \str -> lift . putStrLn $ "Printing: " ++ str
    > one
    Printing: one
    > two
    Printing: two

    ghci> (lift getLine) ~> print
    hello
    hello
    stop repeating me!
    stop repeating me!

    ghci> run $ (lift getLine) >~ doubleUp >~ print
    one
    two
    onetwo
    three
    four
    threefour

    ghci> run $ prompt +> take 3 +> print
    > one
    one
    > two
    two
    > three
    three

    ghci> let maxInput n = prompt +> take n
    ghci> run $ maxInput 2 +> print
    > first
    first
    > second
    second

    ghci> run $ prompt +> filter (/= "Die Antwoord") +> map (++ " sucks") +> print
    > dubstep
    dubstep sucks
    > normal music
    normal music sucks
    > Die Antwoord
    > rock
    rock sucks

[iteratees]: http://okmij.org/ftp/Streams.html
[pipes]: https://www.fpcomplete.com/school/to-infinity-and-beyond/pick-of-the-week/Pipes%20tutorial

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.
