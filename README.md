FreeStream
==========

Iteratee-based streaming library from a free monad

(c) 2014 Gatlin Johnson <gatlin@niltag.net>

Synopsis
---

This exists mostly for my own education about 1) [iteratees][iteratees] and 2) free monads / monad transformers.
It will probably change often and at any given moment could be brilliant, horrifying, inane, or simply wrong.

Load `test.hs` for some sample iteratee usage. The following is extracted from
that file's comments:

    ghci> (v, k) <- run $ "what is this" $> reverseS +> printS
    Printing: siht si tahw
    ghci> v
    "siht si tahw"

    ghci> (v, k) <- run $ prompt *< [ reverseS , insult ]
    > gatlin
    ghci> v
    ["niltag","gatlin sucks"]

    ghci> (v, k) <- run $ [1..10] $> sumS
    ghci> v
    55

    ghci> (v, k) <- run $ "no more spaces" $> (filterSpaces reverseS)
    ghci> v
    "secapseromon"

[iteratees]: http://okmij.org/ftp/Streams.html

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.
