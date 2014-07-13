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

    ghci> feed reverseS $ Chunk "what is this"
    "siht si tahw"

    ghci> feed sumS $ Chunk [1..10]
    55

    ghci> feed sumS $ Chunk . Just $ 10
    10

    ghci> (v, k) <- run prompt
    > wild and exciting user input

    ghci> v
    "wild and exciting user input"

    ghci> forList v getword
    ("wild", Chunk "and exciting user input")

[iteratees]: http://okmij.org/ftp/Streams.html

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.
