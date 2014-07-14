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

    ghci> poll $ Chunk "what is this" $> reverseS
    "siht si tahw"

    ghci> poll $ Chunk [1..10] $> sumS
    55

    ghci> poll $ Chunk [1..10] $< [ prodS , sumS ]
    [3628800,55]

    ghci> poll $ prompt +< [ reverseS , insult ]
    > gatlin
    ["niltag","gatlin sucks"]

    ghci> poll $ Chunk "gatlin" $> insult +> reverseS
    ("skcus niltag",End)

    ghci> poll $ prompt +< [ reverseS , insult ] +> concatS +> printS
    > gatlin
    "niltaggatlin sucks"

[iteratees]: http://okmij.org/ftp/Streams.html

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "FreeStream" somewhere in the subject line.
