Tubes
==========

Write stream processing computations with monadic side effects and compose them
in a series of tubes.

This is similar to the [pipes][pipes] and [conduit][conduit] libraries in
purpose but its implementation is very different. A tube is either a

- `Source m a`, yielding values of type `a` ;
- `Sink m a`, awaiting values of type `a` ; or
- `Channel m a b`, transforming `a` values into `b` values.

These are all aliases for the same fundamental type: `Tube a b m ()`. However
the different variations are all instances of different classes which endow
them with useful capabilities.

The opposite of a tube, the `Pump`, is also provided in this package. While it
has not been explored as thoroughly as the `Tube`, `Pump` is used internally to
run tube computations and fold over streams.

Thorough documentation, examples, and more information are available at
[http://hackage.haskell.org/package/tubes][hackage]

(c) 2014 - 2016 Gatlin Johnson <gatlin@niltag.net>

Licensing
---

See `LICENSE`.

Questions? Comments? Bugs?
---

Use the Issues feature of GitHub to send me bugs. For all other inquiries, please send mail to <gatlin@niltag.net>
with "Tubes" somewhere in the subject line.

Some terms and ideas were stolen from Gabriel Gonzalez, author of the
[pipes][pipes] library.

[pipes]: http://hackage.haskell.org/package/pipes
[conduit]: http://hackage.haskell.org/package/conduit
[hackage]: http://hackage.haskell.org/package/tubes
[free]: http://github.com/ekmett/free/
