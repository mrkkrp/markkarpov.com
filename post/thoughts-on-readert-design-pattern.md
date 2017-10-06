---
title: Thoughts on the ReaderT design pattern
desc: In this post I'm going to describe my approach to designing systems with ReaderT and a bit of domain-oriented programming.
date:
  published: August 31, 2017
---

Some intro about the older post by Snoyman and my approach: modelling
domain-specific logic by building a vocabulary of elementary actions that do
not allow us to get into invalid state. Then building application on top of
that layer. An example of older approach.

The original blog post by Snoyman:
https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

My takeaways from ReaderT pattern:

1. The `ReaderT r IO` stack is stateless and so exception-friendly.
2. Since we can store functions in reader context, we have practically
   unlimited customization possibilities.
3. The same as in the original post, all other monads can be modelled via `ReaderT`.

----

High-level approach:

1. Lens with makeClassy to get Has* type classes. Since concrete
   implementation can't have more than one layer of `ReaderT` due to
   functional dependencies on this type class, concrete realization is going
   to use one big record (or nested records) anyway.
2. Group Has* things together with MonadReader constraint and
   MonadBaseControl IO to get fine-grained constraints per application
   aspect. MonadBase IO is as powerful as MonadIO. Note that this allows to
   limit ourselves via respective constraints in type signatures.
3. Building the vocabulary: where to put the functions: in type class as
   methods or separately. Vote for the latter.
4. Testing support: testing is easy because the functions in reader context
   can be adjusted as necessary.
5. Note that lens are essential so we can alter reader context partially
   with local. If we had just getters, we wouldn't be able to do that. An
   example with persistent and `withReaderT` (we go from a more general
   context to `SqlBackend`).

Extensible effects as presented in the paper [“Freer Monads, More Extensible
Effects”](http://okmij.org/ftp/Haskell/extensible/more.pdf) still hurt my
brain and seem a bit removed from what mainstream Haskell programmers deal
with.
