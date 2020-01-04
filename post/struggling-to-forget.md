---
title: Struggling to forget
desc: In this post I show a use case and a way to implement conditional fixing of existentially quantified variables by universally quantified variables.
date:
  published: January 3, 2020
tag: haskell
---

In this post I show a use case and a way to implement conditional fixing of
[existentially quantified][existential-quantification] variables by
universally quantified variables.

## Motivation

Imagine we want to write a library for working with typed file paths.
[`path`][path] is an example of such a library, but we'll be aiming for
something more flexible. One flaw of `path` is that it forces the users to
know too much about the paths:

```haskell
-- | Path of some base and type.
--
-- The type variables are:
--
--   * @b@ — base, the base location of the path; absolute or relative.
--   * @t@ — type, whether file or directory.
newtype Path b t = Path FilePath
  deriving (Data, Typeable, Generic)
```

To create a value of the type `Path` one has to choose what to expect, or in
other words one has to fix `b` and `t`. Correspondingly, there are four
conversion functions[1](footnote:1):

```haskell
parseAbsDir :: MonadThrow m => FilePath -> m (Path Abs Dir)
parseRelDir :: MonadThrow m => FilePath -> m (Path Rel Dir)
parseAbsFile :: MonadThrow m => FilePath -> m (Path Abs File)
parseRelFile :: MonadThrow m => FilePath -> m (Path Rel File)
```

There are no `parseSomeDir` or `parseRelSome` functions. This makes the
users of `path` to prefer absolute paths and resolve relative paths early in
the program by using `IO`-enabled parsing functions instead. It works fine
most of the time, but sometimes we may want to parse purely yet without
knowing whether our path is absolute or relative.

The type of our path could be this:

```haskell
data Platform = Posix | Win
data Base = Abs | Rel
data Type = Dir | File

data Path (p :: Platform) (b :: Base) (t :: Type) where
  PosixAbsDir :: FilePath -> Path 'Posix 'Abs 'Dir
  PosixAbsFile :: FilePath -> Path 'Posix 'Abs 'File
  PosixRelDir :: FilePath -> Path 'Posix 'Rel 'Dir
  PosixRelFile :: FilePath -> Path 'Posix 'Rel 'File
  WinAbsDir :: FilePath -> Path 'Win 'Abs 'Dir
  WinAbsFile :: FilePath -> Path 'Win 'Abs 'File
  WinRelDir :: FilePath -> Path 'Win 'Rel 'Dir
  WinRelFile :: FilePath -> Path 'Win 'Rel 'File
```

Here I add the platform index because a good library should make distinction
between Posix and Windows paths. Windows paths have the drive letter for
example, while Posix paths do not, so they are not the same thing. Indeed,
we should be able to even mix both in the same program.

Another reason I added an extra type index is to make the combinatorial
explosion really bad. If we want to be able to make some indices existential
while leaving the others fixed, we just won't be able to use existential
wrappers because there are too many possible combinations:

```haskell
data PathSomePlatform b t = <…>
data PathSomeBase p t = <…>
<…>
data PathSomePlatformSomeBase t = <…> -- both platform and base are existential
<…>
```

In other words, we want a smart constructor which can fix arbitrary sub-set
of the type indices, that is, convince the type system that if we know that
we expect an absolute path then `b ~ 'Abs` in the returned `Path` while
leaving things that we do not know existential. Those are to be discovered
later by case-analysis.

## Solution

If GHC had proper existential quantification we wouldn't need [CPS][CPS], but 
right now it is the only way to get the required flexibility on the type level:

```haskell
mk ::
  MonadThrow m =>
  -- ...
  FilePath ->
  (forall p b t. Path p b t -> m r) ->
  m r
```

It wasn't obvious to me at first how to achieve fixing `p` `b` and `t` only
sometimes while leaving them existential in other cases. We spent some time
discussing it with Richard Eisenberg on several occasions, and yet concluded
that it was probably impossible.

Later though, it occured to me that conditional constraining may have
something to do with constraints:

```haskell
data ((x :: k) `Or` (y :: k)) (c :: k -> Constraint) where
  Any :: (x `Or` y) Unconstrained
  First :: (x `Or` y) ((~) x)
  Second :: (x `Or` y) ((~) y)

class Unconstrained (x :: k)
instance Unconstrained x

mk ::
  MonadThrow m =>
  ('Posix `Or` 'Win) pc ->
  ('Abs `Or` 'Rel) bc ->
  ('Dir `Or` 'File) tc ->
  FilePath ->
  (forall p b t. (pc p, bc b, tc t) => Path p b t -> m r) ->
  m r
```

By defining a few values with more friendly names we can then achieve nice
end-user experience:

```haskell
any :: (x `Or` y) Unconstrained
any = Any

posix :: ('Posix `Or` 'Win) ((~) 'Posix)
posix = First

win :: ('Posix `Or` 'Win) ((~) 'Win)
win = Second

abs :: ('Abs `Or` 'Rel) ((~) 'Abs)
abs = First

rel :: ('Abs `Or` 'Rel) ((~) 'Rel)
rel = Second

dir :: ('Dir `Or` 'File) ((~) 'Dir)
dir = First

file :: ('Dir `Or` 'File) ((~) 'File)
file = Second
```

In the case of `any`, the corresponding type index will be fixed anyway
during parsing, yet it won't be immediately known at the type level.

## Case analysis on the `Path p b t` type

The general idea is that the types are not constrained unnecessarily but can
be discovered when needed by performing case-analysis. Consider a pattern
like this:

```haskell
pattern IsPosix :: Path 'Posix b t -> Path p b t
pattern IsPosix path <- (isPosix -> Just path)

isPosix :: Path p b t -> Maybe (Path 'Posix b t)
isPosix = \case
  PosixAbsDir path -> Just (PosixAbsDir path)
  PosixAbsFile path -> Just (PosixAbsFile path)
  PosixRelDir path -> Just (PosixRelDir path)
  PosixRelFile path -> Just (PosixRelFile path)
  _ -> Nothing
```

By using it the users could establish that `p ~ 'Posix` in one branch of
execution while handing the opposite case in the other branch. This does the
same thing that we do routinely with sum types, but for type-level indices.
In a way this is similar to using bare GADTs except the correspondence
between types and data constructors is less precise and you do not discover
more than necessary unless you want to—patterns like this can also be
nested!

## A new library?

While the main difficulty is now resolved it is not clear how to handle the
`any` case in practice for something like `Dir` vs `File`. Indeed, a
trailing slash may guarantee that you have a directory path, but if it is
not there and the user doesn't say what he/she wants, it is hard to guess
what you have—a directory or a file.

Less seriously, how can I release the library if I'm now quoted as a strong
proponent of using *simple* Haskell? People will be confused.

----

> footnotes

  1. There are more, but they are just variations of these, e.g. in the form
     of quasi-quotes or TH helpers.

[path]: https://hackage.haskell.org/package/path
[existential-quantification]: /post/existential-quantification.html
[CPS]: https://en.wikipedia.org/wiki/Continuation-passing_style
