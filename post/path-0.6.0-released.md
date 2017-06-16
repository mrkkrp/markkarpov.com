---
title: Path 0.6.0 released
desc: It took some time, but the most popular Haskell library for dealing with well-typed paths got to its next (semi)-major version, yay!
date:
   published: June 16, 2017
---

As a co-maintainer of [`path`](https://hackage.haskell.org/package/path) and the author/maintainer of its companion
package [`path-io`](https://hackage.haskell.org/package/path-io), I'm happy to announce new releases of these
libraries. In this post I'm going to quickly remind you what the package is
about and then explain important fixes and changes in the latest versions.

## Quick intro

The `path` package is currently the most popular way to work with typed
paths in Haskell. “Typed” means here that file paths are the same `FilePath`
strings internally (for the ease of interaction with the existing “vanilla”
paths and libraries), but only can be created through validating smart
constructors which attach some important information to them at the type
level.

The main type is `Path b t` where

* `b`—“the base location” of the path: absolute `Abs` or relative `Rel`.
* `t`—“type”, file `File` or directory `Dir`.

When your paths are indexed by phantom types like this, it becomes much
harder to shoot yourself in the foot. For example, here are some types of
common functions for working with paths:

* `(</>) :: Path b Dir -> Path Rel t -> Path b t`—append two paths.
* `filename :: Path b File -> Path Rel File`—take file name.
* `createDir :: MonadIO m => Path b Dir -> m ()`—well, create a directory
  (from `path-io`).
* `makeAbsolute :: (MonadIO m, MonadThrow m) => path -> m (AbsPath
  path)`—make a path absolute (`AbsPath` is a type function that maps type
  of a path to its absolute version, this is also from `path-io`).

You probably see now that developing with typed paths is less error-prone.

Who uses the packages? `path` and `path-io` are used in Stack—Haskell's most
popular solution for project management. If that's not
enough, [we can also check reverse dependencies of the `path` package.](http://packdeps.haskellers.com/reverse/path)

For a more lengthy intro, comparison with other similar solutions, etc., see
this [original announcement](http://chrisdone.com/posts/path-package) by Chris Done.

## Changes in Path 0.6.0

After Chris (the author of `path` and its primary
maintainer) [requested to help him maintain some of his packages](https://mail.haskell.org/pipermail/haskell-cafe/2017-February/126401.html), we
(Simon Jakobi, Wojciech Daniło, Joe Hillenbrand, Tom Sydney Kerckhove, and
me) started to work towards a new major version that would close some holes
in the package and make it better at handling some edge cases.

Several changes are related to maintainability and are not really visible to
users. I'm going to talk about the changes which are visible to end users
and some of them are breaking.

The main change is that we now have `"."` as a valid relative directory
path. This allows to close this hole:

```
λ> dir <- parseAbsDir "/"
λ> dirname dir
"/"
λ> :t dirname dir
dirname dir :: Path Rel Dir
```

This was a way to construct an incorrectly “tagged” path: `"/"` is an
absolute path, but yet it has the type `Path Rel Dir`.

In Path 0.6.0 the story looks like this:

```
λ> dir <- parseAbsDir "/"
λ> dirname dir
"./"
λ> :t dirname dir
dirname dir :: Path Rel Dir
```

It must be noted that although we parse `"."` as a valid relative path and
print it in this form, internally it's stored as the empty string. This
allows us to perform appending of paths via appending of strings without any
additional normalization, so the slightly awkward `"."` path composes
nicely.

In general, here is a table that shows how `"."` works in different
contexts:

* `"./" </> "./" = "./"`
* `"./" </> "x/" = "x/"`
* `"x/" </> "./" = "x/"`
* `dirname "x" = "./"`
* `dirname "/" = "./"`
* `dirname "./" = "./"`

Another change is related to the `parent` function. Previously it only could
be used with absolute paths, now you can get `parent` of a relative path as
well:

```
λ> rdir <- parseRelDir "x/"
λ> parent rdir
"./"
λ> dir <- parseAbsDir "/"
λ> parent dir
"/"
```

So `"."` acts as a relative dual to the absolute root `"/"`.

The old behavior also led to an inconsistency, which I'm going to
demonstrate here:

```
λ> isParentOf $(mkAbsDir "/") $(mkAbsDir "/")
False
λ> parent $(mkAbsDir "/")
"/"
```

This is resolved by renaming `isParentOf` to `isProperPrefix` (“proper
prefix” is a prefix of a thing that is not equal to that thing), and
deprecating `isParentOf`. Similarly, `stripDir` is renamed to
`stripProperPrefix`.

I must say that all these changes were proposed and implemented by a single
person—[Harendra Kumar](https://github.com/harendra-kumar). I'd like to thank him for his time and
contributions.

## Changes in Path IO 1.3.0

* The behavior of `listDirRecur`, `copyDirRecur`, and `copyDirRecur'` has
  been changed to not follow symbolic links, which is more consistent with
  how Unix utilities work and seem to be a better default behavior. It's
  still possible to list directories following symbolic links via a more
  general `walkDirAccum` function. The switch also was initiated by Harendra
  Kumar ([discussion](https://github.com/mrkkrp/path-io/issues/12)).

* Added `isSymlink` which allows to test whether a path is a symbolic link
  (this does not depend on a similar function from `directory` and will work
  with older GHCs as well).

* Moved the type functions `AbsPath` and `RelPath` to the `AnyPath` type
  class (previously they were standalone closed type families). To my
  knowledge this should not break existing code in most cases. Addition of
  the `AnyPath` constraint may be necessary in some non-trivial cases
  though.

## Conclusion

I believe that the `path` ecosystem has recently reached a new level of
maturity. If you don't yet use `path`, maybe now is the time to give it a
try!
