---
title: Megaparsec 8
desc: New version of the parsing library has been released.
date:
  published: November 7, 2019
tag: haskell
---

Another year has passed and it is time again for a new major version of
Megaparsec. What is different this time though, is that this is the least
disruptive major release ever. In fact, I think most users will not need to
do anything at all to upgrade.

There are reasons for this:

* There are not so many issues opened and bugs reported. I think it has to
  do with the fact that Megaparsec “just works” these days and mostly in a
  satisfying manner.

* The library is widely used now. At the time of this writing there are [146
  packages on Hackage][reverse-deps] that depend on Megaparsec directly.
  [New exciting libraries][replace-megaparsec] that choose to build on top
  of Megaparsec appear. Projects such as [Idris][idris] and [Dhall][dhall]
  use Megaparsec to solve their parsing issues.

All this tells us that the library got older and more mature, so perhaps
let's not break something that is working. That said, there is always room
for improvement.

## Quality assurance with Nix

Before I started working on version 8, I decided to go for better quality
assurance using Nix. Understanding how many projects now depend on
Megaparsec and remembering my successful experiment with using Nix to find
bugs in [Ormolu][ormolu], I decided to try to use Nix to check for breakage,
performance changes, and bugs by using dependent packages.

I documented the result in the [`HACKING.md`][hacking] file which is now in
Megaparsec's repo.

Apart from the development shell, the Nix expression provides the following
groups of targets:

* `base`, which is a set of closely related packages such as
  `parser-combinators` and `hspec-megaparsec` together with their tests. By
  running `nix-build -A base --no-out-link` developer can make sure that all
  of this still builds and still passes the tests.

* `deps`, which is a set of selected dependencies that I wanted to check for
  build breakage and test suite failures.

* `benches`, which is a collection of benchmarks. This includes Megaparsec's
  micro-benchmarks as well as a few packages that show how the library
  performs on realistic tasks.

Each of these sets can be accessed to “zoom in” on a particular package or
benchmark. For example, I can run `nix-build -A benches.parsers-bench` to
check benchmarks in `parsers-bench`. In short, I found that most packages
will still work with the new changes and those that won't are easy to fix.
In fact, I had to patch the failing packages to continue using the system,
so [the patches for upgrading][patches] are available.

I found no logic or performance regressions.

## Control over parse error locations

Let's talk about the new features. The primitives `failure` and
`fancyFailure` have been replaced by `parseError`:

```haskell
parseError :: MonadParsec e s m => ParseError s e -> m a

-- now 'failure' and 'fancyFailure' are ordinary functions:

failure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m a
failure us ps = do
  o <- getOffset
  parseError (TrivialError o us ps)

fancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m a
fancyFailure xs = do
  o <- getOffset
  parseError (FancyError o xs)
```

This is not about reducing the number of primitives though (which is also
nice). The main feature that `parseError` enables is that we can report
parse errors with arbitrary offsets, not necessarily with current offset
from the parser's state. This is important when you want to make a parse
error to point to a particular part of input even if you already moved past
that point. In the past this had to be accomplished by first obtaining the
correct offset via `getOffset` and then by setting the offset with
`setOffset` just before reporting the parse error. Not only this is ugly, it
is also error-prone: you could forget to restore the correct offset. Here is
a [real example][location-setting-example] from `mmark`:

```haskell
  o' <- getOffset
  setOffset o
  (void . hidden . string) "[]"
  -- ↑ if this fails, we want this to be reported at the offset 'o'
  setOffset (o' + 2)
```

I will not explain the full context here, but it suffices to say that this
code had a bug for some time because I forgot to account for offset
increment after parsing `"[]"` (the `+ 2` part). Now the same thing can be
expressed nicer:

```haskell
  region (setErrorOffset o) $
    (void . hidden . string) "[]"

-- N.B.

region :: MonadParsec e s m
  => (ParseError s e -> ParseError s e)
     -- ^ How to process 'ParseError's
  -> m a
     -- ^ The “region” that the processing applies to
  -> m a
```

`region` used to do the same `getOffset`/`setOffset` hacks and as a side
effect it could change current offset if the function that updates parse
errors changed it. Now `region` can use `parseError` and do away with its
old hacks:

```haskell
region f m = do
  r <- observing m
  case r of
    Left err -> parseError (f err)
    Right x -> return x
```

Nice.

## Better story for multi-error parsers

From the very beginning of the project we were moving slowly in the
direction of supporting multi-error parsing. In version 7 we even started to
return `ParseErrorBundle` instead of old familiar `ParseError`. Everything
was in place for multi-error revolution, except that there was no official
way to report more than one parse error!

One prerequisite for having a multi-error parser is that it should be
possible to skip over a problematic part of input and resume parsing from a
position that is known to be good. This part is accomplished by using the
`withRecovery` primitive (available beginning from Megaparsec 4.4.0):

```haskell
-- | @'withRecovery' r p@ allows continue parsing even if parser @p@
-- fails. In this case @r@ is called with the actual 'ParseError' as its
-- argument. Typical usage is to return a value signifying failure to
-- parse this particular object and to consume some part of the input up
-- to the point where the next object starts.
--
-- Note that if @r@ fails, original error message is reported as if
-- without 'withRecovery'. In no way recovering parser @r@ can influence
-- error messages.

withRecovery
  :: (ParseError s e -> m a) -- ^ How to recover from failure
  -> m a             -- ^ Original parser
  -> m a             -- ^ Parser that can recover from failures
```

Before Megaparsec 8 users had to pick the type `a` to be a sum type
including the possibilities for success and failure. For example, it could
be `Either (ParseError s e) Result`. The parse errors had to be collected
and later manually added to the `ParseErrorBundle` before displaying.
Needless to say, all of this was an example of advanced usage that was not
user friendly.

Megaparsec 8 adds support for *delayed parse errors*:

```haskell
-- | Register a 'ParseError' for later reporting. This action does not end
-- parsing and has no effect except for adding the given 'ParseError' to the
-- collection of “delayed” 'ParseError's which will be taken into
-- consideration at the end of parsing. Only if this collection is empty
-- parser will succeed. This is the main way to report several parse errors
-- at once.

registerParseError :: MonadParsec e s m => ParseError s e -> m ()

-- | Like 'failure', but for delayed 'ParseError's.

registerFailure
  :: MonadParsec e s m
  => Maybe (ErrorItem (Token s)) -- ^ Unexpected item (if any)
  -> Set (ErrorItem (Token s)) -- ^ Expected items
  -> m ()

-- | Like 'fancyFailure', but for delayed 'ParseError's.

registerFancyFailure
  :: MonadParsec e s m
  => Set (ErrorFancy e) -- ^ Fancy error components
  -> m ()
```

These errors can be registered in the error-processing callback of
`withRecovery` making the resulting type `Maybe Result`. This takes care of
including the delayed errors in the final `ParseErrorBundle` as well as
making the parser fail in the end if the collection of delayed errors in not
empty.

With all this, I hope that the practice of writing multi-error parsers will
become more common among the users.

## Other

* As always, for the full list of changes see [the
  changelog][the-changelog].
* I updated all texts including the [official tutorial][official-tutorial]
  to be compatible with version 8. I even extended it to include sections
  teaching how to use the new features.
* Satellite packages such as `hspec-megaparsec` have been updated and now
  work with version 8.

Happy parsing!

[reverse-deps]: https://packdeps.haskellers.com/reverse/megaparsec
[replace-megaparsec]: https://hackage.haskell.org/package/replace-megaparsec
[idris]: https://github.com/idris-lang/Idris-dev
[dhall]: https://github.com/dhall-lang/dhall-haskell
[ormolu]: https://github.com/tweag/ormolu
[hacking]: https://github.com/mrkkrp/megaparsec/blob/master/HACKING.md
[patches]: https://github.com/mrkkrp/megaparsec/tree/31b917b1297950c22925f9ee7f7a588834293103/nix/patches
[location-setting-example]: https://github.com/mmark-md/mmark/blob/8f5534d8068c2b7a139b893639ee5920bcaedd84/Text/MMark/Parser.hs#L787-L790
[the-changelog]: https://github.com/mrkkrp/megaparsec/blob/master/CHANGELOG.md
[official-tutorial]: https://markkarpov.com/tutorial/megaparsec.html
