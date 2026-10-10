---
title: "Ormolu: one refactor that fixed everything"
desc: This post documents a big refactor in Ormolu that resolved all comments-related issues.
date:
  published: August 19, 2026
tag: haskell
---

Well, not everything, not really—only the vast majority of comment- and
Haddock-related issues, which used to be the real Achilles' heel in Ormolu's
design. Let's first look at the examples of the changes that landed, and
then I will try to retrace how we got here.

**Input**

```haskell
{-# LANGUAGE QuasiQuotes #-}

example =
  [ -- A
    [u||], -- B
    -- C
    [u||] -- D
    -- E
  ] -- F
```

**Before**

```haskell
{-# LANGUAGE QuasiQuotes #-}

example =
  [ -- A
    [u||],
    -- B
    -- C
    [u||]
  ]

-- D
-- E
-- F
```

**Now**

```haskell
{-# LANGUAGE QuasiQuotes #-}

example =
  [ -- A
    [u||], -- B
    -- C
    [u||] -- D
    -- E
  ] -- F
```

---

**Input**

```haskell
import Package1
import Package3 (
 hi,
 -- , import1
 test,
 )
import Package2
```

**Before**

```haskell
import Package1
-- , import1

import Package2
import Package3
  ( hi,
    test,
  )
```

**Now**

```haskell
import Package1
import Package2
import Package3
  ( hi,
    -- , import1
    test,
  )
```

---

**Input**

```haskell
data X = X { x :: Int }

f = id
    . -- Some comment
    (\s -> s { x = 1 })
```

**Before** (first pass)

```haskell
f =
  id
    . (\s -> s {x -- Some comment
                = 1})
```

**Before** (second pass)

```haskell
f =
  id
    . ( \s ->
          s
            { x -- Some comment
              =
                1
            }
      )
```

**Now**

```haskell
f =
  id
    . (\s -> s {x = 1}) -- Some comment
```

---

**Input**

```haskell
-- comment
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module Foo where
```

**Before**

```haskell
-- comment
{-# LANGUAGE FlexibleContexts #-}
-- comment
{-# LANGUAGE FlexibleInstances #-}
```

**Now**

```haskell
-- comment
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
```

---

**Input**

```haskell
newNames = let (*) = flip (,) in
    ["Control" * "Monad"

    -- Foo

    -- Bar

    ]
```

**Before**

```haskell
newNames =
  let (*) = flip (,)
   in [ "Control" * "Monad"
  -- Foo

  -- Bar
      ]
```

**Now**

```haskell
newNames =
  let (*) = flip (,)
   in [ "Control" * "Monad"

      -- Foo

      -- Bar
      ]
```

---

**Input**

```haskell
data P = P
  -- | the x
  {x :: Int}
```

**Before** (broken code)

```haskell
data P = P
  {-- | the x
   x :: Int}
```

**Now**

```haskell
data P = P
  { -- | the x
    x :: Int
  }
```

*And many, many more.*

The comment-handling logic in Ormolu had never worked very well. Unlike the
rest of the project it grew organically, in an unprincipled fashion,
motivated by fixing “just one more bizarre case”. It went on like this until
at some point I told myself that I would not act on comment-related issues
anymore. Comment placement worked *somehow*—not nearly badly enough to stop
anyone from using the tool—and I had reached the limits of my ability to put
one more hack on top of the pile.

The reason was structural. Comments were extracted from the parse tree into
a stream, and where each one ended up was decided while printing. `located`
called
`spitPrecedingComments` and `spitFollowingComments`, which consulted five
pieces of mutable state—a destructively popped comment cursor, a
destructively trimmed span cursor, a stack of enclosing spans, the spans
emitted so far on the current line, and a marker for what was emitted
last—and made a decision on the spot. The core predicate,
`commentFollowsElt`, was four conjoined conditions, one of which was a
column-distance guess about whether a comment belonged to the parent node or
to the child. Yeah, it was as bad as it sounds.

The immediate consequence was that we could not answer the question “which
element owns this comment?” without simulating the printer. That made the
rules untestable in isolation, and it meant that every new printing function
was a chance to break comments. It is also why fixes had historically landed as
*more state*—`interferingTxt`, `StatementSpan`,
`getEnclosingComments`—rather than as changes to a rule. The machinery got
harder to reason about with every fix.

The deeper problem was that a single global cursor assumes print order
equals source order. Well, it doesn't. `normalizeImports` sorts and merges
imports before printing. `OpTree` reassociates operator chains. `p_hsDecls`
regroups declarations and hoists pragmas. Anything that visits nodes out of
source order corrupted everything after it—and `spitRemainingComments`, the
end-of-module safety net, is exactly why a misattached comment teleported to
the bottom of the file instead of failing loudly.

## A better solution

The more I thought about the problem the more I realized that Ormolu needs
to assign comments to spans of the elements we print before printing
happens. The sequence would be something like this:

1. Obtain the AST and the comment stream.
2. Run the renderer once with no comments at all and throw the output away.
   What we keep from that pass is the collection of spans that actually took
   part in printing. (In theory this can be obtained by other means, but a
   rendering pass proved to be by far the most reliable way to do it, and it
   is cheap: an extra pass costs a few percent of a run, since most of the
   time goes on parsing rather than on printing.)
3. Decide who owns every comment. This is the heart of the new approach, and
   it is a pure function from the comments and those spans to a list of
   anchor pairs `[(LComment, CommentAnchor)]` where `CommentAnchor` is:

   ```haskell
   -- | Where a comment belongs.
   data CommentAnchor
     = -- | On its own line(s) above the element
       AnchorBefore RealSrcSpan
     | -- | After the element, either on the same line or below it
       AnchorTrailing RealSrcSpan
     | -- | Inside the element, which has no children of its own
       AnchorInside RealSrcSpan
     | -- | Not inside anything: the comment belongs to the module
       AnchorModule
     deriving (Eq, Show)
   ```

   It arranges the spans into a containment forest and then, for each
   comment, finds the element that encloses it most tightly and works out
   which gap between that element's children the comment falls into. A
   comment that starts on the line where the preceding sibling ends trails
   that sibling; otherwise, if a sibling follows, the comment goes before
   it; a comment after the last child trails the enclosing element; and an
   element with no children of its own simply owns whatever was written
   inside it.
4. Turn that association into something the printer can query:

   ```haskell
   data AnchorMap = AnchorMap
     { amBefore :: Map RealSrcSpan [LComment],
       amTrailing :: Map RealSrcSpan [LComment],
       amModule :: [LComment]
     }
   ```

   The pairs are indexed by the span of the element each comment was
   attached to: one map for the comments that go before an element, another
   for those that trail it, and a list for the ones that belong to the
   module rather than to anything in it. While printing, `located` looks up
   the span it is entering and leaving and claims whatever it finds there.
   Claiming is destructive, but keyed rather than sequential, and that is an
   important difference: several AST nodes routinely share a single span,
   and since the printer enters them outermost first, the outermost one
   takes the comment—which is exactly what you want. At the end of the
   module whatever is left is flushed, which catches both the comments that
   belong to no element and anything anchored to an element the printer
   never entered.
5. Finally, let the layout decisions see the comments. This step is easy to
   overlook, but without it the rest does not deliver. Ormolu chooses
   between a single-line and a multi-line layout by looking at the spans of
   the things it is about to print, and comments used to be absent from that
   picture entirely. A construct that fitted on one line was therefore
   printed on one line even when a comment had been written inside it, and
   the comment would then swallow the rest of that line—including the
   closing bracket. That is how a documented field of a short record came
   out as `{-- | the x` and stopped being valid Haskell at all. Now that we
   know which comments belong to a construct before printing it, they count
   towards that decision, and the construct is spread over several lines
   instead.

I thought the plan was solid, but I did not know yet how to get there.

## The process

The last thing I wanted was to make matters worse—break something for the
users without even noticing. So I decided to go slowly and build up some
debugging machinery first. I implemented a custom temporary mode which I
called `comment-placement`. When Ormolu was run with `--mode
comment-placement` it would output the de facto attachment for all comments
it processed. The normal output would be suppressed. I furthermore
implemented a diffing machinery for this kind of output. Next, I extended
the Nix-powered `ormolize` helper to save all placement information for any
Hackage package of interest, along with the normal formatting outputs. I
extended the corpus of data from 42 Hackage packages to 92 while trying to
include stylistically diverse candidates.

Next, I added a whole parallel way of printing comments and a new option
that allowed me to switch back and forth between the old method and the one
I was developing. I would test the differences both on the examples from the
test suite and on Hackage. Alas, the golden test suite proved to be too easy
to satisfy, so I would draw from the corpus of Hackage packages quite often
and find many interesting cases that exposed deviations between the two
approaches. Whenever that would happen I would add new examples to Ormolu's
golden test suite. Iterating like this, I eventually was able to close the
gap completely and establish parity between the old and the new approaches.
This was the moment when I switched to the new approach and reorganized the
code around it. I still preserved the `comment-placement` mode because I
would return to comment placement verification many times over in order to
confirm the exact implications of all changes I made.

That was the point when I was able to resolve at least 14 outstanding issues
in a rather natural manner, as a logical consequence of the new approach.
The issues that got resolved are the following:

* <https://github.com/tweag/ormolu/issues/641>
* <https://github.com/tweag/ormolu/issues/752>
* <https://github.com/tweag/ormolu/issues/786>
* <https://github.com/tweag/ormolu/issues/787>
* <https://github.com/tweag/ormolu/issues/810>
* <https://github.com/tweag/ormolu/issues/822>
* <https://github.com/tweag/ormolu/issues/936>
* <https://github.com/tweag/ormolu/issues/1028>
* <https://github.com/tweag/ormolu/issues/1074>
* <https://github.com/tweag/ormolu/issues/1076>
* <https://github.com/tweag/ormolu/issues/1131>
* <https://github.com/tweag/ormolu/issues/1159>
* <https://github.com/tweag/ormolu/issues/1164>
* <https://github.com/tweag/ormolu/issues/1168>

After verifying that I did not introduce any bugs via the
`comment-placement` mode, I cleaned up and removed the temporary machinery.

## Conclusion

The results of this work are available in [Ormolu 0.9.0.0][ormolu-0900] on
Hackage. Even though the changes may not be visible in most situations, they
will surely save users from quite a few frustrating formatting results.
This is a serious-enough refactor with deep consequences to warrant a major
version bump.

[ormolu-0900]: https://hackage.haskell.org/package/ormolu-0.9.0.0 "_blank"
