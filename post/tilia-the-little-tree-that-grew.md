---
title: "Tilia: the little tree that grew"
desc: An update on my new formatter project and answers to FAQ.
date:
  published: November 15, 2026
tag: haskell
---

A couple months ago I [announced][tilia-announcement] a new formatter for
Haskell—[Tilia][tilia-repo]. The announcement post talked mostly about
technical implementation details which I feel were indeed appreciated by
some, but many more remained puzzled as to what this new formatter brings to
the table—I think this was the top Reddit comment, for example. Shortly
after the announcement someone opened [an issue][comparison-issue] on
Tilia's issue tracker asking for comparison with other formatters. I also
received quite a few questions by email asking about Tilia and the future of
formatting Haskell in general. All this convinced me that a follow-up post
is in order, this time a much more practical one, ideally with examples. I
held off on writing it though, because I was not yet completely happy with
Tilia and wanted to do more work before I'd write about it again. That work
has largely been done now and I have a few exciting updates to share.

## Case study: Clash compiler

A few days after the announcement I was [contacted][clash-fixes] by the
maintainers of [Clash compiler][clash-compiler] who decided to format their
code base with Tilia and contributed some fixes. I am thankful to these
brave early adopters! It is worth taking a brief look at this story, because
it starts answering the question why Tilia takes formatting Haskell one step
forward, and depending on your situation, it may be a big step. After all,
Clash had a [Fourmolu PR][fourmolu-pr] open since 2024, but it could not
land because of CPP.

The approach to taming CPP with a formatter like Fourmolu is to carefully
plant magic comments `{- FOURMOLU_DISABLE -}` and `{- FOURMOLU_ENABLE -}`
around your use of CPP where verbatim preservation of CPP conditionals does
not work or where CPP directives fall at particularly uncomfortable
positions. So, you get something like this:

```haskell file=clash-ffi/src/Clash/FFI/VPI/Control.hs
controlSimulator :: Control -> IO ()
{- FOURMOLU_DISABLE -}
#if defined(VERILOG_2001)
foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_end :: CInt -> CInt -> IO Bool

foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_restart :: CInt -> CInt -> CInt -> CInt -> IO Bool

controlSimulator control = do
  (c, s, r, d) <- send control

  success <- case control of
    Reset{} -> c_vpi_control_restart c s r d
    _       -> c_vpi_control_end c d

  Monad.unless success $
    throwIO $ CouldNotControl control callStack
#else
controlSimulator control =
  throwIO $ CouldNotControl control callStack
#endif
{- FOURMOLU_ENABLE -}
```

There are three problems with this:
1. This adds visual noise—the exact thing a formatter should be helping
   with, not making worse.
2. The code inside such a disabled region is not formatted, therefore
   formatting consistency of the codebase is compromised.
3. If you have a lot of CPP, as Clash does, surrounding half of your code
   with magic comments is just not worth it, so your project is simply not
   formattable.

As I explained in the announcement post, CPP is a first-class formattable
construct for Tilia, therefore no magic comments are needed (the following
snippet is the formatted output):

```haskell file=clash-ffi/src/Clash/FFI/VPI/Control.hs
controlSimulator :: Control -> IO ()

#if defined(VERILOG_2001)
foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_end :: CInt -> CInt -> IO Bool

foreign import ccall "vpi_user.h vpi_control"
  c_vpi_control_restart :: CInt -> CInt -> CInt -> CInt -> IO Bool

controlSimulator control = do
  (c, s, r, d) <- send control

  success <- case control of
    Reset{} -> c_vpi_control_restart c s r d
    _ -> c_vpi_control_end c d

  Monad.unless success
    $ throwIO
    $ CouldNotControl control callStack
#else
controlSimulator control =
  throwIO $ CouldNotControl control callStack
#endif
```

This is a general theme I want to highlight. Tilia needs no
hand-holding—whatever you have it will format (well, with really rare
exceptions). We will see more examples of this when we talk about formatting
operator chains.

Finally, a stylistic change was also requested by Clash maintainers: when
record fields are spelled out after a data constructor they asked to remove
the space in front of the curly brace:

```haskell
f T {fieldOne, fieldTwo} n = fieldOne + fieldTwo + n
   ^
```

This is already Fourmolu's default, they said, and I accepted.

## The status of CPP support

CPP handling was my primary focus after the initial release. The
implementation went through several redesigns since then and now I can
declare that it is robust. I know this because I can format more than 99% of
Hackage with Tilia correctly and idempotently (based on a sample of Nk most
popular packages, which is N GB of Haskell source code), including such
non-trivial users of CPP as `QuickCheck`, `yesod`, and `lens`. This was not
possible with any formatter before.

![Tilia CLI formatting yesod, quickcheck, lens](/static/img/tilia-cli.png)

The `lens` command from the screenshot gives a taste of what is in that
remaining fraction of a percent:
* `benchmarks/plated.hs` conditionally depends on `uniplate`, guarded by the
  `benchmark-uniplate` flag which nothing ever enables. Thus it is not in
  the build plan, therefore there is no way for Tilia to learn about the
  fixity of `|*`. This design decision is not set in stone. If something
  like this is a show-stopper for you, come talk to me in the issue tracker.
* The second decline is a use of a CPP function-like macro (which is even
  worse than CPP conditionals):

  ```haskell
  lessSimplePoly :: forall KVS(k1 k2) (a :: k1) (b :: k2) .
                    Equality a b a b
  lessSimplePoly f = f
  ```

  It does not parse. `quickcheck` and `yesod` also do things like this, but
  not in a way that fails to parse as Haskell. With more ad-hoc code,
  `lens`'s use case can be made to work too. This is a question of where to
  draw the line.

## The status of comment handling

Comment handling in Tilia has reached absolute fidelity—no comments move in
unexpected ways or disappear. Once again, this conclusion is based on the
results from extensive Hackage scans which cover several thousands packages,
manual reviews of selected fragments, as well as AI-assisted audits and
adversary probing. I worked on the problem of comment placement in August,
for a different formatter. As nice as that change was, it did not fix *quite
everything*, so I kept iterating on this, now in Tilia, until I got it
perfect.

## Tilia's fixity inference in practice

One question I received through emails is why knowing operator fixities is
so important. Let me answer with an example. Servant is formatted with
Fourmolu and it is notorious for having many operators, most interestingly
on the type-level, so we can take an example from it:

```haskell file=servant-foreign/test/Servant/ForeignSpec.hs
type TestApi =
  "test"
    :> Header "header" [String]
    :> QueryFlag "flag"
    :> Get '[JSON] Int
    :<|> "test"
    :> QueryParam "param" Int
    :> ReqBody '[JSON] [String]
    :> Post '[JSON] NoContent
    :<|> "test"
    :> QueryParams "params" Int
    :> ReqBody '[JSON] String
    :> Put '[JSON] NoContent
    :<|> "test"
    :> Capture "id" Int
    :> Delete '[JSON] NoContent
    :<|> "test"
    :> CaptureAll "ids" Int
    :> Get '[JSON] [Int]
    :<|> "test"
    :> EmptyAPI
```

Here, the result looks wrong. In an operator chain that spans multiple lines
fixities determine the nesting of operators. Operator chains are very common
in Haskell and there are plenty of different operators that come from both
third-party and first-party modules. How does Fourmolu solve this? During
development, a database of fixities is assembled by scrapping Hoogle. It is
then shipped with Fourmolu. Here are some problems with this approach:

1. The database goes out of date.
2. The database is incomplete because it only scans the most popular
   packages and as the number of packages on Hackage grows the lacunae will
   be more and more obvious.
3. Haskell's import system is more complex than a lookup in a map.
4. No support for inference of fixities of first-party operators.
5. Once Hoogle changes the format of the pages it serves or removes some
   information about re-exports, you are in trouble. At some point in the
   past, Hoogle did in fact remove some information on re-exports that it
   was providing earlier—this is something I discovered back in August
   before I started Tilia. Back then, I fixed the problem, in a different
   formatter.

These problems motivated Tilia's design around inference of operator
fixities directly from source files and, where available, interface files.
It is much harder, but it is the correct thing to do. Was that complexity
worth it? I think so. Tilia correctly rewrites the example snippet from
above as:

```haskell file=servant-foreign/test/Servant/ForeignSpec.hs
type TestApi =
  "test"
    :> Header "header" [String]
    :> QueryFlag "flag"
    :> Get '[JSON] Int
    :<|> "test"
      :> QueryParam "param" Int
      :> ReqBody '[JSON] [String]
      :> Post '[JSON] NoContent
    :<|> "test"
      :> QueryParams "params" Int
      :> ReqBody '[JSON] String
      :> Put '[JSON] NoContent
    :<|> "test"
      :> Capture "id" Int
      :> Delete '[JSON] NoContent
    :<|> "test"
      :> CaptureAll "ids" Int
      :> Get '[JSON] [Int]
    :<|> "test"
      :> EmptyAPI
```

As it does hundreds of other cases I discovered during my comparative
Hackage scans. Tilia got the fixities right every time.

The added benefit is that Tilia knows your first-party operators
automatically, too. This means that no `fixity` or `reexports` fields need
to be manually maintained in `fourmolu.yaml`.

We can ask Tilia for information about operators with `--debug-fixity`:

TODO A nice screenshot with `--debug-fixity`

## Ode to Fourmolu

So, CPP, comments, and fixities are solved. What's next? With all the hard
problems of formatting out of the way, I decided to work on aligning Tilia
better with how Haskellers write their code. After all, what good is it to
have all these principled solutions and not being able to have leading
commas? ;-)

It is time for Tilia to become just a little bit less opinionated. I decided
to do this right and fair. And that means doing some statistical analysis on
Haskell code found in the wild in order to discover *natural* formatting
preferences of the community. Thankfully, that is not even necessary because
we have Fourmolu.

Let's take a moment to appreciate that project. Fourmolu evolved over the
years in an organic and democratic fashion. Every single configuration
option that is present in Fourmolu (and it has 27!) is there because someone
really wanted it badly enough to take the trouble of implementing it. And
then, others enabled those features in their `fourmolu.yaml` files because
they thought that it was a good idea.

Before I continue and tell you what configuration options are available now
in Tilia, I want to explain why I feel a pressure to have as few of them as
possible, and for any option to have as few values as possible. This has to
do with testing. You see, I am paranoid about correctness of my formatters.
Tilia currently has 21k test cases of many different kinds just in its test
suite which runs on every commit. This is not enough though. I have a
collection of techniques for going far beyond my test suite, e.g. those
Hackage scans. I use these fairly regularly in order to ensure that I have
not missed anything. A single Boolean switch means I need to test twice as
much, so I'd rather have as few of them as possible.

The key idea that made it possible to have configuration options in Tilia
without suffering combinatorial explosion is what I call *orthogonality* of
options. Option A is orthogonal to option B iff enabling them together does
not introduce new bugs and does not remove/hide any bugs present when A or B
are enabled separately.

Now suppose I have some options, each of them with only two possible values.
I can then test with the default config and the alternative config, which is
obtained by flipping each option in the default config. Provided every
option is orthogonal I can test everything by only running the renderer
twice. That's affordable. But how do I know that the options are orthogonal?
Well, I can reason about it, but that's not good enough. At the end of the
day it is an empirical question. It has to be. So, if I test with the
default and the alternative configurations and they are both green on
master, then it follows that if I run a nightly job that actually tests
every possible combination and finds them green, then, as far as my test
suite is concerned, my options are orthogonal. And this is what I settled
on.

### Leading commas

Based on a sample of 1,455 `fourmolu.yaml` configurations found in the wild,
by far the most popular option (93% of users) is `comma-style: leading`.
Indeed, as of 2026, based on my scans of actual source code from Hackage,
most Haskell code is still written with leading commas. This style is now
supported in Tilia (its default is trailing commas). You can enable it by
creating a `tilia.toml` file with this:

```toml file=tilia.toml
leading-commas = true
```

This amounts to Fourmolu's `comma-style: leading` as well as
`import-export-style: leading` which felt stylistically inseparable to me.

### Leading arrows

Leading arrows, enabled in Fourmolu with `function-arrows: leading` is
selected by 40% of users, while 57% prefer trailing ones. 3% choose
`leading-args` which I did not even know existed. Trailing arrows are
Tilia's default, while leading ones can be enabled like this:

```toml file=tilia.toml
leading-arrows = true
```

### Indentation step

Despite this formatter's name, even though 4 space indentation is Fourmolu's
default, it is not the most popular choice among its users. Only 28% of
users choose 4 spaces, while 72% opt for 2. I will make no comment on which
is better, only point out that the indentation step can now be configured:

```toml file=tilia.toml
indentation = 4 # 2 is the default
```

Only 2 or 4. It turned out every value of the indentation step brings its
own spectacular bugs, so 3 space indentation is not supported.

While making indentation 4 work for Tilia I used Fourmolu as my guiding
light, since I have no strong opinion on what Haskell code should look like
with this indentation step and there are some unique challenges with it. I
figured that whatever Fourmolu does is probably the right thing to do for
Tilia as well. That worked nicely, but when I applied my normal testing
processes I also discovered a few cases where Fourmolu breaks code at that
indentation step, e.g.:

```haskell
escape = do
  char '\\'
  do gap; return Nothing
    <|> do code <- escapeCode; return (Just code)
```

this formats as

```haskell
escape = do
    char '\\'
    do gap; return Nothing
    <|> do code <- escapeCode; return (Just code)
```

The statements moved two columns to the right, but the line with `<|>` did
not, and now it starts at the column of the statements. In the input `<|>`
chose between the two inner `do` blocks, both after the backslash. In the
output it ends the outer `do` block and takes all of it as its left operand,
so the second alternative no longer needs the backslash. Fourmolu's AST
check catches this and refuses to write the result unless you pass
`--unsafe`.

I had to use my own judgement for what is right output for Tilia in those
cases. If there is interest in fixing those in Fourmolu please reach out and
I will share my collection of failing examples.

### How many configuration options is enough?

So far there are 3 options, each with 2 possible values. This is 8
combinations in total. I think this is a good middle ground. In the next
sections I will explain other changes I made which are all clear
improvements and so are unconditional. As long as all the options stay
orthogonal, perhaps more options can be added, but I do not currently see
strong candidates, not as strong as leading commas, arrows, and indentation,
at least.

## More intelligent preservation of declaration groupping

Even without looking at Fourmolu I noticed that many single-line top-level
declarations without Haddocks are groupped by their authors:

```haskell file=src/Control/Lens/Cons.hs
infixr 5 <|, `cons`
infixl 5 |>, `snoc`
infixr 4 <|~, |>~, <<|~, <|>~, <<<|~, <<|>~
infix  4 <|=, |>=, <<|=, <|>=, <<<|=, <<|>=
```

This is a consistent pattern and it is worth respecting. Sure enough, I
found that Fourmolu already does that. With `respectful: true`, its default,
Fourmolu keeps together any declarations written without an empty line
between them unless one has a Haddock, so a function spanning several lines
stays glued to a data declaration under it. With `respectful: false` it puts
an empty line between every two. Tilia takes the middle course: one-line
declarations of one kind written together stay together, while a declaration
spanning several lines or having a Haddock, and declarations of different
kinds, get an empty line between them. I found that with this rule, every
time Tilia inserts a blank line it looks natural and justified.

## Automatic breaking of excessively long lines

I have grown tired to “nudging” my formatters to break long lines. Not only
it is annoying to do manually, but you also can never be quite sure that
your preferred column limit is applied everywhere consistently. Really, this
is something a formatter should be able to do. Again, I looked at Fourmolu
and found that the feature has existed there since version 0.12.0.0. The
setting is called `column-limit` but its [documentation
page][fourmolu-column-limit] says:

> WARNING: Setting this option may break idempotence.

So I said to myself, “challenge accepted, I will add breaking of long lines
in Tilia without compromising idempotence and with the added challenge of
making it work with CPP”. First thing first, I looked at how this feature is
implemented in Fourmolu. Because I wrote the underlying rendering engine, I
already had some ideas what kind of challenges this could pose.

Fourmolu lays out every construct either on one line or across several, and
takes that decision from the input: a construct written across several lines
is printed across several lines, and one written on a single line stays on a
single line. `column-limit` adds one more condition: a construct written on
a single line is also printed across several lines when its span in the
input is wider than the limit.

The difficulty with idempotence is that every construct that the limit broke
on the first run is written across several lines, while many decisions of
the renderer depend on how the parts of a construct were written, so the
second run may lay the construct out differently. Take this line, with
`column-limit: 80`:

```haskell
pipeline = filter isRelevant . map normalize . concatMap splitIntoWords . lines . Text.toLower $ inputText
```

Fourmolu 0.21.0.0 formats it as:

```haskell
pipeline =
    filter isRelevant
        . map normalize
        . concatMap splitIntoWords
        . lines
        . Text.toLower $
        inputText
```

Run it again and `$` moves:

```haskell
pipeline =
    filter isRelevant
        . map normalize
        . concatMap splitIntoWords
        . lines
        . Text.toLower
        $ inputText
```

Fourmolu puts `$` at the end of a line only when the expression to its left
is written on one line. In the original input it is, so the first run puts
`$` at the end, and only then does the limit break the expression to its
left across five lines. In the second run that expression is written across
five lines, so `$` goes to the start of the line. Fourmolu's issue tracker
has more examples of the kind, such as [#400][fourmolu-400] and
[#409][fourmolu-409].

Tilia's engine works differently in a way that matters here. A renderer does
not print anything, it builds a document in which line breaks are not
decided yet. Parts of the document are grouped, and a group is laid out as a
whole: flat, with every break in it printed as a space, or broken, with
every break printed as a newline. The engine decides which when it prints
the document.

To this I added a third layout, *frangible*: flat where it fits within the
column limit, broken where it does not. Three properties of the engine make
breaking long lines idempotent:

1. The decision is taken while printing, so it is made on the output: from
   the column the group actually starts at, with its contents as they are
   going to be printed. A group breaks only when it runs past the limit flat
   and its first line ends sooner broken, so a group whose contents cannot
   be shortened is left alone.
2. A group the engine breaks comes out exactly as if it had been written
   across several lines, so the next run reproduces it.
3. Decisions are taken from the outside in. A group laid out flat keeps
   every frangible group inside it flat, so the layout of an inner group
   never depends on something decided after it.

The feature is tested in several ways. First, it is always enabled, so it is
exercised extensively during all normal Tilia testing and Hackage scans.
Then I also have a property test that runs every vendored example in the
test suite with 5 different random column limits. This caught some early
bugs, but now it is consistently green.

The default column limit of 80 can be overridden in the configuration file:

```toml file=tilia.toml
column-limit = 100
```

## Separate first- and third- party sections in imports

How do Haskell programmers group their imports? I looked at the newest
release of 2,134 packages on Hackage and took the 24,507 modules that import
both modules of their own package and modules of other packages, setting
aside those with CPP among their imports:

| How the imports are grouped                      | Modules
|--------------------------------------------------|-------:
| One block, no empty lines                        |     53%
| Groups that mix own and other modules            |     19%
| Other packages, then own modules, in two groups  |     12%
| Other packages, then own modules, in more groups |      7%
| Own modules first                                |      6%
| Own and other modules in alternating groups      |      4%

So about half of the code does not group imports at all, and the other half
groups them in many different ways. Still, one line stands out: of the
modules that do split their imports, 60% separate their own modules from
those of other packages, two thirds of them put their own modules last, and
two groups is the most common way to do it.

That is what Tilia does now. Whatever groups the imports are written in, it
prints them in two: imports from other packages first, then the project's
own modules, each group sorted, with an empty line between them. For those
who write one block, this adds a single empty line, and for those who split
their imports further, it keeps the distinction that matters most. This
seems benign enough, so this behavior is always on. It is analogous to
Fourmolu's `import-grouping: by-scope`, except Tilia takes all packages in
your `cabal.project` to be first-party, not only the current package that
is being formatted.

## Layout improvements

Next, I implemented a number of layout improvements. There are too many to
list here, so I will just give one example.

In pure code a computation often goes in steps, each step using the results
of the previous ones. A natural way to write it is a chain of `let`
expressions, each ending its line with `in`:

```haskell file=clash-lib/src/Clash/Backend/VHDL.hs
name' counted countMap fieldName
  | counted HashMapS.! fieldName > 1 =
      -- Seen this fieldname more than once, so we need to add a number
      -- as a postfix:
      let succ' n = Just (maybe (0 :: Int) (+1) n) in
      let countMap' = HashMapS.alter succ' fieldName countMap in
      -- Each field will get a distinct number:
      let count = countMap' HashMapS.! fieldName in
      (countMap', TextS.concat [fieldName, "_", showt count])
```

Why not a single `let` with three bindings? The bindings of one `let` are
all in scope in each other, in any order. A chain reads like a `do` block in
pure code: every binding can use only those above it, so the order of the
steps is explicit, and each step is a natural place for a comment.

Fourmolu treats every `let` as the body of the one before it, which is what
it is, so the chain turns into a staircase, and the comment ends up after an
`in`. This is how it looks on Clash's Fourmolu branch:

```haskell file=clash-lib/src/Clash/Backend/VHDL.hs
name' counted countMap fieldName
  | counted HashMapS.! fieldName > 1 =
      -- Seen this fieldname more than once, so we need to add a number
      -- as a postfix:
      let succ' n = Just (maybe (0 :: Int) (+ 1) n)
       in let countMap' = HashMapS.alter succ' fieldName countMap
           in -- Each field will get a distinct number:
              let count = countMap' HashMapS.! fieldName
               in (countMap', TextS.concat [fieldName, "_", showt count])
```

Every step goes further to the right, so a long chain drifts off the screen.
Tilia recognizes a chain: a `let` whose `in` ends its line and whose body is
another `let` keeps its line, and the next `let` goes under it. Only the
last `let` is laid out as usual:

```haskell file=clash-lib/src/Clash/Backend/VHDL.hs
name' counted countMap fieldName
  | counted HashMapS.! fieldName > 1 =
      -- Seen this fieldname more than once, so we need to add a number
      -- as a postfix:
      let succ' n = Just (maybe (0 :: Int) (+ 1) n) in
      let countMap' = HashMapS.alter succ' fieldName countMap in
      -- Each field will get a distinct number:
      let count = countMap' HashMapS.! fieldName
       in (countMap', TextS.concat [fieldName, "_", showt count])
```

A chain written with `in` at the start of a line is laid out as usual, so
the choice of style stays with the author.

## Correctness improvements

We all know that Haskell formatters perform an AST check: they parse both
the input and the output, compare the two syntax trees, and refuse to write
the result if the trees differ. This check is what lets us run a formatter
over a whole code base without reading every line of the diff, and we have
learned to trust it.

But a formatter does more than lay out code. It also normalizes and sorts:
it merges imports of the same module, sorts import lists and pragmas, and
drops duplicates. These are changes to the syntax tree made on purpose, so
how does the check let them through? It applies the same normalization to
both sides before comparing them. A bug in the normalization therefore
cannot show up in the check: the input goes through the same faulty logic as
the output, and the two come out equal. This is the worst kind of bug—the
formatter silently changes your program. This kind of correctness problems
was my focus with Tilia lately. I will show two examples here.

### Hiding imports

Take this module:

```haskell
import Data.List hiding (insert)
import Data.List hiding (union)

main :: IO ()
main = print (insert 2 [1, 3], union [1, 2] [2, 3])
```

The first import hides `insert` but brings `union` into scope, the second
one does the opposite, so together they bring in all of `Data.List`.
Fourmolu 0.21.0.0 merges the two into one:

```haskell
import Data.List hiding (
    insert,
    union,
 )
```

This import hides both names and the module no longer compiles. Fourmolu
reports success nonetheless, because its check merges the imports of the
input in the same way before comparing. Tilia used to have the same bug.
Now it merges hiding imports only when they hide the same names, and keeps
the rest apart. No check that normalizes both sides can catch a mistake like
this, Tilia's included, so the only defense is to get the normalization
itself right.

### Pragmas and options

The header of a module is even trickier. GHC reads the `LANGUAGE` pragmas
and the flags of the `OPTIONS_GHC` pragmas one after another, in the order
they are written, and a later flag can undo an earlier one. Extensions imply
other extensions: `TypeFamilies` turns on `MonoLocalBinds` and
`RebindableSyntax` turns off `ImplicitPrelude`, so `NoMonoLocalBinds`
written above `TypeFamilies` has no effect, while written below it, it wins.
Some options accumulate rather than override, such as `-fplugin-opt`, and
plugins run in the order they are given. An edition such as `GHC2024`
replaces the language of the component, and when a header names two
editions, the last one counts. Sorting all this is a minefield.

Fourmolu knows about this. It sorts `LANGUAGE` pragmas in four classes:
editions first, then extensions, then the ones that turn an extension off,
and `ImplicitPrelude` and `CUSKs` last. This keeps the common `GADTs`
followed by `NoMonoLocalBinds` working, but it goes wrong where a header is
in alphabetical order, as many headers are:

```haskell
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE TypeFamilies #-}

module Tag where

data Tag a where
  TagBool :: Tag Bool
  TagInt :: Tag Int

isSet :: Tag a -> a -> Bool
isSet tag x = go tag
  where
    go TagBool = x
    go TagInt = x /= 0
```

`TypeFamilies` comes last and turns `MonoLocalBinds` back on, so it is in
force here, and `go`, which is not generalized under it, type checks.
Fourmolu moves `NoMonoLocalBinds` to the end, where it wins, `go` gets
generalized, and the module no longer compiles. Headers like this exist: two
modules of `hledger-lib` write `NoMonoLocalBinds` above `TypeFamilies`.

The `OPTIONS_GHC` pragmas Fourmolu sorts by their text and removes repeated
ones, so `-Wall`, `-Wno-unused-top-binds`, and `-Wall` again, written as
three pragmas, lose the last `-Wall` and with it the warning. Pragmas are
not part of the syntax tree, so the check does not see any of this unless
the output happens to parse differently. These are corner cases, and I have
no doubt they can be fixed in Fourmolu too. My point is that a check that
compares normalized trees cannot be relied upon to find them.

Getting the header right was a nightmare, but Tilia now does it. Options
keep the order they are written in, repeated ones are kept, and `LANGUAGE`
pragmas are sorted, except that two pragmas whose order matters, such as
`TypeFamilies` and a `NoMonoLocalBinds` under it, keep it, and the rest are
sorted around them. On top of that, `--check-ast` does not trust any of this
logic. It reads the flags of the header before and after formatting with
GHC's own flag reader—`processArgs` from `GHC.Driver.CmdLine` run over GHC's
table of flags, `flagsDynamic`, which is the code GHC itself reads
`OPTIONS_GHC` pragmas with—and compares the extensions each side ends up
switching on or off, the edition, and the Safe Haskell mode. The remaining
options must come out the same and in the same order.

## HLS plugin

Last but not least, Tilia now has an [HLS plugin][hls-plugin]. The most
interesting feature of this plugin is that it sources fixity info directly
from the GHC session HLS already has. In this mode of operation it is just
as precise, but also significantly faster because it does not need to work
out fixities on its own. This is the first HLS formatting plugin to take
advantage of this kind of information. I am thankful to [@fendor][fendor]
for nudging me in this direction, since at first I did not recognize this
opportunity myself.

Everything Tilia needs is already in HLS's memory, so formatting is fast.
Here is how long a formatting request takes in the same build of HLS:

| Module                                      | Lines |  Tilia | Fourmolu
|---------------------------------------------|------:|-------:|--------:
| `Control.Monad.Accum` (`mtl`)               |   328 |  14 ms |    41 ms
| `Text.Megaparsec.Char.Lexer` (`megaparsec`) |   552 |  26 ms |    66 ms
| `Control.Monad.Catch` (`exceptions`)        |   925 | 130 ms |   190 ms
| `Control.Lens.Lens` (`lens`)                | 1,523 |  83 ms |   460 ms
| `Data.Vector.Generic` (`vector`)            | 2,740 | 350 ms | 1,000 ms

While other formatters could (with some work) also source fixities from HLS
(although they would need to introduce support for fixity namespaces first),
the technical challenge for those tools is in achieving matching precision
outside of HLS, which is what was discussed above in the section about
fixities.

## Conclusion

Tilia is now much more robust and is extensively tested. Comments, fixities,
and CPP are all solved. With these problems out of the way, the direction I
am taking now is towards eliminating friction between what Tilia outputs and
what people tend to write. This requires a balance: adding a single Boolean
configuration switch doubles the space Tilia needs to test. Leading commas,
leading arrows, and indentation step seem to be worth it, time will tell
what else is.

Tilia will continue to actively improve and advance. That said, I believe
its output has stabilized now and I do not expect significant changes in the
way it lays out the code. Changes will be in other, less obvious areas.
Finally, in anticipation of GHC 10.0, I want to say that a new version of
Tilia with support for GHC 10.0 syntactic features will be available as soon
as `ghc-lib-parser` allows.

[tilia-announcement]: /post/announcing-tilia.html
[tilia-repo]: https://github.com/mrkkrp/tilia
[comparison-issue]: https://github.com/mrkkrp/tilia/issues/12
[clash-fixes]: https://github.com/mrkkrp/tilia/pull/13
[fourmolu-pr]: https://github.com/clash-lang/clash-compiler/pull/2735
[clash-compiler]: https://github.com/clash-lang/clash-compiler
[fourmolu-column-limit]: https://fourmolu.github.io/config/column-limit/
[fourmolu-400]: https://github.com/fourmolu/fourmolu/issues/400
[fourmolu-409]: https://github.com/fourmolu/fourmolu/issues/409
[hls-plugin]: https://github.com/haskell/haskell-language-server/tree/master/plugins/hls-tilia-plugin
[fendor]: https://github.com/fendor
