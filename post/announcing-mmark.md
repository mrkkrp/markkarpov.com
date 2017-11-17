---
title: Announcing MMark
desc: In this post I talk about a new markdown processor I've been working on.
date:
  published: November 17, 2017
---

Yesterday I released a new package called
[`mmark`](https://hackage.haskell.org/package/mmark) (pronounce “em-mark”).
It is a markdown processor written in Haskell. In this post I'd like to
share why I decided to write yet another markdown processor, how it is
different, and what my future plans regarding this project are.

## Motivation

If you're looking for a markdown processor, that is, something that turns
markdown into HTML, there are several options for a Haskeller:

* [`pandoc`](https://hackage.haskell.org/package/pandoc). It is by far the
  most popular choice. Pandoc is well-known, and it's actually more than a
  markdown processor, as it can convert between many different formats of
  documents. It's also often used to produce static HTML for blogs like this
  one due to its rich collection of features.

* [`cmark`](https://hackage.haskell.org/package/cmark) is another markdown
  processor by the same person who authored Pandoc—John MacFarlane. It
  provides Haskell bindings to `libcmark`, the reference parser for [Common
  Mark](http://commonmark.org/), which is a well-defined compatible
  specification of markdown. It's also worth noting that `cmark`, being
  written in C, is quite fast.

* [`cheapskate`](https://hackage.haskell.org/package/cheapskate) is an
  experimental Markdown processor in pure Haskell, again by John MacFarlane.
  It aims to process Markdown efficiently and in the most forgiving possible
  way. It is designed to deal with any input, including garbage, with linear
  performance. Output is sanitized by default for protection against XSS
  attacks.

* [`markdown`](https://hackage.haskell.org/package/markdown) is a solution
  from Michael Snoyman, we all know him. Can parse markdown and convert it
  to HTML. Has additional features that make it good (or rather a bit better
  than others) for publishing (you can customize the parser, for instance).

* [`sundown`](https://hackage.haskell.org/package/sundown) is bindings to
  GitHub's (former) C markdown library. The projects has not been updated
  since 2014 and seems to be abandoned.

* [`discount`](https://hackage.haskell.org/package/discount) is bindings to
  yet another markdown library written in C called well…
  [`discount`](http://www.pell.portland.or.us/~orc/Code/discount/).

*(To come up with the list I used [this
source](https://guide.aelve.com/haskell/markdown-hm7miz9n).)*

All these packages follow the philosophy of accepting any text as valid
markdown, processing it in “the most forgiving possible way”. Sure, it makes
sense if we remember how markdown is usually used on sites: to turn user's
input in the form of plain text into something a bit richer. We would like
to accept any input and punish the user (even if unintentionally) when
he/she makes a mistake by outputting something unexpected. Then the user has
to find the source of the problem and fix it, then fix it again, till the
rendition becomes acceptable.

Now, I'd like to note that this is not the only use of markdown. Source of
this post for example is written in markdown, because well, it's a familiar
and simple format that allows me to write and edit text in a readable way,
and then turn it into HTML for you to browse. While writing this post, do I
really want the markdown processor (still Pandoc in my case) to accept any
input as valid markdown and force me to review output carefully in order to
avoid problems with the final result? Nope! I'd rather prefer it to tell me
explicitly where parsing errors happen and what exactly is wrong. And I'd
like it to be quite strict about that. So this is the first thing I'd like
to have: *I'd like a markdown processor that can say “no” to a user, and
make him/her fix his/her mistakes*.

Another thing is extensibility. *I'd like to provide a framework for writing
powerful extensions.* Sure, different markdown processors do provide
extensions, but you can either opt in or opt out, you typically cannot write
your own custom thing. This is the case with Pandoc for example, it has a
long list of markdown extensions I can enable, but no matter how long the
list is, I'll always want something that isn't there.

These two issues are the main source of motivation behind MMark. Now we can
take a look how they are addressed in that library.

## A “getting started” example

Before we do so though, it makes sense to get a taste of the library. It's
quite minimal on the API side, like almost all my recent projects. I put a
lot of conscious effort to reduce the number of things I expose not to
overwhelm users but still get things done in a flexible way.

This snippet, for example, shows mostly everything you'll ever need as a
regular user:

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text.IO      as T
import qualified Data.Text.Lazy.IO as TL
import qualified Lucid             as L
import qualified Text.MMark        as MMark

main :: IO ()
main = do
  let input = "input.md"
  txt <- T.readFile input -- (1)
  case MMark.parse input txt of -- (2)
    Left errs -> putStrLn (MMark.parseErrorsPretty txt errs) -- (3)
    Right r -> TL.writeFile "output.html" -- (6)
      . L.renderText -- (5)
      . MMark.render -- (4)
      $ r
```

It should be obvious what this little program does:

1. We read a source markdown file as strict `Text`.
2. The source is fed into the `MMark.parse` function which does the parsing.
   It can either fail with a collection of parse errors (yes, it does not
   choke on the first parse error, more about that later) or succeed
   returning a value of the opaque `MMark` type.
3. If parsing fails, we pretty-print the parse errors.
4. Then we just render the document with `MMark.render` first to Lucid's
   `Html ()`
5. …and then to lazy `Text` with `L.renderText`.
6. Finally we write the result as `"output.html"`.

Once you get a value of the `MMark` type, you can do literally only three
things with it (I will return to some of these later):

1. Scan it with `runScanner` . This cannot change the `MMark` document.
2. Apply an extension to `MMark` document with `useExtensions` or
   `useExtensions`. We can pretend that this actually changes the document
   (there is no way for an API user to prove otherwise anyway), but really
   extensions just get fused for final efficient application just before
   rendering.
3. Render it with the `render` function.

Let's try to feed some markdown to this program and see what happens now.

## A taste of strict markdown

Given this input:

```markdown
#My header

Something is __not __ so right about this paragraph.

[Here goes link text, [another link](/my-url)](/my-url).
```

The program outputs the following parse errors:

```
input.md:1:2:
  |
1 | #My header
  |  ^
unexpected 'M'
expecting '#' or white space
input.md:3:21:
  |
3 | Something is __not __ so right about this paragraph.
  |                     ^
'_' should be in left- or right- flanking position
input.md:5:23:
  |
5 | [Here goes link text, [another link](/my-url)](/my-url).
  |                       ^
unexpected '['
expecting ']', inline content, or the rest of inline content
```

Here we can see how the parser has spotted three different problems in one
pass:

* `#My header` is not a valid header in markdown because there must be at
  least one space between the hash sign and the header text itself. This is
  a common mistake and markdown processors would usually fall back and
  interpret this as a paragraph starting with a hash. I decided to catch
  these nasty little mistakes and report them. In the unlikely case when you
  really want to start a paragraph with a hash sign `#`, just escape it with
  backslash.

* Here `__` is part of strong emphasis but it must go after “not” without
  spaces between them. Normal markdown engine would just accept this and
  render underscores literally. Most likely, that's not what you want.
  Again, to put literal underscores it's enough to escape them.

* Putting a link inside of text of another link is not a good idea and we
  can detect that and report too.

After fixing these issues, we get the expected result:

```html
<h1 id="my-header">My header</h1>
<p>Something is <strong>not</strong> so right about this paragraph.</p>
<p><a href="/my-url">Here goes link text</a>.</p>
```

*(You can add enclosing `body` and `html` tags around this manually, for now
MMark doesn't do that for you.)*

MMark is not a fully custom dialect of Markdown though, in most cases it
behaves quite conventionally. We should thank John MacFarlane not only for
developing so many markdown processors, but also for writing [the Common
Mark specification](http://spec.commonmark.org/0.28/) I mentioned above. I
took it as a starting point and only diverged from it where I saw that doing
so would be an improvement. The
[readme](https://github.com/mrkkrp/mmark/blob/master/README.md) of MMark
documents all differences between Common Mark and MMark, so I won't
re-iterate the information here.

Finally, while working on the project I noted several times how essential it
is that I have Megaparsec in its current state in my disposal. Without it I
would not be able to get such nice error-reporting.

## Extensibility

TODO

* Extension system (overview). Mention scanning and how many things may be
  combined into a single scan.

## Let's write some extensions

* Extension system (concrete examples from `mmark-ext`). Something like a
  quick walkthrough of how they are implemented. A link to the actual
  `mmark-ext` package would be nice too.

## Performance and inner workings

* Efficiency. Mention that block-level parsing is quite fast, thanks to
  latest improvements available in Megaparsec 6. Inline parsing can be
  optimized though. Mention the possibility of parallel inline parsing.

## Future plans

MMark is right now in the “proof-of-concept” state, it does not even support
essential things like blockquotes and lists, so it's not for real-world use
yet (you're welcome to play with it anyway, of course). The missing features
should be easy to add to the base I already have, because it looks like the
base has evolved into something that is well-designed for what I have in
mind. So it's just a matter of time when MMark will be power enough to
replace Pandoc at least for my personal use.

Features/ideas related to MMark itself, roughly in the order I'd like to
work on them:

* Implement blockquotes.
* Implement lists (ordered and unordered).
* Optimize inline-level parser without degrading quality of error messages.
* Support link references and link reference definitions (so you can put URL
  at the end of your document).
* Allow image references (the same thing).
* Implement PHP-style footnotes.
* Support for HTML blocks.
* Support for HTML inlines.
* Implement pipe tables as supported by GitHub.
* Support entity and numeric character references.
* Experiment with parallel parsing of inlines.

MMark extensions that would be nice to have in `mmark-ext`:

* An extension to generate anchors statically with results similar to what
  `anchor.js` produces.
* An extension that would allow to mark links so they will open in a new
  tab.
* An extension that would allow interpolation of values from a context, this
  could turn MMark into a sort of template system.
* Syntax highlighting extensions:
    * JSON
    * YAML
    * Haskell, high-quality syntax highlighter which can link to language
      extensions and pragmas in GHC user guide, and does not choke on
      `DataKinds` and `TypeApplications`, god dammit.

The last one probably could be a project on its own :-D
