---
title: Announcing MMark 0.1.0.0
desc: A large release of my strict markdown processor, with a reworked extension system and a jump from CommonMark 0.28 to 0.31.2.
date:
  published: August 16, 2026
tag: haskell
---

[`mmark`](https://hackage.haskell.org/package/mmark) is a strict markdown
processor I wrote in 2017. “Strict” means that not every input is a valid
document and parse errors are possible, and desirable, because they let you
spot markup issues without hunting for them in the rendered output.

Version `0.1.0.0` is the largest release the package has had. It does two
things: it catches the parser up with eight years of CommonMark, and it
rebuilds the extension system so that extensions can perform effects and
report errors of their own against the source of the document. There is a
matching release of
[`mmark-ext`](https://hackage.haskell.org/package/mmark-ext).

This is a breaking release. Every extension will need changes, and there is
a migration section at the end.

## Catching up with CommonMark

The test suite was written against CommonMark 0.28, which came out in 2017.
It now follows 0.31.2 from 2024. Renumbering the roughly 560 examples the
suite referenced was mechanical, but three behaviours actually changed, and
along the way I found four genuine bugs.

### Block quotes

MMark used to define a block quote by indentation: a quote continued for as
long as its content stayed indented. CommonMark instead requires a `>`
character on every line, one per level of nesting, and allows paragraphs
inside a quote to be continued lazily on lines that lack the marker.

MMark now does the same. This is the change most likely to affect existing
documents, because markup like this no longer means what it used to:

```
> footnotes

  1. Here we have the footnote.
```

The list is no longer inside the quote. Write it with the markers instead:

```
> footnotes
>
> 1. Here we have the footnote.
```

Block quotes also take precedence over tables now, so a line beginning with
`>` opens a quote even when it looks like a table header. And unlike
paragraphs, tables cannot be continued lazily: a row that does not carry the
block quote markers of the table it belongs to ends both the table and the
quote.

### Emphasis on a part of a word

This is the change I am happiest about, and it took a reframing to get
there.

MMark's rule for emphasis was described in terms of levels: space characters
are level 0, punctuation level 1, everything else level 2, and a run of
markup characters opens emphasis when `level(L) < level(R)` and closes it
when `level(L) > level(R)`. That is not an arbitrary rule, and writing out
the nine combinations shows why:

`level(L) < level(R)` is exactly what CommonMark calls “left-flanking but
not right-flanking”, and `level(L) > level(R)` is exactly “right-flanking
but not left-flanking”. MMark was computing precisely CommonMark's
classification. What it did differently was refuse to make a decision when a
run is *both*, which is to say when the levels are equal.

And that case is the interesting one, because a delimiter run with word
characters on both sides is emphasis inside a word. MMark rejected all of
them, which meant this did not parse:

```
un*frigging*believable
```

Worse, it meant that subscript and superscript — features MMark advertises —
did not work in the position they exist for:

```
H~2~O
x^2^
```

The new rule adds one sentence to the old one:

> An ambiguous run closes the markup it is inside of when it is exactly what
> that markup is waiting for, and opens new markup otherwise.

All three of the above now work. So do `foo*bar*`, `**foo**bar`,
`5*6*78`, and `foo***bar***baz`.

There is one exception, and it concerns the underscore. A run of underscores
with word characters on both sides is not markup at all, it is literal text:

```
snake_case and to_string() and __dunder__
```

This is the only place in MMark where a markup character does not have to be
backslash escaped to be taken literally, and it exists because underscores
are so common inside identifiers. Asterisks remain the way to emphasize part
of a word. Note that the exception is narrow: `something_` still needs
escaping, because a trailing underscore is an unambiguous *closing* run with
nothing to close.

What stays an error is unchanged in spirit. A run with white space on both
sides can do nothing (`*Something * is not right.`); a run that closes
markup which was never opened is an error; markup that is opened and never
closed is an error. CommonMark renders all three literally. Rejecting them
is the entire point of MMark.

Of the 132 examples in the emphasis section of the specification, MMark now
agrees with CommonMark on 64 and rejects 65 as one of those three kinds of
invalid input. The remaining three contain inline HTML, which MMark does not
support at all.

### Code spans are verbatim now

MMark used to normalize the contents of a code span by collapsing every run
of white space into a single space and trimming both ends. It now follows
CommonMark: only line endings become spaces, and a single space is removed
from each end when the contents both begin and end with a space without
being made of spaces alone.

So white space inside a code span survives:

```
`col1  col2`   ->  <code>col1  col2</code>
`a<tab>b`      ->  <code>a<tab>b</code>
`  `           ->  <code>  </code>
` a`           ->  <code> a</code>
```

This fixed nine specification examples at once. It is also the right
behaviour on principle: a code span is the one place a writer expects the
exact spelling of the input to survive.

### Symbols count as punctuation

CommonMark 0.31 widened “punctuation” to include the Unicode symbol
categories. MMark used `Char.isPunctuation`, which excludes `$`, `+`, `=`,
currency and mathematical symbols. It now counts them, which means `*$*alpha`
is no longer read as emphasized `$`.

### Four bugs

Along the way, four real bugs turned up, each found by a specification
example or a probe rather than by reading code:

* The info string of a fenced code block rejected backticks even when the
  fence was made of tildes. Only a backtick fence can be confused with a
  backtick in its info string, so ` ~~~ aa ``` ~~~ ` was a parse error when
  it should open a code block.
* Code spans stripped a non-breaking space at the end but not in the middle,
  because two different notions of white space were used in the same
  function.
* An unclosed code fence whose last line lacked a line ending was reported
  as “expecting newline” rather than “expecting closing code fence”.
* Extension errors were not sorted by offset, which made megaparsec show the
  wrong source line for an error preceding the one before it. More on that
  below.

Overall, MMark now agrees with CommonMark 0.31.2 on 256 of its 652 examples,
up from 229, and the test suite covers 588 of them. Most of the remainder is
deliberate: setext headings, the list model, HTML, and the strictness
described above.

## The extension system

The old extension system was a monoid of four `Endo`-like functions
accumulated in the document and applied just before rendering. It had three
problems. Extensions could not fail, because `Endo Bni` has no error
channel. They could not perform effects, which people had asked for in
[three separate pull
requests](https://github.com/mmark-md/mmark/pull/117). And nothing carried
source positions, so even if an extension could report a problem it had
nowhere to point.

All three are fixed, and the system now has two clearly separate halves.

### Transformations

A transformation rewrites the document. It is applied right away rather than
being stored, it runs in a monad so it can perform effects, and it can
report errors:

```haskell
runTrans  :: (Bni -> Trans Bni) -> MMark -> Either (ParseErrorBundle Text TransError) MMark
runTransM :: Monad m => (Bni -> TransT m Bni) -> MMark -> m (Either (ParseErrorBundle Text TransError) MMark)
```

Several transformations compose with `>=>` into one, which is then applied
in a single pass, and the errors all of them reported come back together.

The transformation that reaches the blocks and inlines nested inside a
top-level block is your choice, and both directions are available:
`bottomUpBlocks`, `topDownBlocks`, `bottomUpInlines`, `topDownInlines`.

A check that concerns the document as a whole is run once instead:

```haskell
runCheck  :: Trans a -> MMark -> Either (ParseErrorBundle Text TransError) a
runCheckM :: Monad m => TransT m a -> MMark -> m (Either (ParseErrorBundle Text TransError) a)
```

Everything transformation-related lives in `Text.MMark.Trans`.

### Renders

A render replaces or augments the way an element becomes HTML. Renders
cannot be applied ahead of time, because a render needs the rendering
function it is wrapping, so they are collected in a `RenderExtension` and
handed to `render`:

```haskell
render :: RenderExtension -> MMark -> Html ()
```

A render cannot fail. By the time a document is rendered it has already been
transformed and validated, and a render that gave up half way would leave a
truncated document behind. Anything that can fail belongs in a
transformation. Everything render-related lives in `Text.MMark.Render`, and
the old `Text.MMark.Extension` module is gone.

### Source spans

Every block and inline now carries the span of the source it came from, and
this is what makes error reporting worth having:

```haskell
data Span = Span {spanStart :: !Int, spanEnd :: !Int}

blockSpan  :: Block a -> Span
inlineSpan :: Inline -> Span
```

A span is the region of source a node *derives from*, not necessarily the
one it was parsed from. A node an extension creates in place of another
inherits its span, and a node assembled from several others should be given
the `spanUnion` of theirs. This way a node in a transformed document can
still say which part of the input it came from.

### Errors that look like parse errors

The errors a transformation reports are collected in a
`ParseErrorBundle Text TransError` — the very same type the parser produces.
So `errorBundlePretty` renders them against the source of the document
exactly as it renders parse errors. Here is an extension that checks links
over the network and reports the ones that are not `https`:

```haskell
checkLinks :: Inline -> TransT IO Inline
checkLinks i = case i of
  Link ann _ uri _ -> do
    ok <- liftIO (visit uri)
    unless ok $
      report ann ("link is not https: " <> URI.render uri)
    return i
  other -> return other
```

And here is what a document with two such links produces:

```
1:5:
  |
1 | See [one](http://a.com) and [two](http://b.com),
  |     ^
link is not https: http://a.com

1:29:
  |
1 | See [one](http://a.com) and [two](http://b.com),
  |                             ^
link is not https: http://b.com
```

Both errors, each under its own link. `report` accumulates and carries on,
so a document with several problems names them all at once; `abort` gives up
on the document but keeps the errors reported before it. The errors come
back ordered by their position in the document whatever order they were
reported in — getting that wrong is the fourth bug I mentioned above, and
the symptom was megaparsec quietly showing the wrong source line.

## mmark-ext

The companion package was updated and given a fair amount of new material.

### Removals

`fontAwesome` produced Font Awesome 4 markup, a syntax Font Awesome has not
used since version 5, so the icons it emitted rendered as nothing at all.
`obfuscateEmail` required the reader to add jQuery and a snippet of
JavaScript, hid the address from readers without JavaScript and from screen
readers, and does not stop a harvester that runs JavaScript. Both are gone.

### Footnotes

The footnotes extension used to admit in its own documentation that it could
not check that a reference refers to an existing footnote, that footnotes
have corresponding references, or that they appear in the right order. That
is now exactly what it does. `validateFootnotes` reports, at the offending
markup, a reference that leads nowhere, a footnote that nothing refers to, a
footnote referred to more than once — which would give the references the
same HTML id — a reference whose path is not a number, and a second footnote
section.

### New extensions

Three of them are checks, which is where the new machinery pays off:

* `LinkCheck` reports links to a fragment no heading defines, links to a
  local file that is not there, and, with an action you supply, links that
  cannot be reached. The action is yours to write so that the package needs
  no HTTP client of its own.
* `Lint` reports images with no description, an outline that skips a heading
  level, a second level 1 heading, and two headings that MMark gives the
  same id — the last is easy to miss, since ids are derived from heading
  text and two headings with the same words collide silently.
* `SafeUri` refuses the `javascript`, `vbscript`, and `data` schemes. MMark
  parses `[click](javascript:alert(1))` quite happily, because the URI is
  well-formed, so anyone rendering markdown they did not write has a live
  cross-site scripting vector.

And six are content features:

* `Mermaid` renders `mermaid` code blocks, either as `<pre class="mermaid">`
  for the browser or as diagrams rendered ahead of time.
* `ImageDimensions` gives images the `width` and `height` that stop the page
  moving under the reader while they load, measuring PNG, GIF, and JPEG
  files by reading their headers, and gives them `loading="lazy"` and
  `decoding="async"`.
* `Permalinks` gives every heading a link to itself.
* `Emoji` replaces `:shortcode:` with the emoji it names, and reports the
  ones it does not know.
* `LineHighlight` points at the lines of a code block named in its info
  string, as in ` ```haskell {2,4-6} `.
* `Metadata` collects the word count, reading time, title, lead paragraph,
  and first image of a document, for the card that appears when a post is
  shared.

`linkTarget` also learned to add `rel="noopener noreferrer"` to the links it
gives `target="_blank"`, since without it the opened page can reach back
through `window.opener`.

## Migrating

The mechanical part is straightforward. `Text.MMark.Extension` is gone;
import `Text.MMark.Trans` for a transformation or `Text.MMark.Render` for a
render extension. `Extension` is `RenderExtension`. `useExtension` and
`useExtensions` are gone, and so are `blockTrans` and `inlineTrans`; write
the function and give it to `runTrans`. `render` takes the render extensions
as its first argument, so pass `mempty` when there are none. Every
constructor of `Block` and `Inline` takes a `Span` as its first argument.
`runScanner` and `runScannerM` take the document as their second argument
now, so that the whole pipeline reads the same way.

Two things break silently, and they are the ones worth knowing about.

The first is that `Eq` on `Inline` and `Block` now includes spans, so
comparing a parsed node against a literal will simply stop matching, with no
compiler error. This bit me in `mmark-ext`, where the footnotes extension
compared a paragraph's inlines against `Plain "footnotes"`. Compare the
plain text instead.

The second is the block quote change described earlier. Any extension that
matches on a block quote containing particular blocks should be checked
against the markup its users actually write.

## Where this leaves the project

MMark has always been an odd proposition. It sits next to
[`commonmark`](https://hackage.haskell.org/package/commonmark) and
[`pandoc`](https://hackage.haskell.org/package/pandoc), both of which are
better maintained and better at what they optimize for: specification
compliance, output formats, and extensions that add syntax, which MMark's
extensions cannot do at all.

What MMark does that they cannot is refuse. Every other implementation
follows a specification whose design goal is that all input is valid
markdown, so none of them can tell you that you left an emphasis open or
that your table is not the table you meant.

Until now that was a slightly awkward thing to sell. This release makes it a
better story, because the refusing is no longer limited to the parser: an
extension can now check the things a parser cannot know about — that your
internal links resolve, that your images have descriptions, that your
footnotes line up, that your outline does not skip a level — and report them
in your source, with a caret underneath, alongside the parse errors.

That is what I would like MMark to be: not a markdown renderer that happens
to be strict, but a processor that tells you what is wrong with your
document before you publish it.
