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

The API of extension system is presented in the
[`Text.MMark.Extension`](https://hackage.haskell.org/package/mmark/docs/Text-MMark-Extension.html)
module. When designing the extension system my goals were:

1. Make it powerful, so users can write interesting extensions.
2. Make it efficient, so every type of transformation is only applied once
   and the number of traversals of the syntax tree stays constrant no matter
   how many extensions user chooses to apply and how complex they are.
3. Make it easy to write extensions that are very focused in what they do
   and do not interfere with each other in weird and unexpected ways.

I ruled out allowing users to mess with AST directly pretty quickly because
it would be against the points 2 and 3. Instead, we have four
extension-producing functions. They correspond internally to four functions
that are applied to the parsed document in turn:

* `blockTrans` is applied first, as it's quite general and can change
  block-level structure of document as well as inline-level structure.
* `inlineTrans` is applied to every inline in the document obtained in the
  previous step.
* `inlineRender` is applied to every inline; this function produces HTML
  rendition of the inlines and we also preserve the original inline so
  `blockRender` can look at it (sometimes it is useful, we'll see why
  shortly).
* `blockRender` is applied to every block to obtain HTML rendition of the
  whole document.

`Extension`s are combined using `mappend`, because an `Extension` is a
`Monoid` (and obviously also a `Semigroup`). When one combines different
extensions, extensions of the same kind get fused together into a single
function, this is how we keep number of traversals of syntax tree constant.
This allows for faster processing in the end.

There is also the concept of a scanner. We can make a scanner with the
`scanner` function:

```haskell
-- | Create a 'L.Fold' from an initial state and a folding function.

scanner
  :: a                                   -- ^ Initial state
  -> (a -> Block (NonEmpty Inline) -> a) -- ^ Folding function
  -> L.Fold (Block (NonEmpty Inline)) a  -- ^ Resulting 'L.Fold'
scanner a f = L.Fold f a id
```

Which basically just a wrapper over the `Fold` data constructor from the
[`foldl`](https://hackage.haskell.org/package/foldl) library. We'll see how
this is used to run any number of scans over a `MMark` document in a single
pass in the next section.

Finally, to apply an extension, one can use the `useExtension` function:

```haskell
-- | Apply an 'Extension' to an 'MMark' document. The order in which you
-- apply 'Extension's /does matter/. Extensions you apply first take effect
-- first. The extension system is designed in such a way that in many cases
-- the order doesn't matter, but sometimes the difference is important.

useExtension :: Extension -> MMark -> MMark
```

If you have several extensions to apply, there is `useExtensions`, which is
just a shortcut:

```haskell
-- | Apply several 'Extension's to an 'MMark' document.
--
-- This is a simple shortcut:
--
-- > useExtensions exts = useExtension (mconcat exts)
--
-- As mentioned in the docs for 'useExtension', the order in which you apply
-- extensions matters. Extensions closer to beginning of the list are
-- applied later, i.e. the last extension in the list is applied first.

useExtensions :: [Extension] -> MMark -> MMark
useExtensions exts = useExtension (mconcat exts)
```

This note about the order in which extensions are applied may seem
counter-intuitive, but it's a consequence of the fact that `(<>)` associates
to the right and `mconcat` is a right fold inside. So the situation is
really similar to how functions compose with the `Endo` monoid.

## Let's write some extensions

The previous section has “good-to-know” things but it does not show what
writing a useful MMark extension feels like. Let's correct that, I'm going
to walk through some extensions I've released in the
[`mmark-ext`](https://hackage.haskell.org/package/mmark-ext) package. After
that writing your own MMark extension should be easy.

### Transforming inlines

Let's start with something simple. The simplest extension just transforms
inlines, it's created with `inlineTrans`:

```haskell
-- | Create an extension that performs a transformation on 'Inline'
-- components in entire markdown document.

inlineTrans :: (Inline -> Inline) -> Extension
```

It simply lifts an `Inline`-transforming function into an `Extension`.
No-brainer indeed!

We could use it to write a punctuation-prettifying extension:

```haskell
-- | Prettify punctuation.

punctuationPrettifier :: Extension
punctuationPrettifier = Ext.inlineTrans $ \case
  Plain txt -> Plain
    . T.replace "--"  "–"
    . T.replace "---" "—"
    $ txt
  other -> other
```

We could also change the data constructor, but for this one we just keep
`Plain` things `Plain`. This is a simplified version of what is available in
the `mmark-ext` package (`punctuationPrettifier` there just takes a record
of settings that control which transformations to apply).

### Rendering inlines

To alter rendering of an inline we use the `inlineRender` function:

```haskell
-- | Create an extension that replaces or augments rendering of 'Inline's of
-- markdown document. This works like 'blockRender'.

inlineRender
  :: ((Inline -> Html ()) -> Inline -> Html ())
  --  ^                      ^         ^
  --  |                      |         |
  --  “old” rendering        inline    result of rendering
  --      function           to render
  -> Extension
```

Note an interesting thing: we receive the rendering function “constructed so
far”, so we can preserve the rendering logic previously applied extensions
have created. Let's see how it works by writing an extension that allows to
insert [FontAwesome](http://fontawesome.io) icons:

```haskell
-- | Allow to insert @span@s with font awesome icons using autolinks like
-- this:
--
-- > <fa:user>
--
-- This @user@ identifier is the name of icon you want to insert. You can
-- also control the size of the icon like this:
--
-- > <fa:user/fw> -- fixed width
-- > <fa:user/lg> -- large
-- > <fa:user/2x>
-- > <fa:user/3x>
-- > <fa:user/4x>
-- > <fa:user/5x>
--
-- In general, all path components in this URI that go after the name of
-- icon will be prefixed with @\"fa-\"@ and added as classes, so you can do
-- a lot of fancy stuff, see <http://fontawesome.io/examples/>:
--
-- > <fa:quote-left/3x/pull-left/border>
--
-- See also: <http://fontawesome.io>.

fontAwesome :: Extension
fontAwesome = Ext.inlineRender $ \old inline ->
  case inline of
    l@(Link _ fa _) ->
      if URI.uriScheme fa == URI.mkScheme "fa"
        then case URI.uriPath fa of
               [] -> old l
               xs ->
                 let g x = "fa-" <> URI.unRText x
                 in span_
                    [ (class_ . T.intercalate " ") ("fa" : fmap g xs) ]
                    ""
        else old l
    other -> old other
```

If we did not receive `old` rendering function, we would need to use some
sort of default rendering function, and that would ruin composability,
because we would have updated rendering logic for links with `fa` scheme,
and have “reset” it for everything else at the same time. The function we
apply to `inlineRender` can also be thought of as a transformation of inline
rendering function of the type `Inline -> Html ()`:

```haskell
(Inline -> Html ()) -> (Inline -> Html ())
```

### Transforming blocks and scanning

A super-useful extension is the one for generation of table of contents. For
this we need two things:

* A scanner which collects headers.
* An extension that somehow inserts the headers as a nested list of links
  into the document.

Let's use `scanner` to write a scanner:

```haskell
-- | An opaque type representing table of contents produced by the
-- 'tocScanner' scanner.

newtype Toc = Toc [(Int, NonEmpty Inline)]

-- | The scanner builds table of contents 'Toc' that can then be passed to
-- 'toc' to obtain an extension that renders the table of contents in HTML.
--
-- __Note__: Top level header (level 1) is never added to the table of
-- contents. Open an issue if you think it's not a good behavior.

tocScanner
  :: Int -- ^ Up to which level (inclusive) to collect headers? Values from
         -- 2 to 6 make sense here.
  -> L.Fold Bni Toc
tocScanner cutoff = fmap (Toc . reverse) . Ext.scanner [] $ \xs block ->
  case block of
    Heading2 x -> f 2 x xs
    Heading3 x -> f 3 x xs
    Heading4 x -> f 4 x xs
    Heading5 x -> f 5 x xs
    Heading6 x -> f 6 x xs
    _          -> xs
  where
    f n a as =
      if n > cutoff
        then as
        else (n, a) : as
```

You have probably noticed that the extension system does not allow us to
just add things at the beginning or end of a document, as it's fully focused
on transforming existing blocks and inlines. Not sure if it's a good or bad
thing, but I'd like to control where table of contents is inserted anyway,
so let's just have a convention: the extension will replace a code block
with a given info string:

```haskell
-- | Create an extension that replaces a certain code block with previously
-- constructed table of contents.

toc
  :: Text -- ^ Label of the code block to replace by the table of contents
  -> Toc  -- ^ Previously generated by 'tocScanner'
  -> Extension
toc label (Toc xs) = Ext.blockTrans $ \case
  old@(CodeBlock mlabel _) ->
    case NE.nonEmpty xs of
      Nothing -> old
      Just ns ->
        if mlabel == pure label
          then renderToc ns
          else old
  other -> other

-- | Construct @'Block' ('NonEmpty' 'Inline')@ for a table of contents from
-- given collection of headers. This is a non-public helper.

renderToc :: NonEmpty (Int, NonEmpty Inline) -> Block (NonEmpty Inline)
renderToc = UnorderedList . NE.unfoldr f
  where
    f ((n,x) :| xs) =
      let (sitems, fitems) = span ((> n) . fst) xs
          url = Ext.headerFragment (Ext.headerId x)
      in ( Naked (Link x url Nothing :| [])
           : maybeToList (renderToc <$> NE.nonEmpty sitems)
         , NE.nonEmpty fitems )
```

You don't really need to understand how the code above works in details,
it's just an example of what you can do and how easily.

BTW, here is the signature of `blockTrans` for reference:

```haskell
-- | Create an extension that performs a transformation on 'Block's of
-- markdown document.

blockTrans
  :: (Block (NonEmpty Inline) -> Block (NonEmpty Inline))
  -> Extension
```

### Rendering blocks

Finally, here is the scariest function for creating of extensions:

```haskell
-- | Create an extension that replaces or augments rendering of 'Block's of
-- markdown document. The argument of 'blockRender' will be given the
-- rendering function constructed so far @'Block' ('Ois', 'Html' ()) ->
-- 'Html' ()@ as well as an actual block to render—@'Block' ('Ois', 'Html'
-- ())@. The user can then decide whether to replace\/reuse that function to
-- get the final rendering of the type @'Html' ()@.
--
-- The argument of 'blockRender' can also be thought of as a function that
-- transforms the rendering function constructed so far:
--
-- > (Block (Ois, Html ()) -> Html ()) -> (Block (Ois, Html ()) -> Html ())
--
-- See also: 'Ois' and 'getOis'.

blockRender
  :: ((Block (Ois, Html ()) -> Html ()) -> Block (Ois, Html ()) -> Html ())
  -> Extension
```

OK, the argument of `blockRender` just transforms a function of the type
`Block (Ois, Html ()) -> Html ()`. We get a `Block` where every inline has
been pre-rendered to `Html ()` for us, and its source has been put into the
`Ois` wrapper:

```haskell
-- | A wrapper for “originial inlines”. Source inlines are wrapped in this
-- during rendering of inline components and then it's available to block
-- render, but only for inspection. Altering of 'Ois' is not possible
-- because the user cannot construct a value of the 'Ois' type, she can only
-- inspect it with 'getOis'.

newtype Ois = Ois (NonEmpty Inline)

-- | Project @'NonEmpty' 'Inline'@ from 'Ois'.

getOis :: Ois -> NonEmpty Inline
getOis (Ois inlines) = inlines
```

## Performance and inner workings

Performance-wise, there are three things of interest in MMark:

1. Parsing.
2. Scanning.
3. Rendering.

Scanning is done with the `foldl` library, rendering with help of `lucid`.
So performance of these parts depends on performance of the libraries. I
trust Gabriel Gonzalez and Chris Done, *their stuff should work fine* (OK,
I'm also just a bit lazy).

I saved myself the trouble of benchmarking scanning and rendering, but
parsing is the most complex and probably the slowest part of any markdown
processor, so I had to benchmark it carefully.

Now I should probably say a few words about the inner workings of the
parser. I follow the recommendation from the Common Mark specification that
says that it's better to parse block level structure first and then parse
inlines in every block separately.

The `Block` data type is a functor, so quite nicely I first parse `[Block
Isp]` where `Isp` is this:

```haskell
-- | 'Inline' source pending parsing.

data Isp = Isp SourcePos Text
  deriving (Eq, Ord, Show)
```

Then I use Megaparsec's ability to run parsers with custom starting state
and run inline-level parser on every `Isp` thing getting `[Block (Either
(ParseError Char MMarkErr) (NonEmpty Inline))]`, from which I collect all
parse errors for reporting, or if there is none, I can extract the `[Block
(NonEmpty Inline)]` thing, which is exactly what I want to get. During
rendering I again leverage the fact that `Block` is a functor and can turn
every `Inline` into `(Ois, Html ())` in the same way by just using `fmap`.

An astute reader might notice that this opens the possibility of parallel
parsing on inline level, and indeed I think this is a thing to try out. I
have no idea though if this would make the parser faster as a whole, and if
yes, how much faster. This is a thing to explore in the future.

Block-level mostly grabs input line by line using the newer
[`takeWhileP`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:takeWhileP)
and
[`takeWhile1P`](https://hackage.haskell.org/package/megaparsec/docs/Text-Megaparsec.html#v:takeWhile1P)
primitives Megaparsec 6 provides. They make a huge difference in terms of
speed indeed, so I'm satisfied with this. Inline parsing on the other hand
is not so fast and can be optimized. Right now this is the bottleneck of our
parser, and I have added optimizing inline-level parser to my todo list,
although there are still things with higher priority, which I'll mention in
the next section.

## Future plans

MMark is right now in the “proof-of-concept” state, it does not even support
essential things like blockquotes and lists, so it's not for real-world use
yet (you're welcome to play with it anyway, of course). The missing features
should be easy to add to the base I already have, because it looks like the
base has evolved into something that is well-designed for what I have in
mind. So it's just a matter of time when MMark will be powerful enough to
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
