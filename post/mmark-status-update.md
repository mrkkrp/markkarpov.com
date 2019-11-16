---
title: MMark status update
desc: A short update about the MMark project.
date:
  published: February 14, 2018
tag: haskell
---

In November 2017 I announced MMark—~~a~~ the strict markdown processor for
writers. I worked on it actively for some time since then and this post is a
little update about the project, which by the way even has its own [GitHub
organization](https://github.com/mmark-md) now.

## Getting mature

The initial release I made in November could not even handle blockquotes and
lists. The current version closely follows the [CommonMark
specification](http://spec.commonmark.org/0.28/) and in addition to that
supports:

* parsing of an optional YAML block
* strikeout using `~~this~~` syntax
* superscript using `^this^` syntax
* subscript using `~this~` syntax
* automatic assignment of ids to headers
* pipe tables (as on GitHub)

I'm getting interactive feedback from MMark every time I write markdown
thanks to a little bit of Emacs Lisp code, which I'll cover later in the
post. As expected, this caused an expansion of the test suite, which
currently has 632 tests. Most of them were adapted from the CommonMark spec,
but many are custom. I think the library has become quite smooth by now and
I finally can recommend it for serious use.

## Extensions

In addition to the built-in functionality, the
[`mmark-ext`](https://hackage.haskell.org/package/mmark-ext) package
provides many useful extensions:

* `Text.MMark.Extension.Comment`—turn paragraphs into comments by starting
  them with a magic prefix
* `Text.MMark.Extension.FontAwesome`—insert FontAwesome icons
* `Text.MMark.Extension.Footnotes`—insert footnotes
* `Text.MMark.Extension.Kbd`—wrap text with `kbd` tags (MMark does not
  support arbitrary inline HTML, so for this sort of thing you need a proper
  extension)
* `Text.MMark.Extension.LinkTarget`—specify `target` attribute of links, so
  you can make your link e.g. open in new tab
* `Text.MMark.Extension.MathJax`—MathJax support
* `Text.MMark.Extension.ObfuscateEmail`—email obfuscation
* `Text.MMark.Extension.PunctuationPrettifier`—the usual goodies for
  typographically correct punctuation
* `Text.MMark.Extension.Skylighting`—highlight your code blocks like with
  Pandoc
* `Text.MMark.Extension.TableOfContents`—generate and insert tables of
  contents

If you look closely at the extensions, you'll find that they do not
introduce any new fancy syntax (in fact the extension mechanism cannot
affect parsing at all), but rather give additional meaning to the existing
markdown syntax/markup structures. I think it's a nicer approach because it
prevents the markup language from growing into a monstrosity like in Pandoc,
but your opinion may be different.

The
[`Text.MMark.Extension`](https://hackage.haskell.org/package/mmark-0.0.5.5/docs/Text-MMark-Extension.html)
module describes the extension system in more detail.

## Performance

I [have compared](https://github.com/mrkkrp/md-bench) speed and memory
consumption of various Haskell markdown libraries by running them on an
identical, big-enough markdown document and by rendering it as HTML:

Library            | Parsing library     | Execution time | Allocated   | Max residency
-------------------|---------------------|---------------:|------------:|-------------:
`cmark-0.5.6`      | Custom C code       |       323.4 μs |     228,440 |         9,608
`mmark-0.0.5.1`    | Megaparsec          |       7.027 ms |  26,180,272 |        37,792
`cheapskate-0.1.1` | Custom Haskell code |       10.76 ms |  44,686,272 |       799,200
`markdown-0.1.16`  | Attoparsec          |       14.13 ms |  69,261,816 |       699,656
`pandoc-2.0.5`     | Parsec              |       37.90 ms | 141,868,840 |     1,471,080

*Results are ordered from fastest to slowest.*

MMark is the only markdown processor in Haskell without a severe space leak
in it. This is mostly because of latest updates in Megaparsec, which were
inspired by a suggestion to move from `Applicative`/`Alternative`-based
combinators to `Monad`/`MonadPlus`-based ones. I should thank [Vladislav
Zavialov](https://int-index.com/) for the suggestion. The new combinators
are available for everyone in
[`parser-combinators`](https://hackage.haskell.org/package/parser-combinators)
(see
[`Control.Monad.Combinators`](https://hackage.haskell.org/package/parser-combinators-0.4.0/docs/Control-Monad-Combinators.html))
go grab them and use for great good.

## Command line application

There is a [command line app](https://github.com/mmark-md/mmark-cli) now:

```
mmark—command line interface to MMark markdown processor

Usage: mmark [-v|--version] [-i|--ifile IFILE] [-o|--ofile OFILE] [-j|--json]
             [-t|--template FILE] [--ext-comment PREFIX] [--ext-font-awesome]
             [--ext-footnotes] [--ext-kbd] [--ext-link-target] [--ext-mathjax]
             [--ext-obfuscate-email CLASS] [--ext-punctuation]
             [--ext-skylighting] [--ext-toc RANGE]
  Command line interface to MMark markdown processor

Available options:
  -h,--help                Show this help text
  -v,--version             Print version of the program
  -i,--ifile IFILE         Read markdown source from this file (otherwise read
                           from stdin)
  -o,--ofile OFILE         Save rendered HTML document to this file (otherwise
                           write to stdout)
  -j,--json                Output parse errors and result in JSON format
  -t,--template FILE       Use the template located at this path
  --ext-comment PREFIX     Remove paragraphs that start with the given prefix
  --ext-font-awesome       Enable support for inserting font awesome icons
  --ext-footnotes          Enable support for footnotes
  --ext-kbd                Enable support for wrapping things in kbd tags
  --ext-link-target        Enable support for specifying link targets
  --ext-mathjax            Enable support for MathJax formulas
  --ext-obfuscate-email CLASS
                           Obfuscate email addresses assigning the specified
                           class
  --ext-punctuation        Enable punctuation prettifier
  --ext-skylighting        Enable syntax highlighting of code snippets with
                           Skylighting
  --ext-toc RANGE          Enable generation of table of contents using the
                           supplied range of headers to include, e.g. "1-6" or
                           "2-4"
```

It can also be used for playing with the markdown processor almost
interactively because by default input is read from standard input and
output is printed to standard output:

```
$ mmark
So *here* we go!
----------------------- Control-D
<p>So <em>here</em> we go!</p>
```

## Flycheck checker for Emacs users

Finally, Emacs users may find the
[`flycheck-mmark`](https://github.com/mmark-md/flycheck-mmark) package
useful. It defines a Flycheck checker which calls `mmark` command line app
and displays parse errors in the buffer you're editing. It's fun and sort of
strange to have your markdown checked in this way.

It's available via MELPA, so you can just [M-x package-install RET
flycheck-mmark](kbd:), but make sure that you have `mmark` on your `PATH`.
Full setup instructions are given in the
[readme](https://github.com/mmark-md/flycheck-mmark/blob/master/README.md).

## Conclusion

This site is fully powered by MMark now. I can't say that I could actually
make an early switch, although I wanted to. I had to wait till MMark becomes
more mature because I used a few Pandoc extensions, such as footnotes, so I
was in a vendor lock of a sort. However, MMark currently is smooth and
powerful enough for me to use it here. It also allowed me to get a more
customized markdown processor for my needs because I could easily add
several special extensions. For example, I can interpolate my contact info
such as email or Twitter account without hardcoding it. Also, previously I
used some JavaScript to add Bootstrap classes to tables and images, now it's
done via MMark extensions statically.

So give MMark a try next time you decide to create a static blog or
something!
