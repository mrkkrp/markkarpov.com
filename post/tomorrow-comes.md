---
title: Tomorrow comes
desc: A few announcements for the Haskell community.
date:
  published: August 5, 2026
tag: haskell
---

It so happens that in my case leaving a company where I spent the last 8
years sent me down memory lane. Among the effects of that little trip was a
bit of nostalgia that made me remember particularly vividly the period
roughly between 2016 and 2020, which I consider both very interesting and
fruitful for the Haskell community and ecosystem in general, and for me
personally. In retrospect, I must admit that this period was the peak of my
Haskell-related activity. Later, my professional focus shifted to scalable
build systems, Bazel in particular, while my personal interests moved to
philosophy and painting in an attempt to attack questions that simply cannot
be answered through software development.

Now, it's been a while! Time sure flies. With a little bit of spare time on
my hands, I decided to do a few useful things rather than simply remember
and reminisce. This post presents them. It is a kind of public announcement,
really.

## My tutorials

One thing I am proud of is [my Haskell tutorials][tutorials]. It is perhaps
immodest of me, but re-reading them now I realize just how much work went
into them, all well before AI. Back in the day if you wanted to write a good
tutorial you'd read papers, PhD theses, and important blog posts (all of
which you first needed to discover). You would do a lot of reading before
you could write a single line of prose. The time investment there was quite
substantial.

I hope that these tutorials can still be useful even in our day and age, and
that understanding the subtleties of Haskell is still worthwhile. Acting on
that belief, I have done three things to each of them:

1. Fixed grammatical errors. Sadly, there were a few.
2. Improved the flow and made the English prose tighter.
3. Most importantly, updated all substance in order to align it with the
   current state of the ecosystem, as of GHC 9.14. In some cases, that meant
   writing entirely new sections (for example, on backtraces in the tutorial
   on exceptions).

I have also updated the list of recommended tutorials found on the same
page. My goal there was to construct a reasonable, objective, and up-to-date
selection of nice writing on Haskell. I will leave it to you to judge
whether I have achieved that goal.

## Ormolu

I created Ormolu in 2019 and have shaped its design ever since. My work on
it continues at [mrkkrp/ormolu][mrkkrp-ormolu], while an up-to-date Ormolu
Live is now available at [https://ormolu-live.markkarpov.com][ormolu-live].
[Ormolu 0.8.2.0][ormolu-0820] is the first release made from the new repo
and it is already on Hackage, where I continue to maintain the `ormolu`
package.

To avoid confusion: the [tweag/ormolu][tweag-ormolu] repository continues
separately. If you get Ormolu through Hackage, Stackage, or the usual
Haskell tooling, you do not need to do anything—the releases you receive are
the ones I make.

I remain personally committed to maintaining Ormolu. Even though other
things took a fair share of my mental space in the last 6 years or so, I
have not abandoned a single practically useful Haskell package I have ever
published—I assume a kind of responsibility over them. Come to think of it,
Megaparsec is 11 years old at this point—it is a granddaddy of a package
now! Some of my packages are more fun to maintain than others, but my idea
of open source software development has always been about providing value
and reliability rather than simple enjoyment of the process.

## Next steps

I intend to resume posting here from now on. I will most certainly write
about Bazel and about some open source work around it that I am doing right
now. On the Haskell side of things, I am pleased with the state of most of
my packages, and I probably won't create new ones for now.

While working on this website's code I did notice that one of my old
projects—[MMark][mmark]—is actually not bad. It still powers this static
website, and there are quite a few custom extensions that I wrote to adapt
markdown to my needs. Using it, I get the feeling that it is built around a
good idea. Unlike my other projects, though, it never received enough
attention after an initial burst of development around 2018—other things got
in the way. I am curious where I can take it now, at my own pace. My plan is
to switch to a quote syntax that matches [CommonMark][commonmark] and
generally see how close to CommonMark I can get while still preserving what
makes MMark itself: a strict processor where ambiguous input is an error
rather than a guess. The idea here is to make MMark a little less surprising
for people who are already familiar with markdown.

This is all I wanted to share for now. Enjoy summer, and Happy Hacking!

[tutorials]: /learn-haskell.html "_blank"
[mrkkrp-ormolu]: https://github.com/mrkkrp/ormolu "_blank"
[ormolu-live]: https://ormolu-live.markkarpov.com "_blank"
[tweag-ormolu]: https://github.com/tweag/ormolu "_blank"
[ormolu-0820]: https://hackage.haskell.org/package/ormolu-0.8.2.0 "_blank"
[mmark]: https://github.com/mmark-md/mmark "_blank"
[commonmark]: https://commonmark.org/ "_blank"
