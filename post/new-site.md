---
title: New site
desc: I've set up a new site where I'll host my blog, tutorials, and more.
date:
  published: June 6, 2017
---

Finally I have set up my new web site here, at <https://markkarpov.com>. I
plan to post more often than before and also I have the plans to publish new
tutorials and probably other sort of content as well, such as screencasts.

So far I have migrated my old posts (previously located at
<https://mrkkrp.github.io>), [collected links to all my
tutorials](/learn-haskell.html), and made an [overview of my active open
source projects](/oss.html).

You may certainly expect some posts related to optimization of my existing
projects, such as [`text-metrics`](https://github.com/mrkkrp/text-metrics),
which I'm migrating to pure Haskell right now. Also, in case you do not know
yet, Megaparsec 6 is coming this summer, so there will be posts about the
related development as well.

Megaparsec tutorials will be probably moved to this site soon, so I can
manage all my tutorials through one system. After all, I'm still the sole
contributor to Megaparsec's official tutorials, so it makes sense to move
the tutorials to my site. Since the tutorials is the only useful content on
Megaparsec's site, it will be shut down after the migration and the tutorial
links will redirect to new locations on this site.

In case you're interested how this site is built, I'll tell you ;-) This
time I used a combo of `shake`, `stache`, and `pandoc` instead of Hakyll.
I'm quite satisfied so far, because I managed to build a more custom build
system where I control most things via combination of YAML files (and YAML
snippets embedded in Markdown) and Stache templates, passing the `Value`s
directly from one layer to another. This way I can edit YAML and templates
and control almost everything without touching the build system written in
Haskell. I did not make the repo public because it contains some sensitive
data such as Ansible config, which I would like to keep secure.
