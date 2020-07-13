---
title: GitHub actions for Haskell CI
desc: The post explains how I use GitHub actions to setup Haskell CI.
date:
  published: July 15, 2020
tag: devops
---

```toc
```

Like many others, I was using [Travis][travis-ci] and [Circle CI][circle-ci]
for years to setup CI for my open source projects. But the times they are
a-changin'. Recently, [GitHub actions][github-actions] came along as a
viable solution with some benefits:

* **Better integration with GitHub.** Being part of GitHub, GitHub actions
  do not require going to a different site and messing with webhooks.
  Granted, it was not hard previously, but there is a difference between
  just pushing a commit and doing the same plus some manual fiddling and
  clicking through menus. With GitHub actions the setup is 100% declarative
  and ready to go right away.

* **GitHub actions are snappier.** I feel like this is the case. Even when I
  use it with NixOS docker containers the image happens to be ready faster
  than with Circle CI. I have not collected any real statistics to support
  this claim though.

* **GitHub actions come with Haskell support.** Haskell is not Ruby, and for
  me it is a big deal to see that someone up there has thought of us. I
  stayed with Travis for a long time mainly because of [HVR's packages for
  Haskell CI][hvr-haskell-ci], but now something as nice is available with
  GitHub actions.

When I saw [the helpful post][helpful-post] by Dmitrii Kovanikov a few
months ago, I felt convinced that GitHub action-based CI is the future. But
it wasn't until recently that I got the time and the energy to try and
convert all my projects to the new setup. In this post I'm going to explain
the script that I ended up using and why it is the way it is.

## Events

GitHub actions are triggered by [events][events]. For CI I found that `push`
and `pull_request` events are useful.

* `push` alone would suffice if not for pull requests from external
  contributors.
* `pull_request` alone would also suffice if we did not need to run CI on
  the `master` branch. Also, it's worth remembering that there are a bunch
  of actions that belong to the `pull_request` event. For example, the
  action of adding a label to a PR is a `pull_request` event. Because of
  that, to avoid running your CI script unnecessarily often one needs to
  apply filtering by action types.

Given these considerations we arrive at something like this:

```yaml
name: CI
on:
  push:
    branches:
      - master
  pull_request:
    types:
      - opened
      - synchronize
```

Assume this snippet at the top of every example that you will find in this
post. Since it never changes, I will be omitting it for brevity.

## Cabal and Hackage

I usually use Stack/Stackage for development, but Cabal/Hackage is my
preferred choice for CI for open source Haskell libraries. I won't deny that
historically, the predilection towards Cabal has been influenced by those
enticing HVR packages that I have already mentioned, but there is more to
preferring Hackage than this.

Perhaps somewhat contrary to the best practices we often hear about (and
which I do generally support), I like my Hackage snapshot to move. In other
words, I find it helpful to monitor health of my libraries as new versions
of dependencies get released. In practice it means that I run `cabal update`
at the beginning of my CI script to bring the local snapshot of Hackage up
to date on every run.

*If it were about CI for a client project, my choices would be different
depending on the project. Most certainly there would be nothing in my setup
that could change without me knowing about it. But this post is not about
that type of CI setup, it is primarily about setup for open source
libraries.*

Finally, let's face it: without automatic updates of Hackage snapshot, for
most projects I would have never remembered to update it manually. I'd be
running my builds and tests against increasingly old versions of
dependencies.

## The build matrix

We are about to write the script for our job. Like Travis and unlike Circle
CI GitHub actions support the concept of build matrix. Here is how we can
use it:

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        cabal: ["3.2"]
        ghc: ["8.6.5", "8.8.3", "8.10.1"]
    env:
      CONFIG: "--enable-tests --enable-benchmarks"
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-haskell@v1.1.2
        with:
          ghc-version: ${{ matrix.ghc }}
          cabal-version: ${{ matrix.cabal }}
      - run: cabal v2-update $CONFIG
      - run: cabal v2-build $CONFIG
      - run: cabal v2-test $CONFIG
      - run: cabal v2-haddock $CONFIG
      - run: cabal v2-sdist
```

* The script runs against three latest GHC versions, which is generally all
  I want to support for my open source projects.
* The update/build/test/haddock/sdist sequence comprises the functionality
  that I think should work for every library. I use the commands with the
  `v2-` prefix because these aliases are said to be forwards compatible.
* The `CONFIG` environment variable is there because I want to build
  benchmarks and tests in the build step even if I won't be running them.
  The `$CONFIG` part after every command is there to ensure that the
  combination of flags stays the same. Otherwise Cabal may decide to
  recompile things between steps.

## Cache

The right cache setup was not immediately obvious to me. Neither Dmitrii
Kovanikov's post nor [the official example for
Haskell][official-haskell-example] were suitable. I wanted my cache to
contain the most recent version of dependencies and as `cabal update`
gradually changes it, the cache should also be gradually updated.

One detail is useful to keep in mind with respect to GitHub's `cache`
action: there is one-to-one correspondence between the cache key and the
cache contents. In particular, this means that if there is a cache hit the
new cache contents won't be uploaded. This means that the cache key cannot
be “sloppy”—every dependency should be reflected in it. Otherwise we risk
living with increasingly old cashes while still getting cache hits.

The official `cache` example for Haskell is an example of such setup where
cache is never updated:

```yaml
- name: Cache ~/.cabal/packages, ~/.cabal/store and dist-newstyle
  uses: actions/cache@v2
  with:
    path: |
      ~/.cabal/packages
      ~/.cabal/store
      dist-newstyle
    key: ${{ runner.os }}-${{ matrix.ghc }}
```

And this is not bad; this may as well be the way you want your cache to
work. Looking at examples for the other languages though, we see that they
are written with cache changes in mind. Here is one for Python:

```yaml
- uses: actions/cache@v2
  with:
    path: ~/.cache/pip
    key: ${{ runner.os }}-pip-${{ hashFiles('**/requirements.txt') }}
    restore-keys: |
      ${{ runner.os }}-pip-
```

As `requirements.txt` changes the cache key also changes. Exactly what we
want! If we use `cabal freeze` like Dmitrii Kovanikov does in his example
we'll get te `cabal.project.freeze` file which is something akin to
`requirements.txt`. We can hash that. However, Dmitrii's example lacks the
last and very important component which you can see in the Python
example—the `restore-keys` parameter. This is the pattern that is used when
cache hit for the primary key (the `key` pramater) does not happen. With
`restore-keys` we can be sure that the most recent cache will still be used.

Another question is *what to cache*? With Cabal I recommend caching three
directories:

* `~/.cabal/packages`
* `~/.cabal/store`
* `dist-newstyle`

This gives us the following job definition:

```yaml
steps:
  - uses: actions/checkout@v2
  - uses: actions/setup-haskell@v1.1.2
    with:
      ghc-version: ${{ matrix.ghc }}
      cabal-version: ${{ matrix.cabal }}
  - run: cabal v2-update
  - run: cabal v2-freeze $CONFIG
  - uses: actions/cache@v2
    with:
      path: |
        ~/.cabal/packages
        ~/.cabal/store
        dist-newstyle
      key: ${{ runner.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project.freeze') }}
      restore-keys: |
        ${{ runner.os }}-${{ matrix.ghc }}-
  - run: cabal v2-build $CONFIG
  - run: cabal v2-test $CONFIG
  - run: cabal v2-haddock $CONFIG
  - run: cabal v2-sdist
```

## Windows

While we should be thankful that [AppVeyor][appveyor] exists, I have never
been fan of it. All the bigger was my surprise when I discovered that with
GitHub actions I can run exactly the same CI script on Windows by changing a
single line:

```diff
- runs-on: ubuntu-latest
+ runs-on: windows-latest
```

There is just one little detail: the cache won't work. Obviously, the
directories on Windows are named differently.

* `~/.cabal/packages` → `C:\Users\runneradmin\AppData\Roaming\cabal\packages`
* `~/.cabal/store` → `C:\sr`
* `dist-newstyle` stays the same

The directory of Cabal store should have a short name because of the limit
on directory name length on Windows. Better yet, we do not really need to
guess the name of the directory that holds the store. The `setup-haskell`
action provides it as an output. All we have to do is to give the
`setup-haskell` action name and then reference the output in the `cache`
action:

```yaml
build-windows:
  runs-on: windows-latest
  strategy:
    matrix:
      cabal: ["3.2"]
      ghc: ["8.6.5", "8.8.3", "8.10.1"]
  env:
    CONFIG: "--enable-tests --enable-benchmarks"
  steps:
    - uses: actions/checkout@v2
    - uses: actions/setup-haskell@v1.1.2
      id: setup-haskell-cabal # <--- give it a name
      with:
        ghc-version: ${{ matrix.ghc }}
        cabal-version: ${{ matrix.cabal }}
    - run: cabal v2-update
    - run: cabal v2-freeze $CONFIG
    - uses: actions/cache@v2
      with:
        path: |
          C:\\Users\\runneradmin\\AppData\\Roaming\\cabal\\packages
          ${{ steps.setup-haskell-cabal.outputs.cabal-store }}
          dist-newstyle
        key: ${{ runner.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project.freeze') }}
        restore-keys: |
          ${{ runner.os }}-${{ matrix.ghc }}-
    - run: cabal v2-build $CONFIG
    - run: cabal v2-test $CONFIG
    - run: cabal v2-haddock $CONFIG
    - run: cabal v2-sdist
```

## Complete example

For reference, here is the complete example of typical Haskell CI setup that
I use (Linux-only version):

```haskell
name: CI
on:
  push:
    branches:
      - master
  pull_request:
    types:
      - opened
      - synchronize
jobs:
  build:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        cabal: ["3.2"]
        ghc: ["8.6.5", "8.8.3", "8.10.1"]
    env:
      CONFIG: "--enable-tests --enable-benchmarks"
    steps:
      - uses: actions/checkout@v2
      - uses: actions/setup-haskell@v1.1.2
        with:
          ghc-version: ${{ matrix.ghc }}
          cabal-version: ${{ matrix.cabal }}
      - run: cabal v2-update
      - run: cabal v2-freeze $CONFIG
      - uses: actions/cache@v2
        with:
          path: |
            ~/.cabal/packages
            ${{ steps.setup-haskell-cabal.outputs.cabal-store }}
            dist-newstyle
          key: ${{ runner.os }}-${{ matrix.ghc }}-${{ hashFiles('cabal.project.freeze') }}
          restore-keys: |
            ${{ runner.os }}-${{ matrix.ghc }}-
      - run: cabal v2-build $CONFIG
      - run: cabal v2-test $CONFIG
      - run: cabal v2-haddock $CONFIG
      - run: cabal v2-sdist
```

[travis-ci]: https://travis-ci.org/
[circle-ci]: https://circleci.com/
[github-actions]: https://github.com/features/actions
[hvr-haskell-ci]: https://github.com/haskell-CI/haskell-ci
[helpful-post]: https://kodimensional.dev/github-actions
[events]: https://docs.github.com/en/actions/reference/events-that-trigger-workflows
[official-haskell-example]: https://github.com/actions/cache/blob/main/examples.md#haskell---cabal
[appveyor]: https://www.appveyor.com/
