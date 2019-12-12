---
title: Haskell vs OCaml
desc: This is a comparison between the two languages by someone who has written code professionally in both.
date:
  published: December 6, 2019
  updated: December 11, 2019
tag: haskell
---

This is a comparison between the two languages by someone who has written
code professionally in both. That said, I'm much more familiar with Haskell
than with OCaml, so if you think there is a mistake in the text, let me
know.

## Background and users

Haskell is a niche, rarely used language. The [situation is
changing][why-haskell-is-important], but make no mistake, it is very far
away from the mainstream languages such as Python, JavaScript, or Java. The
users of Haskell are banks, payment systems, companies related to the
crypto-currency business, big corporations, and others. It is usually used
because there are engineers who like Haskell and who have enough influence
to promote it for solving actual business problems. Sometimes there are CEOs
who believe that Haskell will give them an edge (visionaries).

OCaml is a product of the French academic circles, although its use is
expanding and it is now not limited to the academy. It is no coincidence
that French research projects like [Coq][coq] are written in OCaml. Many
companies that focus on OCaml ecosystem and tooling are located in France. I
know of at least [OCamlPRO][ocamlpro] and [Tarides][tarides]. Who is the
consumer of those tooling efforts? The crypto boom gave money to the
crypto-currency [Tezos][tezos] which is being developed by several players
including a French company called [Nomadic Labs][nomadic-labs]. Many people
there come from the French academy. Another big player in the OCaml world is
[Jane Street][jane-street] which is a trading firm in the US. Facebook and
Bloomberg also use OCaml, as well [as a few other
companies][ocaml-companies].

## Libraries and ecosystem

If Haskell is a niche language, then OCaml is a super-niche language. The
OCaml community is much smaller. Where Haskell is doing more-or-less fine
with libraries, OCaml has significantly less to propose.

There are some nice libraries in OCaml, but in many areas the situation is
not perfect. For example, you would expect a modern Unicode-aware string
type in your language of choice. While Haskell has `Text`, OCaml mostly uses
its built-in `string` type, which is simply an array of bytes. There is the
[`uutf`][uutf] library, but it is essentially just byte-by-byte
encoder/decoder. Regardless if its technical merits, the story about Unicode
in OCaml is incomplete.

When discussing the issue with OCaml programmers, I was told to [read how
the library works][uucp-explanation], or to introduce implicit invariants *(I'll
validate all my `string`s on the boundaries of my system),* or to define
Unicode-aware string type on per-project basis. It is important to
understand that all of this is not good enough:

* To use a Unicode-aware string type one should not need to read a wall of
  text, unless you need to hack on lower level or to work on the Unicode
  library itself.

* To use a Unicode-aware string type one should not need to re-define it
  every time.

* The fact that we have a valid Unicode text must be reflected on the type
  level and the same type must be used by most libraries in the ecosystem.
  If I have a value of this type, the string inside must be correct Unicode
  text. It is a basic way to leverage the type system.

* There should be a vocabulary of operations for that Unicode type.

Another annoying fact is that for OCaml there is no place where to lookup
library documentation online. You have to build it locally.

In general, I'd compare the library situation to that of [Common
Lisp][lisp-and-haskell]—individual projects may be high quality, but the
integration between libraries and coverage are lacking.

## Tooling

Haskell's tooling is far from perfect. Most people just use Emacs, Vim, or
another popular text editor. There are a few ways to get on-the-fly error
reporting as you edit your program. My preferred choice is [`ghcid`][ghcid],
which is just a terminal application that I run in a different window. There
are solutions which can make the errors appear in your editor, but `ghcid`
is simple and works for any project with any setup as long as you can start
a REPL, which means that I never have a problem with it. Granted, it is a
bit hardcore for users who are used to normal IDEs. There are two package
managers: Cabal and Stack. Both are usable. The older one, Cabal, improved a
lot in the recent years. One can also build Haskell code with [Nix][nix] or
[Bazel][rules-haskell], and many people do.

For OCaml, you'll also need to use a text editor instead of a specialized
IDE. Emacs with Merlin is probably your best bet. I noticed that Merlin
doesn't always predict correct types for expressions, for example it
frequently confuses `bytes` and `string`. Still it's better than nothing.
The package manager is `opam` and the most popular build system is
[`dune`][dune]. The combo works if you are a hacker and you know how to use
it. Otherwise it can be rough. To get a package set which is known to be
good and is under your control, you'd need to setup your own `opam` server.
You do not need to do it with Haskell. Nix support for OCaml doesn't look as
mature as for Haskell, so I wouldn't use it unless I'm working for a big
enough company that could invest in Nix support for OCaml.

Overall, Haskell is doing better with respect to tooling. With Stack you can
build a project with a single command without knowing much about the
infrastructure. A full-featured IDE is still but a dream for the users of
both languages.

## Purity

Now to the features of the languages themselves. Haskell is pure and lazy,
that's a big game changer. Purity is imposed by the type system and it is
one of the main features that gives Haskell its unique feel. Once you
approach language design this bravely, doing away with the old ways of
writing programs, you get a very different language in the end. I think we
discovered a lot by building our programs around purity.

OCaml is impure and strict. Many people will praise strictness because of
more predictable performance, but I'll save the “lazy vs strict” discussion
for another post. Because OCaml is impure, you can do effects anywhere you
want. One consequence of this is that monads, although not unknown of in the
OCaml world, are less popular and the support for monads is way worse. There
is no `do`-notation and no proven ways to combine monadic layers, athough it
looks like [it's about to change][let-star-notation]. You'll probably also
have to have as many bind operators as you have monads in your program—there
is no ad-hoc polymorphism like in Haskell with type classes.

It is no coincidence that OCaml people are more practical. They won't come
after you with a baseball bat in the middle of the night because you used a
partial function. While searching for the truth is such a big thing in
Haskell where every self-respecting programmer will write his/her own effect
system based on the latest and coolest papers, OCaml folks just stay cool
and write their amazing [unikernel operating systems][mirage-os] which work
just fine.

## Error messages

Haskell is well-known for its long and incomprehensible compile errors. I
can't say that OCaml errors are more readable. I think there are fewer
things that may go wrong, but once they do, it can be very hard to figure
out what is going on. The fact that OCaml has polymorphic variants and so
can “grow” types according to the code you write doesn't help. I've seen
very long, barely readable declarations of the compiler's agony. For
example, if type of a module in your `.ml` file is different from its `.mli`
declaration you'll have to find the differences yourself looking at full
module signatures of both. This all could be presented just a bit nicer to
the user.

## Named and optional arguments

I've come to the conclusion that optional and named arguments that OCaml
provides are not a good idea. Granted, this is just my opinion. Here is why:

* **Named arguments**. There are two problems that named arguments solve in
  OCaml:

  * *When you have several arguments of the same type and you do not want to
    mix them up.* In that case naming the arguments helps, but it is not the
    best fix. The problem is that inside of a function which takes named
    arguments you may need to pass those arguments down to other functions.
    Nothing prevents you from mixing things up here, because all the
    arguments still have the same type. The solution is simple: the types of
    different arguments should be different. This what Haskell achieves
    through `newtype`s. Once you have different types for username and
    password you cannot mix them up and you'll be forced to pass them in the
    right order—no mistake is possible.

    Note that consistent naming with labeled arguments in OCaml makes it
    very unlikely that you'll mix up two arguments of the same type. Yet, it
    is only true as long as you pass the arguments unchanged and follow a
    consistent naming scheme. There is a difference between *correct because
    it is easier this way* and *correct because it is the only way it
    compiles*.

  * *When you have many arguments and you don't want to remember their exact
    order.* The problem here is that you do not really need to pass many
    arguments to a function. Perhaps 5 or 6 is the maximum. If you have more
    (and OCaml programs can easily have 13 or more), think how to organize
    your API better. Maybe you could use a reader monad or something else to
    share context. Maybe you need to group several things in a record which
    represents something in your domain. Or perhaps your function is doing
    too much at the same time and you need to decompose it. Common Lisp also
    has named arguments, and this is why, in part, standard functions in
    Common Lisp [tend to do a lot more][cl-reduce] than in Haskell where
    each function does just one thing.

* **Optional arguments** can be replaced by explicit `Maybe` or `option`
  wrappers. This is a bit verbose, but there is not much to gain from that
  little extra brevity (as pointed out above, we do not want too many
  arguments). On the other, optional arguments do not play nicely with
  currying. The last argument of a function cannot be optional, this is why
  some OCaml functions take a dummy unit `()` as their last argument, which
  is not nice.

## Module system

The OCaml module system is [refreshing and interesting][humped-critter], but
it has its own flaws. For example, we have to endlessly repeat the type
definitions and signatures in `.ml` and `.mli` files. There are some tricks
to avoid duplication but they can be a real pain sometimes. I mention this
because even though I can understand that it is nice to have your API in a
single file, I found the workflow a lot less ergonomic than in Haskell. You
end up in a web of modules and endless repetitions of signatures. Many times
when I changed or moved a function I'd have to read through lengthy module
signatures to find where to perform the adjustments to make it compile
again. You do it by looking at the entire signature of a module—often
several hundreds of lines.

One could say that `.mli` files can be generated automatically (which I do
not recommend, it's just a possible objection), but then why make it part of
the source code that is supposed to be written by the programmer?

## Conclusion

It might appear that in my opinion Haskell is better at everything, but it
is not true. The main advantage of OCaml is that its compiler is way simpler
and produces code with more predictable performance. Some of the
consequences are:

* It is easier to port it to new platforms. OCaml can compile to portable
  byte code and to machine code as well as to JavaScript. The story with
  running OCaml in browser is way better that for Haskell.

* OCaml is a better fit for things like unikernel operating systems, see
  [Mirage OS][mirage-os].

Haskell is an alien language, or the most widely used language of the truly
weird ones. It is ahead of its time. OCaml feels somewhat more conventional,
like something caught in the middle of the mutation from conventional
imperative language to something Haskell-like. In the end, both languages
are great and have rather unique and interesting features.

[why-haskell-is-important]: https://www.tweag.io/posts/2019-09-06-why-haskell-is-important.html
[coq]: https://coq.inria.fr
[ocamlpro]: http://www.ocamlpro.com
[tarides]: https://tarides.com
[tezos]: https://tezos.com
[nomadic-labs]: https://www.nomadic-labs.com
[jane-street]: https://www.janestreet.com
[ocaml-companies]: https://ocaml.org/learn/companies.html
[uutf]: https://erratique.ch/software/uutf/doc/Uutf
[uucp-explanation]: https://erratique.ch/software/uucp/doc/Uucp.html#uminimal
[lisp-and-haskell]: /post/lisp-and-haskell.html
[ghcid]: https://hackage.haskell.org/package/ghcid
[nix]: https://nixos.org
[rules-haskell]: https://github.com/tweag/rules_haskell
[dune]: https://dune.build
[let-star-notation]: https://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html
[mirage-os]: https://mirage.io
[cl-reduce]: http://www.lispworks.com/documentation/HyperSpec/Body/f_reduce.htm#reduce
[humped-critter]: /post/what-does-a-humped-critter-have-to-teach-us.html
