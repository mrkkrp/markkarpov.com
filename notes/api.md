---
title: Designing APIs
desc: Some notes about my approach to designing APIs.
date:
  published: June 29, 2019
---

*(These notes is a work in progress.)*

```toc
```

Most software developers have heard of and used APIs—[application
programming interfaces][api]. As it happens with many terms and notions that
we use daily, we tend to think implicitly that we understand them, without
ever trying to get to the bottom of the matter.

If someone asked you:

* What is API?
* What is the difference between good and bad API?
* How to write a good one?

You would probably have a difficult time answering the two last questions in
a clear and precise way. Some people seem to have developed intuitions which
allow them to design good APIs, others are completely lost and no matter how
hard they try the result is terrifying.

These notes try to document my understanding of what is good API and how to
design it.

## What's the job of an API?

If we want to pin down what makes an API “good”, we need to understand its
purpose. First of all, *application programming interface* is an interface
consumed by *humans*, because programmers are humans just like all others.
In that sense it's not so different from UI of a microwave, website, or
application.

Let's consider what user of API wants to accomplish. Given a problem `$P$`
she needs to express a solution `$S$` in terms of API. Let's write it down
this way:

```mathjax
P \rightarrow S
```

However, unless the user is the same person who designed API, there will
always be uncertainly and confusion `$C$` associated with the solution:

* Is this the best solution to my problem?
* Do I understand exactly what it does?

Se we can update the formulae above like this:

```mathjax
P \rightarrow S \times C
```

It makes sense to try to minimize `$C$` so that for any problem user may
have, she gets a solution she is confident in. Moreover, it's usually the
case that `$C$` should be sufficiently low for `$S$` to be considered
satisfactory. Lowering of `$C$` usually is achieved through spending time
`$t$` and doing investigation:

```mathjax
P \times t \rightarrow S \times C
```

A good API is characterized by the fact that **users are able to find
solution with sufficiently low confusion spending as little time as
possible** investigating.

Now, there is also the inverse operation which takes place when other person
(or indeed the original author a few months later) tries to understand what
problem `$P$` a piece of code `$S$` solves:

```mathjax
S \times t \rightarrow P \times C
```

In this case, similarly, with a good API **users are able to understand what
a piece of code does with sufficiently low cofusion spending as little time
as possible** investigating.

Thus, API can be seen as a medium for expressing problems (or ideas) `$P$`
in the language of `$S$`. The better the medium, the faster confusion `$C$`
gets lower as we increase the time `$t$` we're willing to spend on
conversions back and forth. This definition makes sense because time `$t$`
is directly linked to productivity and in the end to output of the business.

Of course there are more variables in the equation, but let's focus on those
we've introduced so far.

## Principles behind good API

The rest of the article is going to talk about principles that can be used
to minimize confusion and so also the time our users will spend using our
API.

One should not be fanatical about any of the principles in particular,
instead we should aim for a careful balance.

### Minimality

When a user opens documentation for an API, she sees a lot of information.
For every piece of information, the user is going to ask the simple
questions:

* How this is related to the problem I'm trying to solve?
* How can it be used to solve my problem?

Naturally, if the answer is “I don't know”, the confusion will grow. The
answer itself depends on a few things, but for now we talk just about the
quantities. Generally, the more objects a user has to deal with, the more
confused she will be.

At the same time there is only so much information people can handle at a
time. Everything “extra” will fall into a sort of *unknown zone*.

How do *unknown zones* contribute to confusion? It is not hard to see, if we
consider what a user might think of them:

* There may be a (better) solution to my problem in the unknown zone.
* Without the knowledge from unknown zone the current understanding of my
  solution may be incorrect.

Size of unknown zone will vary over time and also depends on the user,
however it's generally a good idea to try to make it as small as possible
even for people who just opened your docs for the first time.

Another reason to try to keep APIs minimal is that the smaller public
interface is, the easier it is to maintain and change it without introducing
breaking changes. Obviously, the less stuff you have, the fewer problems
you'll have with it.

From a physiological point of view it also makes sense to try to hide
internal modules and everything that is not of interest for users from
documentation. One should not underestimate the confidence a user may get
from this simple thought:

> I have seen everything there is to this system. There is nothing else, no
  unknown zone. Now I just need to put the known parts together.

**Thus one should question very thoroughly presence of each and every public
element in an API. Question it as if your life depends on it.**

If we took the principle of minimality to the limit, we'd have an API which
has only one function that does exactly what the user wants. Unless all
users always want the same one thing, it's not feasible. The truth is, in
most cases there will be more functions than your user can handle right
away, and there will be unknown zone for some time. What to do?

### Structure

Structuring API serves the purpose of alleviating confusion caused by
unknown zones. Simply put, if I have problem `$P$`, looking at the structure
of API I should be pretty confident the solution will be in section A, and
not in section B or C because their names and descriptions convey very
clearly what they are about, and it's something completely unrelated to
`$P$`.

It makes sense to use structuring to combat unknown zones and within a unit
of structure, such as module, try to follow the principle of minimality.

### Confluence

The idea here is that all users should end up using the same solution `$S$`
for given problem `$P$`. This makes the relation between `$S$` and `$P$`
injective, or simply put, the principle aims to make sure that there is
one-to-one relation between them. This reduces confusion and ambiguity
significantly and also makes sure that two different developers will be able
to figure out `$P$` quickly if they see `$S$` and vice versa.

In practice, in most cases, there won't be one-to-one relation, but we could
try to get closer to that. For example, imagine an API has the function
`foo`. Then you might introduce `foo'` which is just like `foo` except it
behaves a bit differently in a certain case. It could easily get worse and
you might end up adding `foo''`, etc. This is against the principle of
confluence because there is now a wide range of situations that can be
solved by using either `foo`, `foo'`, or `foo''` so that particular choice
of function doesn't make any difference. If so, there will be code which
uses `foo` and the same time there will be code which uses `foo'` or `foo''`
exactly for the same thing. Ideally, there could be also a comment
explaining the choice, but more often than not, you should not count on
that. (Perhaps the code was thoughtlessly copied from somewhere.) As you can
see, inferring why exactly a particular function was used and whether or not
it's safe to replace it by something else just got significantly harder.

Another way to go against the principle of confluence is to make your API
too general for things that your users will want to do. Imagine a
(different) higher-order function `foo` which takes a function as argument.
There is a limited set of functions to plug in `foo`.

Now, given the opportunity, your users will pass all sorts of functions to
`foo` apart from those that you expect them to use for reasons you would not
ever be able to imagine. Again, this makes figuring out what is going on
harder.

It should be kept in mind that confluence taken to the limit defeats
flexibility and composability of APIs.

### Convention

Confusion will be reduced if users see elements and patterns they are
already familiar with.

Unlike other principles here this one can safely be taken to the
limits—there is nothing wrong with API which is conventional in its
entirety. It may still do a good job. However, if there is something that
works well but is original and non-standard, there is nothing wrong with
using that.

### Avoiding leakage

A basic rule in software development. User of a module A should not care or
know how it's implemented in 95% of cases (sometimes access to lower-level
details in necessary).

### Documentation

Good documentation is a sign of maturity in software development.
Documentation is a very efficient means to reduce confusion and so improve
APIs as per the principles we described in the beginning.

Let's see how documentation can augment effects achieved through the
principles we already know:

* **Minimality**: if you have few public objects to describe, chances are,
  you'll be able to spend more time describing each of them and making sure
  it's clear what every element in API does. Otherwise you may end up with
  superficial docs for great number of elements.

* **Structure**: documentation can help to link different components
  together and create meaningful corrections between them, help users go
  from one module to another.

* **Confluence** can be improved by suggesting “canonical” solutions to many
  problems and establishing a sort of cookbook.

* **Convention**: documentation can draw parallels with commonly used
  practices.

* **Avoiding leakage**: documentation should not talk about inner working of
  systems. This information can quickly become obsolete and shouldn't be of
  interest if the module itself is not a leaking abstraction.

## Conclusion

The principles here apply not only to high-level API you expose to your
users. You and your colleagues also deserve good lower-level API to build
upon. This means that all the principles here can and should be applied to
every layer of a system. Code is for humans, and humans like good APIs that
don't confuse them too much.

[haskell]: https://www.haskell.org/
[api]: https://en.wikipedia.org/wiki/Application_programming_interface
