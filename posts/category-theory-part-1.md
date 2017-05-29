---
title: Category Theory Basics, Part I
author: Mark Karpov
description: Category theory notions in simple to understand (I hope), but concise form, part I.
published: September 18, 2016
---

When you do Haskell on daily basis (simply put, earn a living writing
Haskell code), sooner or later you start to regret you don't have background
in advanced math (unless you have such background, of course, and in that
case the post will be probably uninteresting for you). I think it's a
situation in which people who use Haskell as engineers (not researchers)
find themselves at some point.

The most important and relevant math area for a Haskeller is probably
category theory, which *does look scary* at first and Wikipedia
articles/videos of lectures seem to be only partially understandable, always
leaving you with a bunch of new notions unexplained. There is no problem to
understand what `Monad` means and how it's useful in functional programming
(although the abstraction may take some time to sink in), but the feeling of
missing “bigger picture”, full of wonderful ideas may not leave you once you
started to use abstractions from category theory.

So, having intuitive understanding of what the word “injective” means and
more-or-less proper understanding of what some things like “isomorphism”
mean, I decided to read a book that describes all the concepts in order and
using simple language.

The book I found is
called [“Conceptual Mathematics — A First Introduction to Categories”](http://fef.ogu.edu.tr/matbil/eilgaz/kategori.pdf) by
F. William Lawvere and Stephen H. Schanuel. The book does not assume math
background of any sort and *anyone* can understand concepts described in it.
You can give it a try, but the books in not short — 376 pages, and it does
not even get you to monads (probably will need something else after it if I
decide that I haven't got enough). So this post is the first in a series of
blog posts that I want to write as a sort of overgrown cheat sheets that
highlight important ideas from category theory in a concise form.

I hope that working on the blog posts will help me better organize the ideas
from the book in my head, and may be useful for others who do not
necessarily have the time to read the book.

*Note: here is a somewhat similar
post [here](http://science.raphael.poss.name/categories-from-scratch.html),
but it tries to make the concepts more programmer-friendly by providing
examples from “programming world” using pipes, compilers, and circuits. I
have no such intention, and in fact I tested this descriptions on people who
are neither programmers nor mathematicians, and it worked!*

## What is category?

As everything in math, we need some starting points and definitions to build
on. The main definition is of course **category** itself. Category is
defined by **objects** and **maps** (synonyms: *arrows*, *morphisms*,
*functions*, *transformations*). There are also a few laws that should hold
for the objects and maps in order to form a category, but we will get to
them a bit later.

**Objects** can be pretty much everything. The simplest and most intuitive
object is probably *finite set* (just a collection of things), and it will
be discussed in the next section. The term rather means type of something,
not particular value. It's worth noticing that objects in a category may
have additional structure or properties associated with them, and they
should not “lose” the properties when we work within one category (makes
sense, otherwise we would kind of “walk” outside of category).

**Maps** have **domain** (an object), **codomain** (an object again), and a
rule assigning to each element $a$ in the domain, an element $b$ in the
codomain. This $b$ is denoted by $f \circ a$ (or sometimes $f(a)$), read
“$f$ of $a$”. Simply put, domain contains all possible arguments of the
mapping function and codomain contains output values.

Important thing here is that if we say that object $A$ is domain and object
$B$ is codomain of some map, then the map should be defined for every value
$a$ in $A$ (i.e. it should “use” all input values), but not necessarily it
should map to all values in $B$. This may seem obvious, but I want to put it
here explicitly, because I found this “rule” useful for understanding some
conclusions in the book many times. Of course, there can be only one element
in codomain corresponding to a given element in domain, because otherwise
the whole mapping would be ambiguous.

At this point it should be clear that category theory is a very abstract
thing. If we study abstract transformations of abstract objects, then
certainly we study something that takes place in every area of science and
human knowledge, because human knowledge in general has to do with objects,
their relations and mappings.

There are a couple more things that a category should have, and I'll
describe them in a moment, but first it's good to introduce category of
finite sets and some notation that will be helpful for visualization.

## Category of finite sets, internal and external diagrams

In category of finite sets objects are finite sets and maps are rules how to
go from a value in one set to a value in another set. Due to how our brain
works, it's much easier to reason about collections of values and their
mappings, than about abstract categories. We will be using category of
finite sets to explain most concepts here, and to visualize them, we will
draw pictures of the sets and maps between them.

The set $\{John, Mary, Sam\}$ may be drawn just as:

![Internal diagram of a set](/img/ct1-01.svg)

where a dot represents each element. We can also leave off the labels if
then are irrelevant to discussion. Such picture, labelled or not, is called
**internal diagram** of the set.

We can picture a map as collection of arrows that go from elements of one
set to element of another set:

![Internal diagram of a map](/img/ct1-02.svg)

There are also **external diagrams** for the cases when we do not care about
concrete elements of objects:

$$
A \xrightarrow{f} B
$$

## Endomaps and identity maps

A map in which the domain and codomain are the same object is called an
**endomap** (“endo”, a prefix from Greek ἔνδον *endon* meaning “within,
inner, absorbing, or containing” Wikipedia says). For endomaps we have a
special form of internal diagram:

![Internal diagram of an endomap](/img/ct1-03.svg)

It turns out that in every category, for each object, we have a map that
maps elements of an object $A$ to themselves. This map is called **identity
map** and is denoted as $1_\text{A}$. Here is an example of internal diagram
of an identity map (taken from the book, like the pictures above):

![Internal diagram of an identity map](/img/ct1-04.svg)

The $1_\text{A}$ notation will make more sense once we learn about
*composition of maps* in the next section.

**Definition:** *An endomap $e$ is called **idempotent** if $e \circ e =
e$.*

## Composition

The final, fourth (after objects, maps, and identity maps) thing that a
category should have (or support) is the ability to *compose maps*. That's
where all the fun begins.

**Composition** of two maps $f$ and $g$, written as $f \circ g$ (read as
“$f$ after $g$”) is another map with the same domain as domain of $g$ and
the same codomain as codomain of $f$. To find output value for an input $a$
we first “apply” (or follow arrows) of map $g$ and then take the result,
feed it as input to map $f$ and get the final result. Obviously, to feed
result of $g$ as input to $f$, domain of $f$ should be the same of codomain
of $g$.

In a more familiar notation:

$$
f \circ g = f(g(a))
$$

That equation also explains why composition “works” from right to left (with
respect to the $f \circ g$ notation), it's from the desire to preserve order
of functions when we go from a more explicit notation on the right hand side
to notation on the left hand side.

Once we have the $f \circ g$ map we can forget how we got it and treat it
just as an ordinary map, which it is, of course:

$$
A \xrightarrow{g} B \xrightarrow{f} C = A \xrightarrow{f \circ g} C
$$

Not only the composition of maps should be possible, but it should satisfy
these laws so objects and maps “fit together nicely”:

1. The **identity laws**:

    $$A \xrightarrow{1_\text{A}} A \xrightarrow{g} B \Rightarrow A \xrightarrow{g \circ 1_\text{A} = g} B$$

    and

    $$A \xrightarrow{f} B \xrightarrow{1_\text{B}} B \Rightarrow A \xrightarrow{1_\text{B} \circ f = f} B$$

2. The **associative law**:

    $$A \xrightarrow{f} B \xrightarrow{g} C \xrightarrow{h} D \Rightarrow A \xrightarrow{h \circ (g \circ f) = (h \circ g) \circ f} D$$

These rules makes composition of maps work similarly to multiplication of
numbers. The identity laws ensure that identity maps work indeed like number
1, so we can easily remove (or add) them without changing anything. We will
use that trick a lot. The associative law allows us to move parenthesis. The
analogy between multiplication and composition does not extend too far
though, because we cannot generally swap order of maps in composition, since
their inputs and outputs are sort of “typed” by domain and codomain objects.

## Isomorphisms

One of simplest and ubiquitous things in category theory is **isomorphism**.
A map $A \xrightarrow{f} B$ is called an **isomorphism**, or **invertable
map**, if there is a map $B \xrightarrow{g} A$ for which $g \circ f =
1_\text{A}$ and $f \circ g = 1_\text{B}$. Two objects $A$ and $B$ are said
to be **isomorphic** if there is at least one isomorphism $A \xrightarrow{f}
B$.

What does $g \circ f = 1_\text{A}$ mean? It means that if we apply $f$ to
any $a$ from $A$, feed the result into $g$, then we get the same value $a$
we started with. Pretty easy, right?

Isomorphisms are cool. They allow to move freely from one representation of
object to another. For example, Cartesian system of coordinates is based on
the idea that a pair of numbers is isomorphic to a point on plane, then you
work from that.

There are a few properties of isomophisms that come directly from the
associative and identity laws of maps:

* *Reflexive*: $A$ is isomorphic to $A$ (follows from existence of identity
  maps).

* *Symmetric*: if $A$ is isomorphic to $B$, then $B$ is isomorphic to $A$.
  This simply follows from the definition of isomorphism.

* *Transitive* if $A$ is isomorphic to $B$, and $B$ is isomorphic to $C$,
  then $A$ is isomorphic to $C$ (see the proof below).

**Notation**: if $A \xrightarrow{f} B$ has an inverse, then the (one and
only) inverse for $f$ is denoted by the symbol $f^{-1}$ (read “$f$-inverse”,
or “the inverse of $f$”). Yes, again we have the analogies with numbers!

$$
f^{-1} \circ f = 1_\text{A}, f \circ f^{-1} = 1_\text{B}
$$

Let's prove the transitive property now as an exercise. We are given that:

$$A \xrightarrow{f} B, B \xrightarrow{f^{-1}} A, f^{-1} \circ f = 1_\text{A}, f \circ f^{-1} = 1_\text{B}$$
$$B \xrightarrow{k} C, C \xrightarrow{k^{-1}} B, k^{-1} \circ k = 1_\text{B}, k \circ k^{-1} = 1_\text{C}$$

In order to show that $A$ is isomorphic to $C$, we need to show that the $k
\circ f$ map (the only map that takes us from $A$ to $C$) has an inverse
$g$. Replacing $f$ with $k \circ f$ in the definition of isomorphism we
have:

$$g \circ (k \circ f) = 1_\text{A}$$
$$(k \circ f) \circ g = 1_\text{C}$$

To go back from $C$ to $A$ we have the only way: $C \xrightarrow{f^{-1}
\circ k^{-1}} A$, let's show that it actually an inverse for $k \circ f$:

$$(f^{-1} \circ k^{-1}) \circ (k \circ f) = 1_\text{A}$$
$$f^{-1} \circ (k^{-1} \circ k) \circ f = 1_\text{A}$$
$$f^{-1} \circ 1_\text{B} \circ f = 1_\text{A}$$
$$f^{-1} \circ f = 1_\text{A}$$
$$1_\text{A} = 1_\text{A}$$

This makes use of identity laws and associative law we discussed previously.
The second equation can be proved the same way.

If $f$ has an inverse, then $f$ satisfies two cancellation laws:

* If $f \circ h = f \circ k$, then $h = k$.
* If $h \circ f = k \circ f$, then $h = k$.

Let prove the first one. We assume that $f$ has an inverse and that $f \circ
h = f \circ k$ and we try to show that $h = k$. Since $f \circ h$ and $f
\circ k$ are the same map, $f^{-1} \circ (f \circ h)$ and $f^{-1} \circ (f
\circ k)$ are also the same. But now we can use the rules we already know:

$$f^{-1} \circ (f \circ h) = f^{-1} \circ (f \circ k)$$
$$(f^{-1} \circ f) \circ h = (f^{-1} \circ f) \circ k$$
$$1_\text{A} \circ h = 1_\text{A} \circ k$$
$$h = k$$

## Sections and retractions

Let's give names to some special relations of maps that will be very useful
to us later. If $A \xrightarrow{f} B$, then

* a **retraction** for $f$ is a map $B \xrightarrow{r} A$ for which $r \circ
  f = 1_\text{A}$;

* a **section** for $f$ is a map $B \xrightarrow{s} A$ for which $f \circ s
  = 1_\text{B}$.

First thing to note is that we cannot say that some map $r$ is a retraction
*by itself*, it only makes sense to say that $r$ is retraction for another
map $f$. The same for sections. So if we have $r \circ s = 1_\text{A}$, then
$r$ is retraction for $s$ and $s$ is section for $r$.

Useful mnemonics for retraction is that it retracts values “back from where
they come”. While the name “section” is a little trickier.

We can think of a section $B \xrightarrow{s} A$ as a way to select a
“B-section” in a (possibly) bigger object $A$:

![Internal diagram with a section](/img/ct1-05.svg)

This picture also shows an important idea that in order for $f$ to have a
section, its domain $A$ should be at least as big as codomain $B$, not
smaller. (Make sure that this makes sense to you now, imagine $A$ having
only two dots in it and try to travel from dots in $B$ to values in $A$ and
back arriving to the same dots — that's impossible.)

On the other hand, for $f$ to have a retraction, its codomain $B$ should be
at least as big as its domain $A$ (the logic is exactly the same, so I won't
repeat it here).

## Monomorphism and epimorphism

*Suppose a map $A \xrightarrow{f} B$ has a retraction. Then for any set $T$
and for any pair of maps $T \xrightarrow{x_\text{1}} A$, $T
\xrightarrow{x_\text{2}} A$ from $T$ to $A$*

$$\text{if } f \circ x_\text{1} = f \circ x_\text{2} \text{ then } x_\text{1} = x_\text{2}$$

**Proof:** Looking at the definition, we see that the assumption means that
we have a map $r$ for which $r \circ f = 1_\text{A}$. Using the assumption
that $x_\text{1}$ and $x_\text{2}$ are such that f composes with them to get
the same $T \to B$, we can compose further with $r$ as follows:

![Injective map](/img/ct1-06.svg)

$$x_\text{1} = 1_\text{A} \circ x_\text{1} = (r \circ f) \circ x_\text{1} = r \circ (f \circ x_\text{1}) = r \circ (f \circ x_\text{2}) $$
$$= (r \circ f) \circ x_\text{2} = 1_\text{A} \circ x_\text{2} = x_\text{2}$$

**Definitions:** *A map $f$ satisfying the conclusion “for any pair of maps
$T \xrightarrow{x_\text{1}} A$ and $T \xrightarrow{x_\text{2}} A$, if $f
\circ x_\text{1} = f \circ x_\text{2}$ then $x_\text{1} = x_\text{2}$” is
said to be **injective for maps from** $T$.*

*If $f$ is injective for maps from $T$ for every $T$, one says that $f$ is
**injective**, or is a **monomorphism**.*

What does all this stuff mean anyway? Simply put, if you can “cancel” $f$ by
having a way (retraction) to go back, then when you apply $f$ after other
maps and get the same results, then those maps are the same. I.e. you can
cancel application of $f$ and tell if we were given the same maps or
different ones. *After application of $f$ we can still reason of what
happened before the application.* That's the cancellation.

For example, when GHC gets into a situation when it has only result of
type-level function application, but it needs to figure out what argument of
that function was, it complains that the type-level function may be not
injective. Fair enough! (In GHC 8.0, there is a way to annotate type-level
functions telling that they are injective.)

Remember that if $f$ has a retraction, then $f$ satisfies the cancelation
law:

* If $f \circ h = f \circ k$, then $h = k$.

And if $f$ has a section, then $f$ satisfies another cancelation law:

* If $h \circ f = k \circ f$, then $h = k$.

(We talked about $f$ having an inverse, but if $f$ as an inverse, then it
happens to be *both retraction and section for $f$*, we will prove this
later in the post.)

*Suppose a map $A \xrightarrow{f} B$ has a section. Then for any set $T$ and
any pair of maps $B \xrightarrow{t_\text{1}} T$, $B \xrightarrow{t_\text{2}}
T$ from $B$ to $T$*

$$\text{if } t_\text{1} \circ f = t_\text{2} \circ f \text{ then } t_\text{1} = t_\text{2}$$

**Definition:** *A map $f$ with this cancellation property (if $t_\text{1}
\circ f = t_\text{2} \circ f$ then $t_\text{1}=t_\text{2}$) for every $T$ is
called **epimorphism**.*

What happens here? Now we apply $f$ *before* some maps $t_\text{1}$ and
$t_\text{2}$, and we conclude that if the results we get are the same, then
those $t_\text{1}$ and $t_\text{2}$ are also the same. Intuitively, given a
value $b$ from $B$ that goes as an input to $t_\text{1}$ and $t_\text{2}$,
we should be able to “cancel” previous application of $f$ and tell which
value $a$ from $A$ was given to $f$ so that it produced that particular $b$.
The condition that $f$ has a section is exactly the condition that there is
a way to go from values from $B$ (result of $f$) “back” to values from $A$
(inputs for $f$). Note however that $A$ may be “bigger” than $B$ and the
condition does not forbid $A$ from having several $a$ going to the same $b$.

## Composing sections and retractions

Now we are going to consider two really simple propositions.

**Proposition:** *If $A \xrightarrow{f} B$ has a retraction and if $B
\xrightarrow{g} C$ has a retraction, then $A \xrightarrow{g \circ f} C$ has
a retraction.*

**Proof:** Let $r_\text{1} \circ f = 1_\text{A}$ and $r_\text{2} \circ g =
1_\text{B}$. Then a good guess for a retraction of the composite would be
the composite of the retractions *in the opposite order* (which is anyway
the only order in which they can be composed).

![Composition of retractions](/img/ct1-07.svg)

Using familiar tricks:

$$r \circ (g \circ f) = (r_\text{1} \circ r_\text{2}) \circ (g \circ f) = r_\text{1} \circ (r_\text{2} \circ g) \circ f = r_\text{1} \circ 1_\text{B} \circ f$$
$$= r_\text{1} \circ f = 1_\text{A}$$

This proves that $r$ is a retraction for $g \circ f$.

Proving that the composite of two maps each having sections, has itself a
section is left as an exercise for the reader.

## Theorem of uniqueness of inverses

**Theorem (uniqueness of inverses)**: *If $f$ has both a retraction $r$ and
a section $s$, then $r = s$*.

**Proof:** From the definition we have, if $A \xrightarrow{f} B$, both of
the equations

$$r \circ f = 1_\text{A} \text{ and } f \circ s = 1_\text{B}$$

Then by identity and associative law

$$r = r \circ 1_\text{B} = r \circ (f \circ s) = (r \circ f) \circ s = 1_\text{A} \circ s = s$$

## Another definition of isomorphism, automorphism

Using the notions of “section” and “retraction” we can rephrase the
definition of “isomorphism”.

**Definitions:** *A map $f$ is called an **isomorphism** if there exists
another map $f^{-1}$ which is both a retraction and a section for $f$*:

$$A \xrightarrow{f} B, f \circ f^{-1} = 1_\text{B}$$
$$A \xleftarrow{f^{-1}} B, f^{-1} \circ f = 1_\text{A}$$

*Such a map $f^{-1}$ is called **the inverse map for** $f$; since both of
the two equations are required, the theorem of uniqueness of inverses shows
that there is only one inverse.*

**Definition:** *A map that is an endomap and at the same time an isomorphism is usually
called by the one word **automorphism**.*

----

To be continued…
