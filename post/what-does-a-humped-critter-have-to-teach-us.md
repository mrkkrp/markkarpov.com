---
title: What does a humped critter have to teach us?
desc: The post compares OCaml's module system with Haskell's type classes.
date:
  published: September 29, 2019
---

![Camels](/static/img/camels.png)

While attending the [MigareOS hack retreat][retreat] in Marrakesh (Morocco)
I met some interesting people and learned a couple of new things. In
particular, I met a creature I had never met before—Camel, *the ship of
desert*.

I will never forget the calm and profound look of that camel. Sage eyes of
the animal expressed little interest in me, as well as in mundane details of
the situation in general. Some may think that it's sheer arrogance on his
part and I concur. But the camel has serious reasons to look arrogant, even
though he is not as pure as some and his life is not the easiest, for he
lives in the desert.

## OCaml and Haskell

OCaml and Haskell may be closer to each other than to any other popular
programming languages, some differences are:

* OCaml is strict, while Haskell is ~~lazy~~, err… non-strict.
* OCaml has polymorphic variants and objects, which are essentially a form
  of row-types. On the other hand, Haskell has a few unique features of its
  own.
* OCaml has an interesting approach to the organization of its modules.

In my opinion, the module system is the most remarkable feature of OCaml.
This is why I'm going to talk only about it in this post. It's also a good
opportunity to see how it relates to Haskell's type classes.

### Simple example

Let's start with an example. A simple module containing a type for something
like Haskell's `Text` could look like this:

```ocaml
type t = ...

(* OCaml programmers rarely add type annotations to functions *)

let empty = ...
let singleton char = ...
let index text n = ...
let length text = ...
```

The `t` type is a common idiom in OCaml. Modules are usually fine-grained,
so that every type gets its own module. Later we will see more reasons for
doing it this way.

A Haskeller might think now of what bothers Haskellers the most: if every
type gets its own module, *how long is the typical import section of a
module*? The answer is: *around 1 or 2 lines!*

Suppose we store the module above in a file called `text.ml`, then we can
refer to the text type as `Text.t` and to the functions as e.g.
`Text.empty`. You only use top-level `open` (analogous to Haskell's simple,
unqualified `import`) for very few selected modules specially designed to be
“opened” globally. It usually includes a sort of prelude. Everything else is
accessed by specifying the name of the module locally, and who can argue
that this is not a good practice?

OCaml has some nice syntax to encourage opening modules locally:

```ocaml
let average x y =
  let open Int64 in
  x + y / of_int 2
```

Here, we can refer to `Int64.(/)` as `(/)` and to `Int64.of_int` as `of_int`
inside of `let open`. The alternative syntax uses parentheses to do the
same:

```ocaml
let average x y =
  Int64.(x + y / of_int 2)
```

### Module signature and implementation

Another detail is that we do not see an export section in the snippet above.
In fact, OCaml keeps *module signatures* separated from *module
implementations*. Module signatures are usually stored in a different file
with extension `.mli`. `text.mli` in our case could look like this:

```ocaml
type t

val empty : t
val singleton : char -> t
val index : t -> int -> char
val length : t -> int
```

Module signatures work as a boundary that hides implementation details. For
example, above we just say that the type we operate on is `t` and we do not
export its constructors (that would happen if we wrote `=` with definition
after `type t`), so it stays abstract.

What's more, the relation between module signatures and module
implementations is exactly the same as between types and values. We could
define a module signature in a standalone fashion like this:

```ocaml
module type Text = sig
  type t

  val empty : t
  val singleton : char -> t
  val index : t -> int -> char
  val length : t -> int
end
```

A particular module implementation may or may not match the signature. An
implementation matches if it declares all the mentioned types and values and
their signatures match, provided that the abstract type `t` is substituted
with its “real” implementation type.

### Functors

*Functors* are functions from module to module (although functors and
functions do not share the same syntax). For example:

```ocaml
module type Eq = sig
  type t

  val eq : t -> t -> bool
end

module EqFromIndexed (Elt : Eq) (I : sig
  type t

  val index : t -> int -> Elt.t
  val length : t -> int
end) : (Eq with type t := I.t) = struct
  type t = I.t

  let eq x y =
    let len_x = I.length x in
    let len_y = I.length y in
    let rec go i =
      if i < len_x
      then I.index x i = I.index y i && go (i + 1)
      else true in
    if len_x = len_y
    then go 0
    else false
end
```

Here we first defined the type signature `Eq` which captures the necessary
minimum for talking about equality of things: the type `t` and the equality
function `eq`. Then we defined the functor `EqFromIndexed` which takes the
module `Elt : Eq` providing an equality check for elements, as well as the
module `I` providing two functions `index` and `length`. This allow us to
check if two indexable containers are equal by simply traversing all
elements. The definition of `EqFromIndexed` also features an *anonymous
module signature*.

The `Eq with type t := I.t` annotation helps to avoid having an abstract `t`
type in the module produced by the functor, making sure that it is the same
as the `I.t` type from the argument module. Interested readers can find more
information about these subtle details
[here][functors-and-type-abstraction].

We now apply `EqFromIndexed` to an anonymous module providing the `eq` for
`char`s and `Text` that we already have:

```ocaml
module TextEq = EqFromIndexed (struct
  type t = char
  let eq = Char.equal
end) (Text)
```

Now we have a way to generate the equality function `TextEq.eq` for any
given type that can satisfy the requirements imposed by the arguments of
`EqFromIndexed`.

### Type classes

Let's consider an analogous setup in Haskell:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

import Prelude hiding (length)
import Data.Kind (Type)

class Indexed a where

  type Elt a :: Type

  index :: a -> Int -> Elt a
  length :: a -> Int

eqFromIndexed
  :: (Eq (Elt a), Indexed a)
  => a
  -> a
  -> Bool
eqFromIndexed x y =
  if len_x == len_y
    then go 0
    else False
  where
    len_x = length x
    len_y = length y
    go i =
      if i < len_x
        then index x i == index y i && go (i + 1)
        else True
```

Type classes allow us to work in a similar framework where certain
properties (i.e. functions, methods of classes) are first defined from
scratch and then other functions can be built if a certain interface can be
satisfied (`EqFromIndexed` in OCaml and `eqFromIndexed` in Haskell).

Now the principle “one module per type” should make more sense. A module can
be considered as a collection of all defined operations for a particular
type `t` inside it, like collection of all methods of all type classes for a
type.

### Comparison

I think OCaml may be doing better than Haskell here. Here are some thoughts:

* **Naming convention.** As you can see, with OCaml's approach naming
  matters. If a module follows the conventions it can often be passed to a
  functor without having to define another module that can act as a bridge
  to satisfy the naming convention that a functor expects. This makes names
  in modules more predictable.

* **Complexity.** Type classes are a heavy feature in Haskell. It
  complicates things instead of keeping the system flat and simple. Look at,
  say, associated type families. You have normal “standalone” type families
  and then type families associated with a type class. Following OCaml's
  strategy we would have just 1 level instead of 2. OCaml already
  essentially has associated types (remember `Elt.t`?) and it's simply a
  consequence of the fact that it has `type` definitions and the structure
  of its module system.

* **Efficiency.** OCaml's module system reminds me of Haskell's
  [backpack][backpack]. In OCaml land there is one problem less
  performance-wise. At least in theory, generating new functions with
  functors should produce fully specialized efficient code, sparing us of
  the fear that excessive polymorphism will make our code 100 times slower.

* **There are no orphans in the desert land.** OCaml's approach does not
  lead to orphan or overlapping instances because unlike Haskell's type
  classes it doesn't promise to establish a one-to-one correspondence
  between types and values/functions. Which leads us to…

* **Type classes as a way to go from type level to value level.** In modern
  Haskell type classes are often used to get a unique value (functions are
  also values in this context) associated with a type. Here, type class
  definition establishes what sort of value you can get, and instances
  define actual values. If we accept that the “uniqueness” property is
  useful, then we could argue that OCaml doesn't have such a mechanism.
  [Edward Kmett likes this a lot.][type-classes-vs-the-world] But I'd say
  it's not a problem of module system vs type classes. If we have dependent
  types, we automatically can get one-to-one correspondence of similar kind.

## Conclusion

OCaml's modules are first-class values. They allow the language to be simple
yet expressive without the need for type classes providing a typed framework
on a higher level and making it easier to generate performant code.

This post is not meant to be a comprehensive guide to the OCaml module
system, so I'm not showing some usages, such as nesting modules in a single
file or actually passing modules as values (partly to cut
the size of the post, partly because I do not yet understand how this is
useful). If you want to learn OCaml or just read about its features, try
[*Real World OCaml*][real-world-ocaml].

[retreat]: http://retreat.mirage.io/
[functors-and-type-abstraction]: http://caml.inria.fr/pub/docs/manual-ocaml/moduleexamples.html#sec24
[backpack]: https://gitlab.haskell.org/ghc/ghc/wikis/backpack
[type-classes-vs-the-world]: https://www.youtube.com/watch?v=hIZxTQP1ifo
[real-world-ocaml]: https://realworldocaml.org
