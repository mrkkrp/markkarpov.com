---
title: Haskell generics explained
desc: This tutorial serves as an introduction to generics in GHC.
date:
  published: November 22, 2019
  updated: October 31, 2021
---

*This is a new, revised version of [the old tutorial I
wrote][original-tutorial].*

```toc
```

This tutorial serves as an introduction to generics in GHC. Generics is a
way to reduce boilerplate and associated with it errors. More precisely, it
is a way to use the same code with different data types. In this regard it
is very close to polymorphism, which in Haskell comes in two flavors:

* **Parametric polymorphism**, when we have type variables in functions/data
  types. This allows the same function to work with different types of
  arguments, as long as the more general types from a function's signature
  can be unified with the concrete types we want to work with.

* **Ad-hoc polymorphism**, which allows us to perform a computation
  abstracted over instances of one or more type classes. We request that a
  type have some properties of interest and then describe the computation in
  terms of these properties. The code is then applicable to any data type
  that has these properties.

How are generics different? Generics allow us to define functions that work
in terms of general combinators that describe the shape of a data type and
some metadata. This way we can declare how to perform a computation on
almost any data type.

Haskell features that make generics possible are type classes and ad-hoc
polymorphism. The ability to describe a data type in terms of a set of
combinators is our property, captured by the `Generic` type class. Values of
a type that has an instance of this type class can be passed to functions
that are defined in terms of the generic representation, not the data type
itself. These functions are by definition polymorphic and usually hidden
behind a type class interface. The code usually takes the form of automatic
derivation of a type class instance:

1. A given data type gets `Generic` instance automatically, as it can be
   generated by the compiler with the help of the `DeriveGeneric` language
   extension.

2. For a type class `T` of interest, there is a type class instance `T Foo`
   which implements the methods of `T` by inspecting generic representation
   of `Foo`. The representation comes from the `Generic Foo` instance. As
   soon as the condition from the step 1 is satisfied we get the `T Foo`
   instance for free.

If you know about the `Data` and `Typeable` type classes, then you probably
know that it's possible to do something similar using the information that
the methods of those type classes provide. `Data` and `Typeable` are beyond
the scope of this tutorial, but you can read about them in this [blog post
by Chris Done][data-typeable] if you're interested.

## The shape of a data type

What could it look like? Well, if I showed you the data types as they are,
you would probably run away, cursing the tutorial and Haskell. I have a
better idea. Let's start with the simplest thing possible and then iterate
asking ourselves how to tackle some more interesting use cases that we might
need to support. This will force us to make the data types less obvious, but
also more powerful. We will do it step by step until we arrive at the
definitions that are actually used.

For better or worse, algebraic data types lock us into a view of the world
that is made up of sums and products. So, we need to be able to represent
the following:

* Data types without constructors at all: uninhabited types like `Void`.
  This can be described as `data V1`, which has no constructors.

* Constructors without arguments, i.e. `data U1 = U1`.

* Sums: `data (f :+: g) = L1 f | R1 g`. If we have a sum data type with two
  alternatives we can represent other sum data types with any number of
  alternatives via nesting.

* Products: `data (f :*: g) = f :*: g`.

Let's try to use this representation to derive a `Functor` instance.
Deriving such an instance means that we should provide the `fmap` function
which looks like this:

```haskell
fmap :: (a -> b) -> Rep f -> Rep f
```

Here, `Rep f` maps to the type of generic representation of `f :: * -> *`.
There is a problem though. You see, the functor's inner type that changes
from `a` to `b` in this example is not found in `Rep f`! This approach will
work only for the types with kind `*` and type classes such as `Show`. If we
want to use this system to derive a `Functor` instance, we need to allow it
to work with the kind `* -> *`.

The solution is to add one more type parameter `p` to all our combinators:

```haskell
data V1 p
data U1 p = U1
data (f :+: g) p = L1 (f p) | R1 (g p)
data (f :*: g) p = (f p) :*: (g p)
```

This way `fmap` would be:

```haskell
fmap :: (a -> b) -> Rep f a -> Rep f b
--                  p  =  a    p  =  b
```

But what happens to the type classes that work with `*` kinds? Our choices
are:

* Have a separate set of combinator types for each case (`*` and `* -> *`
  kinds).

* Use the most general form (with `p`), but for `*` kinds just treat the
  extra `p` parameter as a dummy type index that has no meaning.

The authors of the generics extension went with the second option, and I
can't blame them. We will see that there are a lot of wrappers already and
we really should try to keep their number from exploding.

Let's try to map from a data type to its representation and see if we're
still missing something:

```haskell
data Maybe a = Nothing | Just a

-- Interestingly, we could build a representation that works on ‘Maybe a’,
-- that is, a thing of kind *, if we wanted to derive something like ‘Show’.
-- At the same time if we wanted to derive ‘Functor’, we would work with
-- ‘Maybe’ of kind * -> *. This means that there are actually two different
-- possible representations depending on our aim. This is addressed with two
-- different generics type classes, as we will see later.

-- For kind *, things like ‘Show’:

-- type: (U1 :+: ?) p
```

How to represent `Just a`? We need to way to allow it to have an argument.
Let's add the following:

```haskell
data Rec0 c p = Rec0 { unRec0 :: c }
```

`Rec` part in the type's name hints that it may be possibly recursive.

But in fact, due to a historical accident, it's defined a bit differently:

```haskell
type Rec0 = K1 R

newtype K1 i c p = K1 { unK1 :: c } -- c is the value, ‘a’ in ‘Maybe a’
--         ^   ^
--         |   |
--         |   +-------- dummy p
--         |
-- type-level tag, R or P
```

You see the type-level tag `R`? There used to be another one, `P` and the
type synonym `type Par0 = K1 P` which is now deprecated. Bottom line: `Rec0`
is used for data constuctor arguments (fields), that are not `p` parameter.

With `Rec0`, we can finally build the representation of `Maybe a`:

```haskell
-- This is the type of our representation: (U1 :+: Rec0 a) p

-- Examples of values for ‘Maybe Int’:

-- Nothing => L1 U1
-- Just 5  => R1 (K1 5) -- remember where K1 comes from?
--            ^
--            |
--            +--- L1 and R1 are from our representation of sum types
```

Let's derive a different representation that works with `* -> *` kinds:

```haskell
-- Type of our representation: (U1 :+: ?) p
```

We need a way to tell if we have an argument of `p` type (like `a` in
`Functor f => f a`) or some other type if we're to write a generic `fmap`
function. For this the generics extension uses `Par1 p`:

```haskell
newtype Par1 p = Par1 { unPar1 :: p } -- “par” stands for “parameter”
```

`Par1` is used to mark occurrences of `p`. Our representation thus becomes:

```haskell
-- The type of our representation: (U1 :+: Par1) p

-- Examples of values for ‘Maybe Int’:

-- Nothing => L1 U1 -- the same
-- Just 5  => R1 (Par1 5)
```

The final example is for lists. Given the standard definition of linked
list:

```haskell
data List a = Nil | Cons a (List a)
```

How do we build its generic representation for the kind `* -> *`? The tricky
part is, of course, `List a`, which is a recursive occurrence of entire
functorish part with the parameter inside it. If we mark occurrences of the
parameter by `Par1`, then why not mark this data constructor too? For that
we have `Rec1`.

```haskell
-- The type of representation: (U1 :+: (Par1 :*: Rec1 List)) p

-- Examples of values for ‘List Int’:

-- Nil                 => L1 U1
-- Cons 5 Nil          => R1 (Par1 5 :*: Rec1 Nil)
-- Cons 5 (Cons 4 Nil) => R1 (Par1 5 :*: Rec1 (Cons 4 Nil))
```

If we had just arguments of a data constructor that are not related to
parameter `p`, plain `Rec0` (`K1`) would be used for both first and second
arguments of `Cons`.

## The `Generic` and `Generic1` type classes

Type classes that map types to their representations are called `Generic`
(for type classes that work with `*` kinds) and `Generic1` (for type classes
that work with `* -> *` kinds). They live in the module called
`GHC.Generics` together with the types used to build the data type
representations that we have just discussed.

Let's see what these type classes have:

```haskell
class Generic a where
  type Rep a :: * -> *
  from :: a -> Rep a p
  to   :: Rep a p -> x

class Generic1 f where
  type Rep1 f :: * -> *
  from1 :: f p -> Rep1 f p
  to1   :: Rep1 f p -> f p
```

`from` and `from1` map values of data types to their generic
representations. `Rep` and `Rep1` are associated type functions (the feature
is enabled by the `TypeFamilies` GHC extension) that take the type of data
we want to manipulate and return the type of its representation. Of course,
if we want to derive `Functor` instances, we need a way to go back from
representation to actual value of target data type. This is done via `to`
and `to1`. The good thing about this, of course, that GHC can derive
`Generic` and `Generic1` for us automatically when the `DeriveGeneric`
language extension is enabled.

Now let's open GHCi and try to infer `Rep` of some type:

```haskell
λ> :t (undefined :: Rep (List a) p)
(undefined :: Rep (List a) p)
  :: D1
       ('MetaData "List" "GenericsTutorial" "main" 'False)
       (C1 ('MetaCons "Nil" 'PrefixI 'False) U1
        :+: C1
              ('MetaCons "Cons" 'PrefixI 'False)
              (S1
                 ('MetaSel
                    'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                 (Rec0 a)
               :*: S1
                     ('MetaSel
                        'Nothing 'NoSourceUnpackedness 'NoSourceStrictness 'DecidedLazy)
                     (Rec0 (List a))))
       p
```

OK, there is just a little bit more to this…

## Metadata wrappers

A representation also has associated metadata. It may look messy and
difficult to read, but I'll explain the logic behind it in a moment. First
of all, metadata should not get in our way if we don't care about it. Thus
all metadata is attached using the same simple wrapper:

```haskell
newtype M1 i c f p = M1 { unM1 :: f p } -- ‘f p’ is what lives inside, U1 for example
--         ^ ^
--         | |
--         | +---- compiler-generated data type that allows us to get meta information
--         |
--  type-level tag, see below
```

The `i` type-level tag can be one of the three:

* `D` for data type metadata `type D1 = M1 D`
* `C` for constructor metadata `type C1 = M1 C`
* `S` for record selector metadata `type S1 = M1 S`

It should be understandable why metadata is attached this way. If we want
to, we can ignore it:

```haskell
f (M1 x) = f x
```

If we want to look at a particular type of metadata we can specify the `i`
type-level tag, or we can leave it unspecified to deal with all metadata at
once.

The `c` type is auto-generated by the compiler and encodes metadata on the
type level. Why not store the metadata on the value level, in the `M1`
constructor? Well, if it were there, we would have to provide it when we
wanted to generate some values, and providing metadata for already existing
data type is certainly something that only the compiler can do properly.
With the current approach, a given data type determines the type of its
representation, including its metadata, so we don't have to bother.

Let's see what metadata wrappers are generated:

* The entire representation is wrapped in `D1`, which provides
  datatype-level information: datatype name, module name, and whether it's a
  `newtype`.

* Every constructor is wrapped in `C1`, which provides information about the
  constructor such as its constructor name, fixity, and whether it's a
  record.

* Every argument of a constructor is wrapped with `S1` (even if it's not
  actually a record selector), which tells us the selector name.

Look [at the Haddock][ghc-generics-datatype] to find out names of functions
that help extract the metadata. Using them is straightforward: feed the
wrapped data to functions like `datatypeName` and get the information.

## Example: deriving `Functor`

After talking so much about `Functor` instances and adding the clumsy `p`
parameter to support them, we absolutely must derive a `Functor` instance
now. In fact, `Functor` instance for generics is already defined in
`GHC.Generics`, so instead of re-implementing it let's just go through the
code:

```haskell
-- If we have a parameter ‘p’, we just map over it, as expected:

instance Functor Par1 where
  fmap f (Par1 p) = Par1 (f p)

-- The same with ‘Rec1’ (just use ‘fmap’ because the inner part is a ‘Functor’):

instance Functor f => Functor (Rec1 f) where
  fmap f (Rec1 a) = Rec 1 (fmap f a)

-- A constructor without fields only can be returned untouched:

instance Functor U1 where
  fmap _ U1 = U1

-- A field that is not ‘p’ parameter should not change:

instance Functor (K1 i c) where
  fmap _ (K1 a) = a

-- Metadata has no effect, just unwrap it and continue with the inner value,
-- if the inner value is an instance of ‘Functor’:

instance Functor f => Functor (M1 i c f) where
  fmap f (M1 a) = M1 (fmap f a)

-- When we have a sum, we should try to map what we get, provided that it
-- contains something that has ‘Functor’ instance:

instance (Functor l, Functor r) => Functor (l :+: r) where
  fmap f (L1 a) = L1 (fmap f a)
  fmap f (R1 a) = R1 (fmap f a)

-- The same for products:

instance (Functor a, Functor b) => Functor (a :*: b) where
  fmap f (a :*: b) = fmap f a :*: fmap f b
```

## Example: counting constructor fields

We are ready to implement a simple and pretty useless type class that will
count constructor fields of a given value.

The type class looks like this:

```haskell
class CountFields a where
  -- | Return number of constuctor fields for a value.
  countFields :: a -> Natural
```

We will start by implementing `countFields` method that works on
representations:

```haskell
instance CountFields (V1 p) where
  countFields _ = 0

instance CountFields (U1 p) where
  countFields _ = 0

instance CountFields (K1 i c p) where
  countFields _ = 1

instance CountFields (f p) => CountFields (M1 i c f p) where
  countFields (M1 x) = countFields x

instance (CountFields (a p), CountFields (b p)) => CountFields ((a :+: b) p) where
  countFields (L1 x) = countFields x
  countFields (R1 x) = countFields x

instance (CountFields (a p), CountFields (b p)) => CountFields ((a :*: b) p) where
  countFields (a :*: b) = countFields a + countFields b
```

Let's write a single function called, say, `defaultCountFields` that does
the counting for any instance of `Generic`:

```haskell
defaultCountFields :: (Generic a, CountFields (Rep a)) => a -> Natural
defaultCountFields = countFields . from
```

But here is a catch—the code above does not compile. `CountFields` has the
kind `CountFields :: * -> Constraint`, but we give it `Rep a` of the kind `*
-> *`.

The typical solution is to have a helper class that works with things of `*
-> *` kind (this also removes the `p` parameters from signatures):

```haskell
class CountFields1 f where
  countFields1 :: f p -> Natural

defaultCountFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
defaultCountFields = countFields1 . from

instance CountFields1 V1 where
  countFields1 _ = 0

instance CountFields1 U1 where
  countFields1 _ = 0

instance CountFields1 (K1 i c) where
  countFields1 _ = 1

instance CountFields1 f => CountFields1 (M1 i c f) where
  countFields1 (M1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :+: b) where
  countFields1 (L1 x) = countFields1 x
  countFields1 (R1 x) = countFields1 x

instance (CountFields1 a, CountFields1 b) => CountFields1 (a :*: b) where
  countFields1 (a :*: b) = countFields1 a + countFields1 b
```

You might have noticed that some data types like `Par0`, `Rec1` did not get
their definitions. This is OK because we work with `Generic`, not `Generic1`
here. As `GHC.Generics` docs say:

* If no `:+:` instance is given, the function may still work for empty
  datatypes or datatypes that have a single constructor, but will fail on
  datatypes with more than one constructor.

* If no `:*:` instance is given, the function may still work for datatypes
  where each constructor has just zero or one field, in particular for
  enumeration types.

* If no `K1` instance is given, the function may still work for enumeration
  types, where no constructor has any fields.

* If no `V1` instance is given, the function may still work for any datatype
  that is not empty.

* If no `U1` instance is given, the function may still work for any datatype
  where each constructor has at least one field.

An `M1` instance is always required, but it can just ignore the
meta-information.

## Packing it in the type classes

Having dealt with the generic implementation of the functionality of
interest, let's put it all together and use a special GHC extension to allow
the user to derive type classes without knowing anything about generics.

For a generic implementation to work without user's definition we need to
provide it as the default definition. As you have already seen, a generic
implementation often involves a `Generic` constraint. It would be ugly and
overly restrictive to add it as a superclass to every type class just to
make deriving easier. The `default` keyword, enabled by the
`DefaultSignatures` language extension, allows us to give a different type
signature for a default implementation of a method:

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  default fmap :: (Generic1 f, Functor (Rep1 f)) => (a -> b) -> f a -> f b
  fmap = to1 . fmap . from1

class CountFields a where
  countFields :: a -> Natural
  default countFields :: (Generic a, CountFields1 (Rep a)) => a -> Natural
  countFields = defaultCountFields
```

This way we can have our cake and eat it too: deriving is easy and no ugly
details are visible!

## Conclusion

Generics is a powerful means of automating writing of error-prone and boring
definitions. The feature is helpful beyond deriving type class instances, as
with a bit of creativity it allows us to reason about data types generically
and generate values in a type-safe way. Finally, there are quite a few very
interesting packages that complement or build on top of GHC generics. Once
you feel comfortable with vanilla generics, libraries like
[`generic-sop`][generics-sop] may be of interest.

[original-tutorial]: https://www.stackbuilders.com/tutorials/haskell/generics/
[data-typeable]: http://chrisdone.com/posts/data-typeable
[ghc-generics-datatype]: https://hackage.haskell.org/package/base/docs/GHC-Generics.html#t:Datatype
[generics-sop]: https://hackage.haskell.org/package/generics-sop
