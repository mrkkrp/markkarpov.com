---
title: Existential quantification
desc: In this post I'm going to give an idea of how existentials can be useful in Haskell.
date:
  published: November 11, 2018
  updated: June 1, 2020
tag: haskell
---

In this post I'm going to give an idea of how existentials can be useful in
Haskell. For some time I wanted to write such a post to explain the topic in
a way that an older version of me could find helpful, because I still
remember the confusion that I felt when I first ran into rank-N types and
the terms *universal* and *existential quantification*.

I'd like to start from the beginning to make the post useful to as many
readers as possible, but I'll be presenting the basics in a concise form
lest make more experienced readers bored.

## Type variables and the `forall` keyword

Type variables in Haskell are always introduced with the keyword `forall`:

```haskell
id :: forall a. a -> a
```

`forall a` means exactly what it suggests: `id` works *for all* `a`. `a`
will *unify with* (or will be *fixed to*) any type that consumer of `id` may
choose.

Fixing or unifying a variable doesn't necessarily mean that we're putting a
concrete type in its place. It might as well unify with another variable:

```haskell
idInt :: Int -> Int
idInt = id -- 'a' unifies with 'Int'

id' :: forall b. b -> b
id' = id -- 'a' unifies with 'b'
```

What matters is that choice of type for such a variable is made.

By default Haskell type signatures allow us to omit `forall`, but it's still
useful to remember that those `forall`s are there. In the post I'll be
writing `forall`s for clarity even when they could have been omitted.

## Rank-N types

Only variables introduced with `forall`s at the beginning of type signature
will be fixed when we use the corresponding function. Other `forall`s deal
with independent type variables:

```haskell
myPrettyPrinter
  :: forall a. Show a -- 'a' will be fixed when we use 'myPrettyPrinter'
  => (forall b. Show b => b -> String) -- but not 'b'
  -> Int
  -> Bool
  -> a
  -> String
```

When we have two levels of `forall`s it's called rank-2 type. And in general
such constructions are called rank-N types.

## Universal and existential quantification

Here are the key things:

* A variable is *universally quantified* when the consumer of the expression
  it appears in can choose what it will be.
* A variable is *existentially quantified* when the consumer of the
  expression it appears in have to deal with the fact that the choice was
  made for him.

Both universally and existentially quantified variables are introduced with
`forall`. There is no `exists` in Haskell. In fact, it's not necessary.

A few examples should help:

* In the function `myPrettyPrinter` above, for consumers of
  `myPrettyPrinter` `a` is universally quantified (we can choose what the
  type will be)
* …while `b` is existentially quantified (we have to be prepared to deal
  with any `b` that will be given to the callback).
* *Inside* the body of `myPrettyPrinter` `a` is existentially quantified
  because the caller of `myPrettyPrinter` already has chosen the type for
  us.
* When we apply the function with `b` in its type signature (the first
  argument of `myPrettyPrinter`), we will be able to choose its concrete
  type. In the body of `myPrettyPrinter` `b` is universally quantified.

## Existential wrappers

Apart from the cases I have mentioned above there is a way to have
existentials by putting values in wrappers that “hide” type variables from
signatures.

```haskell
data Something where
  Something :: forall a. a -> Something
```

The constructor accepts any `a` we like, but after construction we lose the
type information and pattern matching afterwards only reveals that there is
some `a`, but nothing regarding what it is. Compare this to passing a value
to `id`: we can pass anything to it but we lack any information about the
argument inside of the body of `id`.

One thing you can do with existential wrappers that is impossible without
them is returning existentially quantified data from a function. The wrapper
allows us to avoid unification of existentials with outer context and
“escaping” of type variables.

## Why existentials?

Existentials are always about throwing type information away. Why would we
want to do that? The answer is: *sometimes we want to work with types that
we don't know at compile time*. The types typically depend on the state of
external world: they could depend on user's input, on contents of a file
we're trying to parse, etc. Fortunately Haskell's type system is powerful
enough to allow us to do [interesting things][struggling-to-forget] even in
that case.

## How to make use of existentials

We want to work with values of types that we don't know at compile time, but
at run time there are no types at all: they have been erased!

We have to preserve some information about existentially quantified type to
make use of it, otherwise we'll be in the same position as implementers of
`id` having a value and only being able to pass it around never doing
anything meaningful with it.

There are various degrees of how much we might want to preserve:

* We could have `a` in the type `[a]` existentially quantified. There are
  still some things we could do with a value of this type. For example, we
  could compute length of the list. So knowing nothing about a type is also
  an option sometimes when it parametrizes another type and we have
  parametrically-polymorphic functions that work on that type. In this case
  the set of possible types for `a` is open i.e. it can grow.

* We could assume that the existentially quantified type has certain
  properties (instances):

  ```haskell
  data Showable where
    Showable :: forall a. Show a => a -> Showable
  ```

  Pattern-matching on `Showable` will give us the corresponding dictionary
  back. This allows us to do as much as the knowledge from the attached
  constraint permits. Again, the set of possible types for `a` is open (new
  instances of `Show` can be defined).

* We could use GADTs to restore exact types of existentially quantified
  variables later:

  ```haskell
  data EType a where
    ETypeWord8  :: EType Word8
    ETypeInt    :: EType Int
    ETypeFloat  :: EType Float
    ETypeDouble :: EType Double
    ETypeString :: EType String

  data Something where
    Something :: EType a -> a -> Something
  ```

  Matching on one of the data constructors of `EType` reveals `a` and after
  that we are free to do anything with the value of corresponding type
  because we know it.

  With this approach the set of possible types for `a` is limited and
  closed. It can be expanded by changing the definition of `EType` though.

Let's take a closer look at the last method.

## Constraints vs GADTs

How did the approach with GADTs come into existence? Usually we start with
the second approach—the one with constraints. Well, let's see…

* The approach with constraints is good, except as you develop an
  application you'll want to know more and more about existential types, so
  the list of constraints will grow and grow.
* It's even worse though: at some point there will be parts of code that
  require incompatible instances, so there won't be any type `a` that
  satisfies all of them. That's a problem.

One solution is to add more constructors to the wrapper:

```haskell
data Something where
  SomethingIntegral :: forall a. (Show a, Integral a) => a -> Something
  SomethingFloating :: forall a. (Show a, Floating a) => a -> Something
  SomethingStringy  :: forall a. (Show a, IsString a) => a -> Something
```

But we haven't solved the problem properly because now we classify the
existential value by a single pre-chosen criteria: it's either `Integral` or
`Floating` or `IsString`. After a short while we are likely to find
ourselves in a situation when we want to use a different criteria that is
completely unrelated to the previously chosen one:

* We could want to do something with instances of `Num`. In that case we add
  `Num a` to constraints of both data constructors `SomethingIntegral` and
  `SomethingFloating` and then we have to pattern match on both and do the
  same thing in each branch.
* We could be interested in an instance that only `Word8` has, or only
  `Word8` and `String` have… you see the point.

GADT-driven approach allows us to recover any dictionaries of interest if
the actual value is an instance of right type classes. This is as
fine-grained as it can be:

```haskell
reifyIntegralDict :: EType a -> Maybe (Dict (Integral a))
reifyIntegralDict = \case
  ETypeWord8 -> Just Dict
  ETypeInt   -> Just Dict
  _          -> Nothing

reifyFloatingDict :: EType a -> Maybe (Dict (Floating a))
reifyFloatingDict = \case
  ETypeFloat  -> Just Dict
  ETypeDouble -> Just Dict
  _           -> Nothing

reifyFooDict :: EType a -> Maybe (Dict (Foo a))
reifyFooDict = \case
  ETypeWord8  -> Just Dict
  ETypeString -> Just Dict
  _           -> Nothing
```

## Example: vector indexed by existential length

You might be thinking that this approach actually amounts to having a sum
data type like this:

```haskell
data EType
  = ETypeWord8 Word8
  | ETypeInt Int
  | ETypeFloat Float
  | ETypeDouble Double
  | ETypeString String
```

And that is correct in this case, but there also can be:

* Several existentially quantified type variables in a wrapper which leads
  to combinatorial explosion if we try to enumerate all the combinations.
* Types that are recursively defined and there is no way to enumerate all
  their variants at all. It could be a vector indexed by existential length
  or something indexed by a list of type-level.

Here is an example of that—a vector indexed by existential length:

```haskell
data Vector (n :: Nat) a where
  Nil :: Vector 0 a
  Cons :: a -> Vector n a -> Vector (n + 1) a

data SomeVector where
  SomeVector :: KnownNat n => EType a -> Vector n a -> SomeVector
```

This is a sort of combination of all the three approaches I listed above:

* We still can do some things with `Vector` without knowing type of its
  elements or its length.
* We can recover exact type of the elements if we wish.
* Luckily, there is enough type-level machinery around `Nat`s (provided by
  the `base` library) to avoid using singletons, so that knowing that `n` is
  an instance of `KnownNat` is sufficient for everything we might want to
  do.

## Type assertions: back to concrete types

It is not always necessary to write programs passing around existentials and
reifying dictionaries as needed, or worse yet, proving things with
[`Decision`][decision] and [`(:~:)`][type-eq]. Some would be inclined to say
that these are the techniques of perverts. In many cases we want to make an
assumption about types and throw an exception when the assumption turns out
to be wrong. This way we can indeed go back to concrete types!

Here is an example of this that uses the definition of `Vector` given above:

```haskell
assertVector
  :: SomeVector -- we could have parsed this from a file
  -> Vector 5 Int -- and we only want to continue if the type is this
assertVector (SomeVector etype (v :: Vector n a)) =
  case etype of
    ETypeInt -> -- reifies that a ~ Int
      case sameNat (Proxy :: Proxy n) (Proxy :: Proxy 5) of
        Nothing -> error "expected a vector of length 5"
        Just Refl -> -- reifies that n ~ 5
          v -- now we can return v because we know enough
    _ -> error "expected a vector of Ints"
```

The method allows us to write e.g. parsers that return existentials such as
`SomeVector` while still being able to go to the more comfortable realm of
concrete types later in the program.

## A nice conclusion

When rich types and the real world collide, existentials often arise. Yet
with due diligence one can tame them and still write beautiful and
meaningful programs.

[decision]: https://hackage.haskell.org/package/singletons/docs/Data-Singletons-Decide.html#t:Decision
[type-eq]: https://hackage.haskell.org/package/singletons/docs/Data-Singletons-Decide.html#t::-126-:
[struggling-to-forget]: /post/struggling-to-forget.html
