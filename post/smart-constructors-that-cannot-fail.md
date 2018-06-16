---
title: Smart constructors that cannot fail
desc: The post describes design of a library which implements refined types. Novelty of the library is that it allows us to establish meaningful connections between properties and do logical conclusions.
date:
  published: June 16, 2018
---

> Wicked.\
  —Arnaud Spiwack

In type theory, a refinement type is a type endowed with a predicate which
is assumed to hold for any element of the refined type. That's a pretty
useful thing and is achieved in Haskell typically by using what is called
*smart constructors*.

For example, I could create a new type called `GreaterThanFive` and only
export `mkGreaterThanFive` and not its “normal” data constructor:

```haskell
newtype GreaterThanFive = GreaterThanFive Int

mkGreaterThanFive :: Int -> Maybe GreaterThanFive
mkGreaterThanFive n =
  if n > 5
    then Just (GreaterThanFive n)
    else Nothing

unGreaterThanFive :: GreaterThanFive -> Int
unGreaterThanFive (GreaterThanFive n) = n
-- BTW, be careful to do this instead of exposing a record selector, which
-- could be used to change the inner value defeating the purpose of this
-- technique.
```

Now to create a value of the `GreaterThanFive` type you have to go through
the ~~hoops~~ check. This ensures that if you ever get a value from
`GreaterThanFive` it'll be indeed greater than 5. What a trick!

An important thing to note right away is that by putting additional
constraints on arguments of a function, often times that function itself can
be made total (and pure, if we otherwise used `MonadThrow` or similar to
report errors):

```haskell
-- Division by zero cannot happen here.
myDivide :: Int -> GreaterThanFive -> Int
myDivide n m = n `div` unGreaterThanFive m
```

Purity and totality are good, so we would like to push the checks to the
boundaries of our systems and inside do the neat stuff in the spirit of
`myDivide`.

So far so good. I was wondering the other day though why refinement types
are so underrated and underused in Haskell? Well, let's face it, they are
annoying to construct exactly because the check can fail, and so the smart
constructors have to live in an environment that gives us the ability to
report errors in some way. And that's not always handy. Even if you can push
*some checks* for *some types* to the boundaries of your system, you have
not done. What about things that are produced deep inside your system? To
use the trick there, you again have to drop into the nasty realm of
`MonadThrow`.

That can't be helped, can it? There are ways.

* [Liquid Haskell](https://ucsd-progsys.github.io/liquidhaskell-blog/),
  which is great, but not first-class in Haskell (yeah, it could be).
* The [`refined`](https://hackage.haskell.org/package/refined) library,
  which is sort of OK now that they have dropped the `Functor` instance of
  `Refined p x`.

Can we do better? Well, let's try an alternative design now.

## Basics

The first I'd like to propose is to separate primitives for construction of
`Refined` types and actual establishing of properties. Because well, we
could establish some properties this morning and then establish some more in
the afternoon. We should be able to! This also suggests that if we want to
have many different properties we could actually store them in a phantom
type that is a list on the type level, so here we go:

```haskell
-- | @'Refined' ps a@ is a wrapper around @a@ proving that it has properties
-- @ps@. @ps@ is a type-level list containing /properties/, that is, void
-- data types symbolizing various concepts.

newtype Refined (ps :: [*]) a = Refined a

-- | 'refined' creates a refined type with no associated properties. We
-- don't demand anything, and so quite conveniently this is a pure function.

refined :: a -> Refined '[] a
refined = Refined

-- | We can erase information we know by using 'unrefined'.

unrefined :: Refined ps a -> a
unrefined (Refined a) = a
```

That looks like a solid foundation to me. What's about properties?

```haskell
class Prop a p where
  type PropProjection a p :: *
  checkProp :: Proxy p -> a -> Either String (PropProjection a p)
```

Here, let `p` be a property, something like this:

```haskell
data NotEmpty
```

But this `NotEmpty` thing is only a name for some concept. I can easily have
a non-empty `String`, `Text`, `ByteString`, or a vector. So we really need
the other ingredient, the type that has the property, `a`. There is no
functional dependency between them as they don't determine each other.

You might be thinking now though, “I thought we're talking about types
equipped with predicates, what the heck is `PropProjection` though”?

## Projections and a bit of category theory

Well yeah, but why not imagine properties (also) as some sort of morphisms
that connect types? If we consider property `p` as a morphism, then `a` will
be its domain and `PropProjection a p` will be its codomain!

Examples should help.

We could have a property telling that a `Text` value is not empty, then:

```haskell
type PropProjection Text NotEmpty = NonEmptyText -- e.g. a newtype
```

We could do the same for linked lists (`a` can only have the kind `*`
currently):

```haskell
type PropProjection [Char] NotEmpty = NonEmpty Char
```

This connects the `NonEmpty` type (non-empty lists), normally obtainable via
the smart constructor `nonEmpty` to our list. Once we have proven that our
list is `NotEmpty` (that the name of the property, `p` thing), we'll be able
to get `NonEmpty` projection *purely without possibility of failure*.

We could have a property called `Length` and in that case we could say:

```haskell
type PropProjection Text Length = Int
```

We could also have a property `IsURI` telling if a `Text` value is a valid
`URI`. In that case we could say (assuming that `URI` is such a type that
can represent only valid `URI`s, as it should):

```haskell
type PropProjection Text IsURI = URI
```

Yes, that's right. The idea is simple and not new: knowing properties of
types enables us to do a lot more purely and totally.

So we know now domain and codomain, but what is morphism itself? Properties
are morphisms (`IsURI`) connecting types (such as `Text` and `URI`), and
they do that via:

```haskell
checkProp :: Proxy p -> a -> Either String (PropProjection a p)
```

And actual category may be viewed as the Kleisli category for the `Either
String` monad, if you're interested in that stuff.

The upshot is that we can do the following:

* We can *establish* properties by “probing” them with `checkProp`.
* If we have established a property, we can use it to get to the *property
  projection* purely and that can't fail.

This “*getting to*” part is often not the main thing. It's just a nice and
very practical thing to be able to do, as we'll discover shortly. Some
properties don't have very meaningful projections in this setup, such as for
example `GreaterThan`:

```haskell
data GreaterThan (n :: Nat)

instance (Integral a, KnownNat n) => Prop a (GreaterThan n) where
  type PropProjection a (GreaterThan n) = a -- could be () as well
  checkProp Proxy n =
    if n > fromIntegral (natVal (Proxy :: Proxy n))
      then Right n
      else Left "not your day"
```

## Establishing properties

There are ways to establish properties. We'll start with relatively
uninteresting, “brute-force” methods.

First, we could just assume stuff to be true:

```haskell
assumeProp
  :: forall q ps a. (Prop a q)
  => Refined ps a
  -> Refined (ps `AddProp` q) a
assumeProp = coerce
```

With `TypeApplications` it's quite a nice thing to write:

```haskell
myText :: Refined '[NotEmpty] Text
myText = assumeProp @NotEmpty (refined "foo")
```

Sometimes it's a handy thing to do too, but needless to say, it's unsafe and
may be a source of bugs.

Next, in various monads such as `MonadThrow`, `MonadFail`, and `MonadError`
you could just check:

```haskell
estPropThrow
  :: forall q ps m a. ( Prop a q
                      , KnownSymbol (PropName q)
                      , MonadThrow m
                      )
  => Refined ps a
  -> m (Refined (ps `AddProp` q) a)

-- estPropFail, estPropError are in the same spirit so they are not shown
```

Slightly better, you could use Template Haskell to check something at the
compile time:

```haskell
estPropTH
  :: forall q ps a. ( Prop a q
                    , KnownSymbol (PropName q)
                    , TH.Lift a
                    )
  => Refined ps a
  -> TH.Q TH.Exp -- returns (Refined (ps `AddProp` q) a) as usual
```

That's it. Quite a few ways. We could use for example `estPropError` in your
parser or what have you, and since `Either` is an instance of `MonadError`,
we could indeed match on `Either` and report parse error in typed and nice
ways.

If something goes wrong, this is what you get:

```haskell
data RefinedException = RefinedException
  { rexpCallStack :: !CallStack
    -- ^ Location where the check failed
  , rexpValue :: !String
    -- ^ Value that failed to satisfy a property check
  , rexpPropName :: !String
    -- ^ Name of the property in question
  , rexpIssue :: !String
    -- ^ Description of why the property check failed
  } deriving (Show, Typeable)
```

That concludes the boring part.

## Identity property and composition of properties

I said that we have a category of refined types where properties are
morphisms. Well, questions you want to ask are probably “what is the
identity in that category” and “how to compose those properties”?

I'm glad you asked.

`IdProp` is our identity property. As it happens with this sort of thing, it
doesn't tell us much about anything at all:

```haskell
data IdProp

instance Prop a IdProp where
  type PropProjection a IdProp = a
  checkProp Proxy = Right
```

Composition is a bit more interesting. Intuitively, it should compose two
checks and get us to the final property projection.

```haskell
-- | 'Via' is the composition in the category of refined types.

data (t :: *) `Via` (p :: *)

instance (Prop (PropProjection a p) t, Prop a p) => Prop a (t `Via` p) where

  type PropProjection a (t `Via` p) = PropProjection (PropProjection a p) t

  checkProp Proxy =
    checkProp (Proxy :: Proxy p) >=> checkProp (Proxy :: Proxy t)

infixl 5 `Via`
```

Yeah. So ```t `Via` p``` works as a property. It means that `p` holds and
then `t` holds on projection of `p`. What can we do with this?

Well, remember I said we could have `Length` property. Let's define it for
`Text`:

```haskell
data Length

instance Text => Prop Text Length where
  type PropProjection Text Length = Int -- yeah, not so refined, but real
                                        -- world will be happier with this
  checkProp Proxy = Right . T.length
```

An interesting thing here of course that `Length` always “holds” for any
`Text`. So we could fearlessly `assumeProp @Length`, but I'll show you a
better way soon.

So now, I can state this:

```haskell
rText0 :: Refined '[] Text
rText0 = refined "foobar"

-- In real programs use 'estMonadThrow' or similar, don't just blindly
-- assume things!
rText1 :: Refined '[GreaterThan 5 `Via` Length]
rText1 = assumeProp @(GreaterThan 5 `Via` Length) rText0
```

How about:

```haskell
rText :: Refined '[UriAbsolute `Via` IsURI]
```

We can establish properties of projections, or properties of any types that
have “connection” to our current refined type via other properties.

Take a moment to understand. ```GreaterThan 5 `Via` Length``` is a property
of something I can obtain (purely) from my `Text`. So it's also a property
of `Text` in a way. Nothing bad happened.

Now of course, I want to get length of my `Text` and it better be equipped
with `GreaterThan 5` property by construction!

Let's just follow the morphism:

```haskell
-- | Obtain a projection as a refined value, i.e. follow a morphism created
-- by a property.

followProp :: forall p ps a. (Prop a p, ps `HasProp` p)
  => Refined ps a
  -> Refined (ProjectionProps ps p) (PropProjection a p)

-- ...

rLength :: Refined '[GreaterThan 5] Int
rLength = followProp @Length rText1
```

Notice how the ``` `Via` Text``` part disappeared.

Just in case you're wondering if you can follow composite properties (with
`Via` in them), the answer is “yes, you can”. ```t `Via` p``` will first
follow `p` and then `t` as you'd expect.

## Are we done yet?

No.

We really just started. No jokes, the whole thing was a preparation for this
part. So watch closely now.

Simple things first. We can select a subset of properties or re-order them:

```haskell
-- | Select some properties from known properties.

selectProps :: forall qs ps a. (ps `HasProps` qs)
  => Refined ps a
  -> Refined qs a
selectProps = coerce
```

So I guess it's best to use concrete type-level lists in API instead of
constraints like `HasProps`. But I haven't decided on this yet.

Now. Why not have ways to reason about stuff?

```haskell
-- | An @'Axiom' name vs qs p@ allows to prove property @p@ if properties
-- @qs@ are already proven. @name@ and arguments @vs@ determine both @qs@
-- and @p@.

class Axiom (name :: Symbol) (vs :: [*]) (qs :: [*]) (p :: *) | name vs -> qs p
```

(It's a pity we can't have proper theorems, but I'll leave that to Coq for
now.)

I could apply an `Axiom` with this:

```haskell
applyAxiom
  :: forall name vs p qs ps a. (Prop a p, Axiom name vs qs p, ps `HasProps` qs)
  => Refined ps a
  -> Refined (ps `AddProp` p) a
```

…to learn a new fact about my refined value.

The clever thing about `name` here is that it allows to determine both `qs`
and `p` without enumerating them explicitly. You may be wondering what the
heck is `vs`, but that will be clearer from examples.

First, `IpProp` demands something to be formalized:

```haskell
-- | We always can assume that a value has 'IdProp'.
instance Axiom "id_prop" '[]   '[]   IdProp
--             name      args  need  conclusion

-- | An existing property can be pre-composed with 'IdProp'.
instance Axiom "id_prop_pre" '[a]  '[a]  (a `Via` IdProp)
--             name          args  need  conclusion

-- | Pre-composition of 'IdProp' can be dropped.
instance Axiom "id_prop_pre'" '[a]  '[a `Via` IdProp]  a
--             name           args   need              conclusion

-- | An existing prperty can be post-composed with 'IdProp'.
instance Axiom "id_prop_post" '[a]  '[a]  (IdProp `Via` a)
--             name           args  need  conclusion

-- | Post-composition of 'IdProp' can be dropped.
instance Axiom "id_prop_post'" '[a]  '[IdProp `Via` a]  a
--             name            args  need               conclusion
```

So `vs`, or arguments is just a helper collection of types (to help
determine `qs` and `p` because of the functional dependency) that is
sometimes necessary in order to state more interesting axioms, such as this
one:

```haskell
instance CmpNat n m ~ GT => Axiom "weaken_gt"
    '[V n, V m] '[GreaterThan n] (GreaterThan m)
```

Where `V` is just a helper to allow us to have heterogeneous type-level
lists with respect to kinds of elements:

```haskell
data V (a :: k)
```

So that's how it works:

```haskell
n :: Refined '[GreaterThan 5] Int
n = assumeProp @(GreaterThan 5) (refined 10)

m :: Refined '[GreaterThan 4] Int
m = selectProps $ applyAxiom @"weaken_gt" @'[V 5, V 4] n
```

Axioms may be quite interesting.

For example, we could demand that a `URI` is absolute, let's call that
property `UriAbsolute`. Then we could have the property stating that `URI`
is convertible to `Text`, let's call it `IsText`.

Then what can we say about this?

```haskell
instance Axiom "abs_uri_not_empty" '[] '[UriAbsolute] (NotEmpty `Via` IsText)
```

Which reads: if you know what your `URI` is absolute, then once you render
it to `Text`, that rendering will be not empty.

## Conclusion

I guess we could deduce of lot of stuff this way. Then add whatever
assumptions we need to the checks at the boundaries of our systems, e.g. in
parsers.

I have coded up the whole thing here (will put it on Hackage shortly):

* https://github.com/mrkkrp/facts

Check it out and maybe try to use, I have no idea how it feels if used at
scale. Maybe not very good!

## Thanks

To Arnaud Spiwack, Facundo Dominguez, and Mathieu Boespflug for discussing
the design with me.
