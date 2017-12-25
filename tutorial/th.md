---
title: Template Haskell tutorial
desc: The tutorial explains how to use Template Haskell for metaprogramming in Haskell.
date:
  published: December 24, 2017
---

The tutorial aims to introduce the reader to *Template Haskell* (TH)—the
language extension that adds meta-programming capabilities to the Haskell
language. Here I assume some familiarly with Haskell, perhaps beginner or
intermediate level, although these terms are rather nebulous and subjective.
To express the prerequisites in a more tangible form: if you know what a
monad is, you should probably be OK.

TH has the reputation of being an expert-level topic that mere mortals are
not prepared to comprehend. I don't think this is so. The ideas behind TH
are simple and make sense, while specific details can be always looked up in
the Haddocks.

The tutorial cannot possibly cover every use of TH, and so it is structured
in such a way so we only get to see the most common, conventional, and
benign uses of the feature.

## Motivation

One of the main difficulties with TH is perhaps deciding whether it is the
best solution to a problem at hand. Writing code that generates code is
generally considered an indication that the tools of expression provided by
the language and/or programmer's imagination have failed to address a
particular problem and meta-programming is used as a last resort to get
things done. True or not, TH is quite popular and so knowing your way around
it is a valuable skill that can be used to do things that often cannot be
achieved otherwise.

Let's list some uses of TH:

* *Automatic deriving of type class instances* is still perhaps the most
  common use case for TH. Even though the same problem can often be
  addressed by *generics*, they are known to make compilation times longer
  (compared to TH-based solutions), so TH is still the preferred method of
  automatic instance deriving in libraries like `aeson` and `lens`.

* *Compile-time construction of values of refined types* that turns invalid
  inputs into compilation failures.

* *Compile-time loading and processing of data from external files*, which
  is very useful sometimes. Even though this involves running `IO` during
  compilation, it's a relatively innocent use case of that dangerous
  feature.

Reasons not to use TH:

* TH helpers are often viewed as black boxes that do “magic”. It is not
  clear at all what a thing of the type `Q [Dec]` does, it might do anything
  (we will see that any code that generates declarations has the same `Q
  [Dec]` type, no matter what sort of declarations it generates).
  Documentation becomes the main source of information about semantics of TH
  code.

* TH imposes restrictions on where the user should define TH functions
  themselves and sometimes also how to order definitions in files where TH
  functions are used.

## The `Q` monad

Generation of code requires certain features to be available to us:

* The ability to generate new unique names that cannot be captured.

* The ability to retrieve information about a thing by its name. Usually we
  want to know about functions and types, but there are also ways to learn
  about a module, get collection of instances of a particular type class,
  etc.

* The ability to put and get some custom state that is then shared by all TH
  code in the same module.

* The ability to run `IO` during compilation, so we can e.g. read something
  from a file.

These features are usually achieved through *monads* in Haskell, and so it
should not come as a surprise that there is a special monad called `Q`
(short for “quotation”) that hosts all functions provided by TH.

## Splicing

The only purpose of having a value of the type `Q a` is to use `a` in a
Haskell program somehow. `a` can be anything in intermediate monadic
expressions, but when we're about to insert the generated code into a
Haskell source file, there are only four options:

* [Declaration](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Dec)
  `Dec`, which includes the top-level things like function and data type
  definitions. In fact, we would like to be able to generate several
  declarations at a time, so the type that is actually used (and expected by
  the interpolating machinery) is `[Dec]`.

* [Expression](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Exp)
  `Exp`, such as `x + 1` or `\x -> x + 1`. It is probably the most common
  thing to generate.

* [Type](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Type)
  `Type` such as `Int` or `Maybe Int` or just `Maybe`. The type doesn't have
  to be saturated (i.e. may have any kind), so it may be pretty much
  anything one can encounter on the type level.

* [Pattern](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Pat)
  `Pat` that we use for pattern-matching.

I suggest you follow the links in the list above and glance at the
definitions of `Dec`, `Exp`, `Type`, and `Pat`. Note the naming convention:
the data constructors are suffixed with letters that hint about the data
type they belong to: `Dec` constructors end with a “D”, `Exp` constructors
end with an “E”, `Type` constructors end with a “T”, and `Pat` constructors
end with a “P”. This makes it easy to distinguish e.g. an expression
variable `VarE` from a pattern variable `VarP`.

Using the data types, slowly, through pain and suffering, we can indeed
construct an expression:

```haskell
myFunc :: Q Exp
myFunc = do
  x <- newName "x" -- generate a unique variable name, we'll cover names later
  return $ LamE    -- lambda expression
    [VarP x]       -- pattern matching on 'x'
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))
    -- here we have an infix expression: we apply (+) to 'x' and integer
    -- literal 1
```

*(No, I did not construct this manually, but for now let's pretend I'm that
crazy.)* The `TemplateHaskell` language extension enables the special syntax
`$(exp)` where `exp` is an arbitrary expression producing `Q [Dec]`, `Q
Exp`, `Q Type`, or `Q Pat`. This allows to interpolate the generated code
into normal Haskell source code.

For example, I can now use `myFunc` like this:

```haskell
λ> :set -XTemplateHaskell -- don't forget to enable the extension
λ> $(myFunc) 3
4
-- The parentheses are not necessary if 'myFunc' doesn't take any arguments.
-- If it did, it would be something like '$(myFunc arg) 3'. In other words,
-- parentheses are only needed around expressions.
λ> $myFunc 3
4
λ> let f = (* 2) . $myFunc
λ> f 10
22
```

This is called *splicing*. The expression following after the dollar sign is
called a *splice*. A splice can occur in place of an expression, a pattern,
a type, or as a top-level declaration. It's worth noting that declarations
*may* be spliced without the preceding `$` because they live on the
top-level and there is no syntactical ambiguity. `makeLens` from the `lens`
package is a common example:

```haskell
makeLens ''MyRecord -- Yes, we'll get to this quoting style too!
-- the same:
$(makeLens ''MyRecord)
```

Note that the `$` symbol now has an additional meaning and so ambiguity is
possible in some cases. When `$` is used in splices, there must be no space
between `$` and the identifier or opening parenthesis that follows after it.
To use `($)`—the application operator, be sure to add at least one space
between the operator and the following code.

## Limitations of TH

Using TH currently has some limitations:

* *Staging restriction*, which means that inside a splice one can only use
  functions that are already compiled, i.e. defined in other modules, not in
  the same module that contains the splice. This is a pretty nasty
  limitation that makes developers have a separate module for TH code,
  typically called `TH`.

* TH often makes you order your definitions in a particular way. To quote
  the GHC user manual:

    > Top-level declaration splices break up a source file into *declaration
    > groups*. A *declaration group* is the group of declarations created by
    > a top-level declaration splice, plus those following it, down to but
    > not including the next top-level declaration splice. N.B. only
    > top-level splices delimit declaration groups, not expression splices.
    > *The first declaration group in a module includes all top-level
    > definitions down to but not including the first top-level declaration
    > splice.*
    >
    > Each declaration group is mutually recursive only within the group.
    > Declaration groups can refer to definitions within previous groups,
    > but not later ones.

Let see an example of this. Suppose we want to use the `lens` library to
generate some lenses. We could have code like this:

```haskell
data MyRecord = MyRecord         -- <<< first declaration group
  { _myRecordFoo :: Foo          --
  , _myRecordBar :: Bar          --
  , _myRecordBaz :: Baz          --
  }                              --
                                 --
getRecordFoo :: MyRecord -> Foo  --
getRecordFoo = view myRecordFoo  --
                                 --
makeLenses ''MyRecord            -- <<< second declaration group
-- ^ Generates lenses: 'myRecordFoo', 'myRecordBar' and 'myRecordBaz'.
```

Sadly, this code won't compile. The first declaration group includes the
definitions of `MyRecord` and `getRecordFoo`, but not the generated lenses.
This means that `myRecordFoo` is out of scope in `getRecordFoo`.

We could fix this by placing `getRecordFoo` after the `makeLenses
''MyRecord` splice:

```haskell
data MyRecord = MyRecord         -- <<< first declaration group
  { _myRecordFoo :: Foo          --
  , _myRecordBar :: Bar          --
  , _myRecordBaz :: Baz          --
  }                              --
                                 --
makeLenses ''MyRecord            -- <<< second declaration group
                                 --
getRecordFoo :: MyRecord -> Foo  -- can see 'MyRecord' from the
getRecordFoo = view myRecordFoo  -- previous group
```

The first declaration group, consisting of just `MyRecord` now cannot see
`getRecordFoo`, and in case you need it, you'll be forced to move all the
code that uses `getRecordFoo` into the second declaration group, after
`makeLenses ''MyRecord`. In most cases this is not a big deal (after all, in
many languages you must define function *before* you use it), but
nevertheless we're used to the fact that Haskell does not care about
ordering of our definitions, so this limitation is a pity.

## Quotation

As we have seen, the Haskell AST that TH can build and manipulate is not
small and not easy to work with at all. Unfortunately, it's also possible to
produce an AST of a correct shape that does not represent a Haskell program
that compiles. In other words, manual construction of AST is tedious and
error-prone.

Luckily, there is a way to get AST of arbitrary Haskell code by using
*quotation*. There are four types of quotations that are enabled by the
`TemplateHaskell` language extension:

Thing produced | Quotation syntax | Type
---------------|------------------|----------
Declaration    | `[d| … |]`       | `Q [Dec]`
Expression     | `[e| … |]`       | `Q Exp`
Type           | `[t| … |]`       | `Q Type`
Pattern        | `[p| … |]`       | `Q Pat`

Indeed, we need several different quoters because the same code may mean
different things is different contexts, for example:

```haskell
λ> runQ [e| Just x |] -- an expression
AppE (ConE GHC.Base.Just) (UnboundVarE x)
λ> runQ [p| Just x |] -- a pattern
ConP GHC.Base.Just [VarP x_0]
```

Since most of the time we work with expressions, the more lightweight quote
syntax `[| … |]` is equivalent to `[e| … |]`:

```haskell
λ> runQ [| Just x |] -- an expression again
AppE (ConE GHC.Base.Just) (UnboundVarE x)
```

Not only quotation can be used to quickly discover representation of a piece
of Haskell code, it can be used in place of manually constructed ASTs:

```haskell
myFunc :: Q Exp
myFunc = [| \x -> x + 1 |]
```

I think you'll agree that this version of `myFunc` is shorter and easier to
understand. The most wonderful thing about quoters is that we can actually
use splicing inside them:

```haskell
add2 :: Q Exp
add2 = [| $myFunc . $myFunc |]
```

This way we can write the code we want to generate almost as usual, using
splicing just to vary pieces of code that need to change algorithmicly.

Let's try `add2`:

```haskell
λ> $add2 10
12
λ> runQ add2
InfixE
  (Just (LamE [VarP x_2] -- lambda
        (InfixE (Just (VarE x_2))
                (VarE GHC.Num.+)
                (Just (LitE (IntegerL 1))))))
  (VarE GHC.Base..) -- functional composition
  (Just (LamE [VarP x_3] -- lambda
        (InfixE (Just (VarE x_3))
                (VarE GHC.Num.+)
                (Just (LitE (IntegerL 1))))))
```

It works as intended to our complete satisfaction.

What is that `runQ` thing though? In GHCi we work in the `IO` monad, so it's
natural to assume from the examples above that it should have the type:

```haskell
runQ :: Q a -> IO a
--      ^      ^
--      |      |
-- we have   but we want
--   this      this
```

`runQ` is usually used just to play with TH in GHCi (we'll see the reason
behind this shortly), so for that purpose we can safely assume that it has
this type indeed. If you are a beginner or you just don't want to know
additional (and quite optional) details, just [skip to the next
section](#names) now.

For those who want to dig it further, we can see that things are a bit more
complicated:

```haskell
runQ :: Quasi m => Q a -> m a
```

[`Quasi`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#t:Quasi)
is the type class for monads that provide all the capabilities for
meta-programming we have mentioned in the beginning when we introduced `Q`.
You can click that link and take a look for yourself.

In fact, `Q a` is just an existential wrapper around `Quasi m => m a` under
the hood:

```haskell
newtype Q a = Q { unQ :: forall m. Quasi m => m a }

runQ :: Quasi m => Q a -> m a
runQ (Q m) = m
```

There are two instances of `Quasi` that are visible to users: `Q` and `IO`.
The instance for `Q` is trivial and the instance of `IO` is simply very
limited in functionality: from the numerous methods of `Quasi` it only
supports four: `newName`, `runIO`, `reportError` and `reportWarning`,
throwing exceptions when any other method is called. So `IO` can't be used
to run any non-trivial TH code, only for debugging purposes we have just
seen.

Such definition of `Q` suggests that the authors of TH wanted us to work in
a concrete monad and at the same time they wanted to leave themselves an
opportunity to define the instance of `Quasi` that actually does all the
work somewhere else (it's apparently not for us to see).

## Names

As we know, the same name can refer to different things depending on the
context where it is used. This is why working with names has its own
subtleties we're going to discuss now.

When we generate or manipulate code, we work with two types of names:

* Names that mean something in the current context. *Current context* may be
  the context of meta-program that generates code we're going to splice, or
  it may be the context where we do splicing. In both cases we may want to
  just name a thing that is currently in scope and then do something with
  it.

* Names that do not correspond to anything in current context. For example,
  if we generate a lambda expression, we may want to bind its arguments and
  for that we need such “new” names.

    This second group of names can be divided into two subgroups:

    * Names that can be captured. This means that that after we do splicing
      we end up with generated code that contains *capturable* names that
      can be actually bound or used in the enclosing lexical context.

    * Names that cannot be captured.

First of all, there is the syntax for quoting names of functions and types
(it's also enabled by the `TemplateHaskell` extension):

* To quote a function name, add a single quote in the front of it: `id` →
  `'id`.

* To quote a type, add two single quotes in the front of it: `MyRecord` →
  `''Record`. (I guess this is because quoting of types with one single
  quote is already used in `DataKinds`.)

This method always produces names that refer to the thing that is currently
in scope. We saw this in the example with `makeLenses :: Name -> Q [Dec]`
where we passed it the name of our record `''MyRecord`. Similarly, we saw it
in the first definition of `myFunc`, in the AST for the infix expression
involving the quoted `(+)` function:

```haskell
InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1)))
--                           ^^^^
```

When we defined `myFunc`, `(+)` that comes from `Prelude` was in the scope
and so we were able to refer to it as `'(+)`.

When we use quotation, it works absolutely the same. Every name in a TH
quote is looked up in the current scope. In other words, the scope we're
operating in when we use one of the quotes determines directly what we will
get in the resulting AST:

```haskell
λ> runQ [| x |]
UnboundVarE x
```

`x` is not defined in this GHCi session and I get `UnboundVarE x`. However,
if I define `x` first and then run the same code, the result will be
different:

```haskell
λ> let x = 42
λ> runQ [| x |]
VarE Ghci4.x
```

This `Ghci4.x` is the name of the variable, it is bound, and it cannot be
captured:

```haskell
λ> let withX = it -- 'it' is bound to the result of last evaluated
                  -- expression in GHCi
λ> let x = 99 in $(return withX) -- binding 'x' has no effect on the result
42
```

The quoted Haskell code produces the same AST that the code placed in the
same module\/scope\/context would produce. If we modify the last example so
that the `x` that is bound to `99` is in scope when we quote `x`, we'll get
an expression referring to that `x`:

```haskell
λ> let x = 99 in $( [| x |] )
99
```

Even though the quotes lookup everything from the current scope, it does not
mean that new names cannot be generated this way, they can be:

```haskell
λ> runQ [| \x -> x + 1 |]
LamE [VarP x_4]
     (InfixE (Just (VarE x_4))
             (VarE GHC.Num.+)
             (Just (LitE (IntegerL 1))))
```

This `x_4` name was generated automatically for us. This is the same sort of
name we introduced with the `newName :: String -> Q Name` function in the
first implementation of `myFunc`. It's new and it cannot be captured.

One way to introduce a capturable name is via the `mkName :: String -> Name`
function:

```haskell
λ> runQ [| $(varE (mkName "x")) + 1 |]
InfixE (Just (VarE x)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
λ> let xPlus1 = it
λ> let x = 99 in $(return xPlus1) -- value of variable named 'x' influences
                                  -- the result of evaluation
100
```

The
[`Language.Haskell.TH.Lib`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Lib.html)
module contains helper functions that take and return AST values in the `Q`
monad, which sometimes helps produce shorter code, because these helpers
compose well with quotation and splicing. Here we used `varE :: Name -> Q
Exp` instead of `VarE :: Name -> Exp`.

Another way to introduce a capturable name is apparently by using an unbound
name in a quote:

```haskell
λ> withZ <- runQ [| z + 1 |]
λ> withZ
InfixE (Just (UnboundVarE z)) (VarE GHC.Num.+) (Just (LitE (IntegerL 1)))
λ> let z = 100 in $(return withZ)
101
```

But this approach seems quite fragile to my taste. (What if we later define
`z` somewhere in the same module?)

Capturable names are sometimes useful. For example, the
[`hamlet`](https://hackage.haskell.org/package/hamlet) template system
allows to use this syntax `#{name}` to refer to values in a template. The
template then generates Haskell code where such names come out as capturable
names, so they can be bound. The resulting effect is that values that are
bound in the context where a template is used can be accessed in templates,
which is pretty cool.

## Retrieving information about things

Now that we know a little about names, we can go on to learn how to lookup
information about named things.

There are quite a few “reifying” functions that allow to do that:

* [`reify :: Name -> Q
  Info`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reify)
  is the most commonly used one. It allows to look up general information
  [`Info`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Info)
  about a thing.

* [`extsEnabled :: Q
  [Extension]`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:extsEnabled)
  returns the list of all enabled language extensions at the splicing site.

* [`isExtEnabled :: Extension -> Q
  Bool`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:isExtEnabled)
  allows to check whether a particular language extension is enabled.

* [`reifyInstances :: Name -> [Type] ->
  `](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyInstances)
  returns a list of visible instances of `Name` (type class name) for types
  `[Type]`.

* There are more of them, for more rare use cases:
  [`reifyFixity`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyFixity),
  [`reifyRoles`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyRoles),
  [`reifyAnnotations`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyAnnotations),
  [`reifyConStrictness`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH.html#v:reifyConStrictness).

Reifying functions take `Name`s, but there is one more question to ask about
a name: does it name a thing that is in scope when we write our meta-program
or does it name a thing that is in scope when we execute the meta-program at
splicing site? So far the names were looked up in the scope of meta-program,
not in the scope of splicing site. If we need to access a thing from the
latter scope, there are two ways to do that:

* We could take the name as an argument, like the `makeLenses` function
  does. In that case we construct the `Name` at the splicing site (e.g. by
  quoting it) and it ends up naming a thing from that scope.

* We can use the `lookupTypeName` and `lookupValueName` functions, which
  lookup names at the splicing site.

Note the signatures of these functions:

```haskell
lookupTypeName  :: String -> Q (Maybe Name)
lookupValueName :: String -> Q (Maybe Name)
```

`Name` itself cannot change meaning depending on context. When you have a
`Name`, it names one specific thing, always. So it makes sense that
`lookupValueName` and `lookupTypeName` take `String`s and return `Name`s.

Let's now use the reifying functions for something more practical.

## Example 1: instance generation

The example is going to be a little contrived. The aim is to show how all
the tools we have seen so far work together, but without throwing a “wall of
code” at the reader.

Suppose we want to know how many different non-bottom values inhabit a type.
We could start first without TH like this:

```haskell
class Countable a where
  count :: Proxy a -> Integer
```

The `Proxy` is needed here because methods of a type class cannot lack a
“connection” to the type that is an instance of the type class. In other
words, there must be an `a` somewhere in the signature of `count`. We are
not interested in a particular value of the type `a`, but still we must
clarify which `a` we mean by passing `Proxy a`. If this doesn't make any
sense, it's OK, you can still continue reading.

How do we write the instances? It looks like we could leverage the existing
`Enum` and `Bounded` type classes, which already solve the problem, but only
for a limited set of types. If a type is an instance of both `Enum` and
`Bounded`, then we can define `count` like so:

```haskell
instance (Enum a, Bounded a) => Countable a where
  count Proxy = fromIntegral $
    1 + fromEnum (maxBound :: a) - fromEnum (minBound :: a)
```

This is not going to work though if we want to be able to define instances
of `Countable` for more complex product and sum types. The reason is that
the instance above already defines `Countable` for any `a` possible, it's
just we then have this additional constraint `(Enum a, Bounded a)` added. In
other words, when Haskell searches for an instance, it only looks at the
right-hand side ignoring the constraints, and so `a` matches everything.

We could do better by writing a TH helper that handles two cases:

* If a type is an instance of `Enum` and `Bounded`, then generate the
  instance like the one we have just seen, but for a concrete type.

* Otherwise analyze the type to figure out if it's a product or sum type (or
  indeed something mixed) and use arithmetic to calculate the number of
  non-bottom values in the assumption that `Countable` is defined for the
  types inside such a composite type.

Let's solve the first part of the task:

```haskell
deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count Proxy = fromIntegral $
      1 + fromEnum (maxBound :: $a) - fromEnum (minBound :: $a)
  |]
  where
    a = conT name
```

`conT` is just `return . ConT` and `ConT` is a data constructor of `Type`
that represents a type constructor. Quoting and splicing go well together
and defining `deriveCountableSimple` was indeed simple.

To try this out, I derived a few instances this way:

```haskell
deriveCountableSimple ''Bool
deriveCountableSimple ''Word8
deriveCountableSimple ''Char
```

We can try it now:

```haskell
λ> count (Proxy :: Proxy Bool)
2
λ> count (Proxy :: Proxy Word8)
256
λ> count (Proxy :: Proxy Char)
1114112
```

Looks reasonable. Let's handle the second case:

```haskell
deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count Proxy = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    countTypeE t = [| count (Proxy :: Proxy $(return t)) |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x
```

Let's see what is going on:

* We first `reify` the given `name` and get information about it. We are
  only interested in type constructors, so we pattern on `TyConI`. Further,
  from the information that `TyConI` contains we're only interested in the
  collection of data constructors `cons'`.

* For every constructor we take every sub-type and construct an expression
  that counts the number of values that inhabit the type, this is done in
  `countTypeE`.

* For `NormalC` and `RecC` constructors we just multiple expressions we've
  got for the individual types, this is done in `handleCon` (we handle
  product type this way).

* Finally, we add together the expressions for all data constructors—this
  way we handle sum types.

Let's play with it now:

```haskell
data Foo
  = Foo Bool Bool

deriveCountableComposite ''Foo
```

```haskell
λ> count (Proxy :: Proxy Foo)
4 -- = 2 + 2
```

This makes sense, let's see if it can handle a sum type:

```haskell
data Foo
  = Foo Bool Bool
  | Bar Word8 Bool

deriveCountableComposite ''Foo
```

```haskell
λ> count (Proxy :: Proxy Foo)
516 -- = 2 * 2 + 256 * 2
```

It works! Let's combine the two cases into the single `deriveCountable`
helper:

```haskell
deriveCountable :: Name -> Q [Dec]
deriveCountable name = do
  let ts = [ConT name]
  hasEnum    <- isInstance ''Enum    ts
  hasBounded <- isInstance ''Bounded ts
  if hasEnum && hasBounded
    then deriveCountableSimple    name
    else deriveCountableComposite name
```

Done, now we can use `deriveCountable` in both cases and it'll figure out
what to do on its own.

## Viewing the generated code

Sometimes it is helpful to be able to see the code we're generating at
splice sites. GHC [allows to do
that](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#viewing-template-haskell-generated-code)
with the `-ddump-splices` flag. Stack seems to eat that output though, so I
had to add also `-ddump-to-file` and search for a file with the suffix
`-splices` in the `.stack-work/dist` directory.

Here is what I've got:

```haskell
src/Main.hs:22:1-22: Splicing declarations
    deriveCountable ''Bool
  ======>
    instance Countable Bool where
      count Proxy
        = (fromIntegral
             $ ((1 + (fromEnum (maxBound :: Bool)))
                  - (fromEnum (minBound :: Bool))))

src/Main.hs:23:1-23: Splicing declarations
    deriveCountable ''Word8
  ======>
    instance Countable Word8 where
      count Proxy
        = (fromIntegral
             $ ((1 + (fromEnum (maxBound :: Word8)))
                  - (fromEnum (minBound :: Word8))))

src/Main.hs:24:1-22: Splicing declarations
    deriveCountable ''Char
  ======>
    instance Countable Char where
      count Proxy
        = (fromIntegral
             $ ((1 + (fromEnum (maxBound :: Char)))
                  - (fromEnum (minBound :: Char))))

src/Main.hs:25:1-21: Splicing declarations
    deriveCountable ''Foo
  ======>
    instance Countable Foo where
      count Proxy
        = (((count (Proxy :: Proxy Bool))
              * ((count (Proxy :: Proxy Bool)) * 1))
             + (((count (Proxy :: Proxy Word8))
                   * ((count (Proxy :: Proxy Bool)) * 1))
                  + 0))
```

*(All the mess will be optimized away and we'll end with just plain numbers,
don't worry.)*

This is a useful debugging tool.

## Lifting Haskell values to TH expressions

So far we have been constructing expressions manually or by using quotation.
What about getting an expression that “re-constructs” a value we already
have? This could be used to deliver values generated in the `Q` monad to the
outside world.

The solution comes naturally in the form of the
[`Lift`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Syntax.html#t:Lift)
type class:

```haskell
class Lift t where
  lift :: t -> Q Exp
```

`lift` takes a value and returns an expression that re-constructs it. We
could define some instances like so:

```haskell
instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Char where
  lift x = return (LitE (CharL x))
```

The `template-haskell` package defines `Lift` instances for all common data
types. GHC also knows how to define `Lift` for new types. It is enough to
enable the `DeriveLift` language extension and we're all done (example from
the Haddocks):

```haskell
{-# LANGUAGE DeriveLift #-}

module Main (main) where

import Language.Haskell.TH.Syntax

data Bar a
  = Bar1 a (Bar a)
  | Bar2 String
  deriving Lift
```

However, sometimes we want to lift values of types that do not define or
derive `Lift` and so we risk introducing [*orphan
instances*](https://wiki.haskell.org/Orphan_instance). Even worse, even if
we were OK with defining orphan instances for things like `Text` and
`ByteString`, we can't (at least not by using the `DeriveLift` extension):

```haskell
{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main (main) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)

deriving instance Lift Text
```

This produces the following compilation error:

```
• Can't make a derived instance of ‘Lift Text’:
    The data constructors of ‘Text’ are not all in scope
      so you cannot derive an instance for it
• In the stand-alone deriving instance for ‘Lift Text’
```

The `text` and `bytestring` libraries do not expose the constructors, so
`DeriveLift` refuses to do its magic.

Not everything is lost though. It so happens that the `Data` class provides
enough introspection capabilities for `lift`ing, so TH has the following
helper:

```haskell
liftData :: Data a => a -> Q Exp
```

If something is an instance of the `Data` type class, we can just lift it
with the `liftData` function. This is great, no orphan instances are
necessary, let's try it out.

In one module we define a function that takes `Text` and generates an
expression that appends `"!"` to it:

```haskell
foo :: Text -> Q Exp
foo txt = [| $(liftData txt) <> "!" |]
```

This even compiles. In another module we try to use it:

```haskell
main :: IO ()
main = TIO.putStrLn $(foo "Here goes the text")
```

And this is when it blows up:

```
• Can't find interface-file declaration for variable Data.Text.Internal.pack
    Probable cause: bug in .hi-boot file, or inconsistent .hi file
    Use -ddump-if-trace to get an idea of which file caused the error
```

*This is scary.* I'll save your time and we won't go into the internals of
`liftData` here. It suffices to say that `liftData` uses
[`toConstr`](https://hackage.haskell.org/package/base/docs/Data-Data.html#v:toConstr)
internally which returns `pack` for `Text`. The rest of the machinery
apparently expects this `pack` function to be in the same module the data
type is defined, `Data.Text.Internal`, but `pack` is defined in `Data.Text`,
thus we get the error.

`Text` is a pretty common type, how do we lift it? The first step would be
to define a lifting function manually:

```haskell
liftText :: Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
```

Here we first `lift` a `String` and then apply `T.pack` (assuming we have
imported `Data.Text` qualified as `T`). Now we could use `liftText` directly
to lift a `Text` value, but what if it's inside a data structure? The
general solution to lifting involving `Text` seems to be the following:

```haskell
foo :: Text -> Q Exp
foo txt = [| $e <> "!" |]
  where
    e = dataToExpQ (fmap liftText . cast) txt
```

This
[`dataToExpQ`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#v:dataToExpQ)
function in combination with
[`cast`](https://hackage.haskell.org/package/base/docs/Data-Typeable.html#v:cast)
(that comes from `Data.Typeable`) does the trick.

Let's see what `dataToExpQ` does:

```haskell
dataToExpQ :: Data a => (forall b. Data b => b -> Maybe (Q Exp)) -> a -> Q Exp
```

`dataToExpQ` works just like `liftData` but it allows to overwrite lifting
for the values for which `forall b. Data b => b -> Maybe (Q Exp)` returns
`Just`. Don't be afraid of the rank-2-type here. The `forall` quantification
of `b` inside that function in parentheses means that the function literally
works *for all* `b`, but the choice of `b` is made not at the call site of
`dataToExpQ`, but at the call site of this `forall b. Data b => b -> Maybe
(Q Exp)` function. Similarly, the choice of `a` is made at the call site of
`dataToExpQ` which also has an implicit `forall a.` at the beginning of its
type signature. See the symmetry? (If you're a beginner, you may not
understand rank-N-types immediately, in that case don't despair. Still, I
tried my best to explain.)

`cast` performs type-safe casting between two types:

```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

Here, if `a` has the same type representation (which the `Typeable` type
class allows to query via the `typeRep` function) as `b`, we get a `b` value
inside `Just`.

We can use `cast` here because `Typeable` is a superclass of `Data`:

```haskell
class Typeable a => Data a where
  -- …
```

If something from the above is not clear, it's OK. Just grab this trick and
use it next time you need to lift data that contains `Text` or similar
types, you should be OK.

One more thing: you need at least GHC 8 to use `dataToExpQ`.

## Example 2: creating refined values at compile time

Now we are prepared to write a TH helper that allows to construct values of
refined types at compile time turning invalid inputs into compilation
errors.

Our practical example will be taken (although in a simplified form) from an
existing library I wrote, it's called
[`modern-uri`](https://hackage.haskell.org/package/modern-uri). In the
library we have a function that takes `Text` representing a URI as input and
outputs `Maybe URI`:

```haskell
data URI = URI
  { uriScheme    :: Maybe (RText 'Scheme)
  , uriAuthority :: Either Bool Authority
  , uriPath      :: [RText 'PathPiece]
  , uriQuery     :: [QueryParam]
  , uriFragment  :: Maybe (RText 'Fragment)
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

mkURI :: Text -> Maybe URI
```

`Nothing` means that the input was not a correct `URI`. Our task thus
becomes:

* Run `mkURI` at compile time.
* If the returned value is `Nothing`, signal a compile-time error. Otherwise
  lift the entire `URI` data structure we have parsed.

By now we know how to tackle every part of the task. Here is the complete TH
helper:

```haskell
mkURI' :: Text -> Q Exp
mkURI txt =
  case mkURI txt of
    -- Instead of 'fail' we could also use 'reportError'. There is also
    -- 'reportWarning' just in case you ever want to report warnings.
    Nothing -> fail "The input does not contain a valid URI"
    Just uri -> dataToExpQ (fmap liftText . cast) uri -- the same trick
```

We could finish the section on this, but there is a nicer way, syntax-wise,
to make use of such a validating helper. The feature we're going to explore
is called quasi-quotes. It turns out that TH allows us to define our own
custom quasi-quoters that are like `d`, `e`, `t`, and `p` we saw earlier.

Defining a quasi-quoter is easy. It is enough to import the
[`QuasiQuoter`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter)
data type from `Langauge.Haskell.TH.Quote`:

```haskell
data QuasiQuoter = QuasiQuoter
  { quoteExp  :: String -> Q Exp
  , quotePat  :: String -> Q Pat
  , quoteType :: String -> Q Type
  , quoteDec  :: String -> Q [Dec]
  }
```

A quasi quoter may be used in the four familiar contexts, so it has four
corresponding functions that take the `String` from the quote and return
something to splice.

Usually we only want to use a quasi quoter in one context, so the others are
either omitted or `undefined` or replaced by `error`s. These failures will
be at compile time, so it's OK to do the following:

```haskell
uri :: QuasiQuoter
uri = QuasiQuoter
  { quoteExp  = \str ->
      case mkURI (T.pack str) of
        Nothing -> fail "The input does not contain a valid URI"
        Just x  -> dataToExpQ (fmap liftText . cast) x
  , quotePat  = error "Usage as a parttern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec  = error "Usage as a declaration is not supported" }
```

I like to use `error` with helpful messages instead of `undefined` some
people use.

To use our new quasi-quoter we need to enable the `QuasiQuotes` language
extension:

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import TH
import qualified Data.Text.IO as TIO
import qualified Text.URI     as URI

main :: IO ()
main = TIO.putStrLn (URI.render x)
  where
    x = [uri| https://markkarpov.com |]
```

If the string inside the `uri` quasi-quoter is not a valid URI, the
compilation will fail. One more type of error is caught at complie time!

## Running `IO` in `Q`

Using `IO` in TH generally makes the compilation process dependent on
external conditions that may contribute to unexpected compilation failures.
Thus it makes sense to think twice before running `IO` from TH.

That said, the function that lifts `IO` into `Q` is called simply
[`runIO`](https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:runIO):

```haskell
runIO :: IO a -> Q a
```

Needless to say, one can do a lot with such a tool, for good or for evil.
One example of a good use is the
[`gitrev`](https://hackage.haskell.org/package/gitrev) package which allows
to insert information about active branch and last commit of code that is
being compiled. It works by literally running the `git` executable at
complie time and then lifting the fetched data.

A far more common use case for `IO` in `Q` is reading from files. In that
case compilation usually starts to depend on contents of the file being
read, and so it's a good idea to tell GHC that changes in that file should
cause re-compilation of the module where the file-reading TH helper is
spliced. This is done via the
[`addDependentFile`](https://hackage.haskell.org/package/template-haskell-2.12.0.0/docs/Language-Haskell-TH-Syntax.html#v:addDependentFile)
function:

```haskell
addDependentFile :: FilePath -> Q ()
```

(Why it lives in `Language.Haskell.TH.Syntax` and not in
`Language.Haskell.TH` is beyond me, what has it to do with syntax?)

## Example 3: the `file-embed` package

Finally, in our last example, let's re-implement (in a simplified form) the
popular package
[`file-embed`](https://hackage.haskell.org/package/file-embed), which allows
to load contents of a file and splice them as a `IsString a => a` value (the
type of string literals in Haskell in the presence of the
`OverloadedStrings` language extension.)

If we have this in `TH.hs` file:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module TH
  ( embedFile )
where

import Data.String (IsString (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

embedFile :: FilePath -> Q Exp
embedFile path = do
  str <- runIO (readFile path)
  addDependentFile path
  -- We lift the 'String' literal to the polymorphic 'IsString a => a' form.
  [| fromString str |]
```

Then we can use it like this:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import TH
import qualified Data.Text.IO as TIO

main :: IO ()
main = TIO.putStrLn $(embedFile "src/Main.hs")
```

The program outputs its own source code. No `src/Main.hs` file is expected
to exist when we run the binary, the source code is dumped into the
executable itself. Note how the `IsString a => a` value was instantiated to
`Text` automatically because `Text` is an instance of `IsString`.

## Conclusion

This is by no means a complete TH tutorial, some more rarely used tools and
functions were not covered. Still, the tutorial should get you on the speed
and give a taste of what meta-programming in Haskell looks like. For further
information refer directly to the Haddocks:

https://hackage.haskell.org/package/template-haskell

Good luck!
