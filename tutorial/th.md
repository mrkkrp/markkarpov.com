---
title: Template Haskell tutorial
desc: The tutorial explains how to use Template Haskell for metaprogramming in Haskell.
date:
  published: December 24, 2017
  updated: August 3, 2026
---

```toc
```

This tutorial aims to introduce the reader to *Template Haskell* (TH)—the GHC
extension that adds metaprogramming capabilities to Haskell. I assume some
familiarity with the language, perhaps at a beginner or intermediate level,
although these terms are rather nebulous and subjective. To express the
prerequisites in a more tangible form: if you know what a monad is, you should
probably be OK.

TH has the reputation of being an expert-level topic that mere mortals are
not prepared to comprehend. I don't think this is so. The ideas behind TH are
simple and make sense, while the specific details can always be looked up in
the Haddocks.

The tutorial cannot possibly cover every use of TH, so it is structured in
such a way that we only get to see the most common, conventional, and benign
uses of this feature.

## Motivation

Perhaps one of the main difficulties with TH is deciding whether it is the
best solution to the problem at hand. Writing code that generates code is
generally considered a sign that the tools of expression provided by the
language, or the programmer's imagination, have failed to address a particular
problem, so that metaprogramming has to be used as a last resort. Whether or
not that is fair, TH is quite popular, and knowing your way around it is a
valuable skill that lets you do things that often cannot be achieved
otherwise.

Let's list some uses of TH:

* *Automatic deriving of type class instances* is still perhaps the most
  common use case for TH. Even though the same problem can often be
  [addressed by generics][generics], generics are known to make compilation
  times longer than TH-based solutions, so TH remains the preferred method of
  automatic instance derivation in libraries like `aeson` and `lens`.

* *Creation of TH DSLs* that are integrated into systems built in Haskell.
  Examples of such DSLs are the model-declaration language used in
  [`persistent`][persistent] and various other mini-languages used in the
  [`yesod`][yesod] web framework.

* *Compile-time construction of values of refined types*, which turns invalid
  inputs into compilation failures.

* *Compile-time loading and processing of data from external files*, which is
  very useful sometimes. Even though this involves running `IO` during
  compilation, it's a relatively innocent use of that dangerous feature.

Reasons not to use TH:

* TH helpers are often viewed as black boxes that do “magic”. It is not clear
  at all what a thing of type `Q [Dec]` does; it might do anything (we will
  see that any code that generates declarations has the same `Q [Dec]` type,
  no matter what sort of declarations it generates). Documentation becomes the
  main source of information about the semantics of TH code.

* TH imposes restrictions on where TH functions may be defined, and sometimes
  also on how definitions must be ordered in files where TH functions are
  used.

## The `Q` monad

Generation of code requires certain features to be available to us:

* The ability to generate new unique names that cannot be captured.

* The ability to retrieve information about a thing by its name. Usually we
  want to know about functions and types, but there are also ways to learn
  about a module, get the collection of instances of a particular type class,
  etc.

* The ability to put and get some custom state that is then shared by all TH
  code in the same module.

* The ability to run `IO` during compilation, so that we can, for example,
  read something from a file.

These features are usually achieved through *monads* in Haskell, and so it
should not come as a surprise that there is a special monad called `Q`
(short for “quotation”) that hosts all the functions provided by TH.

## Splicing

The only purpose of having a value of type `Q a` is to use `a` in a Haskell
program somehow. `a` can be anything in intermediate monadic expressions, but
when we're about to insert the generated code into a Haskell source file,
there are only five options:

* [Declaration][dec] `Dec`, which includes top-level things like function and
  data type definitions. In fact, we would like to be able to generate several
  declarations at a time, so the type that is actually used (and expected by
  the interpolating machinery) is `[Dec]`.

* [Expression][exp] `Exp`, such as `x + 1` or `\x -> x + 1`. It is probably
  the most common thing to generate.

* [Typed expression][code], produced by a *typed quotation* and represented
  by the `Code` type. It is essentially an expression `Exp` carrying a phantom
  type tag corresponding to the type of the expression inside. For example,
  `Code Q Int` means that the expression evaluates to an `Int`. (In older code
  and pre-9.0 GHCs you'll see the type `Q (TExp Int)` instead; more on this in
  the section on [typed expressions](#typed-expressions).)

* [Type][type] `Type`, such as `Int`, `Maybe Int`, or just `Maybe`. The type
  doesn't have to be saturated (i.e. it may have any kind), so it may be
  pretty much anything one can encounter at the type level.

* [Pattern][pat] `Pat`, which we use for pattern matching.

I suggest you follow the links in the list above and glance at the definitions
of `Dec`, `Exp`, `TExp`, `Type`, and `Pat`. Note the naming convention: the
data constructors are suffixed with letters that hint at the data type they
belong to: `Dec` constructors end with a “D”, `Exp` constructors end with an
“E”, `Type` constructors end with a “T”, and `Pat` constructors end with a
“P”. This makes it easy to distinguish, for example, an expression variable
`VarE` from a pattern variable `VarP`.

Using the data types, slowly but surely, we can indeed construct an
expression:

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

The `TemplateHaskell` language extension enables the special syntax `$(exp)`
where `exp` is an arbitrary expression producing `Q [Dec]`, `Q Exp`, `Q
Type`, or `Q Pat`. This allows us to interpolate the generated code into
normal Haskell source code.

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

This is called *splicing*. The expression following the dollar sign is called
a *splice*. A splice can occur in place of an expression, a pattern, or a
type, or as a top-level declaration. It's worth noting that declarations *may*
be spliced without the preceding `$`, because they live at the top level and
there is no syntactic ambiguity. `makeLenses` from the `lens` package is a
common example:

```haskell
makeLenses ''MyRecord -- Yes, we'll get to this quoting style too!
-- the same:
$(makeLenses ''MyRecord)
```

Note that the `$` symbol now has an additional meaning, and so ambiguity is
possible in some cases. When `$` is used in splices, there must be no space
between `$` and the identifier or opening parenthesis that follows it. To use
`($)`—the application operator—be sure to add at least one space between the
operator and the following code.

## Limitations of TH

Using TH currently has some limitations:

* The *staging restriction*, which means that inside a splice one can only use
  functions that are already compiled, i.e. defined in other modules, not in
  the same module that contains the splice. This is a pretty nasty limitation
  that forces developers to keep a separate module for TH code, typically
  called `TH`.

* TH often makes you order your definitions in a particular way. To quote
  the GHC user manual:

  > Top-level declaration splices break up a source file into *declaration
  > groups*. A *declaration group* is the group of declarations created by a
  > top-level declaration splice, plus those following it, down to but not
  > including the next top-level declaration splice. N.B. only top-level
  > splices delimit declaration groups, not expression splices. *The first
  > declaration group in a module includes all top-level definitions down to
  > but not including the first top-level declaration splice.*
  >
  > Each declaration group is mutually recursive only within the group.
  > Declaration groups can refer to definitions within previous groups, but
  > not later ones.

Let's see an example of this. Suppose we want to use the `lens` library to
generate some lenses. We might have code like this:

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

The first declaration group, consisting of just `MyRecord`, now cannot see
`getRecordFoo`, and should you need it, you'll be forced to move all the code
that uses `getRecordFoo` into the second declaration group, after `makeLenses
''MyRecord`. In most cases this is not a big deal (after all, in many
languages you must define a function *before* you use it), but nevertheless
we're used to the fact that Haskell does not care about the ordering of our
definitions, so this limitation is a pity.

## Quotation

As we have seen, the Haskell AST that TH can build and manipulate is neither
small nor easy to work with. Unfortunately, it's also possible to produce an
AST of a correct shape that does not represent a Haskell program that
compiles. In other words, manual construction of an AST is tedious and
error-prone.

Luckily, there is a way to obtain the AST of arbitrary Haskell code: use
*quotation*. The `TemplateHaskell` language extension enables five kinds of
quotation:

Thing produced   | Quotation syntax | Type
-----------------|:----------------:|:----------:
Declaration      | `[d| … |]`       | `Q [Dec]`
Expression       | `[e| … |]`       | `Q Exp`
Typed expression | `[|| … ||]`      | `Code Q a`
Type             | `[t| … |]`       | `Q Type`
Pattern          | `[p| … |]`       | `Q Pat`

We'll return to `Code` in the section on [typed
expressions](#typed-expressions).

We need several different quoters because the same code may mean different
things in different contexts, for example:

```haskell
λ> runQ [e| Just x |] -- an expression
AppE (ConE GHC.Internal.Maybe.Just) (UnboundVarE x)
λ> runQ [p| Just x |] -- a pattern
ConP GHC.Internal.Maybe.Just [] [VarP x_0]
```

Since most of the time we work with expressions, the more lightweight quote
syntax `[| … |]` is equivalent to `[e| … |]`:

```haskell
λ> runQ [| Just x |] -- an expression again
AppE (ConE GHC.Internal.Maybe.Just) (UnboundVarE x)
```

Quotation can be used not only to quickly discover the representation of a
piece of Haskell code, but also in place of manually constructed ASTs:

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
splicing just to vary the pieces of code that need to change algorithmically.
Note, though, that splicing *declarations* inside *declaration* quoters is
still not supported.

Let's try `add2`:

```haskell
λ> $add2 10
12
λ> runQ add2
InfixE
  (Just (LamE [VarP x_2] -- lambda
        (InfixE (Just (VarE x_2))
                (VarE GHC.Internal.Num.+)
                (Just (LitE (IntegerL 1))))))
  (VarE GHC.Internal.Base..) -- functional composition
  (Just (LamE [VarP x_3] -- lambda
        (InfixE (Just (VarE x_3))
                (VarE GHC.Internal.Num.+)
                (Just (LitE (IntegerL 1))))))
```

It seems to work.

## Typed expressions

Quotation for typed expressions is a bit special: it is the only way to
create a *typed expression*, i.e. it's the introduction form for the `Code`
type. This way the compiler can ensure that the phantom type always
corresponds to what is inside. A typed quotation `[|| … ||]` has the type
`Code Q a`, where `a` is the type of the quoted expression. The
`template-haskell` package also provides a convenient type synonym `CodeQ a
= Code Q a`.

*A bit of history: before GHC 9.0, typed quotation produced a value of type
`Q (TExp a)` instead, where [`TExp`][texp] is a thin wrapper carrying a
phantom type over an ordinary `Exp`. GHC 9.0 replaced `Q (TExp a)` with
`Code Q a` (per the [“Make `Q (TExp a)` into a newtype”][code-texp-proposal]
proposal). `TExp` still exists, and `Code` is defined roughly as `newtype Code
m a = Code { examineCode :: m (TExp a) }`; the functions
`examineCode`/`liftCode` and `unTypeCode` convert between the old and new
representations if you ever need to. If you have to support both GHC 8 and
GHC 9, the [`th-compat`][th-compat] package smooths over the difference.*

For example, let's rewrite `myFunc` using typed quotation:

```haskell
myFuncTyped :: Code Q a
myFuncTyped = [|| \x -> x + 1 ||]
```

I left `a` there on purpose, to check what GHC will propose as the inferred
type:

> Couldn't match type `a` with `Integer -> Integer`

Thus:

```haskell
myFuncTyped :: Code Q (Integer -> Integer)
myFuncTyped = [|| \x -> x + 1 ||]
```

Returning something polymorphic, however, is not possible out of the box:

```haskell
myFuncTyped :: Code Q (Num a => a -> a)
myFuncTyped = [|| \x -> x + 1 ||]
```

GHC says:

> Illegal qualified type: `Num a => a -> a` \
  Suggested fix: Perhaps you intended to use `ImpredicativeTypes`

*Impredicative polymorphism* is when you try to replace a polymorphic
variable with an expression that itself contains a `forall`. In the case
above, there is an implicit `forall` before the `Num a` constraint, so the
`a` in `Code Q a` would have to be instantiated to a qualified (hence
polymorphic) type—and that's exactly what plain Hindley–Milner forbids.

GHC's suggestion to reach for `ImpredicativeTypes` is a bit of a red herring,
though. Enabling it does silence the “illegal qualified type” complaint, but
the quotation then fails with a different error:

> Couldn't match type: `p0 -> p0` \
  with: `Num a => a -> a`

The reason is that a typed quotation `[|| e ||]` is always checked at type
`Code m τ` where `τ` is a *monotype*: GHC infers a single, ungeneralised
type for the quoted expression rather than a polymorphic scheme. Here it
infers `p0 -> p0` for `\x -> x + 1` (with a lingering `Num p0` constraint),
and no choice of the monotype `p0` can ever equal the polymorphic `Num a =>
a -> a`. That's also why an explicit `:: forall a. Num a => a -> a`
annotation *inside* the brackets doesn't rescue it—there is simply nowhere
for the quoted term to acquire a `forall`. Left to its own devices, GHC
would just default the constraint and settle on the monotype `Integer ->
Integer`. The real lesson, then, is not to make a typed quotation
polymorphic in the first place—give it a concrete, monomorphic type instead.

Further, there is a special syntax for splicing of typed expressions. Let's
try to write a typed version of `add2`:

```haskell
add2Typed :: Code Q (Integer -> Integer)
add2Typed = [|| $$myFuncTyped . $$myFuncTyped ||]
```

Normal splices cannot be used in quotations for typed expressions, and vice
versa—typed splices cannot be used in quotations for untyped expressions.
This is why we simply had to start by writing a typed version of `myFunc`!

When using the double dollar syntax, the compiler will make sure that we're
splicing our typed expression in a correct context, so there won't be any type
errors.

Apart from splicing, there is another way to eliminate a typed
expression—just use `unTypeCode` to recover the underlying untyped `Exp`:

```haskell
unTypeCode :: Code Q a -> Q Exp
```

A bit more information about typed expressions can be found in [this blog
post][typed-th].

## A few words about `runQ`

What is that `runQ` thing, though? In GHCi we work in the `IO` monad, so from
the examples above it's natural to assume that it should have the type:

```haskell
runQ :: Q a -> IO a
--      ^      ^
--      |      |
-- we have   but we want
--   this      this
```

`runQ` is usually used just to play with TH in GHCi (we'll see the reason
behind this shortly), so for that purpose we can indeed safely assume that it
has this type. If you are a beginner, or you just don't want to know
additional (and quite optional) details, [skip to the next section](#names)
now.

For those who want to dig further, we can see that things are a bit more
complicated:

```haskell
runQ :: Quasi m => Q a -> m a
```

[`Quasi`][quasi] is the type class for monads that provide all the
capabilities for metaprogramming we mentioned at the beginning, when we
introduced `Q`. You can click that link and take a look for yourself.

In fact, `Q a` is just a wrapper around `Quasi m => m a` under the hood:

```haskell
newtype Q a = Q { unQ :: forall m. Quasi m => m a }

runQ :: Quasi m => Q a -> m a
runQ (Q m) = m
```

There are two instances of `Quasi` that are visible to users: `Q` and `IO`.
The instance for `Q` is trivial, and the instance for `IO` is simply very
limited in functionality: of the numerous methods of `Quasi`, it only supports
four—`newName`, `runIO`, `reportError`, and `reportWarning`—throwing an
exception when any other method is called. So `IO` can't be used to run any
non-trivial TH code, only for the debugging purposes we have just seen.

This definition of `Q` suggests that the authors of TH wanted us to work in a
concrete monad and at the same time wanted to leave themselves the option of
defining the instance of `Quasi` that actually does all the work somewhere
else (it's apparently not for us to see).

`Quasi` collects the *full* metaprogramming interface—name generation,
reification, `runIO`, and so on. Since GHC 9.0 there is also a much smaller
[`Quote`][quote] class sitting underneath it:

```haskell
class Monad m => Quote m where
  newName :: String -> m Name
```

`Quote` provides just enough to *desugar quotations* (all a quotation strictly
needs is the ability to invent fresh names). Up to now we've written the types
of quotations as `Q Exp` and `Code Q a` for simplicity, but the real,
overloaded types you'll find in the Haddocks are `Quote m => m Exp` and `Quote
m => Code m a`, tied not to `Q` but to any `Quote` instance. `Q` is an
instance of both `Quote` and `Quasi`, so in practice you can keep writing
everything in terms of `Q` and never think about `Quote` again; knowing about
it just makes those more general signatures less surprising.

## Names

As we know, the same name can refer to different things depending on the
context in which it is used. This is why working with names has its own
subtleties, which we're going to discuss now.

When we generate or manipulate code, we work with two types of names:

* Names that mean something in the current context. The *current context* may
  be the context of the metaprogram that generates the code we're going to
  splice, or it may be the context where we do the splicing. In both cases we
  may just want to name a thing that is currently in scope and then do
  something with it.

* Names that do not correspond to anything in the current context. For
  example, if we generate a lambda expression, we may want to bind its
  arguments, and for that we need such “new” names.

  This second group of names can be divided into two subgroups:

  * Names that can be captured. This means that after we do the splicing we end
    up with generated code that contains *capturable* names that can actually
    be bound or used in the enclosing lexical context.

  * Names that cannot be captured.

First of all, there is the syntax for quoting names of functions and types
(it's also enabled by the `TemplateHaskell` extension):

* To quote a function name, add a single quote in front of it: `id` → `'id`.

* To quote a type, add two single quotes in front of it: `MyRecord` →
  `''MyRecord`. This quoting convention follows from the fact that Haskell has
  different namespaces for values and types, and so we must be able to quote a
  data constructor `'MyRecord` as well as a type constructor `''MyRecord`
  without ambiguity.

This method always produces names that refer to the thing that is currently
in scope. We saw this in the example with `makeLenses :: Name -> Q [Dec]`,
where we passed it the name of our record, `''MyRecord`. Similarly, we saw it
in the first definition of `myFunc`, in the AST for the infix expression
involving the quoted `(+)` function:

```haskell
InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1)))
--                           ^^^^
```

When we defined `myFunc`, the `(+)` that comes from the `Prelude` was in
scope, and so we were able to refer to it as `'(+)`.

When we use quotation, it works exactly the same way. Every name in a TH quote
is looked up in the current scope. In other words, the scope we're operating
in when we use one of the quotes directly determines what we will get in the
resulting AST:

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

This `Ghci4.x` is the name of the variable; it is bound, and it cannot be
captured:

```haskell
λ> let withX = it -- 'it' is bound to the result of last evaluated
                  -- expression in GHCi
λ> let x = 99 in $(return withX) -- binding 'x' has no effect on the result
42
```

The quoted Haskell code produces the same AST that the code placed in the same
module\/scope\/context would produce. If we modify the last example so that
the `x` bound to `99` is in scope when we quote `x`, we'll get an expression
referring to that `x`:

```haskell
λ> let x = 99 in $( [| x |] )
99
```

Even though the quotes look everything up in the current scope, it does not
mean that new names cannot be generated this way:

```haskell
λ> runQ [| \x -> x + 1 |]
LamE [VarP x_4]
     (InfixE (Just (VarE x_4))
             (VarE GHC.Internal.Num.+)
             (Just (LitE (IntegerL 1))))
```

This `x_4` name was generated automatically for us. This is the same sort of
name we introduced with the `newName :: String -> Q Name` function in the
first implementation of `myFunc`. It's new, and it cannot be captured.

One way to introduce a capturable name is via the `mkName :: String -> Name`
function:

```haskell
λ> runQ [| $(varE (mkName "x")) + 1 |]
InfixE (Just (VarE x)) (VarE GHC.Internal.Num.+) (Just (LitE (IntegerL 1)))
λ> let xPlus1 = it
λ> let x = 99 in $(return xPlus1) -- value of variable named 'x' influences
                                  -- the result of evaluation
100
```

The [`Language.Haskell.TH.Lib`][lib-module] module contains helper functions
that take and return AST values in the `Q` monad, which sometimes helps
produce shorter code, because these helpers compose well with quotation and
splicing. Here we used `varE :: Name -> Q Exp` instead of `VarE :: Name ->
Exp`.

Another way to introduce a capturable name is, apparently, by using an unbound
name in a quote:

```haskell
λ> withZ <- runQ [| z + 1 |]
λ> withZ
InfixE (Just (UnboundVarE z)) (VarE GHC.Internal.Num.+) (Just (LitE (IntegerL 1)))
λ> let z = 100 in $(return withZ)
101
```

But this approach seems quite fragile for my taste. (What if we later define
`z` somewhere in the same module?)

Capturable names are sometimes useful. For example, the [`hamlet`][hamlet]
template system allows us to use the syntax `#{name}` to refer to values in a
template. The template then generates Haskell code where such names come out
as capturable names, so that they can be bound. The resulting effect is that
values bound in the context where a template is used can be accessed in the
template, which is pretty cool.

## Retrieving information about things

Now that we know a little about names, we can go on to learn how to look up
information about named things.

There are quite a few “reifying” functions that allow us to do that:

* [`reify :: Name -> Q Info`][reify] is the most commonly used one. It
  allows us to look up general information [`Info`][info] about a thing.

* [`extsEnabled :: Q [Extension]`][exts-enabled] returns the list of all
  enabled language extensions at the splicing site.

* [`isExtEnabled :: Extension -> Q Bool`][is-ext-enabled] allows us to check
  whether a particular language extension is enabled.

* [`reifyInstances :: Name -> [Type] -> Q [InstanceDec]`][reify-instances]
  returns a list of visible instances of `Name` (type class name) for types
  `[Type]`.

* There are more of them, for rarer use cases:
  [`reifyFixity`][reify-fixity], [`reifyRoles`][reify-roles],
  [`reifyAnnotations`][reify-annotations],
  [`reifyConStrictness`][reify-con-strictness].

Reifying functions take `Name`s, but there is one more question to ask about a
name: does it name a thing that is in scope when we write our metaprogram, or
does it name a thing that is in scope when we execute the metaprogram at the
splicing site? So far the names have been looked up in the scope of the
metaprogram, not in the scope of the splicing site. If we need to access a
thing from the latter scope, there are two ways to do that:

* We could take the name as an argument, like the `makeLenses` function does.
  In that case we construct the `Name` at the splicing site (e.g. by quoting
  it) and it ends up naming a thing from that scope.

* We can use the `lookupTypeName` and `lookupValueName` functions, which look
  up names at the splicing site.

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

This example is going to be a little contrived. The aim is to show how all the
tools we have seen so far work together, but without throwing a “wall of code”
at the reader.

Suppose we want to know how many different non-bottom values inhabit a type.
We could start without TH, like this:

```haskell
class Countable a where
  count :: Integer
```

Notice that `a` does not appear anywhere in the type of `count`. It used to be
that a method had to mention its class variable somewhere in its signature, so
you'd see a `Proxy a` argument threaded through purely to pin down which `a`
was meant. These days we can drop the `Proxy` and instead select the instance
with a *visible type application* at the call site, e.g. `count @Bool`. For
this to typecheck we need two extensions: `AllowAmbiguousTypes` (because `a` is
now “ambiguous”—it can only ever be fixed by a type application) at the class
definition, and `TypeApplications` wherever we call `count`. If this doesn't
make any sense, it's OK—you can still continue reading.

How do we write the instances? It looks like we could leverage the existing
`Enum` and `Bounded` type classes, which already solve the problem, but only
for a limited set of types. If a type is an instance of both `Enum` and
`Bounded`, then we can define `count` like so:

```haskell
instance (Enum a, Bounded a) => Countable a where
  count = fromIntegral $
    1 + fromEnum (maxBound @a) - fromEnum (minBound @a)
```

This is not going to work, though, if we want to be able to define instances
of `Countable` for more complex product and sum types. The reason is that the
instance above already defines `Countable` for every possible `a`, just with
this additional constraint `(Enum a, Bounded a)`. In other words, when Haskell
searches for an instance, it only looks at the right-hand side, ignoring the
constraints, and so `a` matches everything.

We could do better by writing a TH helper that handles two cases:

* If a type is an instance of `Enum` and `Bounded`, then generate an instance
  like the one we have just seen, but for a concrete type.

* Otherwise, analyze the type to figure out whether it's a product or a sum
  type (or indeed something mixed) and use arithmetic to calculate the number
  of non-bottom values, on the assumption that `Countable` is defined for the
  types inside such a composite type.

Let's solve the first part of the task:

```haskell
deriveCountableSimple :: Name -> Q [Dec]
deriveCountableSimple name = [d|
  instance Countable $a where
    count = fromIntegral $
      1 + fromEnum (maxBound @($a)) - fromEnum (minBound @($a))
  |]
  where
    a = conT name
```

`conT` is just `return . ConT`, and `ConT` is a data constructor of `Type`
that represents a type constructor. Quoting and splicing go well together, and
defining `deriveCountableSimple` was indeed simple.

To try this out, I derived a few instances this way:

```haskell
deriveCountableSimple ''Bool
deriveCountableSimple ''Word8
deriveCountableSimple ''Char
```

We can try it now:

```haskell
λ> count @Bool
2
λ> count @Word8
256
λ> count @Char
1114112
```

Looks reasonable. Let's handle the second case:

```haskell
deriveCountableComposite :: Name -> Q [Dec]
deriveCountableComposite name = do
  TyConI (DataD _ _ _ _ cons' _) <- reify name
  [d|
     instance Countable $(conT name) where
       count = $(foldr addE [| 0 |] $ f <$> cons')
   |]
  where
    f (NormalC _ ts) = handleCon (snd <$> ts)
    f (RecC    _ ts) = handleCon (thd <$> ts)
    f _              = fail "unsupported data type"
    handleCon ts = foldr mulE [| 1 |] (countTypeE <$> ts)
    countTypeE t = [| count @($(return t)) |]
    addE x y     = [| $x + $y |]
    mulE x y     = [| $x * $y |]
    thd (_,_,x)  = x
```

Let's see what is going on:

* We first `reify` the given `name` and get information about it. We are only
  interested in type constructors, so we pattern-match on `TyConI`. Further,
  from the information that `TyConI` contains we're only interested in the
  collection of data constructors, `cons'`.

* For every constructor we take every subtype and construct an expression that
  counts the number of values that inhabit that type; this is done in
  `countTypeE`.

* For `NormalC` and `RecC` constructors we just multiply the expressions we've
  got for the individual types; this is done in `handleCon` (this is how we
  handle product types).

* Finally, we add together the expressions for all the data constructors—this
  is how we handle sum types.

Note that `f` only handles `NormalC` and `RecC`; it silently fails on the
other constructor forms (`InfixC`, `GadtC`, `RecGadtC`). Pattern-matching on
raw `Dec`/`Con` values like this is also fragile across GHC releases,
because the shape of these types changes from version to version (`DataD`,
for example, gained fields over the years). In real code you would reach for
the [`th-abstraction`][th-abstraction] package, which presents datatype
information through a single normalized interface that papers over these
differences and handles all the constructor forms for you. We do it by hand
here only to keep the example self-contained.

Let's play with it now:

```haskell
data Foo
  = Foo Bool Bool

deriveCountableComposite ''Foo
```

```haskell
λ> count @Foo
4 -- = 2 + 2
```

This makes sense; let's see if it can handle a sum type:

```haskell
data Foo
  = Foo Bool Bool
  | Bar Word8 Bool

deriveCountableComposite ''Foo
```

```haskell
λ> count @Foo
516 -- = 2 * 2 + 256 * 2
```

It works! Let's combine the two cases into a single `deriveCountable` helper:

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

Done—now we can use `deriveCountable` in both cases and it'll figure out what
to do on its own.

## Viewing the generated code

Sometimes it is helpful to be able to see the code we're generating at splice
sites. GHC [allows us to do that][viewing-th-code] with the `-ddump-splices`
flag. If a build tool swallows that output, add `-ddump-to-file` and look for a
file with the `-splices` suffix under the build directory (`dist-newstyle` for
`cabal`, `.stack-work` for Stack). If you use HLS, it will also render the
expansion of a splice inline in your editor.

Here is what I've got:

```haskell
src/Main.hs:22:1-22: Splicing declarations
    deriveCountable ''Bool
  ======>
    instance Countable Bool where
      count
        = (fromIntegral
             $ ((1 + fromEnum (maxBound @Bool)) - fromEnum (minBound @Bool)))

src/Main.hs:23:1-23: Splicing declarations
    deriveCountable ''Word8
  ======>
    instance Countable Word8 where
      count
        = (fromIntegral
             $ ((1 + fromEnum (maxBound @Word8)) - fromEnum (minBound @Word8)))

src/Main.hs:24:1-22: Splicing declarations
    deriveCountable ''Char
  ======>
    instance Countable Char where
      count
        = (fromIntegral
             $ ((1 + fromEnum (maxBound @Char)) - fromEnum (minBound @Char)))

src/Main.hs:25:1-21: Splicing declarations
    deriveCountable ''Foo
  ======>
    instance Countable Foo where
      count
        = ((count @Bool * (count @Bool * 1))
             + ((count @Word8 * (count @Bool * 1)) + 0))
```

This is a useful debugging tool.

## Lifting Haskell values into TH expressions

So far we have been constructing expressions manually or by using quotation.
What about getting an expression that “reconstructs” a value we already have?
This could be useful for delivering values generated in the `Q` monad to the
outside world.

The solution comes naturally in the form of the [`Lift`][lift] type class:

```haskell
class Lift (t :: TYPE r) where
  lift      :: Quote m => t -> m Exp
  liftTyped :: Quote m => t -> Code m t
```

`lift` takes a value and returns an (untyped) expression that reconstructs it,
while `liftTyped` produces the typed counterpart. (`liftTyped` was added to
the class in GHC 9.0; before that `Lift` had only `lift`, whose type was the
less general `t -> Q Exp`.)

*The `(t :: TYPE r)` in the class head is levity polymorphism. An ordinary
type such as `Int` has kind `Type`, but that is really just a synonym for
`TYPE 'LiftedRep`—`TYPE r` is the kind of *all* types, indexed by a runtime
representation `r` that says how a value is laid out (boxed and lifted,
unboxed, etc.). By quantifying over any `r` rather than fixing `Type`,
`Lift` can have instances not only for boxed types but also for unboxed ones
like `Int#` (of kind `TYPE 'IntRep`) or `Double#` (of kind `TYPE
'DoubleRep`). You can ignore the annotation entirely when lifting ordinary
values; it's there so the class is not artificially restricted to lifted
types.*

We could define some instances like so:

```haskell
instance Lift Integer where
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Char where
  lift x = return (LitE (CharL x))
```

In practice you almost never write `Lift` instances by hand, and you rarely
even implement `liftTyped` yourself—`DeriveLift` (below) handles both methods
for you.

The `template-haskell` package defines `Lift` instances for all common data
types. GHC also knows how to define `Lift` for new types. It is enough to
enable the `DeriveLift` language extension, and we're done (example from the
Haddocks):

```haskell
{-# LANGUAGE DeriveLift #-}

module Main (main) where

import Language.Haskell.TH.Syntax

data Bar a
  = Bar1 a (Bar a)
  | Bar2 String
  deriving Lift
```

What about types you don't own? For the common ones you usually don't have
to do anything at all: the `text` and `bytestring` libraries ship `Lift`
instances for `Text` and `ByteString` upstream (`text` has done so since
version 1.2.4.0, and recent `bytestring` likewise), so

```haskell
foo :: Quote m => Text -> m Exp
foo txt = [| $(lift txt) <> "!" |]
```

just works. Many other common libraries (`containers`, `vector`, and so on)
do the same, and where an upstream instance is still missing,
[`th-lift-instances`][th-lift-instances] fills the gaps.

Historically this was not the case, so it's worth knowing the general
technique for when you hit a type that genuinely has no `Lift` instance and
whose data constructors aren't exported (so `DeriveLift` can't help either).
As it turns out, the `Data` class provides enough introspection to reconstruct
a value, so TH offers the following helper:

```haskell
liftData :: (Data a, Quote m) => a -> m Exp
```

If something is an instance of the `Data` type class, we can lift it with
`liftData`, and no orphan `Lift` instance is necessary. There is a catch,
though: `liftData` uses [`toConstr`][to-constr] internally, and it expects
the constructor it reports to live in the same module as the data type. For
a type like the old `Text`, whose `Data` instance reported `pack` (defined
in `Data.Text`, not in the internal module where `Text` is defined), this
used to fail with a scary interface-file error. The fix was to *override*
the lifting for the problematic type. Suppose we still needed to lift a
hypothetical `Text` that had no `Lift` instance; we would write a bespoke
lifter,

```haskell
liftText :: Quote m => Text -> m Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)
```

which first `lift`s the underlying `String` and then applies `T.pack`
(assuming `Data.Text` is imported qualified as `T`). We then plug it into
`liftData`'s more flexible sibling to handle the type wherever it appears
inside a larger structure:

```haskell
foo :: Quote m => Text -> m Exp
foo txt = [| $e <> "!" |]
  where
    e = dataToExpQ (fmap liftText . cast) txt
```

This [`dataToExpQ`][data-to-expq] function in combination with
[`cast`][cast] (that comes from `Data.Typeable`) does the trick. Again, for
`Text` and `ByteString` specifically you no longer need any of this—plain
`lift` is enough—but the `dataToExpQ`/`cast` pattern remains the go-to escape
hatch for any type that resists both `Lift` and `DeriveLift`.

Let's see what `dataToExpQ` does:

```haskell
dataToExpQ
  :: (Quote m, Data a)
  => (forall b. Data b => b -> Maybe (m Exp)) -> a -> m Exp
```

`dataToExpQ` works just like `liftData`, but it allows us to override the
lifting for the values for which `forall b. Data b => b -> Maybe (m Exp)`
returns `Just`. Don't be afraid of the rank-2 type here. The `forall`
quantification of `b` inside that function in parentheses means that the
function literally works *for all* `b`, but the choice of `b` is made not at
the call site of `dataToExpQ`, but at the call site of this `forall b. Data
b => b -> Maybe (m Exp)` function. Similarly, the choice of `a` is made at
the call site of `dataToExpQ`, which also has an implicit `forall a.` at the
beginning of its type signature. See the symmetry? (If you're a beginner,
you may not understand rank-N types immediately; in that case, don't
despair.)

`cast` performs type-safe casting between two types:

```haskell
cast :: (Typeable a, Typeable b) => a -> Maybe b
```

Here, if `a` has the same type representation (which the `Typeable` type
class allows us to query via the `typeRep` function) as `b`, we get a `b`
value inside `Just`.

We can use `cast` here because `Typeable` is a superclass of `Data`:

```haskell
class Typeable a => Data a where
  -- …
```

If something in the above is not clear, it's OK. Just grab this trick and use
it next time you need to lift data that contains a type with no usable `Lift`
instance.

## Example 2: creating refined values at compile time

Now we are prepared to write a TH helper that allows us to construct values of
refined types at compile time, turning invalid inputs into compilation errors.

Our practical example will be taken (in a simplified form) from an existing
library I wrote, called [`modern-uri`][modern-uri]. In the library we have a
function that takes `Text` representing a URI as input and outputs `Maybe
URI`:

```haskell
data URI = URI
  { uriScheme    :: Maybe (RText 'Scheme)
  , uriAuthority :: Either Bool Authority
  , uriPath      :: [RText 'PathPiece]
  , uriQuery     :: [QueryParam]
  , uriFragment  :: Maybe (RText 'Fragment)
  } deriving (Show, Eq, Ord, Data, Typeable, Generic, Lift)

mkURI :: Text -> Maybe URI
```

`Nothing` means that the input was not a correct `URI`. Our task thus
becomes:

* Run `mkURI` at compile time.
* If the returned value is `Nothing`, signal a compile-time error. Otherwise
  lift the entire `URI` data structure we have parsed.

By now we know how to tackle every part of the task. Since `URI` derives
`Lift` (all of its leaf types, including `Text`, are liftable nowadays), the
helper is short:

```haskell
mkURI' :: Text -> Q Exp
mkURI' txt =
  case mkURI txt of
    -- Instead of 'fail' we could also use 'reportError'. There is also
    -- 'reportWarning' just in case you ever want to report warnings.
    Nothing  -> fail "The input does not contain a valid URI"
    Just uri -> lift uri
```

Note that we stay in the concrete `Q` monad here rather than a polymorphic
`Quote m`, because we call `fail`, which needs `MonadFail`—a capability `Q`
has but the bare `Quote` class does not. And had `URI` *not* derived `Lift`,
we could still have lifted it with the escape hatch from the previous section,
`dataToExpQ (fmap liftText . cast) uri`.

We could finish the section here, but there is a nicer way, syntax-wise, to
make use of such a validating helper. The feature we're going to explore is
called *quasi-quotes*. It turns out that TH allows us to define our own
custom quasi-quoters, like the `d`, `e`, `t`, and `p` we saw earlier.
Quasi-quoters are more common than you might think: they are the machinery
behind many everyday libraries, such as the string-interpolation packages
[`neat-interpolation`][neat-interpolation],
[`string-interpolate`][string-interpolate], and [`PyF`][pyf]. Whenever you
see a `[foo| … |]` bracket with custom syntax inside, a quasi-quoter is
turning that string into an AST at compile time.

Defining a quasi-quoter is easy. It is enough to import the
[`QuasiQuoter`][quasi-quoter] data type from `Language.Haskell.TH.Quote`:

```haskell
data QuasiQuoter = QuasiQuoter
  { quoteExp  :: String -> Q Exp
  , quotePat  :: String -> Q Pat
  , quoteType :: String -> Q Type
  , quoteDec  :: String -> Q [Dec]
  }
```

A quasi-quoter may be used in the four familiar contexts, so it has four
corresponding functions that take the `String` from the quote and return
something to splice.

Usually we only want to use a quasi-quoter in one context, so the others are
either omitted, left `undefined`, or replaced by `error`s. These failures will
happen at compile time, so it's OK to do the following:

```haskell
uri :: QuasiQuoter
uri = QuasiQuoter
  { quoteExp  = \str ->
      case mkURI (T.pack str) of
        Nothing -> fail "The input does not contain a valid URI"
        Just x  -> lift x
  , quotePat  = error "Usage as a pattern is not supported"
  , quoteType = error "Usage as a type is not supported"
  , quoteDec  = error "Usage as a declaration is not supported" }
```

I like to use `error` with helpful messages instead of the `undefined` some
people use.

To use our new quasi-quoter, we need to enable the `QuasiQuotes` language
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

If the string inside the `uri` quasi-quoter is not a valid URI, compilation
will fail. One more type of error caught at compile time!

## Running `IO` in `Q`

Using `IO` in TH generally makes the compilation process dependent on
external conditions that may contribute to unexpected compilation failures.
Thus, it makes sense to think twice before running `IO` from TH.

That said, the function that lifts `IO` into `Q` is called simply
[`runIO`][run-io]:

```haskell
runIO :: IO a -> Q a
```

Needless to say, one can do a lot with such a tool, for good or for evil. One
example of a good use is the [`gitrev`][gitrev] package, which allows us to
insert information about the active branch and last commit of the code that is
being compiled. It works by literally running the `git` executable at compile
time and then lifting the fetched data.

A far more common use case for `IO` in `Q` is reading from files. In that case
compilation usually starts to depend on the contents of the file being read,
and so it's a good idea to tell GHC that changes in that file should cause
recompilation of the module where the file-reading TH helper is spliced. This
is done via the [`addDependentFile`][add-dependent-file] function:

```haskell
addDependentFile :: FilePath -> Q ()
```

(Why it lives in `Language.Haskell.TH.Syntax` and not in
`Language.Haskell.TH` is beyond me. What does it have to do with syntax?)

## Example 3: the `file-embed` package

Finally, in our last example, let's reimplement (in a simplified form) the
popular [`file-embed`][file-embed] package, which lets us load the contents of
a file and splice them as an `IsString a => a` value—the type of string
literals in Haskell in the presence of the `OverloadedStrings` language
extension.

If we have this in a `TH.hs` file:

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

The program outputs its own source code. No `src/Main.hs` file is expected to
exist when we run the binary; the source code is stored in the executable
itself. Note how the `IsString a => a` value was instantiated to `Text`
automatically, because `Text` is an instance of `IsString`.

## Conclusion

This is by no means a complete TH tutorial; some of the more rarely used tools
and functions have not been covered. Still, the tutorial should get you up to
speed and give you a taste of what metaprogramming in Haskell looks like. For
further information, refer directly to the Haddocks:

<https://hackage.haskell.org/package/template-haskell>

Remember to also consider the [`th-abstraction`][th-abstraction] package for
real work. It normalizes variations in the interface for inspecting datatype
information across different versions of the `template-haskell` library.

Good luck!

[persistent]: https://hackage.haskell.org/package/persistent
[yesod]: https://hackage.haskell.org/package/yesod
[hamlet]: https://hackage.haskell.org/package/hamlet
[modern-uri]: https://hackage.haskell.org/package/modern-uri
[gitrev]: https://hackage.haskell.org/package/gitrev
[file-embed]: https://hackage.haskell.org/package/file-embed
[th-abstraction]: https://hackage.haskell.org/package/th-abstraction
[th-compat]: https://hackage.haskell.org/package/th-compat
[th-lift-instances]: https://hackage.haskell.org/package/th-lift-instances
[neat-interpolation]: https://hackage.haskell.org/package/neat-interpolation
[string-interpolate]: https://hackage.haskell.org/package/string-interpolate
[pyf]: https://hackage.haskell.org/package/PyF

[dec]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Dec
[exp]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Exp
[texp]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:TExp
[code]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Code
[type]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Type
[pat]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Pat
[quasi]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#t:Quasi
[quote]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#t:Quote
[quasi-quoter]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Quote.html#t:QuasiQuoter
[reify]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reify
[info]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#t:Info
[exts-enabled]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:extsEnabled
[is-ext-enabled]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:isExtEnabled
[reify-instances]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyInstances
[reify-fixity]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyFixity
[reify-roles]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyRoles
[reify-annotations]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyAnnotations
[reify-con-strictness]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:reifyConStrictness
[lift]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#t:Lift
[to-constr]: https://hackage.haskell.org/package/base/docs/Data-Data.html#v:toConstr
[data-to-expq]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#v:dataToExpQ
[cast]: https://hackage.haskell.org/package/base/docs/Data-Typeable.html#v:cast
[run-io]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:runIO
[add-dependent-file]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Syntax.html#v:addDependentFile

[lib-module]: https://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH-Lib.html

[typed-th]: https://www.cs.drexel.edu/~mainland/2013/05/31/type-safe-runtime-code-generation-with-typed-template-haskell/
[code-texp-proposal]: https://ghc-proposals.readthedocs.io/en/latest/proposals/0195-code-texp.html
[viewing-th-code]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#viewing-template-haskell-generated-code
[orphan-instance]: https://wiki.haskell.org/Orphan_instance
[generics]: /tutorial/generics.html
