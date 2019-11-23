---
title: GHC optimization and fusion
desc: The tutorial is my attempt to explain all important GHC optimization ideas in one place. I included some benchmarked examples that should help to demonstrate the techniques in practice.
date:
  published: November 22, 2019
---

*This is a new, revised version of [the old tutorial I
wrote][original-tutorial].*

```toc
```

The tutorial is my attempt to explain all important GHC optimization ideas
in one place. I included some benchmarked examples that should help to
demonstrate the techniques in practice.

## GHC pragmas

Pragmas are a sort of special hints for the compiler. You should be familiar
with the `LANGUAGE` pragma that enables language extensions in GHC, e.g.:

```haskell
{-# LANGUAGE OverloadedStrings #-}
```

The same syntax is used for all GHC pragmas.

We will cover 3 kinds of pragmas:

1. `INLINE` and `INLINABLE`
2. `SPECIALIZE`
3. `RULES`

### Inlining

When a program is compiled, functions become labels—strings associated with
positions in machine code. To call a function, its arguments must be put in
appropriate places in memory, stack, and registers. Execution flow then
jumps to the address where the function begins. After executing the function
body it is necessary to restore the state of the stack and registers, so
they look just like before calling the function. We also need to jump back
to continue executing the program. All these manipulations are not free. For
a short function they may actually take longer than execution of the
function body itself.

The idea of inlining is simple: insert the function body directly where we
would otherwise have a function call. The functions that are inlined tend to
be short, so duplication of code is minimal. At the same time we may get a
considerable performance boost. Inlining is perhaps the simplest and yet
very efficient way to improve performance in certain cases. Furthermore, we
will see shortly that inlining in GHC is not just about eliminating calls
themselves, it's also a way to let other optimizations be applied.

#### How GHC does inlining by itself

When GHC decides whether to inline a particular function or not, it looks at
its size and assigns some sort of weight to that function in a given
context. The decision whether to inline a function or not is made on a
per-call basis and a given function may be inlined in one place and called
in another. We won't go into the details of how a function's weight (or
cost) is calculated, but it should make sense that the lighter the function,
the keener the compiler is to inline it.

It is worth noticing that GHC is careful about avoiding excessive code bloat
and it does not inline blindly. Generally, a function is inlined when it
makes at least some sense to inline it. When deciding whether to inline, GHC
considers the following:

* **Does it make sense to inline at a particular call site?** Consider this
  example:

  ```haskell
  map f xs
  ```

  Here, inlining `f` would produce `map (\x -> body) xs`, which is not any
  better than the original, so GHC does not inline it.

  The example can be generalized: *GHC only inlines functions that are
  applied to as many arguments as they have syntactically on the left-hand
  side (LHS) of function definition.* This makes sense because otherwise the
  body would need to be wrapped with a lambda anyway.

  To clarify, let's steal one more example from the GHC user guide:

  ```haskell
  comp1 :: (b -> c) -> (a -> b) -> a -> c
  comp1 f g = \x -> f (g x)

  comp2 :: (b -> c) -> (a -> b) -> a -> c
  comp2 f g x = f (g x)
  ```

  `comp1` has only two arguments on its LHS, while `comp2` has three, so a
  call like this

  ```haskell
  map (comp1 not not) xs
  ```

  optimizes better than a similar call with `comp2`.

* **How much code duplication inlining would cause?** Code bloat is bad as
  it increases compilation time, size of program, and lowers cache hit
  rates.

* **How much work duplication would inlining cause?** Consider the examples
  from the paper [*Secrets of the GHC inliner*][secrets-of-ghc-inliner]:

  ```haskell
  let x = foo 1000 in x + x
  ```

  where `foo` is expensive to compute. Inlining `x` would result in two
  calls to `foo` instead of one.

  Let's see another example:

  ```haskell
  let x = foo 1000
      f = \y -> x * y
  in … (f 3) … (f 4)
  ```

  This example shows that the work can be duplicated even if `x` only
  appears once. If we inline `x` in its occurrence site it will be
  evaluated every time `f` is called. This is why inlining inside a lambda
  may be not a good idea.

It is not surprising that GHC is quite conservative about work duplication.
However, it makes sense to put up with some duplication of work because
inlining often opens new transformation opportunities at the inlining site.
Avoiding the call itself is not the only (and actually not the main) reason
to do inlining. Inlining puts together pieces of code that were previously
separate thus allowing next passes of the optimizer to do more wonderful
work.

With this in mind, you shouldn't be too surprised to find out that the body
of an inlineable function (or right-hand side, RHS) is not optimized by GHC.
This is an important point that we'll revisit later. It is not optimized to
allow other machinery to do its work after inlining. For that machinery it
is important that the function's body is intact because it operates on a
rather syntactic level and optimizations, if applied, would leave almost no
chance for the machinery to do its trick. For now remember that the bodies
of functions that GHC sees as inlineable won't be optimized, they will be
inserted “as is”.

One of the simplest optimization techniques GHC can use with inlining is
plain old beta-reduction—application of functions to their arguments. But
this is nothing short of compile-time evaluation of a program! Which means
that GHC should somehow ensure that it terminates.

This brings us to the two edge cases:

* **Self-recursive functions are never inlined.** This should be quite
  obvious, because if we chose to inline it, we would never finish.

* **With mutually recursive definitions**, **GHC selects** one or more
  **loop breakers**. Loop breakers are just functions that GHC chooses to
  call (i.e. not inline) to break the loop it would get into if it started
  to inline everything. For example, if we have `a` defined via `b` and `b`
  defined via `a`, we can choose either of them as a loop breaker. GHC tries
  not to select a function that would be very beneficial to inline (but if
  it has no choice, it will).

Finally, before we move on to discussing how one can manually control
inlining, it's important to understand a couple of things about how compiled
Haskell programs are stored.

Just like with many other languages that compile to native machine code,
after compilation we get `.o` files, called [object files][object-file].
They contain object code, which is machine code that can be used in an
executable, but cannot usually be executed on its own. Every module produces
an object file of its own. It's hard to work with just object files, because
they contain information in not very friendly form—you can execute it, but
you cannot reason about it.

To keep additional information about a compiled module, GHC also creates
*interface files*. They contain the version of GHC that was used, list of
modules that the compiled module depends on, list of things it exports and
imports, and other information. Most importantly, interface files contain
the bodies of inlineable functions which can be used for cross-module
inlining. This is an important thing to understand: *we cannot inline a
function if we don't have its body verbatim*. Unless the function's body is
dumped in an interface file, we only have object code which cannot be used
for inlining.

#### How to control inlining

Let's discuss how to control inlining explicitly.

##### `INLINEABLE`

```haskell
myFunction :: Int -> Int
myFunction = …
{-# INLINEABLE myFunction #-}
```

The main effect of the `INLINEABLE` pragma is that GHC will keep in mind
that this function may be inlined, even if it would not consider it for
inlining otherwise. We don't get any guarantees about whether the function
will be inlined or not in any particular case, but now unfolding of the
function will be dumped to an interface file, which means that it's possible
to inline it in another module.

With a function marked `INLINEABLE`, we can use the special built-in
function called `inline`, which will tell GHC to try very hard to inline its
argument at a particular call site, like this:

```haskell
foo = bar (inline myFunction) baz
```

Semantically, `inline` it just an identity function.

Let's see an example of `INLINEABLE` in action. We have a module `Goaf` (it
stands for *GHC optimization and fusion*) with this:

```haskell
module Goaf
  ( inlining0 )
where

inlining0 :: Int -> Int
inlining0 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
```

Here I managed to convince GHC that `inlining0` doesn't look very
inlineable. If we compile with `-O2` (as we will do in every example from
now on) and dump `Goaf.hi` interface file, we will see no unfolding of
`inlining0`'s body (if you use a different version of GHC you may be unable
to reproduce this output exactly):

```
$ ghc --show-iface Goaf.hi

…

142c0e92c650162b33735c798cb20be3
  $winlining0 :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: [0] -}
e447f016aa264b71f156911b664944d0
  inlining0 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $winlining0 ww1 of ww2 { DEFAULT -> I# ww2 } }) -}

…
```

`$winlining0` is a compiled function that works on unboxed integers `Int#`
and it is not inlineable. `inlining0` itself is a thin wrapper around it
that turns result of type `Int#` into normal `Int` by wrapping it with
`Int`'s constructor `I#`. We will not go into detailed explanations about
unboxed data and primitives, but `Int#` is just your bare-metal,
hard-working C `int`, while `Int` is our familiar boxed, lazy Haskell `Int`.

We see two important things here:

* `inlining0` itself (in the form of `$winlining0`) is not dumped into the
  interface file, that means that we have lost the ability to look inside
  it.

* Still, GHC turned the `inlining0` function into a wrapper which itself is
  inlineable. The idea is that if `inlining0` is called in an arithmetic
  context with some other operations on `Int`s, GHC might be able to
  optimize further and better glue things working on `Int#`s together.

Now let's use the `INLINEABLE` pragma:

```haskell
inlining1 :: Int -> Int
inlining1 x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# INLINEABLE inlining1 #-}
```

which results in:

```
…

033f89de148ece86b9e431dfcd7dde8c
  $winlining1 :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: INLINABLE[0],
     Unfolding: <stable> (\ (ww :: Int#) ->

       … a LOT of code…

6a60cad1d71ad9dfde046c97c2b6f2e9
  inlining1 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $winlining1 ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
```

The result is almost the same, but now we have the complete unfolding of
`$winlining1` in our interface file. It is unlikely that this will improve
performance considerably because our functions are rather slow and executed
only once:

```
benchmarking inlining0
time                 5.653 ms   (5.632 ms .. 5.673 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.614 ms   (5.601 ms .. 5.627 ms)
std dev              39.86 μs   (33.20 μs .. 48.70 μs)

benchmarking inlining1
time                 5.455 ms   (5.442 ms .. 5.471 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.447 ms   (5.432 ms .. 5.458 ms)
std dev              38.08 μs   (28.36 μs .. 58.38 μs)
```

As expected, inlining gives only marginal improvement in this case.

It turns out that not only inlining requires access to original function
body to work, some other optimizations do as well. The `INLINEABLE` pragma
effectively removes module boundaries that could otherwise prevent other
optimizations from being applied. We will see how this works with
specializing in the next section. Because of that, it is not unusual to see
`INLINEABLE` used on a self-recursive function. The intention is not to
inline the function, but to dump its definition into interface file.

##### `INLINE` and `NOINLINE`

A more straightforward approach to control inlining is to use the `INLINE`
pragma. When GHC calculates the weight of a function, this pragma makes the
function seem very lightweight, to the extent that GHC will always decide to
inline it. `{-# INLINE myFunction #-}` will cause unconditional inlining of
`myFunction` everywhere (except for edge cases, like when `myFunction` is
self-recursive).

Inlining is always an option for the compiler, unless you tell it that a
particular function should not be inlined. Sometimes you will want to be
able to do that. In such cases the `NOINLINE` pragma may be helpful.

Let's take an example from the [`http-client-tls`][http-client-tls] package
which adds TLS support to `http-client`. The package defines HTTP `Manager`
that stores information about open connections. The manager is expensive to
create and ideally you should have only one such manager for maximal
connection sharing. To make it easier, there is `globalManager :: IORef
Manager` which you can get and set when you're in the `IO` monad. To get the
`IORef` of a global manager the following code is used:

```haskell
globalManager :: IORef Manager
globalManager =
  unsafePerformIO (newManager tlsManagerSettings >>= newIORef)
{-# NOINLINE globalManager #-}
```

`unsafePerformIO` has the type `IO a -> a`—it runs effectful `IO` code while
disguising itself as a pure value. Why is this useful? We want `IORef`, not
`IO IORef`, as the latter is just a recipe of how to get an `IORef` pointing
to one more such manager. `unsafePerformIO` allows us to run the `IO` action
that produces `IORef` once and share the result for all future use—this is
the treatment that only pure values get. The `globalManager` value meets the
conditions for sharing—it is named and lives on the top-level. Yet, there is
a catch: GHC can just inline `globalManager` at its call sites, causing
re-evaluation. To prevent this we add the `NOINLINE` pragma.

### Specializing

To understand how specializing works we first need to review how ad-hoc
polymorphism with type classes is implemented in GHC.

When there is a type class constraint in the signature of a function:

```haskell
foo :: Num a => a -> a
foo = …
```

It means that the function should work differently for different `a`. This
is accomplished by passing around a dictionary that is indexed by the
methods of a given type class. The example above turns into:

```haskell
foo :: Num a -> a -> a
foo d = …
```

Note the `d` argument of type `Num a`. This is a dictionary that contains
functions that implement the methods of the `Num` type class. When a method
of that type class needs to be called, the dictionary is indexed by the name
of that method and the extracted function is used. Not only does `foo`
accept the dictionary as an additional argument, it also passes it to
polymorphic functions inside `foo`, and those functions may pass it to
functions in their bodies:

```haskell
foo :: Num a -> a -> a
foo d = … bar d …
  where
    bar, baz :: Num a -> a -> a
    bar d = … baz d …
    baz d = …
```

Passing and indexing is not free—it makes your program slower. At the same
time it is not possible to run a polymorphic function without fixing its
types. Then it should be possible for GHC to figure out which implementation
should be used in every place and speed up things considerably. When we turn
a polymorphic function into one specialized for concrete type(s), we do
specializing.

Syntactically, a `SPECIALIZE` pragma can be put anywhere its type signature
can be put:

```haskell
foo :: Num a => a -> a
foo = …
{-# SPECIALIZE foo :: Int -> Int #-}
```

The specialized type may be any type that is less polymorphic than the type
of the original function. In other words, the following is valid:

```haskell
{-# SPECIALIZE f :: <type> #-}
```

when

```haskell
f_spec :: <type>
f_spec = f
```

is valid.

The effect of the pragma is to generate a specialized version of the
specified function and a rewrite rule which rewrites calls to the original
function to calls to its specialized version whenever the types match.

It is important to understand the relationship between inlining and
specializing—they both produce specialized code in the end. Inlining is more
general but also more wasteful because it relies on insertion of function
body at every call site. The body will be optimized and specialized again
and again every time leading to longer compilation times and code bloat.
With specializing the compiler generates a number of versions of the
function in question with fixed types and will use them when appropriate. If
there is no matching specialized version, you'll get non-specialized slow
code. This is why marking a function as inlineable is sometimes better—you
don't need to guess which types the users of code will need, you get them
all covered.

For a practical example let's start with this code:

```haskell
module Goaf
  ( special0'
  , special0 )
where

special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]

special0 :: Int -> Int
special0 x = special0' x `rem` 10
```

In the interface file we get:

```
…

3d2b7aef38f4af3a87867079a7fb9d7d
  $w$sspecial0' :: Int# -> Int#
  {- Arity: 1, HasNoCafRefs, Strictness: <S,U>, Inline: [0] -}

9aab4f68c56ea324d5b4f1ae96f44304
  special0 :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Unfolding: InlineRule (1, True, False)
                (\ (x :: Int) ->
                 case special0_$sspecial0' x of wild2 { I# x1 ->
                 I# (remInt# x1 10#) }) -}
97c360215ea1cab7acdf5a4928d349e8
  special0' :: (Num a, Enum a) => a -> a
  {- Arity: 3, HasNoCafRefs,
     Strictness: <S(C(C(S))LLLLLL),U(C(C1(U)),A,U,A,A,A,C(U))><L,U(A,A,A,A,A,A,C(C1(U)),A)><L,U> -}
efc0709eeb0afdb2be8cdce06cc54623
  special0_$sspecial0' :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $w$sspecial0' ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
"SPEC special0' @ Int" [ALWAYS] forall ($dNum :: Num Int)
                                       ($dEnum :: Enum Int)
  special0' @ Int $dNum $dEnum = special0_$sspecial0'
```

GHC is really good at specializing if a polymorphic function defined and
used in the same module. I could not really find a case where GHC would fail
to specialize on its own, bravo! The specialized version of `special0'` is
called `$w$sspecial0'` here and it works on `Int#` for maximal speed.

What else do we see? `special0'` is compiled, but not dumped into the
interface file. This means that if we use it from another module we should
get considerably worse performance compared to `special0`.

Let's try:

```
benchmarking special0
time                 5.457 ms   (5.436 ms .. 5.477 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.481 ms   (5.470 ms .. 5.492 ms)
std dev              35.69 μs   (29.94 μs .. 44.88 μs)

benchmarking special0_alt   <---- defined in a separate module
time                 5.462 ms   (5.436 ms .. 5.496 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.472 ms   (5.458 ms .. 5.485 ms)
std dev              41.42 μs   (33.29 μs .. 55.02 μs)
```

What is going on? `special0_alt` was able to take advantage of the
specialized function `$w$sspecial0'`! But if we remove the export of
`special0`, the situation changes as `special0_alt` will not be able to find
the appropriate specialization anymore:

```
benchmarking special0_alt
time                 912.0 ms   (866.2 ms .. 947.7 ms)
                     1.000 R²   (NaN R² .. 1.000 R²)
mean                 931.0 ms   (919.8 ms .. 939.9 ms)
std dev              13.88 ms   (0.0 s .. 15.45 ms)
```

Let's try to fix the ×167 slowdown by specializing `special01` explicitely:

```haskell
special0' :: (Num a, Enum a) => a -> a
special0' x =
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000] +
  product [x..1000000]
{-# SPECIALIZE special0' :: Int -> Int #-}
```

This brings our specialization back:

```
  special0'_$sspecial0' :: Int -> Int
  {- Arity: 1, HasNoCafRefs, Strictness: <S(S),1*U(U)>m,
     Inline: INLINE[0],
     Unfolding: InlineRule (1, True, False)
                (\ (w :: Int) ->
                 case w of ww { I# ww1 ->
                 case $w$sspecial0' ww1 of ww2 { DEFAULT -> I# ww2 } }) -}
"SPEC special0'" [ALWAYS] forall ($dNum :: Num Int)
                                 ($dEnum :: Enum Int)
  special0' @ Int $dNum $dEnum = special0'_$sspecial0'
```

And `special0_alt` starts to run fast again:

```
benchmarking special0_alt
time                 5.392 ms   (5.381 ms .. 5.403 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 5.399 ms   (5.392 ms .. 5.408 ms)
std dev              25.12 μs   (16.60 μs .. 38.90 μs)
```

To recapitulate:

* GHC has no problem specializing for you when a polymorphic function is
  used in the same module it's defined: it has its body and it knows what to
  do.

* Lack of specialization makes polymorphic functions run very slowly.

* If you can guess which specializations to request from GHC when you write
  your module prefer `SPECIALIZE`.

* Otherwise rely on `INLINE` and `INLINEABLE`.

### Rewrite rules

Haskell, being a pure language, makes it possible to perform a wide range of
transformations without changing meaning of the programs. Let's see how this
is done with rewrite rules.

#### The `RULES` pragma

The `RULES` pragma allows us to write arbitrary rules how to transform
certain combinations of functions. Here is an example of `RULES` in use:

```haskell
{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
  #-}
```

* There may be zero or more rules in a `RULES` pragma, which you may write
  each on its own line or even several in one line separating them by
  semicolons.

* Closing `#-}` should start in a column to the right of the opening `{-#`.

* Each rule has a name, enclosed in double quotes. The name itself has no
  significance. It is only used when GHC needs to refer to the rule in its
  output.

* Each variable mentioned in a rule must either be in scope (e.g. `map`), or
  bound by the `forall` (e.g. `f`, `g`, `xs`). The variables bound by the
  `forall` are called *pattern variables*.

* A pattern variable may optionally have a type signature. But if the type
  of the pattern variable is polymorphic, it *must* have a type signature.
  For example:

    ```haskell
    {-# RULES
    "fold/build"  forall k z (g :: forall b. (a -> b -> b) -> b -> b).
                  foldr k z (build g) = g k z
      #-}
    ```

    Since `g` has a polymorphic type, it must have a type signature.

* The left hand side of a rule must consist of a top-level variable applied
  to arbitrary expressions. For example, this is not OK:

    ```haskell
    {-# RULES
    "wrong1"   forall e1 e2.  case True of { True -> e1; False -> e2 } = e1
    "wrong2"   forall f.      f True = True
      #-}
    ```

    In `"wrong1"`, the LHS is not an application; in `"wrong2"`, the LHS has
    a pattern variable in the head.

* A rule does not need to be in the same module as the variables it
  mentions, though of course they need to be in scope.

* All rules are implicitly exported from the module, and are therefore in
  force in any module that imports the module that defined the rule,
  directly or indirectly. (That is, if `A` imports `B`, which imports `C`,
  then `C`'s rules are in force when compiling `A`.) The situation is very
  similar to that with instance declarations.

* Inside a rule `forall` is treated as a keyword, regardless of any other
  flag settings. Furthermore, inside a rule, the language extension
  `-XScopedTypeVariables` is automatically enabled.

* Like other pragmas, the `RULES` pragmas are always checked for scope
  errors, and are typechecked. Typechecking means that the LHS and RHS of a
  rule are typechecked, and must have the same type.

The GHC user guide explains what rewrite rules do (I edited it a bit):

> GHC uses a very simple, syntactic, matching algorithm for matching a rule
  LHS with an expression. It seeks a substitution which makes the LHS and
  expression syntactically equal modulo alpha-conversion (that is, a rule
  matches only if types match too). The pattern (rule), but not the
  expression, is eta-expanded if necessary. (Eta-expanding the expression
  can lead to laziness bugs.) But no beta-conversion is performed (that's
  called higher-order matching).

This requirement of verbatim matching modulo alpha conversion in combination
with the fact that a lot is going on during the optimization process in GHC
makes working with rules a bit tricky. That is, sometimes rules do not fire.
Some cases of this are covered in the next section *Gotchas*.

On the other hand, when several rules match at once, GHC will choose one
arbitrarily to apply. You might be wondering “why not to choose e.g. the
first one”—well, given that rules are much like instance declarations with
respect to how they are imported, there is no order for them. The only thing
GHC can do is to either apply none or pick one randomly and apply that.

#### Gotchas

Even though GHC tries to apply the rules as it optimizes the program, there
are many ways for things to go south. This may make the experience of
writing rewrite rules frustrating. This section highlights some problems you
might encounter.

* **GHC does not attempt to verify whether RHS has the same meaning as
  LHS**. It's responsibility of the programmer to ensure that the rules do
  not change the meaning of the expressions! An example of a rule that may
  seem obviously correct could be something like this:

  ```haskell
  {-# RULES
  "double reverse" forall xs. reverse (reverse xs) = xs
    #-}
  ```

  At first glance it makes sense, doesn't it? The `"double reverse"` rule
  nevertheless does not preserve meaning of expression it transforms.
  `reverse (reverse xs)` applied to an infinite list would diverge, never
  yielding any element, while the infinite list `xs` can be consumed
  normally, given that it's never forced in its entirety.

* **GHC does not attempt to ensure that rules terminate**. For example:

  ```haskell
  {-# RULES
  "loop" forall x y. f x y = f y x
    #-}
  ```

  will cause the compiler to go into an infinite loop.

* Not only every transformation must not introduce any differences in
  meaning, ability to terminate, etc., but also it is desirable that we get
  the same result regardless the order in which we apply the
  transformations. This property is called *confluence*.

  Here is an example that will hopefully demonstrate what is meant:

  ```haskell
  {-# RULES
  "f/f" forall x. f (f x) = f x
  "f/g" forall x. f (g x) = fg x
    #-}
  ```

  The `"f/f"` rule states that `f` is a kind of idempotent function, while
  the `"f/g"` rule recognizes the particular combination of `f` and `g` and
  replaces it with the ad-hoc implementation `fg`.

  Now consider the rewriting of `f . f . g`. If we first apply `"f/f"`, then
  we'll end up with `fg x`, but if we first apply `"f/g"`, then we'll get `f
  . fg`. The system is not confluent. An obvious fix would be to add this
  rule:

  ```haskell
  {-# RULES
  "f/fg" forall x. f (fg x) = fg x
    #-}
  ```

  which makes the system confluent. **GHC does not attempt to check if your
  rules are confluent**, so take some time to check your rule set for
  confluence.

* **Writing rules matching on methods of type classes is futile** because
  methods will be specialized by GHC before rewrite rules have a chance to
  be applied. Such rules won't fire because the types specialized functions
  won't match the types specified in the rewrite rules.

While inlining can get in the way of rewrite rules, it can also help glue
together different pieces of code. There is a special modifier to `INLINE`
pragma called `CONLIKE` that tells GHC “hey, if inlining this any number of
times helps some rewrite rules fire, go wild and inline”. `CONLIKE` stands
for “constructor-like”. GHC maintains the invariant that every constructor
application has arguments that can be duplicated at no cost: variables,
literals, and type applications, hence the name. You can find more about
this in the paper [*Secrets of the GHC inliner*][secrets-of-ghc-inliner].

#### Phase control

As you can see, a lot is happening during optimization passes and things
have the potential to interfere with each other in undesirable ways. There
must be a way to say: this should happen first, that should happen after.
Well, there is a way.

GHC has the concept of *simplifier phases*. The phases are numbered. The
first phase that runs currently has number 4, then go number 3, 2, 1, and
finally the last phase has number 0.

Unfortunately, phase separation does not give fine-grained control, just
enough for us to construct something that works. In an ideal world, we would
like to be able to specify which optimization procedure depends on which,
but instead we have only two options:

1. Specify starting from which phase given rewrite rule or inline/specialize
   pragma should be enabled.

2. Specify up to which phase (not including) a rule should be enabled.

This boils down to adding `[n]` or `[~n]` after the pragma's name:

```haskell
                         -- Before phase 2     Phase 2 and later
{-# INLINE   [2]  f #-}  --      No                 Yes
{-# INLINE   [~2] f #-}  --      Yes                No
{-# NOINLINE [2]  f #-}  --      No                 Maybe
{-# NOINLINE [~2] f #-}  --      Maybe              No

{-# INLINE   f #-}       --      Yes                Yes
{-# NOINLINE f #-}       --      No                 No
```

Regarding “maybe”:

> By “Maybe” we mean that the usual heuristic inlining rules apply (if the
  function body is small, or it is applied to interesting-looking arguments
  etc).

Phase control also works for `SPECIALIZE` and on a per-rule basis in
`RULES`. Let's take a look at how it works with the `SPECIALIZE` pragma:

```haskell
foo :: Num a => a -> a
foo = …
{-# SPECIALIZE [1] foo :: Int -> Int #-}

⇒

fooForInts :: Int -> Int -- generated by GHC
fooForInts = …
{-# NOINLINE [1] foo #-}
{-# RULES    [1] foo = forForInts #-}
```

Here the phase indication for `SPECIALIZE` has the effect of disabling
inlining till it's time to activate the specializing rule.

As an example of how phase control may be indispensable with rewrite rules,
it's enough to look at `map`-specific rules found in `Prelude`:

```haskell
-- Up to (but not including) phase 1, we use the "map" rule to
-- rewrite all saturated applications of map with its build/fold
-- form, hoping for fusion to happen.
-- In phase 1 and 0, we switch off that rule, inline build, and
-- switch on the "mapList" rule, which rewrites the foldr/mapFB
-- thing back into plain map.
--
-- It's important that these two rules aren't both active at once
-- (along with build's unfolding) else we'd get an infinite loop
-- in the rules.  Hence the activation control below.
--
-- The "mapFB" rule optimizes compositions of map.
--
-- This same pattern is followed by many other functions:
-- e.g. append, filter, iterate, repeat, etc.

{-# RULES
"map"       [~1] forall f xs.   map f xs                = build (\c n -> foldr (mapFB c f) n xs)
"mapList"   [1]  forall f.      foldr (mapFB (:) f) []  = map f
"mapFB"     forall c f g.       mapFB (mapFB c f) g     = mapFB c (f.g)
  #-}
```

Note two important points here:

1. Without phase control both rules `"map"` and `"mapList"` would be active
   at the same time and GHC would go into an infinite loop. Phase control is
   the only way to make this set of rules work.

2. We first use the `"map"` rule, and then we use `"mapList"` which
   essentially rewrites the function back into its `map` form. This strategy
   is called *pair rules*. The rules try to rewrite a function in
   fusion-friendly form, but if by the time we hit the phase 1 fusion still
   did not happen, it's better to rewrite it back.

   It may be not obvious how the result of `"map"` is going to match the
   `"mapList"` rules, but if you keep in mind the definition of `build g = g
   (:) []` and the fact that it will most certainly be inlined by phase 1,
   then `"mapList"` should make perfect sense.

This brings us to the next major topic of this tutorial…

## Fusion

Before we start talking about fusion, we need to define what fusion is.
*Fusion is a technique that allows us to avoid constructing intermediate
results* (lists, vectors, arrays…) *when chaining operations* (functions).

To demonstrate the benefits of fusion it is enough to start with a simple
composition of functions you may find yourself writing quite often. The only
difference is that we will use our own, homemade functions implemented
naively:

```haskell
map0 :: (a -> b) -> [a] -> [b]
map0 _ []     = []
map0 f (x:xs) = f x : map0 f xs

foldr0 :: (a -> b -> b) -> b -> [a] -> b
foldr0 _ b []     = b
foldr0 f b (a:as) = foldr0 f (f a b) as

nofusion0 :: [Int] -> Int
nofusion0 = foldr0 (+) 0 . map0 sqr

sqr :: Int -> Int
sqr x = x * x
```

Let's see how `nofusion0 [0..1000000]` performs:

```
benchmarking nofusion0
time                 155.4 ms   (146.4 ms .. 162.4 ms)
                     0.996 R²   (0.980 R² .. 1.000 R²)
mean                 155.1 ms   (151.3 ms .. 159.0 ms)
std dev              5.522 ms   (3.154 ms .. 7.537 ms)
```

With [`weigh`][weigh] I'm getting the following:

```
Case                  Bytes  GCs  Check
nofusion0       249,259,656  448  OK
```

In a lazy language like Haskell laziness just changes when parts of
intermediate lists are allocated, but they still must be allocated because
the next step in the pipeline takes them as input. That is the overhead we
want to eliminate with fusion.

Can we do better if we rewrite everything as a single function that sums and
multiplies in one pass? Let's give it a try:

```haskell
manuallyFused :: [Int] -> Int
manuallyFused []     = 0
manuallyFused (x:xs) = x * x + manuallyFused xs
```

Let's benchmark it:

```
benchmarking manuallyFused
time                 17.10 ms   (16.71 ms .. 17.54 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 17.18 ms   (16.87 ms .. 17.62 ms)
std dev              932.8 μs   (673.7 μs .. 1.453 ms)

Case                 Bytes  GCs  Check
manuallyFused   96,646,160  153  OK
```

The improvement is dramatic. We just manually fused the two functions and
produced code that runs faster, consumes less memory, and does the same
thing. But should we give up on composability and elegance—the main benefits
of functional programming?

What we would like to achieve is the following:

1. Write beautiful, composable programs.
2. Avoid allocating intermediate results where possible.

The point 2 can be and has been addressed differently:

* We can build a vocabulary of primitive operations in such a way that they
  do not produce results immediately. When the primitives are combined, they
  produce a function that does not produce results immediately either. To
  get the final result, we need a function that can run the composite action
  we have constructed. This is how the [`repa`][repa] package works for
  example.

* We want to have our cake and eat it too. We can expose an interface where
  every primitive produces result immediately, but we also add rewrite rules
  that will make GHC rewrite our expressions in such a way that in the end
  the compiler gets one tight loop without intermediate allocations.

Let's see the first approach in action.

### Fusion without rewrite rules

Returning to the example with `map` and `foldr`, we can re-write the
functions differently using the principles we have just discussed. It is
essential for fusion that we don't write our functions as transformations on
whole lists, because then we are back to the problem of creating those lists
at some point.

In fact, it is not obvious how to have several independent functions that
conceptually work on linked lists without re-creating the list structure in
some form. So, we won't start with fusion that works on linked lists.
Instead, let's start with a more obvious example—arrays.

#### Fusing arrays

An array can be represented as a combination of its size and a function that
takes index and returns a value at that index. We can write:

```haskell
data Array a = Array Int (Int -> a)

rangea :: Int -> Array Int
rangea n = Array n id

mapa :: (a -> b) -> Array a -> Array b
mapa f (Array size g) = Array size (f . g)

foldra :: (a -> b -> b) -> b -> Array a -> b
foldra f b (Array size g) = go 0 b
  where
    go n b' | n < size  = go (n + 1) (f (g n) b')
            | otherwise = b'

fuseda :: Int -> Int
fuseda = foldra (+) 0 . mapa sqr . rangea
```

Here, we have what `repa` calls *delayed arrays*:

* Note that the function `rangea` allows us to create arrays which have
  elements filled with their indices.

* Now if you take a look at `mapa`, it doesn't really do anything other than
  making the indexing function just a little bit more complex, so we don't
  create any intermediate results with it.

* `foldra` allows us to traverse an entire array and get a value computed
  from all its elements. It plays the role of consumer in our case.

* `fuseda 1000000` is the same as `manuallyFused [0..1000000]`, but runs
  much faster.

Of course `fuseda` is not equivalent in power to `manuallyFused`, but it
shows that it is possible to have composability and speed at the same time.
We get this by just changing the indexing function without actually doing
anything with the real array.

#### Fusing lists

Now let's try to do something like this for linked lists. We should start
with the idea of not touching the real list, but modifying a function that
does… what? What should such a function do with a list? If the most basic
function of an array is to be indexed by the position of its elements, then
what is the most basic function of a list? How is a linked list consumed?

If we have a list `[a]`, then the way it's usually consumed is via
*unconsing*. There is a function named `uncons` in `Data.List` for that,
let's take a look at it:

```haskell
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (a:as) = Just (a, as)
```

Here we can get the head of a list and its remainder, but if the list is
empty, we get nothing. This idea is implemented by using `Maybe` in the
type. Let's try to represent a delayed list as a wrapper around
`uncons`-like function:

```haskell
newtype List a = List ([a] -> Maybe (a, [a]))
```

How about `map` and `foldr`? It looks like they follow from that definition
rather naturally:

```haskell
map1 :: (a -> b) -> List a -> List b
map1 g (List f) = List h
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')
```

This does not type check though:

> Couldn't match type `a` with `b`

What's the problem? Well, remember that we just want to make the inner
function more complex. In this particular case, it means that it should
consume a list of type `[a]` and produce a list of type `[b]`, which means
that the inner function should have the type `[a] -> Maybe (b, [a])`
(remember, we produce elements of `[b]` one at a time). Clearly, this type
signature differs from what we have so far. We should adjust it:

```haskell
newtype List a b = List ([a] -> Maybe (b, [a]))
```

The type `List a b` means: we produce a list of elements of type `b` from a
list of elements of type `a`. Not a very intuitive type to have for a thing
like a linked list, but let's put up with it and see what comes out of it.

Now `map1` compiles:

```haskell
map1 :: (a -> b) -> List s a -> List s b
map1 g (List f) = List h
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')
```

The signature says: when you have a list with `a` elements obtained by
consuming something of type `s`, I will give you another list that produces
`b` elements, still consuming the same `s`.

Let's implement `foldr1` (this is not `foldr1` from `Predule`, the numeric
suffix just shows which example it belongs to). To implement `foldr1` we
need something to consume, because we want to get a single value in the end.

We could pass a source of values directly to `foldr1`, but it's not nice for
two reasons:

1. We want the signature of `foldr1` to stay as close to the familiar
   signature of `foldr` as possible.

2. `foldr` is just one primitive that forces a delayed list, what about
   other ones? Should we add an extra argument to all of them? This is not
   elegant.

Perhaps we could store the initial list together with the function we
already have `[a] -> Maybe (b, [a])`:

```haskell
data List a b = List ([a] -> Maybe (b, [a])) [a]
```

We should remember though that we want to pass that list unchanged until we
want to “force” consumption of that list:

```haskell
map1 :: (a -> b) -> List s a -> List s b
map1 g (List f s) = List h s
--             ^           ^
--             |  “as is”  |
--             +-----------+
  where
    h s' = case f s' of
      Nothing       -> Nothing
      Just (x, s'') -> Just (g x, s'')

foldr1 :: (a -> b -> b) -> b -> List s a -> b
foldr1 g b (List f s) = go b s
  where
    go b' s' = case f s' of
      Nothing       -> b'
      Just (x, s'') -> go (g x b') s''
```

Now that we store the initial list in `List` itself, we can write a function
that converts a normal list to a delayed one:

```haskell
fromLinkedList :: [a] -> List a a
fromLinkedList = List uncons
```

And just for the sake of completeness, here is how to get it back:

```haskell
toLinkedList :: List a b -> [b]
toLinkedList (List f s) = unfoldr f s
```

`unfoldr` comes from `Data.List`:

```haskell
unfoldr :: (s -> Maybe (a, s)) -> s -> [a]
unfoldr f s = case f s of
  Nothing      -> []
  Just (x, s') -> x : unfoldr f s'
```

`unfoldr` takes an initial state, passes it to a given function and gets one
element of the final list and new state. It continues till `Nothing` is
returned.

Finally, we can build `fused1` that solves the same problem of summing up a
list of squared numbers:

```haskell
fused1 :: [Int] -> Int
fused1 = foldr1 (+) 0 . map1 sqr . fromLinkedList
```

Elegance and composability: check. Let's benchmark it:

```
benchmarking fused1
time                 3.422 ms   (3.412 ms .. 3.433 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.432 ms   (3.427 ms .. 3.440 ms)
std dev              19.74 μs   (14.15 μs .. 29.65 μs)

Case                 Bytes  GCs  Check
fused1          80,000,016  153  OK
```

It is the fastest implementation so far! What's wrong with `manuallyFused`
though? Shouldn't it be the fastest? Well, it's not tail-recursive, but we
can rewrite it like this:

```haskell
manuallyFused' :: [Int] -> Int
manuallyFused' = go 0
  where
    go !n []     = n
    go !n (x:xs) = go (n + x * x) xs
```

And then it wins:

```
benchmarking manuallyFused'
time                 3.206 ms   (3.202 ms .. 3.210 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.213 ms   (3.210 ms .. 3.217 ms)
std dev              11.28 μs   (7.599 μs .. 17.46 μs)

Case                  Bytes  GCs  Check
manuallyFused'   80,000,016  153  OK
```

Returning to `List`, one thing we would like to do is to remove the type of
elements it consumes. That is, if you have a list of `a` elements, shouldn't
it be `List a`? Let's see the definition of `List` again:

```haskell
data List a b = List ([a] -> Maybe (b, [a])) [a]
```

`[a]` here doesn't really ever change according to our idea of not touching
it. Its type should be just the same as the type of the argument its
companion function `[a] -> Maybe (b, [a])` consumes. We could hide it then
using [existential quantification][existentials]:

```haskell
data List b = forall a. List ([a] -> Maybe (b, [a])) [a]
-- or
data List a = forall s. List (s -> Maybe (a, s)) s
```

With this we get the following signatures (implementations stay the same):

```haskell
fromLinkedList :: [a] -> List a
toLinkedList   :: List a -> [a]
map1           :: (a -> b) -> List a -> List b
foldr1         :: (a -> b -> b) -> b -> List a -> b
```

Much better!

This section has demonstrated that fusion is doable and nice without rewrite
rules. In the next section we will explore so-called fusion systems.

### `build`/`foldr` fusion system

Another approach to avoid intermediate results can be summarized as the
following: we can use functions that operate on normal lists, arrays,
vectors, etc. and let GHC rewrite combinations of these functions so that we
still get one tight loop in the end.

Here is where rewrite rules come into play. There is one problem with this
approach though—too many functions to account for. The standard dictionary
of a functional programmer includes a few list-specific functions: `map`,
`filter`, `(++)`, `foldr`, `foldl`, `dropWhile`, etc. Let's say we want to
be able to work with 10 functions so that they all play nicely together and
get rewritten into high-performance code by GHC. Then we need to account for
(at least!) 10 × 10 = 100 combinations of these functions. Remember about
verifying that every transformation is correct, confluent, and terminating.
Do you feel lucky?

Fusion with many different functions is hard. Instead we would like to do
the following:

1. Rewrite the given function as a combination of very few selected and
   general functions that form a *fusion system*.

2. Do transformations on these functions and simplify their combinations
   using often just one rewrite rule.

In this section we will consider the `build`/`foldr` fusion system. It is
used in the `base` package and powers all the functions on lists we take for
granted.

`foldr` is a familiar function, but what is `build`? It looks like this:

```haskell
build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []
```

The purpose of `build` is to keep a list in delayed form by abstracting over
the `(:)` operation and the empty list `[]`. The argument of the function is
another function that takes a cons-like operation `(a -> b -> b)` and a
null-like starting point `b`. The `g` function produces something of the
type `b`. This is a generalization of functions that produce list-like
constructions.

`build` gives `(:)` to `g` for consing and `[]` as the starting point. This
way we get our list back. The following example is taken from Duncan Coutts'
thesis called [*Stream Fusion: Practical shortcut fusion for coinductive
sequence types*][stream-fusion]:

```haskell
build l == [1,2,3]
  where
    l cons nil = 1 `cons` (2 `cons` (3 `cons` nil))
```

The fusion system with `build` and `foldr` has only one rule:

```haskell
foldr f z (build g) = g f z
```

How does it help to eliminate intermediate lists? Let's see, `build g`
builds some list, while `foldr f z` goes through the list “replacing” `(:)`
applications with `f` and the empty list with `z`, in fact this is a popular
explanation of what `foldr` does:

```haskell
foldr f z [1,2,3] = 1 `f` (2 `f` (3 `f` z))
```

With that in mind, `g` is perfectly prepared to receive `f` and `z` directly
to deliver exactly the same result!

Let's rewrite our example using the `build`/`foldr` fusion system:

```haskell
map2 :: (a -> b) -> [a] -> [b]
map2 _ []     = []
map2 f (x:xs) = f x : map2 f xs
{-# NOINLINE map2 #-}

{-# RULES
"map2"     [~1] forall f xs. map2 f xs               = build (\c n -> foldr2 (mapFB c f) n xs)
"map2List" [1]  forall f.    foldr2 (mapFB (:) f) [] = map2 f
"mapFB"    forall c f g.     mapFB (mapFB c f) g     = mapFB c (f . g)
  #-}

mapFB :: (b -> l -> l) -> (a -> b) -> a -> l -> l
mapFB c f = \x ys -> c (f x) ys
{-# INLINE [0] mapFB #-}

foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 _ b []     = b
foldr2 f b (a:as) = foldr2 f (f a b) as

{-# RULES
"build/foldr2" forall f z (g :: forall b. (a -> b -> b) -> b -> b). foldr2 f z (build g) = g f z
  #-}

fused2 :: [Int] -> Int
fused2 = foldr2 (+) 0 . map2 sqr
```

* We need `NOINLINE` on `map2` to silence the warning that `"map2"` may
  never fire because `map2` may be inlined first. `map2` will not be inlined
  because it is self-recursive, but GHC can't figure that out (yet).

* `mapFB` is a helper function that takes a consing function `c` and the
  function we want to apply to the new head of the list `f`. Lambda's head
  inside binds new head of the list `x` (`f` is applied to it) and `ys`
  which is the rest of the list. The function's LHS has only two arguments
  to facilitate inlining. We want it to be inlined, but only at the end
  because some rules match on it and they would be broken if it were inlined
  too early.

* Inlining is essential because it brings together otherwise separate pieces
  of code and lets GHC manipulate them as a whole.

* We are familiar with the `"map2"` and `"build/foldr2"` rewrite rules
  already. `"mapFB"` is rather trivial. It was noted previously, we have
  here what is called *pair rules*. The `"map2List"` rule rewrites back to
  `map2` if by the phase 1 fusion did not happen. This is also why we have
  the normal definition for `map2`, not `build (…)` one—if fusion doesn't
  happen, `build`/`foldr` implementation actually performs worse.

Let's see how `fused2` performs:

```
benchmarking fused2
time                 107.5 ms   (103.8 ms .. 110.2 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 107.3 ms   (104.6 ms .. 109.8 ms)
std dev              3.768 ms   (2.519 ms .. 6.098 ms)

Case                  Bytes  GCs  Check
fused2          161,259,568  310  OK
```

It is better than the version without fusion, but still far from the faster
implementations we have seen. What's the problem?

Inlining is essential for fusion. Notice that `foldr2` is self-recursive, so
will not be inlined and it will not be rewritten either (unlike `map2`).
Let's make it non-recursive and inline:

```haskell
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2 f z = go
  where
    go []     = z
    go (y:ys) = y `f` go ys
{-# INLINE [0] foldr2 #-}
```

We specify phase 0 because we want GHC to inline it, but only after fusion
has happened (remember that if we inlined it too early it would break our
fusion rules and they wouldn't fire).

Let's give it another shot:

```
benchmarking fused2
time                 17.87 ms   (17.48 ms .. 18.33 ms)
                     0.996 R²   (0.992 R² .. 0.998 R²)
mean                 17.94 ms   (17.61 ms .. 18.42 ms)
std dev              962.6 μs   (689.0 μs .. 1.401 ms)

Case                  Bytes  GCs  Check
fused2           96,646,160  153  OK
```

Nothing to be ashamed of. In fact, this is the same result we would get if
we used `map` and `foldr` directly from the `base` package.

Indeed most functions can be re-written via `foldr` and `build`. Most, but
not all. In particular `foldl` and `zip` cannot be fused efficiently when
written via `build` and `foldr`. Unfortunately, we don't have the space to
cover all the details here. [Duncan Coutts' thesis][stream-fusion] is a
wonderful read if you want to know more about the matter.

### Stream fusion

Now we know what fusion is, but you may have heard of *stream fusion*.
Stream fusion is a fusion technique that fuses streams. What is a stream? I
think it's acceptable to describe a stream as a list, but without the
overhead that is normally associated with linked lists.

#### Stream fusion without skip

In fact, while trying to fuse operations on lists, we already have developed
a stream fusion system! Remember our definition for delayed list:

```haskell
data List a = forall s. List (s -> Maybe (a, s)) s
```

`List` represents what's called *stream without skip*. That is, we can
either get an element with this model or finish processing, no third option.
We will return to the skip problem later in this section.

Let's rewrite the definition in a more common form before we continue:

```haskell
data Stream a = forall s. Stream (s -> Step a s) s

data Step a s
  = Yield a s
  | Done
```

Nothing has really changed here, we just introduced the `Step` data type
which is the same as `Maybe (a, s)`.

What can we do with this approach now? One thing we can attempt is to write
functions that have familiar signatures with linked lists in them and add
rewrite rules to make them run fast.

The rewrite rule we want to use is simpler than the `build`/`foldr` rule.
Remember that we can turn a list into a stream like this:

```haskell
stream :: [a] -> Stream a -- aka fromLinkedList
stream = Stream f
  where
    f []     = Done
    f (x:xs) = Yield x xs
```

and we can get our list back:

```haskell
unstream :: Stream a -> [a] -- aka toLinkedList
unstream (Stream f s) = go s
  where
    go s' = case f s' of
      Done        -> []
      Yield x s'' -> x : go s''
```

Then it should make sense that:

```haskell
stream (unstream s) = s
```

Converting from stream and then back to stream doesn't change anything. If
we write our functions as functions on streams and wrap them into
`stream`/`unstream` pair of functions, we should get functions that operate
on lists:

```haskell
map3 :: (a -> b) -> [a] -> [b]
map3 f = unstream . map3' f . stream

map3' :: (a -> b) -> Stream a -> Stream b
map3' g (Stream f s) = Stream h s
  where
    h s' = case f s' of
      Done        -> Done
      Yield x s'' -> Yield (g x) s''

foldr3 :: (a -> b -> b) -> b -> [a] -> b
foldr3 f z = foldr3' f z . stream

foldr3' :: (a -> b -> b) -> b -> Stream a -> b
foldr3' g b (Stream f s) = go b s
  where
    go b' s' = case f s' of
      Done        -> b'
      Yield x s'' -> go (g x b') s''
```

And at the same time, with this rewrite rule:

```haskell
{-# RULES
"stream/unstream" forall (s :: Stream a). stream (unstream s) = s
  #-}
```

GHC will make intermediate conversions to and fro shrink:

```haskell
fused3 :: [Int] -> Int
fused3 = foldr3 (+) 0 . map3 sqr
--       foldr3' (+) 0 . stream . unstream . map3' sqr . stream
--                       ^               ^
--                       | nuked by rule |
--                       +---------------+
--       foldr3' (+) 0 . map3' sqr . stream
```

`(.)` will be inlined, so the rules will start to match and we will get
exactly this code.

As we already know, it's *fast*:

```
benchmarking fused3
time                 3.450 ms   (3.440 ms .. 3.459 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 3.459 ms   (3.450 ms .. 3.474 ms)
std dev              34.64 μs   (23.78 μs .. 52.99 μs)

Case                  Bytes  GCs  Check
fused3           80,000,016  153  OK
```

Now we have functions that work on normal lists, and yet their combinations
are very fast. Note that exactly this approach is used in popular libraries
like [`vector`][vector] and [`text`][text].

#### Stream fusion with skip

The fusion system without skip works, but it's not powerful enough for all
functions we may want to use, such as `filter`. Let's try to write `filter3`
to find out why. There is probably only one way to write `filter3` given
limitation of the framework we have developed:

```haskell
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f = unstream . filter3' f . stream

filter3' :: (a -> Bool) -> Stream a -> Stream a
filter3' p (Stream f s) = Stream g s
  where
    g s' = case f s' of
      Done -> Done
      Yield x s'' ->
        if p x
          then Yield x s''
          else g s''

fusedFilter :: [Int] -> Int
fusedFilter = foldr3 (+) 0 . filter3 even . map3 sqr
```

The problem here is that if we need to skip a value, the only thing we can
do is to recursively call `g`, which is not good, as the compiler can't
flatten, inline, and further optimize recursive functions.

```
benchmarking fusedFilter
time                 10.79 ms   (10.76 ms .. 10.82 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.81 ms   (10.79 ms .. 10.84 ms)
std dev              69.54 μs   (47.87 μs .. 118.8 μs)

Case                    Bytes  GCs  Check
fusedFilter       100,000,056  192  OK
```

If we introduce `Skip`, `g` ceases to be self-recursive (adjustments to
other functions are not shown here):

```haskell
-- <…>

data Step a s
  = Yield a s
  | Skip s
  | Done

-- <…>

filter3' :: (a -> Bool) -> Stream a -> Stream a
filter3' p (Stream f s) = Stream g s
  where
    g s' = case f s' of
      Done -> Done
      Skip    s'' -> Skip s''
      Yield x s'' ->
        if p x
          then Yield x s''
          else Skip s''
```

This results in some speed and space improvements:

```
benchmarking fusedFilter
time                 8.904 ms   (8.880 ms .. 8.926 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 8.923 ms   (8.906 ms .. 8.941 ms)
std dev              50.94 μs   (36.94 μs .. 74.59 μs)

Case                   Bytes  GCs  Check
fusedFilter       80,000,016  153  OK
```

The introduction of `Skip` promised fame and fortune, but it's not so much
of an improvement in this case.

[original-tutorial]: https://www.stackbuilders.com/tutorials/haskell/ghc-optimization-and-fusion/
[secrets-of-ghc-inliner]: http://research.microsoft.com/en-us/um/people/simonpj/Papers/inlining/inline.pdf
[object-file]: https://en.wikipedia.org/wiki/Object_file
[http-client-tls]: https://hackage.haskell.org/package/http-client-tls
[weigh]: https://hackage.haskell.org/package/weigh
[repa]: https://hackage.haskell.org/package/repa
[stream-fusion]: http://community.haskell.org/~duncan/thesis.pdf
[vector]: https://hackage.haskell.org/package/vector
[text]: https://hackage.haskell.org/package/text
[existentials]: /post/existential-quantification.html
