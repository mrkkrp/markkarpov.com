---
title: Exceptions tutorial from IH book
desc: This is a tutorial about exceptions in Haskell which originally was written as a chapter for the Intermediate Haskell book.
date:
  published: March 3, 2019
---

*This text originally was written as a chapter for the [Intermediate
Haskell][ih] book. Due to lack of progress with the book in the last year,
other authors agreed to let me publish the text as a standalone tutorial so
people can benefit at least from this part of our work.*

```toc
```

## Motivation for exceptions

It is not uncommon to hear the opinion that exceptions are ugly and hard to
work with. Especially in a language like Haskell, with its strong and
powerful type system, perfectly capable of expressing exceptional situations
in a pure way (i.e. in simplest case as a sum type containing both values
that belong to normal results and values that represent adverse outcomes),
why should we “contaminate” such a language with the concept of exceptions?

It is a question we must answer first lest the reader be left in a position
of doubt as to when it is natural to use exceptions and when a more pure way
of organizing an API is better. The topic itself is a bit controversial in
the community:

* Some believe that once you start dealing with exceptions, you should go
  all the way with them and not to pretend that your code is “safe” from
  exceptions by adding additional `MaybeT` or `ExceptT` layers.

* Others are concerned with purity and try to “disinfect” their APIs from
  exceptions completely (see for example the description of the
  [`hasql`][hasql] package, where the exception-free API is apparently
  perceived by the author as a merit).

* The third opinion is that exceptions, as the name suggests, are for
  exceptional situations, while explicit encoding of adverse outcomes in the
  type of returned value is for the situations that are to be quite common,
  and so an obligatory, explicit handling is desirable.

To see the motivation for introducing exceptions in Haskell, let us consider
a simple arithmetic expression:

```haskell
percent :: Double -> Double -> Double
percent x y = (x / y) * 100
```

This does look conventional, but the `(/)` operator is not total! As we all
know well, one does not just divide by zero without certain consequences. So
the whole `percent` function has a “morally wrong” type, or at least such a
type that does not reflect an important part of `percent`'s semantics. We
must note that it is still convenient to have `percent` in this form.

If we went with the approach of purists, we could end up writing programs
like this:

```haskell
percent :: Double -> Double -> Maybe Double
percent x y = (* 100) <$> (x /? y)
  where
    a /? 0 = Nothing
    a /? b = Just (a / b)
```

While it does not look that bad in this little example, we could quickly
find ourselves writing all our code in `Maybe` or `Either` monads and
corresponding transformers. `Either SomeError` monad in particular would
have to account (in `SomeError`) for enormous number of things that can go
wrong. In a real-world program, there are a lot of failure scenarios that
are indeed rather “exceptional” and normally do not happen. `SomeError` also
would need to be extendable somehow because combining code from different
libraries A and B would mean building a sum type capable of representing
exceptional situations of both A and B. It is somewhat inconvenient to code
that way.

What to do? Language design is often about balancing different arguments and
considerations. In our case, we balance ease of use with correctness. By
“correctness” here we mean explicit inclusion of adverse conditions in types
and thus forcing handling of those conditions.

Precise semantics of exception throwing and handling will be given in the
next sections, but it suffices to say for now that intuitively, exceptions
(in many programming languages) have the following properties:

* **They shortcut execution and propagate.** This is handy, because once
  something exceptional has happened, our confidence in success of the
  program should be realistically very low, so we want to shut it down.

* **They can be caught.** This follows the philosophy *make common things
  easy and more tricky things possible*. If an exception can be caught, it
  becomes not just a way to abort the entire program, it becomes a means to
  control program's execution flow opening the possibility to recover from
  an exceptional situation, not less powerful than explicit handling with
  something like the `ExceptT` monad transformer.

Thus, whether to return a result wrapped in `Maybe`, `Either`, or to throw
an exception may be decided on how common the failure in question is and how
much attention we want to draw to it. If the failure is common, it makes
sense to force the consumer of the API to handle it in all cases for her own
good (by wrapping in `Maybe`, `Either`, `ExceptT`, etc.). Otherwise, the
exceptional case should be covered by throwing an exception, which works a
lot like an implicit short-circuiting monad anyway, except it is neither
visible nor the programmer is forced to think about it until he/she needs
to.

Other reasons to prefer throwing exceptions include:

* preserving laziness (as we do not need to check constantly whether we have
  run into a failure and should short-circuit the execution or not);

* more efficient code (the fact that code throws exceptions does not make it
  run slower).

Finally, sometimes it is necessary or convenient to make some function
partial and signal an error that would indicate that the programmer made a
mistake. We try hard to avoid these cases in Haskell, but sometimes one has
to bite the bullet. In that case exceptions allow us to signal the error
using functions like `error` and `assert`.

## Throwing exceptions

Recall from the first example that it should be possible to throw exceptions
from pure code. Given that Haskell has non-strict semantics where evaluation
happens on demand (i.e. in order that is generally hard to predict), and
that throwing an exception actually changes control flow dramatically, we
seem to be in a tricky position immediately because of the possible
non-determinism.

Consider the following example:

```haskell
x :: Integer
x = error "foo" + error "bar"
```

When `x` is evaluated, what will be thrown, the exception from `error "foo"`
or `error "bar"`? It is not possible to tell, since the order in which `(+)`
evaluates its arguments is not defined.

Programs in Haskell often do not have predictable evaluation order, so the
only way to reason about them is to consider values we want to compute.
Indeed, what is an exception as not an additional sort of value that can
inhabit any type?

Let us take a look at the signature of `throw` function and compare it with
bottom, shown here in the form of the `undefined` function:

```haskell
throw     :: Exception e => e -> a
undefined :: a
```

We can see that `throw` can lift an instance of the `Exception` type class
(more about the type class later) into any value at all, just like
`undefined`—bottom value—is a value found in any lifted type. It is
generally well-known that all lifted Haskell values are inhabited by bottom,
but in the presence of exceptions, it is also true that exceptions inhabit
any type in a sense.

## Catching exceptions

If exceptions in Haskell could not be caught, we could consider them bottom
values and the non-determinism `throw` introduces would be not that bad, but
once we equip the language with a way to catch exceptions, they can
influence behavior of our programs, and if exceptions are non-deterministic,
our programs become non-deterministic as well. Not fun!

Let us take an example from the paper [*A semantics for imprecise
exceptions*][a-semantics-for-imprecise-exceptions]:

```haskell
let x = (1/0) + (error "Urk")
in getException x == getException x
```

Here, `getException :: a -> Either Exception a` is a hypothetical function
that allows us to catch exceptions in pure code. What that expression will
return, `True` or `False`? It would seem natural to state that the result
should be `True`, but it could be `False` just as well because of the
non-determinism (i.e. the two occurrences of `x` could be replaced by `x`'s
definition, and `(+)` could evaluate its left or right argument first).

One solution the above-mentioned paper proposes is to return the full set of
exceptions in every case, so `getException x == getException x` evaluates to
`True` reliably. That is not efficient, however, as once an exception is
thrown, we would need to force the entire expression to collect all other
exceptions no matter what.

In reality, there is no good way to make throwing/catching of exceptions in
a non-strict language deterministic, so the next best thing we can do is to
admit that `getException` is not an angel, and so we have to put it into
`IO`, where all sorts of sins are known to be witnessed and, in time,
forgiven.

With `getException :: a -> IO (Either Exception a)`, we can write:

```haskell
main :: IO ()
main = do
  let x = (1/0) + (error "Urk")
  v1 <- getException x
  v2 <- getException x
  print (v1 == v2)
```

This still can print `True` or `False`, but now it is nothing to worry
about, because we are in the `IO` monad, which (as we know) depends on the
state of `RealWorld` (including phase of the moon).

The `Control.Exception` module features many functions useful for dealing
with exceptions. One such function is `throw` (which we already know) yet
there is no `getException`. But there is `catch`:

```haskell
catch :: Exception e
  => IO a        -- ^ The computation to run
  -> (e -> IO a) -- ^ Handler to invoke if an exception is raised
  -> IO a
```

It is quite similar to `getException` in that it also lives in `IO`.
However, the first argument (the computation to catch exceptions from) also
lives in the `IO` monad. That is because `IO a` is a more common case as we
will see in the next section.

## `throwIO`

While it is somewhat hard to predict when `throw` will raise an exception in
pure code, there is no problem with raising an exception from the `IO` monad
(or any `IO`-enabled monad stack) at certain point of *execution*.

The `IO` monad imposes ordering to execution by being a state monad that
passes around a magical value that is never looked at, but creates logical
dependencies between separate `IO` actions that are bound together with the
bind operator `(>>=)`. In this setting it is possible to have `throwIO ::
Exception e => e -> IO a`, which will throw exactly when it is executed:

```haskell
main :: IO ()
main = do
  foo
  throwIO myException
  bar
```

Here `myException` will be thrown after `foo` but before `bar`.

We must note the crucial difference between `throw e :: a` and `throwIO e ::
IO a`, which is exactly the difference between *evaluation* and *execution*:
evaluation triggers exception in the case of `throw e`, while execution
(when the magical state “goes through” `throwIO`) triggers exception in the
case of `throwIO e`. This is best demonstrated by the example with `seq`
found in the docs of `throwIO`:

```haskell
throw e   `seq` x {- => -} throw e -- throwing triggered by evaluation
throwIO e `seq` x {- => -} x       -- throwing triggered by execution
```

``e `seq` x`` is a function that artificially creates a dependency between
`e` and `x`, then returns `x`. Evaluation of `x` requires evaluation of `e`
as if its value directly depended on it. Evaluation forced by `seq` does not
trigger exception in the case of `throwIO`, because the expression is not
*executed*.

`throwIO`, being much more predictable (execution order is more predictable
than evaluation order in Haskell), is generally preferred in the community
and it is a good idea to throw all your exceptions using `throwIO` instead of
`throw` if at all possible.

## Implementation of throwing and catching

A desirable property of exceptions is that computing of a value with an
exceptional component does not make program slower or otherwise
less-efficient. Unlike explicit bookkeeping with `Maybe`, `Either`, or
similar, if an exception is not raised, it is the same as if there is no
exception at all.

Here is how synchronous exceptions are implemented:

* `getException` (or `catch`) marks the evaluation stack and evaluates its
  argument to weak head normal form.

* When an exception is raised, the stack is trimmed to the top-most
  `getException` mark. After that a handler can be run or a value indicating
  that an exception has happened can be returned.

* If evaluation has completed without raising an exception, the computed
  value can be returned.

* If a *pure expression* throws, its thunk is replaced by the exception
  because we can be sure that it will throw every time we try to evaluate
  the thunk.

The section is worth ending with the following quote from [*A semantics for
imprecise exceptions*][a-semantics-for-imprecise-exceptions]:

> Notice that an exceptional value behaves as a first class value, but it is
  never explicitly represented as such. When an exception occurs, instead of
  building a value that represents it, we look for the exception handler
  right away. The semantic model (exceptional values) is quite different
  from the implementation (evaluation stacks and stack trimming). The
  situation is similar to that with lazy evaluation: a value may behave as
  an infinite list, but it is certainly never explicitly represented as
  such.

## Defining hierarchy of exceptions

Let us consider how Haskell keeps its exceptions extendable. It is natural
to think about exceptions as objects in a hierarchy:

![Exception hierarchy][exception-hierarchy]

It is well-known that in object-oriented world the set of operations is
fixed, while the set of objects is extendable. In functional programming the
situation is reversed: typically data types are fixed, and the set of
functions on these objects is extendable. This works pretty well most of the
time, and is especially natural in certain domains, like compilers, but now
that we would like to build a hierarchy for exceptions, the functional
solution is going to look somewhat… *stylized*.

To have an extensible group of data types that share a certain property,
Haskell has type classes. We have already seen that `throw` has the
signature with the mysterious `Exception` constraint:

```haskell
throw :: Exception e => e -> a
```

Anything that is an instance of `Exception` can be thrown. Let us take a
look at the type class:

```haskell
class (Typeable e, Show e) => Exception e where

  toException :: e -> SomeException
  toException = SomeException

  fromException :: SomeException -> Maybe e
  fromException (SomeException e) = cast e

  displayException :: e -> String
  displayException = show
```

First of all, an `Exception` needs to be printable (in case it bubbles to
the top level and terminates a program), hence `Show` is a superclass of
`Extension`. `displayException` (which was added later) allows us to define
a human-readable representation of exception, which is by default
`show`-based.

Of course, the interesting part of the definition here is the `toException`
and `fromException` methods: they provide a way to inject and project an
`Exception` to/from `SomeException`.

And here is how `throw` itself implemented in [`base`][base]:

```haskell
throw :: Exception e => e -> a
throw e = raise# (toException e)
```

This `raise# :: b -> o` primitive can in principle throw anything at all,
but that usage would lead to a chaos because there would be no way to select
a branch of the entire exception hierarchy, or select all possible
exceptions. With every exception converted to `SomeException` before
throwing, we know that every exception is always wrapped in `SomeException`.

`SomeException`, being the root of our hierarchy, defined as:

```haskell
data SomeException = forall e. Exception e => SomeException e
```

It is an existential wrapper around instances of `Exception`. It hides
information about the type `e` stored inside `SomeException`. The only thing
we know about `e` is that it is an instance of `Exception`.

The throwing part should be clear now, but what about catching? We want to
be able to say “I want to catch only arithmetic exceptions, like division by
zero”. Here is where `fromException` comes into play. The default
implementation does everything we need: it unwraps `SomeException`, then
tries to `cast` the inner value to the desired type `e`. If we have got
something inside `Just`, the exception is of the correct type and we can do
something with it, otherwise we just re-throw it. This is what happens
inside of `catch` and similar functions that catch exceptions:

```haskell
catch :: Exception e
  => IO a         -- ^ The computation to run
  -> (e -> IO a)  -- ^ Handler to invoke if an exception is raised
  -> IO a
catch (IO io) handler = IO $ catch# io handler'
  where
    handler' e =
      case fromException e of
        Just e' -> unIO (handler e')
        Nothing -> raiseIO# e
```

*If we try to call `catch` (or other general exception-catching function)
without adding any information about the type of exception to catch, we will
get an ambiguous type error.*

`cast` can be used only with instances of `Typeable`, which is why it is a
superclass of the `Exception` type class. The `Typeable` type class,
derivable by the compiler with the `DeriveTypeable` language extension
(which is not necessary with newer versions of GHC though), allows us to get
type of a value at runtime. We will not go into details of that here, but it
suffices to say that knowing type of a value we can check if it is the same
as the type we want to convert to, so we can go from abstract `e` to a
concrete type.

The `fromException` method works with `SomeException` just fine, it never
fails (so when we catch `SomeException`, we catch every exception possible):

```haskell
instance Exception SomeException where
  -- …
  fromException = Just
```

Right now we have a hierarchy with two levels, `SomeException` as the root,
and all instances of `Exception` as its immediate children. It is however
quite simple to add another existential wrapper between `SomeException` and
concrete instance of `Exception` with the aim of selecting a subset of all
exceptions that are wrapped with that wrapper.

Let us pick the `SomeAsyncException` from `base` as an example of such a
wrapper. It is defined in the same fashion as `SomeException`:

```haskell
data SomeAsyncException = forall e. Exception e => SomeAsyncException e
```

There is nothing unusual in `SomeAsyncException`'s `Exception` instance, it
uses the default definitions. If we take a look how `Exception` instance is
defined for `AsyncException`—data type for async-specific exception like
stack overflow—we can see the following:

```haskell
instance Exception AsyncException where
  toException = toException . SomeAsyncException
  fromException x = do
    SomeAsyncException a <- fromException x
    cast a
```

`toException` simply wraps `AsyncException` in `SomeAsyncException` before
calling `toException` again (now the `Exception` instance of
`SomeAsyncException` is used), which in turn wraps the whole thing in
`SomeException`.

`fromException` first unwraps `x :: SomeException` with `fromException`
(again the instance of `SomeAsyncException` is at work here), the result is
returned in `Maybe`, so the `do` block is in the `Maybe` monad here. If we
were lucky enough to extract `SomeAsyncException`, we can try to `cast` it
to get to the `ArithException` type.

If we define another exception data type with a similar instance definition,
we will be able to catch it and `AsyncException` at the same time by
specifying just `SomeAsyncException` as the type of exception to catch. We
have a working hierarchy for our exceptions now.

*This system of exception organization was proposed in the paper [An
extensible Dynamically-Typed Hierarchy of Exceptions][ext-exceptions].*

## Asynchronous exceptions

So far we have been talking about synchronous exceptions, that is,
exceptions that affect the same thread where evaluation of exceptional value
takes place. *Asynchronous exceptions* are initiated outside of the thread
they affect. Haskell supports *fully-asynchronous exceptions* that allow us
to:

* interrupt the program from the outside (for example when the user presses
  Ctrl+C in a terminal);

* receive signals about exceptional conditions that relate to resource
  consumption: stack overflow, reaching of allocations limits, heap
  overflow, etc.;

* kill threads that are recognized as *permanently blocked* by the Haskell's
  run time system;

* control execution in a multi-threaded environment allowing one thread to
  throw exceptions to another thread to terminate the latter (used to
  implement timeouts and other multi-thread control idioms).

I mentioned the phrase “fully-asynchronous exceptions”, but what exactly
does it mean?

There are different ways to implement asynchronous exceptions. One way
(preferred in the imperative world), is to employ semi-asynchronous
techniques where the thread that initiates an exception sets a flag, which
is then periodically checked by the thread that is to receive the exception.
This however cannot work in Haskell, as we should be able to interrupt
evaluation of pure code, but polling of a global flag would certainly be a
side-effect.

In the volatile, mutable world of imperative programming, it is just too
dangerous to allow interruption at arbitrary moment in time. In contrast to
that, there is absolutely no danger in terminating evaluation of pure code,
because it does not perform any mutations or side-effects. That means that
all pure code works fine with fully-asynchronous exceptions without change.
We should note that this model also has higher modularity because the target
thread does not need to do anything in order to be able to receive
asynchronous exceptions.

Speaking of semantics of values being evaluated in the presence of
asynchronous exceptions, we can say that since the exceptions can strike at
any time during evaluation of any value in our program, we cannot consider
asynchronous exceptions as part of semantics of value that is being
evaluated when such an exception happens.

### Masking asynchronous exceptions

Oftentimes it is necessary to ensure a certain level of atomicity so that
asynchronous exceptions do not strike in the middle of a composite operation
leading to partially updated, inconsistent state of program.

To illustrate this, consider a toy example—the `updateVar` function, which
applies a function to the contents of a mutable variable:

```haskell
updateVar :: MVar a -> (a -> IO a) -> IO ()
updateVar m f = do
  x  <- takeMVar m -- (1)
  x' <- f x        -- (2)
  putMVar m x'     -- (3)
```

A quick reminder, `MVar`s work like this:

* `MVar a` can be either empty or have a value of type `a` inside.

* `takeMVar` extracts a value from given mutable location if it is full,
  otherwise it blocks waiting for the value to be placed there.

* `putMVar` places a value in the specified mutable location. If it is
  already full, it blocks waiting for the location to become empty.

In the current form, `updateVar` is not an atomic operation, because
asynchronous exception can strike between (1) and (3), during evaluation of
`f x` (2) leaving `m` empty. To prevent asynchronous exceptions from
interrupting execution of `updateVar`, we can use `mask`:

```haskell
--      | blocking callback              |
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
--       | unblocking callback  |
```

It probably looks a bit confusing, so let me explain. The argument of `mask`
is the function to run with asynchronous exceptions disabled or “masked” (or
rather delayed). That function in turn receives another callback that allows
us to unmask asynchronous exceptions.

On a lower level, there are wrappers like `block :: IO a -> IO a` and
`unblock :: IO a -> IO a` that modify the *masking state* of thread while
the inner code in executed. When `block` and `unblock` are nested, only the
innermost layer matters:

```haskell
-- async exceptions:
block   (block io)   -- io blocked
block   (unblock io) -- io unblocked
unblock (unblock io) -- io unblocked
unblock (block io)   -- io blocked
```

Masking state of the current thread can be looked up with the
`getMaskingState :: IO MaskingState` function. The `MaskingState` type is
the enumeration of all possible masking states:

```haskell
getMaskingState :: IO MaskingState

data MaskingState
  = Unmasked              -- ^ Thread is not inside mask or uninterruptibleMask
  | MaskedInterruptible   -- ^ Thread is inside mask.
  | MaskedUninterruptible -- ^ Thread is inside uninterruptibleMask.
```

`mask` is defined like this:

```haskell
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
mask io = do
  b <- getMaskingState
  case b of
    Unmasked              -> block $ io unblock
    MaskedInterruptible   -> io block
    MaskedUninterruptible -> io blockUninterruptible
```

The definition gives us a hint about behavior of nested `mask`s. We can see
that the inner “unblocking” callback does not necessarily unmask
asynchronous exceptions, but rather returns masking state to what it was
outside of current `mask`. Ignore `blockUninterruptible` for now, we will
get to it soon.

Let us continue with the examples:

```haskell
mask (\_ -> mask (\_ -> io))               -- io blocked
mask (\_ -> mask (\restore -> restore io)) -- io blocked
```

Here the first `mask` layer blocks asynchronous exceptions and the second
`mask` layer can not unblock with `restore` because that callback only
returns masking state to what it was outside of the respective `mask` call.
The behavior is exactly what we usually want: when we enclose a region of
code with `mask`, we want to be sure async exceptions are masked, no matter
what happens inside.

The guarantee extends even further: `mask` affects all code that is
lexically enclosed by it, even if we fork with `forkIO` and `forkOS` (the
new thread inherits the parental masking state). This is important because
otherwise there would be no way to prevent exceptions happening between (1)
and (2) in the following example, and so they could leak:

```haskell
x <- takeMVar m
forkIO $               -- (1)
  mask $ \unmask -> do -- (2)
    x' <- catch (unmask (…))
      (\e -> putMVar m x >> throw (e :: SomeException))
    putMVar m x'
```

There is also `mask_` in `Control.Exception` for the cases when we only want
to block:

```haskell
mask_ :: IO a -> IO a
mask_ io = mask $ \_ -> io
```

Let us use `mask` in our example to make it more correct in the presence of
asynchronous exceptions:

```haskell
updateVar :: MVar a -> (a -> IO a) -> IO ()
updateVar m f = mask $ \unmask -> do
  x  <- takeMVar m         -- (1)
  x' <- unmask (f x)       -- (2)
  putMVar m x'             -- (3)
```

This is a bit better in the sense that asynchronous exceptions can not
strike between (1) and (2), (2) and (3), but they still can happen on the
line (2), while `f x` is being evaluated.

There are two possible ways to fix it:

* Run `f x` without unmasking, wrapping the whole thing with `mask_`. This
  solution has a flaw however: if `f x` takes a very long time to compute or
  worse yet, hangs, there is no way to abort the thread—we are stuck.

* Add `catch` to handle exceptions thrown from `f x` and so re-fill `m` with
  the old value should we be forced to abort the computation.

Let us explore the solution involving `catch`:

```haskell
updateVar :: MVar a -> (a -> IO a) -> IO ()
updateVar m f = mask $ \unmask -> do
  x <- takeMVar m
  x' <- catch (unmask (f x))
    (\e -> putMVar m x >> throw (e :: SomeException))
  putMVar m x'
```

This code behaves as expected: the value inside `m` is guaranteed to be
updated and if an asynchronous exception strikes while we are in the unmasked
state old value will be preserved.

Now here comes a subtle detail about masking. It is explained in papers and
in the Simon Marlow's book [*Parallel and Concurrent Programming in
Haskell*][parallel-and-concurrent], but we will try to re-iterate it here as
concisely and clearly as possible (because it is easy to get confused):

* Blocking (for example while waiting for a value to be put into `m`, as in
  `takeMVar m`) with asynchronous exceptions disabled is a bad thing. This
  increases the probability of deadlock that cannot be interrupted (normally
  some deadlocks are detected by the Haskell's runtime system and an
  exception such as `BlockedIndefinitelyOnMVar` is thrown to blocking
  threads to shut them down).

* To deal with that, certain operations like `takeMVar` were made
  *interruptible*, that is, the `mask` can be “pierced” and an asynchronous
  exception can get through it. But they are only interruptible *when they
  actually block*, for example when `takeMVar m` is waiting for `m` to be
  filled with a value (otherwise `mask` would be meaningless).

Let us consider various scenarios of what might happen:

* When `takeMVar m` is blocking, we can throw an asynchronous exception to
  the thread we are working in and it will be treated in fact as a
  *synchronous* exception happening exactly before `takeMVar m`. The thread
  will be killed. *Asynchronous exceptions become synchronous* inside
  `mask`, because it is known which operations are interruptible and so the
  exception becomes synchronized with respect to other operations in the
  thread.

* `putMVar` is also interruptible, because it can block (waiting for its
  argument to become empty). Does it mean that an asynchronous exception can
  interrupt either of our `putMVar`s from the example above? No, it can not.
  We can see from the example that `m` is guaranteed to be empty after the
  last `takeMVar` invocation, so `putMVar`s cannot be interrupted in that
  case, as it does not need to wait for `m` to become empty.

As we can see, the tricky part in behavior of so-called “interruptible”
operations is that they only become interruptible when they actually block.
*All operations that may block indefinitely are designated as
interruptible.*

### Uninterruptible masking

Now that we know that masking asynchronous exceptions with `mask` can be
pierced, we also need to discuss a way to mask in an uninterruptible fashion
and when it is useful.

Similar to `mask`, there is `uninterruptibleMask`, which uses
`blockUninterruptible` instead of `block`. It should be noted that the
ability to interrupt operations is there for a good reason: if your program
hangs inside of an uninterruptible mask, it will become unresponsive and
there will be no way to kill it.

That said, sometimes uninterruptible mask is useful. To show an example of
that, let us first introduce the `bracket` function:

```haskell
bracket
  :: IO a         -- ^ Computation to run first (acquire resource)
  -> (a -> IO b)  -- ^ Computation to run last (release resource)
  -> (a -> IO c)  -- ^ Computation to run in-between
  -> IO c         -- ^ Returns the value from the in-between computation
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
```

Everything in `bracket` happens with asynchronous exceptions masked, except
for `thing a`.

`onException :: IO a -> IO b -> IO a` is another useful function from the
`Control.Exception` module. ``f `onException` g`` tries to run `f`, but if
it throws an exception, `onException` runs `g` and re-throws:

```haskell
onException :: IO a -> IO b -> IO a
onException f g = f `catch` \e -> do
  _ <- g
  throwIO (e :: SomeException)
```

`bracket` allows us to guarantee that some resource will be released no
matter what. However, there are a number of things that can invalidate this
promise. For now we will consider one particular use case—working with
temporary directories:

```haskell
withTempDirectory
  :: FilePath           -- ^ Directory in which to create temp directory
  -> String             -- ^ Name pattern for the temp directory
  -> (FilePath -> IO a) -- ^ Callback that receives the name of temp directory
  -> IO a               -- ^ Result returned from the callback
withTempDirectory targetDir template = bracket
  (createTempDirectory targetDir template)
  (ignoringIOErrors . removeDirectoryRecursive)
```

The failure scenario is taken from [this blog post by Roman
Cheplyaka][bracket]:

* `createTempDirectory targetDir template` completes successfully.

* User's action completes successfully creating some files in the temporary
  directory (the third argument of `withTempDirectory`, it is not bound
  explicitly because of eta-reduction).

* Clean up `removeDirectoryRecursive` begins, but while it is working an
  asynchronous exception is received. Even though it is inside `mask` (it is
  `after a` in the definition of `bracket`), individual file deletions are
  interruptible (because they may block indefinitely), so the mask gets
  pierced. Result: the guarantees of `withTempDirectory` are broken.

One solution is to use something like this:

```haskell
withTempDirectory'
  :: FilePath
  -> String
  -> (FilePath -> IO a)
  -> IO a
withTempDirectory targetDir template action =
  uninterruptibleMask $ \restore -> do
    tdir <- createTempDirectory targetDir template
    let after = ignoringIOErrors . removeDirectoryRecursive
    r <- restore (action tdir) `onException` after tdir
    after tdir
    return r
```

`uninterruptibleMask` makes sure that creation of temporary directory and
its deletion will not be interrupted.

Use `uninterruptibleMask` only when you know for sure that its inner
computation will not ever block for a long period of time. Chances are, most of
the time you will not need `uninterruptibleMask`, but it is good to know about
it and have it in your programming toolbox.

*The functions that guarantee release of resources in case of exception,
such as `bracket`, may also let you down for reasons that are not related to
asynchronous exceptions. It is important to remember that when the main
thread of program finishes, the program exits instantly, without sending
asynchronous exceptions to child threads and so without giving them a chance
to clean up properly. If the program in question uses `bracket` (or similar
function) to manipulate internal data (such as an `MVar`), it is OK.
However, if it manipulates an object from the outside (for example, creates
a temporary directory and then deletes it), it is only guaranteed to clean
up properly if it is run in the main thread. The most natural solution to
this is not to fork manually, but with `withAsync` from the [`async`][async]
package. That function will ensure that the forked thread is killed when the
inner computation returns or throws.*

### The `throwTo` function

`throwTo` allows us to raise an arbitrary exception in a thread with known
`ThreadId`:

```haskell
throwTo :: Exception e => ThreadId -> e -> IO ()
```

The docs for `throwTo` are *exceptionally* good, so let us just quote them
here with some clarifications:

* Exception delivery synchronizes between the source and target thread:
  `throwTo` does not return until the exception has been raised in the
  target thread. The calling thread can thus be certain that the target
  thread has received the exception. Exception delivery is also atomic with
  respect to other exceptions. Atomicity is a useful property to have when
  dealing with race conditions: e.g. if there are two threads that can kill
  each other, it is guaranteed that only one of the threads will get to kill
  the other.

* This means in particular that use of `mask` can block the thread that
  attempts to throw an exception to a thread in masked state. Like any
  blocking operation, `throwTo` is therefore interruptible, but unlike other
  interruptible operations, however, `throwTo` is *always interruptible*,
  even if it does not actually block.

* If the target thread is currently making a foreign call, then the
  exception will not be raised (and hence `throwTo` will not return) until
  the call has completed. This is the case regardless of whether the call is
  inside a mask or not. However, in GHC a foreign call can be annotated as
  interruptible, in which case a `throwTo` will cause the RTS to attempt to
  cause the call to return.

* There is no guarantee that the exception will be delivered promptly,
  although the runtime will endeavour to ensure that arbitrary delays do not
  occur. In GHC, an exception can only be raised when a thread reaches a
  *safe point*, where a safe point is where memory allocation occurs. Some
  loops do not perform any memory allocation inside the loop and therefore
  cannot be interrupted by a `throwTo`.

* Whatever work the target thread was doing when the exception was raised is
  not lost: the computation is suspended until required by another thread.
  This is best understood if we imagine an expensive pure computation that
  is interrupted by an asynchronous exceptio—what we have evaluated so far
  is not lost.

* If the target of `throwTo` is the calling thread, then the behaviour is
  the same as `throwIO`, except that the exception is thrown as an
  asynchronous exception. This means that if there is an enclosing pure
  computation, which would be the case if the current IO operation is inside
  `unsafePerformIO` or `unsafeInterleaveIO`, that computation is not
  permanently replaced by the exception, but is suspended as if it had
  received an asynchronous exception.

* Note that if `throwTo` is called with the current thread as the target,
  the exception will be thrown even if the thread is currently inside `mask`
  or `uninterruptibleMask`.

This are certainly a lot of subtle points to keep in mind. The main takeaway
is that `throwTo` is synchronized with the thread it throws to (i.e. it does
not return till the exception has been raised) and so it may block if the
target thread is in `mask` or doing a foreign call.

### How asynchronous exceptions are implemented

Information on implementation of asynchronous exceptions can be found in the
paper [*Asynchronous exceptions in Haskell*][async-exceptions-in-haskell].
Here we just briefly enumerate the main points for the curious readers:

1. Every thread has a data block associated with it to store thread-specific
   data. The data includes the masking state we have discussed and a queue
   of asynchronous exceptions pending delivery.

2. When thread is not in masked state, the queue of asynchronous exceptions
   is checked at regular intervals and if there are exceptions pending, they
   are delivered.

3. When thread's masking state goes from “masked” to “unmasked”, the queue
   is checked right away instead of waiting for the next check as described
   in (2).

4. When `getException` or `catch` marks the evaluation stack, it also saves
   current masking state so it can be restored after handling an exception.

5. Two more marks (or “frames” in the terminology of the above-mentioned
   paper) are necessary: one for `block` and another one for `unblock`. When
   execution passes either of these, masking state changes accordingly.
   There are also some rules for the purpose of keeping the evaluation stack
   from growing unnecessarily, but we will not include them here.

6. `throwTo` simply places an exception in the queue of target thread then
   blocks till the exception is delivered.

## Lifting exception-related functionality

The functions from the `Control.Exception` module (which we advise to
examine on your own too, it is well documented) cover all the needs we might
have dealing with exceptions. However, they only work in the `IO` monad.
That is a sane choice for the module from the `base` package as lifting of
these functions into arbitrary monad stacks is not always straightforward,
and `base` cannot depend on [`transformers`][transformers].

In this section, we are going to look at the [`exceptions`][exceptions]
package first. Then we will consider a somewhat more flexible but
lower-level alternative in the form of the [`monad-control`][monad-control]
package. Finally, we will examine the newer [`unliftio`][unliftio] package.

### Lifting with `exceptions`

The `exceptions` package provides three principal type classes:

* `MonadThrow` for monads that support an analogue of `throwIO`.

* `MonadCatch` for monads that provide lifted `catch`.

* `MonadMask` for monads that provide lifted `mask` and
  `uninterruptibleMask` functions.

`throwM` of `MonadThrow` is defined just as lifted `throwIO`, except for
pure monads, for which it just returns an “empty” value (which in monadic
setting also shortcuts execution):

```haskell
class Monad m => MonadThrow m where
  throwM :: Exception e => e -> m a

instance MonadThrow [] where
  throwM _ = []

instance MonadThrow Maybe where
  throwM _ = Nothing

instance MonadThrow IO where
  throwM = Control.Exception.throwIO

instance MonadThrow m => MonadThrow (StateT s m) where
  throwM = lift . throwM

-- etc.
```

Indeed, there is generally no problem with throwing exceptions, so it is
rather trivial to define such a class. Arguably, the ability to throw in
pure setting is a good thing, although the practice shows that industrial
users are mainly interested in lifting throwing functionality through
monadic transformers.

Next, here goes `MonadCatch`:

```haskell
class MonadThrow m => MonadCatch m where
  catch :: Exception e => m a -> (e -> m a) -> m a

instance MonadCatch IO where
  catch = Control.Exception.catch

instance MonadCatch m => MonadCatch (StateT s m) where
  catch = liftCatch catch

-- etc.
```

Not every monad that implements `MonadThrow` is an instance of `MonadCatch`.
For example `Maybe` throws away the information about what went wrong, so
there cannot be a `MonadCatch` instance for it.

On the other hand, if we try to convert something we caught to the expected
type and fail (`fromException` returns `Nothing`), we need to re-throw. This
is why `MonadThrow` is a superclass of `MonadCatch`.

What about the implementations? Well, the case of `IO` looks trivial, as do
most others. The instance definitions for `StateT` and other transformers
are more interesting though. We re-use here `catch` defined for `m` monad,
but lift it using `liftCatch`, which is:

```haskell
-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (a,s) -> Catch e (StateT s m) a
liftCatch catchE m h =
  StateT $ \s -> runStateT m s `catchE` \e -> runStateT (h e) s
```

Where `Catch` is just a type synonym:

```haskell
type Catch e m a = m a -> (e -> m a) -> m a
```

From that, we can see that `liftCatch` turns a catching function that
catches `e` in monad `m` which returns a value of type `(a,s)` into a
catching function that catches the same exception `e` in monad `StateT s m`
and returns just `a`. This is the sort of code that is type-driven, meaning
we can write the implementation mostly by just following the types. If we
replace `catchE` by `catch` from `Control.Exception` for simplicity and
assume `m` fixed to `IO`, the function starts to look more understandable:

```haskell
-- | Lift a @catchE@ operation to the new monad.
liftCatch
  :: Exception e
  => StateT s IO a     -- ^ m, action to run
  -> (e -> StateT s IO a) -- ^ h, exception handler
  -> StateT s IO a
liftCatch m h =
  StateT $ \s -> runStateT m s `catch` \e -> runStateT (h e) s
```

The result is in `StateT`, so we start by putting its constructor in place,
and so we have the state `s`. Both arguments of “vanilla” `catch` must be in
plain `IO`, so we have to run `m` and `h` in order to unwrap them and get to
the `IO` monad.

This shows that it is quite feasible to lift `catch` into most monadic
stacks. Let us see about `MonadMask`.

```haskell
class MonadCatch m => MonadMask m where
  mask                :: ((forall a. m a -> m a) -> m b) -> m b
  uninterruptibleMask :: ((forall a. m a -> m a) -> m b) -> m b

instance MonadMask IO where
  mask                = Control.Exception.mask
  uninterruptibleMask = Control.Exception.uninterruptibleMask

instance MonadMask m => MonadMask (StateT s m) where
  mask a = StateT $ \s -> mask $ \u -> runStateT (a $ q u) s
    where
      q :: (m (a, s) -> m (a, s)) -> StateT s m a -> StateT s m a
      q u (StateT b) = StateT (u . b)
  uninterruptibleMask a =
    StateT $ \s -> uninterruptibleMask $ \u -> runStateT (a $ q u) s
      where
        q :: (m (a, s) -> m (a, s)) -> StateT s m a -> StateT s m a
        q u (LazyS.StateT b) = StateT (u . b)

-- etc.
```

The ability to receive/catch exceptions is a logical prerequisite for
masking of asynchronous exceptions, hence the `MonadCatch` (and by
implication `MonadThrow`) is a superclass of `MonadMask`.

Let us take a look at the case of `StateT s m` again. `mask` receives the `a
:: (forall a. m a -> m a) -> m b` argument which expects to get this `forall
a. m a -> m a`—unmasking callback working in the `StateT s m` monad. Let us
see how we provide that. First of all, the result of `mask` must be in
`StateT s m`, so we put the `StateT` constructor in place and so we get the
state `s`. Inside the lambda that binds `s` we use `mask` of underlying
monad `m`, which we require to be an instance of `MonadMask`. That `mask` in
turn receives unmasking callback `u` (remember that the `\u -> …` lambda is
in masked state, so its argument `u` happens to be the unmasking callback)
which works in the inner monad `m`, not `StateT s m`. To feed this unmasking
callback into `a` we lift it from `m (a, s) -> m (a, s)` to `StateT s m a ->
StateT s m a` with `q` which is rather trivial. Finally, `a` applied to `q
u` is of the type `StateT s m`, but the inner `mask` expects something in
`m`, so we run it. The entire `mask $ \u -> runStateT (a $ q u) s` thing
thus ends up being of the right type `m (a, s)` to be put into `StateT $ \s
-> …`. Again, this is an example of type-driven programming.

`uninterruptibleMask` works exactly the same.

Having these there basic concepts (throwing, catching, and masking async
exceptions) abstracted in that way, the `exceptions` package seems to be
well-equipped to define lifted versions of most everything. For example,
here is how `bracket` is defined:

```haskell
bracket :: MonadMask m => m a -> (a -> m b) -> (a -> m c) -> m c
bracket acquire release use = mask $ \unmasked -> do
  resource <- acquire
  result <- unmasked (use resource) `onException` release resource -- (1)
  _ <- release resource                                            -- (2)
  return result
```

It does make sense, although the generality of the lifting technique until
recently did not extend quite long enough to support all useful monads.
Also, when we deal with stateful monadic stacks, there may be several
different implementations of `bracket` that differ in how the state is
passed around, and depending on your use-case, you may want one or another:

* State from successful computation `use` affects `release`. When `use`
  fails, `release` runs with the same state as were passed to `use`.
* `release` always runs with the same state as `use`.
* `acquire` can/cannot affect state that is passed to `use`.
* Etc.

As an exercise, figure out how `bracket` will pass around state in the case
of `StateT s m` monad transformer. Definitions of `MonadCatch` and
`MonadMask` instances for `StateT s m` and `bracket` are shown above. Hint:
concentrate on the difference between the lines (1) and (2). You will also
need the definition of lifted `onException` from `exceptions`:

```haskell
onException :: MonadCatch m => m a -> m b -> m a
onException action handler = action `catchAll` \e -> handler >> throwM e
  where
    catchAll :: MonadCatch m => m a -> (SomeException -> m a) -> m a
    catchAll = catch -- specialization of catch that catches all exceptions
```

Next, let us consider `ExceptT e m` as our monad. This monad transformer is
short-circuiting:

```haskell
newtype ExceptT e m a = ExceptT (m (Either e a))

instance (Monad m) => Monad (ExceptT e m) where
  return a = ExceptT $ return (Right a)
  m >>= k = ExceptT $ do
    a <- runExceptT m
    case a of
      Left  e -> return (Left e)
      Right x -> runExceptT (k x)
```

In English, if at some point `(>>=)` gets `m` with inner value inside
`Left`, we finish immediately ignoring the computation `k` altogether. This
is how monadic bind works for this transformer. Applying this to the
definition of `bracket` we can see that if `acquire` or `use resource`
happen to return something inside `Left`, the later actions (including
`release`) have no chance to run—guarantees of `bracket` are broken.

Until recently `MonadMask` had to rely on the guarantee that `MonadCatch`
catches all possible exceptions and there is no other way for the
computation to exit early—therefore, although there was a possible instance
of `MonadCatch` for `ExceptT`, it was not considered valid and so there was
no `MonadMask` instance for `ExceptT`.

Since version `0.9.0` of the `exceptions` package, the problem is solved by
introducing a new method of the `MonadMask` type class:

```haskell
generalBracket
  :: m a
  -- ^ Acquire some resource
  -> (a -> ExitCase b -> m c)
  -- ^ Release the resource, observing the outcome of the inner action
  -> (a -> m b)
  -- ^ Inner action to perform with the resource
  -> m (b, c)
```

where `ExitCase` is:

```haskell
data ExitCase a
  = ExitCaseSuccess a -- ^ Success
  | ExitCaseException SomeException -- ^ Exception thrown
  | ExitCaseAbort -- ^ Aborting without exceptions, e.g. with Left
  deriving Show
```

The solution is simple: for monads like `ExceptT` we explicitly handle the
case when the inner computation exited by “aborting” computation (e.g. with
`Left` result), not by throwing an exception. Then in the release handler we
can clean up in that case as well.

Functions like `bracket` and `finally` were previously defined using the
`mask` method of `MonadMask`, now they are defined via `generalBracket`
which allows them to work in monads like `ExceptT` properly.

To learn how to write instances see [the documentation][general-bracket] for
`generalBracket` on Hackage. The documentation is so good that it would be
pure duplication to try to add an explanation here as well.

### Lifting with `monad-control`

The approach used in the [monad-control][monad-control] package is to
temporarily “unlift” complex monadic stack to some base monad such as `IO`
so we can use the existing functions like `catch` and `bracket` from `base`,
then “restore” the monadic stack when we are done.

To understand why we need to do unlifting instead of following a more
familiar path—lifting `IO` computation into a more complex `IO`-enabled
monadic stack—consider the `catch` function:

```haskell
catch :: Exception e
  => IO a              -- ^ The computation to run
  -> (e -> IO a)       -- ^ Handler to invoke if an exception is raised
  -> IO a
```

Even though we can use `liftIO` for lifting after applying arguments to
`catch`, it also expects `IO a` as argument, and here `liftIO` cannot help,
in fact, we need something opposite.

We need a way to unlift to `IO a`. The good news is that there is a way to
unlift without losing information so we can re-construct virtually any
monadic stack built from the familiar transformers.

To understand why it is so, look at the definitions of some monad
transformers:

```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
```

You probably have noticed the common structure inside the wrappers: `m`
containing what we will call *state*—information that can be used to
recreate the transformer. `ReaderT` is stateless—its state is just the
monadic value `a` (in `ReaderT r m a`), while `StateT` carries state `s`,
and so its state is `(a, s)`, similar to `WriterT`. (The lambdas are not of
any concern to us here, as we can wrap anything with a lambda, it is not
part of the state.)

Since we cannot escape `IO`, there is no `IOT` monad transformer, and so if
there is `IO` in monadic stack, it is always at the bottom. If we remove the
`newtype`s, unlifting a complex monad transformer, we always get a value
that produces something like `IO st` where `st` is the state we mentioned
earlier. For example:

```haskell
type MyStack r s w a = ReaderT r (StateT s (WriterT w IO)) a

{- isomorphic to -}

r -> s -> IO ((a, s), w)
```

Simply put, when transformers are stacked, their states are combined. With
the example shown, if we are currently in `MyStack`, we have `r` and `s` to
apply and get `IO ((a, s), w)`, which we can pass to a function such as
`catch`.

Result of `catch`, being of the type `IO ((a, s), w)` can be wrapped back
into lambdas and `newtype`s to the effect that we restore `MyStack` monad
back. This is the idea behind `monad-control`.

Now that the idea should be clear, let us see what form it takes in the
actual library.

Meet the `MonadBaseControl b m` type class which allows us to lift functions
that work in monad `b` (like `catch`, `b ~ IO`) into a more complex monadic
stack `m`:

```haskell
class MonadBase b m => MonadBaseControl b m | m -> b where -- (1)
  type StM m a :: *                                        -- (2)
  liftBaseWith :: (RunInBase m b -> b a) -> m a            -- (3)
  restoreM :: StM m a -> m a                               -- (4)

type RunInBase m b = forall a. m a -> b (StM m a)          -- (5)
```

1. `MonadBase` is a superclass of `MonadBaseControl`. `MonadBase b m` is
   best understood as a generalization of `MonadIO` where we can lift
   arbitrary base monad `b` (not just `IO`) into `m`. `MonadBase` is a
   fairly trivial tool and will not be of any interest in this chapter. Note
   that `MonadBaseControl` is a multi-parameter type class which has the
   functional dependency `m -> b`. The functional dependency helps the
   compiler resolve type ambiguity. It says: if you know what `m`
   instantiated to, then you can learn `b` by searching existing instances
   for an instance with matching `m`. It is guaranteed by the compiler that
   `b` is uniquely identified by choice of `m`.

2. `StM m a` is an associated type of the type class `MonadBaseControl`.
   This feature is enabled by the `-XTypeFamilies` language extension. This
   is the type of state we have talked about.

3. `liftBaseWith` is a function that takes another function `RunInBase m b
   -> b a`, inside which we generate a value in base monad `b`. The
   `RunInBase m b` argument is the “running” or “unlifting” callback. Look
   at the definition (5), for a value `m a` this function strips all monadic
   layers till the desired base monad `b` inside which we get the state `StM
   m a`.

4. `restoreM` allows us to restore/replace state in the monad `m` from a
   value of the type `StM m a`.

5. Type-synonym for the unlifiting callback, as explained in (3).

It is beneficial to learn how various instances of `MonadBaseControl` are
defined. Let us start from `MonadBaseControl IO IO`:

```haskell
instance MonadBaseControl IO IO where
  type StM IO a  = a
  liftBaseWith f = f id
  restoreM       = return
```

This should make sense, to unlift `IO a` we do not need to do anything.
Similarly it is not difficult to restore this monad's state because it has
none.

`ReaderT` is a bit more interesting:

```haskell
instance MonadBaseControl b m => MonadBaseControl b (ReaderT r m) where
  type StM (ReaderT r m) a = StM m (StT t a)
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
```

Here we step into the territory of monad transformers. The definition builds
on the assumption that the inner monad `m` in `ReaderT r m` is also an
instance of `MonadBaseControl b m`. To use that instance though, we first
need to go from `ReaderT r m` to `m`, i.e. we need to unlift a monad
transformer, `ReaderT`. How do we do that?

The answer is: using the second important type class of
`monad-control`—`MonadTransControl`:

```haskell
class MonadTrans t => MonadTransControl t where
  type StT t a :: *
  liftWith :: Monad m => (Run t -> m a) -> t m a -- like liftBaseWith
  restoreT :: Monad m => m (StT t a) -> t m a    -- like restoreM

type Run t = forall n b. Monad n => t n b -> n (StT t b)
```

`MonadTransControl` to `MonadTrans` is the same as `MonadBaseControl` to
`MonadBase`. `MonadTrans` establishes a “connection” between monad
transformer `t m` and its inner monad `m` allowing to lift `m` into `t m`
with `lift`. Note that `lift` lifts through only one monadic layer.
`MonadTransControl` also allows us to unlift through one monadic layer.
`MonadBase`, as we have mentioned already, allows us to lift through many
layers with a single `liftBase` call, similarly `liftBaseWith` lifts through
all layers till we reach the base monad.

Knowing this, let us quickly go through the definitions of
`defaultLiftBaseWith` and `defaultRestoreM`:

```haskell
defaultLiftBaseWith
  :: (MonadTransControl t, MonadBaseControl b m)
  => (RunInBaseDefault t m b -> b a)
  -> t m a
defaultLiftBaseWith f = liftWith $ \run ->
  liftBaseWith $ \runInBase ->
    f $ runInBase . run

defaultRestoreM
  :: (MonadTransControl t, MonadBaseControl b m)
  => ComposeSt t m a
  -> t m a
defaultRestoreM = restoreT . restoreM
```

`defaultLiftBaseWith` unlifts through one monadic layer with `liftWith`,
then we again call `liftBaseWith` recursively which either happens to use a
“terminal” instance like `MonadBaseControl IO IO` or another instance which
unlifts through next monadic layer in exactly the same manner.

`defaultRestoreM` restores state in base monad with `restoreM` than lifts
base monad through one layer with `restoreT`.

`defaultLiftBaseWith` and `defaultRestoreM` are used to implement not only
`ReaderT` instance, but also `StateT` and most other instances because
actual transformer-specific logic is defined in `MonadTransControl`
instances:

```haskell
instance MonadTransControl (ReaderT r) where
  type StT (ReaderT r) a = a
  liftWith f = ReaderT $ \r ->
    f $ \t -> runReaderT t r
  restoreT = ReaderT . const

instance MonadTransControl (StateT s) where
  type StT (StateT s) a = (a, s)
  liftWith f = StateT $ \s ->
    liftM (\x -> (x, s)) (f $ \t -> runStateT t s)
  restoreT = StateT . const
```

A common idiom with `monad-control` is to return monadic state from the
function passed to `liftBaseWith`:

```haskell
RunInBase m b -> b (StM m a)
```

And then use `restoreM` to immediately restore monadic state. This is
captured by the `control` helper:

```haskell
control :: MonadBaseControl b m
  => (RunInBase m b -> b (StM m a))
  -> m a
control f = liftBaseWith f >>= restoreM
```

Finally, let us show how we can use `bracket` from `Control.Exception` with
a complex monadic stack:

```haskell
liftedBracket
  :: StateT s IO a        -- ^ Computation to run first (acquire resource)
  -> (a -> StateT s IO b) -- ^ Computation to run last (release resource)
  -> (a -> StateT s IO c) -- ^ Computation to run in-between
  -> StateT s IO c
liftedBracket acquire release use = control $ \runInBase ->
  bracket
    (fst <$> runInBase acquire) -- we dicard state s from (a, s)
    (runInBase . release)
    (runInBase . use)
```

Note that `acquire` and `release` cannot modify state `s`, it is restored
from the state returned from `runInBase . use`. With a bit more effort we
could “fix” that:

```haskell
liftedBracket'
  :: StateT s IO a        -- ^ Computation to run first (acquire resource)
  -> (a -> StateT s IO b) -- ^ Computation to run last (release resource)
  -> (a -> StateT s IO c) -- ^ Computation to run in-between
  -> StateT s IO c
liftedBracket' acquire release use = control $ \runInBase ->
  bracket
    (runInBase acquire) -- returns (a, s)
    (\(a, s) -> runInBase (put s >> release a))
    (\(a, s) -> runInBase (put s >> use a))
```

State modifications made in `acquire` now influence both `restore` and
`use`. State from `use` is restored, state from `restore` is discarded
(because there is no way to pick that `b` value from the top level
signature). This shows that not only `monad-control` allows us to do this
sort of lifting, it also allows us to control precisely what happens to
monadic state.

### Lifting with `unliftio`

The [`unliftio`][unliftio] package is a newer, simpler, and safer
alternative to `monad-control`. It uses the same idea of unlifting monadic
stacks but it takes a bit different form:

```haskell
class MonadIO m => MonadUnliftIO m where

  -- | Capture the current monadic context, providing the ability to
  -- run monadic actions in 'IO'.

  askUnliftIO :: m (UnliftIO m)
  askUnliftIO = withRunInIO (\run -> return (UnliftIO run))

  -- | Convenience function for capturing the monadic context and running an 'IO'
  -- action with a runner function. The runner function is used to run a monadic
  -- action @m@ in @IO@.

  withRunInIO :: ((forall a. m a -> IO a) -> IO b) -> m b
  withRunInIO inner = askUnliftIO >>= \u -> liftIO (inner (unliftIO u))

-- | The ability to run any monadic action @m a@ as @IO a@.
--
-- This is more precisely a natural transformation. We need to new
-- datatype (instead of simply using a @forall@) due to lack of
-- support in GHC for impredicative types.

newtype UnliftIO m = UnliftIO { unliftIO :: forall a. m a -> IO a }
```

`withRunInIO` is essentially the same thing as `liftBaseWith` from
`monad-control`, only specialized to `IO` as base monad (the most common use
case, if not the only). The `UnliftIO` newtype is necessary to be able to
return function that has universally quantified arguments (introduced with
`forall`s) in its signature (this is “impredicative polymorphism” the
documentation mentions). We only need this in `askUnliftIO`. The method is
rather unique to `unliftio`, it provides running function `forall a. m a ->
IO a` that one can pass around freely and apply several times, to different
`a` types.

The second difference between the library and `monad-control` is that it
only defines instances of `MonadUnliftIO` for stateless monadic stacks which
are isomorphic to `ReaderT` over `IO`. This way the question how to combine
state from different branches of computation does not arise.

## How to avoid catching asynchronous exceptions

The last (but not least) issue we have to consider is the inability to tell
whether an exception we have caught was synchronous or asynchronous. When we
enclose code with a function like `catch`, it catches everything that
matches the exception type:

```haskell
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

catchErrorCall :: IO () -> IO ()
catchErrorCall m = catch m h
  where
    h :: ErrorCall -> IO ()
    h e = putStrLn ("Caught: " ++ displayException e)

synchronous :: IO ()
synchronous = catchErrorCall $
  throwIO (ErrorCallWithLocation "Synchronously thrown." "")

asynchronous :: IO ()
asynchronous = withAsync (catchErrorCall (threadDelay 1000000)) $ \a ->
  throwTo (asyncThreadId a) (ErrorCallWithLocation "Asynchronously thrown." "")
```

```
λ> synchronous
Caught: Synchronously thrown.
λ> asynchronous
Caught: Asynchronously thrown.
```

It is often not an issue because normally we prefer to be very specific
about type of exception we want to catch. For example, if we want to catch
`HttpException` (assume that it is an exception that an HTTP client library
throws when something goes wrong), it is very unlikely that someone will
throw it to our thread asynchronously. And even if he/she does, I would
argue that it is not a problem with *our* code.

Things start to get worse when we want to catch all exceptions, that is, we
specify `SomeException` as the type of exception to catch. As we have
learned, asynchronous exceptions, like all other exceptions are converted to
`SomeException` before being thrown, so by catching `SomeException` we catch
both asynchronous and synchronous exceptions. This may lead to quite
unexpected results, because we probably want to safeguard against issues
that happen synchronously inside the code enclosed by `catch`, not to catch
things like `ThreadKilled` (which should just kill our thread).

One simple way to solve the problem is to assume that the exceptions that
are typically thrown asynchronously are wrapped in `SomeAsyncException`
(which is the case for all exceptions from `Control.Exception`). Here is how
this could be done:

```haskell
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

catchOnlySync :: Exception e => IO a -> (e -> IO a) -> IO a
catchOnlySync = catchJust $ \e -> -- catchJust from Control.Exception
  case fromException e of
    Nothing -> fromException e
    Just (SomeAsyncException _) -> Nothing

catchAllSync :: IO () -> IO ()
catchAllSync m = catchOnlySync m h
  where
    h :: SomeException -> IO ()
    h e = putStrLn ("Caught: " ++ displayException e)

synchronous :: IO ()
synchronous = catchAllSync $
  throwIO (ErrorCallWithLocation "A synchronous exception." "")

asynchronous :: IO ()
asynchronous = withAsync (catchAllSync action) $ \a ->
  throwTo (asyncThreadId a) ThreadKilled
  where
    action = do
      threadDelay 1000000
      print "Boo!"
```

```
λ> synchronous
Caught: A synchronous exception.
λ> asynchronous -- ThreadKilled wasn't caught
```

This relies on a convention, not on something the compiler can check or
enforce. `SomeAsyncException` can be thrown synchronously, `ErrorCall` could
be thrown asynchronously—nothing prevents that. If, however, we trust that
every exception to be thrown asynchronously has a correctly defined
`Exception` instance that wraps it with `SomeAsyncException`, we are safe.

Alternative solution is the following: we run the action to catch exceptions
from in a separate thread forked with e.g. `withAsync` (we will refer to it
as *worker thread*) and setup exception handler that catches all exceptions
in that thread. When a synchronous exception is thrown there we catch it,
pack result in `Either SomeException a` and return to the main thread, where
we can do whatever we like with it. If an asynchronous exception strikes in
the main thread, it propagates to the worker thread and so shuts it down.

### `safe-exceptions`

It would be an omission not to mention the
[`safe-exceptions`][safe-exceptions] package which solves the problem of
asynchronous vs synchronous exceptions in a systematic way. The library
defines functions like `catch` which only catch synchronous exceptions by
testing the type of exception using the `SomeAsyncException` wrapper.

Next, it follows the following logic (taken from [readme of the
package][safe-exceptions-readme]):

* If the user is trying to install a cleanup function (such as with
  `bracket` or `finally`), we don't care if the exception is synchronous or
  asynchronous: call the cleanup function and then re-throw the exception.

* If the user is trying to catch an exception and recover from it, only
  catch sync exceptions and immediately rethrow async exceptions.

It should be noted that `unliftio` also provides exception-handling
functions with the same behavior as the ones in `safe-exceptions`. The
functions are lifted with `MonadUnliftIO` instead of being defined in terms
of classes from `exceptions`. We recommend just using the
`UnliftIO.Exception` module from `unliftio`.

[ih]: https://intermediatehaskell.com/
[hasql]: https://hackage.haskell.org/package/hasql
[deepseq]: https://hackage.haskell.org/package/deepseq
[base]: https://hackage.haskell.org/package/base
[async]: https://hackage.haskell.org/package/async
[transformers]: https://hackage.haskell.org/package/transformers
[exceptions]: https://hackage.haskell.org/package/exceptions
[monad-control]: https://hackage.haskell.org/package/monad-control
[unliftio]: https://hackage.haskell.org/package/unliftio
[a-semantics-for-imprecise-exceptions]: https://www.microsoft.com/en-us/research/wp-content/uploads/1999/05/except.pdf
[exception-hierarchy]: /static/img/exception-hierarchy.svg
[ext-exceptions]: http://community.haskell.org/~simonmar/papers/ext-exceptions.pdf
[parallel-and-concurrent]: http://chimera.labs.oreilly.com/books/1230000000929
[bracket]: https://ro-che.info/articles/2014-07-30-bracket
[async-exceptions-in-haskell]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/asynch-exns.pdf
[general-bracket]: https://hackage.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#v:generalBracket
[safe-exceptions]: https://hackage.haskell.org/package/safe-exceptions
[safe-exceptions-readme]: https://github.com/fpco/safe-exceptions#readme
