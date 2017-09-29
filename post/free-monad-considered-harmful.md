---
title: Free monad considered harmful
desc: Before you start writing your code using free monads read this, you may change your mind.
date:
  published: September 27, 2017
  updated: September 29, 2017
---

Now and then blog posts explaining and promoting use of free monads pop up,
so for the sake of diversity I decided to write a post advising against free
monads. What I'm going to tell is not new and there have been good comments
on Reddit and in chats explaining downsides of free monads as basis of
Haskell programs, but I thought it might be useful to write a proper post on
the topic.

## What and why

I'm not going to explain free monads here, as it has been
[done](http://www.haskellforall.com/2012/06/you-could-have-invented-free-monads.html)
[before](http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html).
As the aim of this post is to show that one should not write program logic
in free monads, I can assume that the reader is already familiar with the
concept. Nevertheless, let's reiterate quickly some points, so we have a
proper continuous narrative here.

So free monad is a data type that given a functor `f` gives us a monad `Free
f` “for free” (or the most “unconstrained” monad we can get for that
functor), like this:

```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Free x) = Free (fmap f <$> x)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free x = Free (fmap f <$> x)
  Free x <*> my     = Free ((<*> my) <$> x)

instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free x >>= f = Free ((>>= f) <$> x)
```

(Showing `Functor` and `Applicative` instances here even though everyone
seem to skip them.)

Being so abstract, free monad can't really do anything but to build a data
structure representing monadic computation, which we can then inspect or
interpret in some way. Let's steal an example from [another blog
post](http://www.parsonsmatt.org/2017/09/22/what_does_free_buy_us.html):

```haskell
data Terminal a
  = GetLine (String -> a)
  | PrintLine String a
  deriving Functor

type TerminalM = Free Terminal

liftF :: Functor f => f a -> Free f a
liftF = Free . fmap return

getLine :: TerminalM String
getLine = Free (GetLine return)

printLine :: String -> TerminalM ()
printLine str = liftF (PrintLine str ())

myProgram :: TerminalM ()
myProgram = do
  a <- getLine
  b <- getLine
  printLine (a ++ b)
```

Here we go:

* `Terminal` is our functor algebra
* `TerminalM` is our free monad
* `liftF` is a way to lift a functor value into the free monad
* `getLine` and `printLine` are the actions in the free monad
* `myProgram` is the entire program consisting of those actions bound
  together using monadic bind.

So now we could take this `myProgram` value and interpret/transform it in
many ways. We could make an `IO ()` action from it that accepts two lines of
input, concatenates them, and prints the result to the console—the classic
example of the power free monad gives.

## Problems with free monads

### Inspection

I'll start by stating that free monads are not *that* easy to inspect as it
may seem from introductory blog posts. Sure, we could write an interpreter:

```haskell
interpret :: TerminalM a -> IO a
interpret = foldFree $ \case
  GetLine next ->
    next <$> System.IO.getLine
  PrintLine str next ->
    next <$ putStrLn str
```

But it's necessary to understand that in order to analyze `myProgram`, we
would need to provide a full environment for getting through its layers. For
example, we would need a way to generate the values `getLines` would return
(we need to give something to the function inside `GetLine` to make it
produce next action), and since we have a monad here, further actions can
depend on those values (we can't pass something dummy there!).

Truth to be told, functions will almost inevitably be in the functor you
build upon (as in `GetLine`), and functions are not very inspectable. Sure,
this is just a consequence of the fact that free monads can model things
with complex behavior, but it was necessary to note that explicitly in a
post such as this.

### Efficiency

Another problem is inefficiency. Recall the definition of `(>>=)` for free
monad:

```haskell
instance Functor f => Monad (Free f) where
  return = pure
  Pure a >>= f = f a
  Free x >>= f = Free ((>>= f) <$> x)
```

What does it mean? Every time we use `(>>=)` (and with `do` notation it's
all the time), the whole structure accumulated so far needs to be traversed
with `fmap` and then at the end (where `Pure` thing hangs) `(>>= f)` will be
applied and chances are the “snake” will grow by one layer. This is
obviously inefficient.

There are efforts to improve the situation, for example in the paper [“Freer
monads, more extensible
effects”](http://okmij.org/ftp/Haskell/extensible/more.pdf) the authors
introduce *freer monads* which further lighten prerequisites on the type
`f`, so it doesn't even need to be a `Functor`. The solution involves
storing the function (a Kleisli arrow) to apply to some initial value and
just composing functions on the right hand side of `(>>=)` with that using
Kleisli composition in a fashion that is quite similar to the approach based
on coyoneda lemma for functors.

Freer monads are still mostly an esoteric zone and it's not common to see
them used, but they are at least better than free monads with respect to
efficiency of building of actual data structure.

### Composability

Now there is the third issue: combining code in free monads with different
functor types. This can be solved by making the actual functor type
polymorphic and then using functor injection as shown for example in [this
blog post](http://degoes.net/articles/modern-fp-part-2). We can apply this
to the terminal example:

```haskell
-- | Could get fancier, but this will do.

class (Functor f, Functor g) => Inject f g where
  inject  :: f a -> g a
  project :: g a -> Maybe (f a)

getLine :: Inject Terminal f => Free f String
getLine = Free (inject $ GetLine return)

printLine :: Inject Terminal f => String -> Free f ()
printLine str = liftF (inject $ PrintLine str ())

myProgram :: Inject Terminal f => Free f ()
myProgram = do
  a <- getLine
  b <- getLine
  printLine (a ++ b)
```

Which means if you're clever and you really want to write composable code
with free monads, then you can do that. But is it really the best way to
write code?

## A better solution

So we want to be able to interpret a monadic action in different ways,
inspect/transform it, etc. Well, Haskell already has a mechanism for giving
different concrete meanings to the same abstract (read polymorphic) thing.
It's called *type classes*. It's simple, efficient, familiar, composable,
and if you really want to build data structures representing your actions to
do whatever you want with them, guess what… you can do that too.

Let's return to our (or “their”, since it's stolen anyway) terminal example.
We start by defining a `MonadTerm` type class which abstracts actions
related to working with a terminal:

```haskell
class Monad m => MonadTerm m where
  getLine   :: m String
  printLine :: String -> m ()

myProgram :: MonadTerm m => m () -- TerminalM ()
myProgram = do
  a <- getLine
  b <- getLine
  printLine (a ++ b)
```

### Efficiency

Now we can have a very efficient implementation in terms of `IO`:

```haskell
instance MonadTerm IO where
  getLine   = System.IO.getLine
  printLine = printLine

main :: IO ()
main = myProgram
```

Or more realistically in a more complex application it would be something on
top of `ReaderT r IO`, following the [`ReaderT` design
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern), so
the instance definition would be more like:

```haskell
instance (HasMyEnvA r, HasMyEnvB r) => MonadFoo (ReaderT r IO) where
  -- …
```

The approach with using this simple `ReaderT`-based stack for things that
actually run real actions is nice as explained in the post I link to above.
It's exception-friendly because of the stateless nature of the reader monad
transformer and the context is easy to manipulate. In particular, I advise
to always use lenses with this setup because with them it's possible to
change (not only read) a specific component of the abstract `r` thing, so
you can have a region of code with changed environment using `local` with
`Lens'` and `withReader` with the more general `Lens` type.

Note that if performance is of any concern, it's possible to use
`INLINEABLE` and `SPECIALIZE` pragmas to squeeze out any undesirable
polymorphism for maximal efficiency (unless the polymorphic code is defined
in the same module where it's used with concrete monadic stack, in which
case GHC is able to specialize by itself).

Free monads can't possibly compete with this approach in terms of
performance.

### Inspection

The second reason to prefer writing in polymorphic monads to writing in free
monads is that this approach is *strictly more powerful* in the sense that
we can recover the same data structure simply by defining an instance of
`MonadTerm`:

```haskell
instance MonadTerm TerminalM where
  getLine       = Free (GetLine return)
  printLine str = liftF (PrintLine str ())

myFreeProgram :: TerminalM ()
myFreeProgram = myProgram
```

Let's see:

* the effects in `myProgram` are constrained, we only use the methods of
  `MonadTerm` there;
* we can turn it into very efficient run-able code without the need to first
  construct a data structure and then interpret it;
* still we can do everything that we could do if we wrote `myProgram` in
  free monad directly.

### Composability

If we want to combine actions from two different type classes, we just need
to merge the constraints:

```haskell
class Monad m => MonadTerm m where
  getLine :: m String
  printLine :: String -> m ()

class Monad m => MonadLog m where
  log :: String -> m ()

myProgram :: (MonadTerm m, MonadLog m) => m ()
myProgram = do
  a <- getLine
  log "got a"
  b <- getLine
  log "got b"
  printLine (a ++ b)
```

And this is familiar to any Haskeller. No brain damage can be incurred from
reading the code. With some effort we can still recover the same data
structure we would get if we wrote directly in free monad, and [G.
Allais](https://github.com/gallais) showed in the comments that one in fact
can do the following transformation:

```haskell
newtype LogTerm f a = LogTerm { runLogTerm :: Free f a }
  deriving (Functor, Applicative, Monad)

instance Inject Terminal f => MonadTerm (LogTerm f) where
  getLine = LogTerm (Free $ inject (GetLine return))
  printLine str = LogTerm (liftF $ inject (PrintLine str ()))

instance Inject Log f => MonadLog (LogTerm f) where
  log str = LogTerm (liftF $ inject (Log str ()))

liberate :: (Inject Terminal f, Inject Log f)
  => (forall m. (MonadTerm m, MonadLog m) => m a)
  -> Free f a
liberate = runLogTerm
```

So it is possible to go from type class-based representation to free
representation quite straightforwardly.

## Conclusion

Of course the title is a click bait and I do not mean to be so categorical.
Free monads do have their uses, but in most cases I'd think twice before
committing to that style of programming because it's somewhat tedious and
inefficient. So the post is just a fair warning and a demonstration of
alternative solutions.
