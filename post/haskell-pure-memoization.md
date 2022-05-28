---
title: Pure memoization in Haskell
desc: Investigation of how and when pure memoziation works in Haskell
date:
  published: May 31, 2022
tag: haskell
---

One of the first examples of memoization that I have seen comes from the
[Haskell Wiki][haskell-wiki]:

```haskell
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)
```

```haskell
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
  where
    fib 0 = 0
    fib 1 = 1
    fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

It has always felt like magic to me. Somehow just defining the `fib`
function inside another function and then indexing a list of its
applications suddenly makes a huge difference with respect to performance.
How does this work? One should ask that question not just out of
curiosity—knowing inner workings of this optimization technique will also
give us intuition about what can break it in our programs.

Let's put both functions in a module and compile it using `-ddump-simpl` to
dump output of the simplifier:

```haskell
Rec {
slow_fib_rgr :: Int -> Integer
[GblId, Arity=1, Unf=OtherCon []]
slow_fib_rgr
  = \ (ds1_d1GW :: Int) ->
      case ds1_d1GW of wild_X1E { GHC.Types.I# ds2_d1GZ ->
      case ds2_d1GZ of {
        __DEFAULT ->
          + @Integer
            GHC.Num.$fNumInteger
            (slow_fib_rgr (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 2#)))
            (slow_fib_rgr
               (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 1#)));
        0# -> 0;
        1# -> 1
      }
      }
end Rec }

Rec {
ds_r1Hl :: [Integer]
[GblId]
ds_r1Hl
  = map
      @Int
      @Integer
      (\ (ds1_d1GO :: Int) ->
         case ds1_d1GO of wild_X1E { GHC.Types.I# ds2_d1GR ->
         case ds2_d1GR of {
           __DEFAULT ->
             + @Integer
               GHC.Num.$fNumInteger
               (memoized_fib_rgs
                  (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 2#)))
               (memoized_fib_rgs
                  (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 1#)));
           0# -> 0;
           1# -> 1
         }
         })
      (enumFrom @Int GHC.Enum.$fEnumInt (GHC.Types.I# 0#))

memoized_fib_rgs :: Int -> Integer
[GblId, Arity=1, Unf=OtherCon []]
memoized_fib_rgs
  = \ (ds1_d1GN :: Int) -> !! @Integer ds_r1Hl ds1_d1GN
end Rec }
```

If we manage to read through obfuscation we will see that `slow_fib` has
been compiled to a simple recursive function, not much different from the
original source code. However, look what happened to `memoized_fib`! The
part that constructs the list has floated outside of `memoized_fib` and
became an independent top-level definition. One thing about top-level
definitions we should always remember is that results of their evaluation
are kept during the entire lifetime of the program—they are not
garbage-collected.

One detail about `memoized_fib_rgs` and `ds_r1Hl` is essential here—they are
calling each other. If `fib` in `memoized_fib` just called itself
recursively there would be no memoization, similar to `slow_fib`.
`memoized_fib_rgs` has to index into `ds_r1Hl` because there should be
something that remembers intermediate results of the computation. The main
performance advantage of `memoized_fib` then comes from the fact that the
two execution branches:

```haskell
fib n = memoized_fib (n-2) + memoized_fib (n-1)
        ^^^^^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^^
```

re-use each other's results instead of re-computing them.

How can we break memoziation in `memoize_fib`? Well, perhaps we could
prevent GHC from floating the list outside of the function. We can do it
like so:

```haskell
memoized_fib_x :: Integer -> Int -> Integer
memoized_fib_x x = (map fib [0 ..] !!)
  where
    fib 0 = x
    fib 1 = x + 1
    fib n = memoized_fib_x x (n-2) + memoized_fib_x x (n-1)
```

`memoized_fib_x` is similar to `memoized_fib`, but the Fibonacci sequence
starts from `x`. A minor change, but the list `ds_d1Gz` now has to live
inside its parent function:

```haskell
Rec {
memoized_fib_x_rhc :: Integer -> Int -> Integer
[GblId, Arity=1, Unf=OtherCon []]
memoized_fib_x_rhc
  = \ (x_awl :: Integer) ->
      let {
        ds_d1GN :: [Integer]
        [LclId]
        ds_d1GN
          = map
              @Int
              @Integer
              (\ (ds1_d1GP :: Int) ->
                 case ds1_d1GP of wild_X1E { GHC.Types.I# ds2_d1GS ->
                 case ds2_d1GS of {
                   __DEFAULT ->
                     + @Integer
                       GHC.Num.$fNumInteger
                       (memoized_fib_x_rhc
                          x_awl (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 2#)))
                       (memoized_fib_x_rhc
                          x_awl (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 1#)));
                   0# -> x_awl;
                   1# -> + @Integer GHC.Num.$fNumInteger x_awl 1
                 }
                 })
              (enumFrom @Int GHC.Enum.$fEnumInt (GHC.Types.I# 0#)) } in
      \ (ds1_d1GO :: Int) -> !! @Integer ds_d1GN ds1_d1GO
end Rec }
```

No memoization happens in this case—`memoized_fib_x` is terribly slow.
Perhaps this example may seem contrived to the reader, but let's now take a
look at the popular package for pure memoization called
[`MemoTrie`][MemoTrie]. This library provides a useful function called
`memo`:

```haskell
memo :: HasTrie t => (t -> a) -> t -> a
memo = untrie . trie
```

`memo` is not very different from `memoized_fib`. The difference is that
instead of a list it uses a custom data structure that holds results of
application of the given function `t -> a` to all values in its domain `t`.

```haskell
class HasTrie a where
  data (:->:) a :: * -> *

  trie :: (a -> b) -> (a :->: b)
  untrie :: (a :->: b) -> (a  ->  b)
```

`trie` constructs this structure, while `untrie` indexes it. Implementations
for 2-tuple `(,)` and `Either` should clarify the principle:

```haskell
instance (HasTrie a, HasTrie b) => HasTrie (a,b) where
  newtype (a,b) :->: x = PairTrie (a :->: (b :->: x))

  -- Transform (a,b) into a structure of answers for each 'a' value; every
  -- element in this structure is in turn indexed by 'b'.

  trie f = PairTrie (trie (trie . curry f))
  untrie (PairTrie t) = uncurry (untrie .  untrie t)

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  data (Either a b) :->: x = EitherTrie (a :->: x) (b :->: x)

  -- 'EitherTrie' holds the result for the case when the argument is 'Left'
  -- as well as the result for the case when the argument is 'Right'.

  trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
  untrie (EitherTrie s t) = either (untrie s) (untrie t)
```

Laziness is essential for this to work. Similar to `memoized_fib` where the
list was infinite, the `(:->:)` structure is also possibly very large or
infinite. For more details about the library see [this blog
post][memo-trie-blog-post] by Conal Elliot.

Now that we understand how `memo` works we can see that its promise of
memoizing a given function does not always hold. It is guaranteed to work
only when the trie can be floated up and become an independent top-level
definition.

For example, this works:

```haskell
fixedFib :: Int -> Integer
fixedFib = memo fib
  where
    fib 0 = 0
    fib 1 = 1
    fib n = fixedFib (n-2) + fixedFib (n-1)

main :: IO ()
main = do
  n <- readLn
  print (fixedFib n)
```

as well as this:

```haskell
fixedFib_x :: Integer -> Int -> Integer
fixedFib_x x = memo fib
  where
    fib 0 = 0
    fib 1 = x
    fib n = x * (fixedFib_x x (n-2) + fixedFib_x x (n-1))

main :: IO ()
main = do
  let myMultiplier = 2
  read
  print ()
```

as well as this:

```haskell
```

…but not this:

```haskell
```

…

One should be conscious of this because there is nothing in the type of
`memo` that indicates when and how it works.

[haskell-wiki]: https://wiki.haskell.org/Memoization
[MemoTrie]: https://hackage.haskell.org/package/MemoTrie
[memo-trie-blog-post]: http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries
