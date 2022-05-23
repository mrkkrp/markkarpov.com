---
title: Pure memoization in Haskell
desc: Investigation of how and when pure memoziation works in Haskell
date:
  published: May 31, 2022
tag: haskell
---

One of the first examples of memoization that I saw comes from the [Haskell
Wiki][haskell-wiki]:

```haskell
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)
```

```haskell
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

It has always felt like magic to me. Somehow just putting the `fib` function
inside another function and then indexing a list of its applications
suddenly makes a huge difference with respect to performance. How does this
work? One should ask that question not just out of curiosity—knowing what
makes this optimization technique work will also help us to know what can
break it in our programs.

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
are kept during the entire lifetime of the program.

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

share each other's results instead of re-computing them.

How can we break memoziation in `memoize_fib`? Well, perhaps we could
prevent GHC from floating the list outside of the function. Like so:

```haskell
memoized_fib_x :: Integer -> Int -> Integer
memoized_fib_x x = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = x
         fib n = x * (memoized_fib_x x (n-2) + memoized_fib_x x (n-1))
```

`memoized_fib_x` is similar to `memoized_fib`, but every Fibonacci number is
multiplied by `x`. A minor change, but the list `ds_d1Gz` now has to live
inside its parent function:

```haskell
Rec {
memoized_fib_x_rgs :: Integer -> Int -> Integer
[GblId, Arity=1, Unf=OtherCon []]
memoized_fib_x_rgs
  = \ (x_aAr :: Integer) ->
      let {
        ds_d1Gz :: [Integer]
        [LclId]
        ds_d1Gz
          = map
              @Int
              @Integer
              (\ (ds1_d1GB :: Int) ->
                 case ds1_d1GB of wild_X1E { GHC.Types.I# ds2_d1GE ->
                 case ds2_d1GE of {
                   __DEFAULT ->
                     * @Integer
                       GHC.Num.$fNumInteger
                       x_aAr
                       (+ @Integer
                          GHC.Num.$fNumInteger
                          (memoized_fib_x_rgs
                             x_aAr (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 2#)))
                          (memoized_fib_x_rgs
                             x_aAr (- @Int GHC.Num.$fNumInt wild_X1E (GHC.Types.I# 1#))));
                   0# -> 0;
                   1# -> x_aAr
                 }
                 })
              (enumFrom @Int GHC.Enum.$fEnumInt (GHC.Types.I# 0#)) } in
      \ (ds1_d1GA :: Int) -> !! @Integer ds_d1Gz ds1_d1GA
end Rec }
```

No memoization happens in this case, `memoized_fib_x` is terribly slow.
Perhaps this example may seem contrived to the reader, but let's now take a
look at the popular package for pure memoization called
[`MemoTrie`][MemoTrie]. This library provides a useful function called
`memo`:

```haskell
memo :: HasTrie t => (t -> a) -> t -> a
```

`memo` works exactly like `memoized_fib`.

But the type of `memo` is somewhat misleading. Nothing prevents us from
using `memo` inside another definition:

```
```

Familiar?

----

<http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries>

This is why

<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html#core-representation-and-simplification>

Build on the idea of using laziness to store result values for all domain
values of the function without computing them unless they are forced.

Start simple, iterate to make it work for arbitrary types. Probably
inevitable use of type classes. Maybe it is even possible without type
classes? Hmm.

Consider cases when this approach stops working.

Consider memorized local functions. Do they work, why?

Mention [`MemoTrie`][MemoTrie].

[haskell-wiki]: https://wiki.haskell.org/Memoization
[MemoTrie]: https://hackage.haskell.org/package/MemoTrie
