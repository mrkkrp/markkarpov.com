---
title: Pure memoization in Haskell
desc: Investigation of how and when pure memoziation works in Haskell
date:
  published: May 23, 2022
tag: haskell
---

Let's talk about pure memoization in Haskell. It has always felt like magic
to me.

<https://wiki.haskell.org/Memoization>

```
slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n-2) + slow_fib (n-1)
```

```
memoized_fib :: Int -> Integer
memoized_fib = (map fib [0 ..] !!)
   where fib 0 = 0
         fib 1 = 1
         fib n = memoized_fib (n-2) + memoized_fib (n-1)
```

But why does this refactoring make the inner `fib` remember results of its
invocation while it doesn't happen quite in the same way to `slow_fib`?

<http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries>

This is why

<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html#core-representation-and-simplification>
