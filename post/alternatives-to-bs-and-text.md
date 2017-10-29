---
title: Alternatives to ByteString and Text
desc: We all know the ByteString and Text types, but are they always the best choice? What else does the Haskell ecosystem provide?
date:
  published: October 29, 2017
---

The `ByteString` and `Text` (strict and lazy) types have a great adoption in
the Haskell ecosystem and community. Usually when one has binary data, or
data that cannot contain characters outside of ASCII range, `ByteString` is
preferred. `Text` on the other hand allows to work with arbitrary streams of
characters, including Unicode characters, similarly to `String`, but in a
more efficient way.

The both libraries
([`bytestring`](https://hackage.haskell.org/package/bytestring) and
[`text`](https://hackage.haskell.org/package/text)) come with a handy
collection of operations that are stream fuse-able and sufficient for
solving virtually any task. However, it happens so that there are other
under-appreciated types and libraries similar in their purpose to
`ByteString` and `Text`. These may be a better choice in some circumstances,
but I believe many Haskellers are not aware of them. This post aims to
rectify that.

## `ShortByteString`

`ShortByteString` lives in the same package as `ByteString`—`bytestring`.
Even without looking at the documentation this fact suggests that there is
something not-so-perfect about `ByteString`, otherwise there would be no
additional different type.

Let's see what's wrong. Here is the definition of strict `ByteString`:

```haskell
data ByteString = PS {-# UNPACK #-} !(ForeignPtr Word8) -- payload
                     {-# UNPACK #-} !Int                -- offset
                     {-# UNPACK #-} !Int                -- length
```

We can see that the payload is a pointer to an array of bytes (`Word8` in
Haskell). “Foreign” in `ForeignPtr` means that the data the pointer points
to is managed not by Haskell's GC, but possibly by some external means.

`ForeignPtr` may have a finalizer associated with it. This finalizer is run
when Haskell run time detects that there are no more references to the the
pointer within Haskell heap and stack. Seems about right, it's not that easy
to cause memory leaks by forgetting to free a `ForeignPtr`. We should be OK
then? The problem is that Haskell GC cannot move around things `ForeginPtr`
points to, because foreign code would become… err *very* fragile if it did.
This means that once a `ByteString` is allocated, its payload stays at the
same address. When many `ByteStrings` are allocated/freed more or less
intensively the impossibility of moving them around to optimize memory usage
contributes to memory fragmentation.

So if you work with many `ByteString`s, you may start to obverse something
like a space leak when memory usage is much higher than the amount of data
you actually have.

`ShortByteString` is different. It uses `ByteArray#` as payload:

```haskell
data ShortByteString = SBS ByteArray#
```

Let's refresh what `ByteArray#` is:

> A `ByteArray#` is a just a region of raw memory in the garbage-collected
> heap, which is not scanned for pointers. It carries its own size (in
> bytes).

Sounds good! Not only we do not have to worry about memory fragmentation,
but `ByteArray#` takes care of storing its own size, so the representation
is more compact than that of `ByteString`.

## Conclusion

I must say it's really great that we have the “common ground” types like
`Text` and `ByteString` that help building bridges between different
packages. …something…
