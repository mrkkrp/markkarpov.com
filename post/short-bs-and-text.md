---
title: Short ByteString and Text
desc: We all know the ByteString and Text types, but are they always the best choice? What else does the Haskell ecosystem provide?
date:
  published: October 29, 2017
---

The `ByteString` and `Text` (strict and lazy) types have a great adoption in
the Haskell ecosystem. Usually when one has binary data, or data that cannot
contain characters outside of ASCII range, `ByteString` is preferred. `Text`
on the other hand allows to work with arbitrary streams of characters,
including Unicode characters, similarly to `String`, but in a more efficient
way.

The both libraries
([`bytestring`](https://hackage.haskell.org/package/bytestring) and
[`text`](https://hackage.haskell.org/package/text)) come with a handy
collection of operations that are stream fuse-able and sufficient for
solving virtually any task. However, it happens so that there are other
under-appreciated types and libraries that complement `ByteString` and
`Text`. These may be a better choice in some circumstances, but I believe
many Haskellers are not aware of them. This post aims to rectify that.

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
points to (they are in what is called *pinned memory*), because foreign code
would become… err very fragile if it could (and also because moving around a
long `ByteString`s is not very efficient). This means that once a
`ByteString` is allocated, its payload stays at the same address. When many
short `ByteStrings` are allocated/freed more or less intensively the
impossibility of moving them around to optimize memory usage contributes to
memory fragmentation.

So if you work with many short `ByteString`s, you may start to obverse
something like a space leak when memory usage becomes much higher than the
amount of data you actually have.

`ShortByteString` is different. It uses `ByteArray#` as payload:

```haskell
data ShortByteString = SBS ByteArray#
```

Let's refresh what `ByteArray#` is:

> A `ByteArray#` is a just a region of raw memory in the garbage-collected
> heap, which is not scanned for pointers. It carries its own size (in
> bytes).

Not only we do not have to worry about memory fragmentation, but
`ByteArray#` takes care of storing its own size, so the representation is
also more compact than that of `ByteString`.

If we look at the
[`Data.ByteString.Short`](https://hackage.haskell.org/package/bytestring/docs/Data-ByteString-Short.html)
module we however will find that `ShortByteString` does not support as many
operations as `ByteString`. Here goes a quote about where to use
`ShortByteString`:

> It is suitable for use as an internal representation for code that needs
> to keep many short strings in memory, but it should not be used as an
> interchange type. That is, it should not generally be used in public APIs.
> The `ByteString` type is usually more suitable for use in interfaces; it
> is more flexible and it supports a wide range of operations.

It's also worth remembering that conversion between `ShortByteString` and
`ByteString` is an *O(n)* operation involving copying of payload.

## `ShortText`

The `ShortText` type provided by the new
[`text-short`](https://hackage.haskell.org/package/text-short) package to
`Text` is roughly what `ShortByteString` to `ByteString`—a more memory
efficient, but also quite limited in functionality variation.

Here is the definition of strict `Text`:

```haskell
data Text = Text
    {-# UNPACK #-} !A.Array          -- payload (Word16 elements)
    {-# UNPACK #-} !Int              -- offset (units of Word16, not Char)
    {-# UNPACK #-} !Int              -- length (units of Word16, not Char)
```

And the definition of `ShortText`:

```haskell
newtype ShortText = ShortText ShortByteString
-- essentially has a ByteArray# inside
```

`ShortText` is different from `Text` in the following:

* It uses UTF-8 internally, while `Text` uses UTF-16.
* It does not support slicing because it does not store start/end offsets
  saving some bytes this way as well.

So the use case for `ShortText` is when you have to store a lot of short
text values in memory. Check the
[`Data.Text.Short`](https://hackage.haskell.org/package/text-short-0.1.1/docs/Data-Text-Short.html)
module for collection of supported operations on `ShortText` values.

## Conclusion

The `ShortByteString` and `ShortText` should help scaling applications that
tend to store large quantities of short `ByteString`s/`Text` values. As easy
as it is, just replacing a type may actually help to reduce memory usage
considerably.
