---
title: Short ByteString and Text
desc: We all know the ByteString and Text types, but are they always the best choice? What else does the Haskell ecosystem provide?
date:
  published: October 30, 2017
---

The `ByteString` and `Text` (strict and lazy) types have a great adoption in
the Haskell ecosystem. Usually when one has binary data, or data that cannot
contain characters outside of ASCII range, `ByteString` is preferred. `Text`
on the other hand allows to work with arbitrary streams of characters,
including Unicode characters, similarly to `String`, but in a more efficient
way.

The both libraries
([`bytestring`](https://hackage.haskell.org/package/bytestring) and
[`text`](https://hackage.haskell.org/package/text)) come with collections of
operations that are stream fuse-able and sufficient for solving virtually
any task. However, it happens so that there are other under-appreciated
types and libraries that complement `ByteString` and `Text`. These may be a
better choice in some circumstances, but I believe many Haskellers are not
aware of them. This post aims to rectify that.

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
then? The problem is that Haskell GC cannot move around things `ForeignPtr`
points to (they are in what is called *pinned memory*), because foreign code
would become… err very fragile if it could, and also because moving around a
long `ByteString` is not very efficient. This means that once a `ByteString`
is allocated, its payload stays at the same address. When many short
`ByteStrings` are allocated/freed more or less intensively the impossibility
of moving them around to optimize memory usage contributes to memory
fragmentation.

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
efficient, but also quite limited in functionality variation (check the
[`Data.Text.Short`](https://hackage.haskell.org/package/text-short-0.1.1/docs/Data-Text-Short.html)
module for collection of supported operations on `ShortText` values).

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

* It uses UTF-8 internally, while `Text` uses UTF-16. This allows
  `ShortText` to have a more compact representation in most cases, see for
  example [this link](http://utf8everywhere.org/#asian) about UTF-8 vs
  UTF-16 compactness.

* It does not support slicing because it does not store start/end offsets
  saving several bytes this way as well (may be a big deal if you have many
  short `ShortText` values).

So the use case for `ShortText` is when you have to store a lot of short
text values in memory. This is the case e.g. with Cabal which needs to store
thousands of package names. Cabal 2.0 uses `ShortText` under the hood, see
[the heap
profiles](https://github.com/haskell/hackage-server/issues/600#issuecomment-315625369)
for example.

## The `text-utf8` project

Using UTF-8 instead of UTF-16 is a good idea not only for short text values.
Indeed, there are several reasons (other than saving memory) to prefer
UTF-8:

* **Encoding and decoding to UTF-8 becomes very cheap.** I think most people
  will agree that UTF-8 is probably the most popular encoding right now, so
  the ability to covert from/to UTF-8 efficiently is a nice thing. Right now
  for encoding we need to do the conversion from UTF-16 to UTF-8 in vanilla
  `text`. Decoding UTF-8 values would just amount to checking if a given
  sequence of bytes is a valid UTF-8 string.

* **It becomes possible to write efficient parsers** over UTF-8 when
  syntactic-relevant code-points are in the ASCII plane.

There is a project called [`text-utf8`](https://github.com/text-utf8) which
aims to take the familiar `text` library and make it use UTF-8 instead of
UTF-16 under the hood. So far the results are:

* Improved memory consumption.

* It's on par with UTF-16-based `text` in terms of performance, but the
  developers behind `text-utf8` think that they make it faster than original
  `text` as well.

From a talk with Herbert Valerio Riedel who works on that project I learned
that `text-utf8` will be released in foreseeable future and if it's
successful the changes will be incorporated back into `text`.

## Conclusion

The `ShortByteString` and `ShortText` should help scaling applications that
tend to store large quantities of short `ByteString`s/`Text` values. As easy
as it is, just replacing a type may actually help to reduce memory usage
considerably.

*I'd like to thank Herbert Valerio Riedel for reading a draft of this post,
telling me almost everything I have written here, and for developing
`short-text` and `text-utf8` projects :-D*
