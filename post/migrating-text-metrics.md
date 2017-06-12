---
title: Migrating text metrics to pure Haskell
desc: One more blog post about Haskell competing with C in speed.
date:
  published: June 13, 2017
---

It's been a while since I first published the [`text-metrics`](https://github.com/mrkkrp/text-metrics) package,
which allows to calculate various string metrics between `Text` values.
Originally the package was written primarily in C with wrappers in Haskell.
At the time I needed maximal speed and did not care whether the algorithms
themselves are coded is C or Haskell, as long as they work, and work fast.

However, recently there were quite inspiring blog posts about Haskell
competing with C in terms of speed and memory consumption. One such a blog
post is Chris Done's [“Fast Haskell: Competing with C at parsing XML”](http://chrisdone.com/posts/fast-haskell-c-parsing-xml),
which is a most entertaining read. Now that I have more free time I decided
to go back to `text-metrics` and rewrite it in pure Haskell. The benefits
are:

* Things like GHCJS will be able to use the package.
* Pure Haskell is hopefully more cross-platform. Can't really tell whether
  there are any issues with compiling packages with C bits on Windows
  nowadays, because I have not had access to a Windows machine for 3 years.
* The C implementation does not work correctly when the inputs contain
  characters from a tiny subset of Unicode that is represented by more than
  one `Word16` value per character. This subset includes historic scripts,
  emoji, and less-used Chinese ideographs. It would be good to make the
  functions work correctly with those characters as well.
* One more proof that Haskell can be quite fast when it is necessary.

## Hamming distance

I decided to start with the simplest algorithm in `text-metrics`: *Hamming
distance*. This distance is only defined for inputs of equal length, so the
signature looks like this:

```haskell
hamming :: Text -> Text -> Maybe Natural
```

Well, at least it did. I decided to replace `Natural` with `Int`, because
all the API we are going to use take and return `Int`s, so it's no use to
try to use proper types like `Natural`, which is also slower to work with.

Hamming distance is just the number of characters at the same indices that
are different. So C implementation was:

```c
unsigned int tmetrics_hamming (unsigned int len, uint16_t *a, uint16_t *b)
{
  unsigned int acc = 0, i;
  for (i = 0; i < len; i++)
    {
      if (*(a + i) != *(b + i)) acc++;
    }
  return acc;
}
```

How to do the same in Haskell? Well, first of all we need an efficient way
to traverse `Text` values character by character. We should remember that
`Text` is a lot like a list of `Word16` values internally. As I mentioned,
some characters are represented as two consecutive `Word16` values, and most
characters are just one `Word16`. So we don't know which is which, and that
essentially makes it [impossible to have indexing](https://hackage.haskell.org/package/text/docs/Data-Text.html#v:index) in *O(1)*.

So we can't access a character at a certain offset (as in `*(a + i)`), we
should perform something like synchronous unconsing on both `Text` values
(good that they are of the same length!). The `text` package provides tools
for this in [`Data.Text.Unsafe`](https://hackage.haskell.org/package/text/docs/Data-Text-Unsafe.html). The `Iter` data type is our friend:

```haskell
data Iter = Iter {-# UNPACK #-} !Char {-# UNPACK #-} !Int
```

The `Char` value is an “unconsed” character and `Int` is the length of the
character as a number of `Word16` values (1 or 2). We will use three
functions:

* `iter :: Text -> Int -> Iter`—*O(1)* Iterate (unsafely) one step forwards
  through a UTF-16 array, returning the current character and the delta to
  add to give the next offset to iterate at.
* `iter_ :: Text -> Int -> Int`—*O(1)* Iterate one step through a UTF-16
  array, returning the delta to add to give the next offset to iterate at.
* `lengthWord16`—*O(1)* Return the length of a `Text` in units of `Word16`.

That said, here is a Haskell implementation of Hamming distance:

```haskell
hamming :: Text -> Text -> Maybe Int
hamming a b =
  if T.length a == T.length b
    then Just (go 0 0 0)
    else Nothing
  where
    go !na !nb !r =
      let TU.Iter cha da = TU.iter a na
          TU.Iter chb db = TU.iter b nb
      in if | na  == len -> r
            | cha /= chb -> go (na + da) (nb + db) (r + 1)
            | otherwise  -> go (na + da) (nb + db) r
    len = TU.lengthWord16 a
```

First we have to confirm that the input values are of the same length. Due
to the reasons mentioned earlier, we have to traverse each of them to
compute their lengths. If the lengths are equal we can start counting
characters at the same positions that are different. `go` is a
tail-recursive helper that steps through the `Text` values using `iter`.
Strictness annotations were optional in this case as GHC was able to figure
out strictness itself, but I decided to keep them there, just in case.

Let's see if we are close to the C implementation with this:

Case                  | Allocated | GCs |   Max
----------------------|-----------|-----|------
hamming (C)/5         |     1,360 |  0  |   400
hamming (C)/10        |     2,408 |  0  |   688
hamming (C)/20        |     4,056 |  0  | 1,096
hamming (C)/40        |     6,056 |  0  | 1,368
hamming (C)/80        |     8,504 |  0  | 1,464
hamming (C)/160       |    13,384 |  0  | 1,656
hamming (Haskell)/5   |     1,344 |  0  |   400
hamming (Haskell)/10  |     2,392 |  0  |   688
hamming (Haskell)/20  |     4,040 |  0  | 1,096
hamming (Haskell)/40  |     6,040 |  0  | 1,368
hamming (Haskell)/80  |     8,488 |  0  | 1,464
hamming (Haskell)/160 |    13,368 |  0  | 1,656

The weigh report shows that memory consumption is essentially the same.
And here is a Criterion benchmark:

![Hamming distance Criterion report](/static/img/text-metrics-hamming.png)

This shows that the new pure Haskell implementation is not slower. In fact,
it's even a bit faster for short strings. The result can be explained by the
fact that to pass `Text` to C it has to be copied, and that also takes some
cycles.

## Jaro distance

The Jaro distance $d_\text{j}$ of two given strings $s_\text{1}$ and
$s_\text{2}$ is

$$ d_\text{j} = \begin{cases} 0 & \quad \text{if } m = 0 \\ \frac{1}{3} \left(\frac{m}{|s_\text{1}|} + \frac{m}{|s_\text{2}|} + \frac{m-t}{m}\right) & \quad \text{otherwise} \\ \end{cases}$$

Where

* $s_\text{i}$ means the length of the string $s_\text{i}$;
* $m$ is the number of matching characters;
* $t$ is half the number of transpositions.

Two characters from $s_\text{1}$ and $s_\text{2}$ respectively are
considered matching only if they are the same and not farther than
$\left\lfloor\frac{max(|s_\text{1}|,|s_\text{2}|)}{2}\right\rfloor - 1$.

A matching pair is counted as a transposition also if the characters are
close enough but in the opposite order. See more about this
in [the Wikipedia article](https://en.wikipedia.org/wiki/Jaro%E2%80%93Winkler_distance).

The implementation is considerably more verbose for this one:

```haskell
jaro :: Text -> Text -> Ratio Int
jaro a b =
  if T.null a || T.null b
    then 0 % 1
    else runST $ do
      let lena = T.length a
          lenb = T.length b
          d =
            if lena >= 2 && lenb >= 2
              then max lena lenb `quot` 2 - 1
              else 0
      v <- VUM.replicate lenb (0 :: Word8)
      r <- VUM.replicate 3 (0 :: Int) -- tj, m, t
      let goi !i !na !fromb = do
            let TU.Iter ai da = TU.iter a na
                (from, fromb') =
                  if i >= d
                    then (i - d, fromb + TU.iter_ b fromb)
                    else (0, 0)
                to = min (i + d + 1) lenb
                goj !j !nb =
                  when (j < to) $ do
                    let TU.Iter bj db = TU.iter b nb
                    used <- (== 1) <$> VUM.unsafeRead v j
                    if not used && ai == bj
                      then do
                        tj <- VUM.unsafeRead r 0
                        if j < tj
                          then VUM.unsafeModify r (+ 1) 2
                          else VUM.unsafeWrite  r 0 j
                        VUM.unsafeWrite v j 1
                        VUM.unsafeModify r (+ 1) 1
                      else goj (j + 1) (nb + db)
            when (i < lena) $ do
              goj from fromb
              goi (i + 1) (na + da) fromb'
      goi 0 0 0
      m <- VUM.unsafeRead r 1
      t <- VUM.unsafeRead r 2
      return $
        if m == 0
          then 0 % 1
          else ((m % lena) +
                (m % lenb) +
                ((m - t) % m)) / 3
```
