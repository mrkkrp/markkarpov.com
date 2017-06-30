---
title: "A major upgrade to Megaparsec: more speed, more power"
desc: The post describes extension of the Stream type class that allowed to make the Megaparsec library more efficient and simplify code base.
date:
  published: July 1, 2017
---

It looks like comparing Haskell's performance with C (even just via FFI)
causes too much disturbance in the force for some reason, so this time I'll
be comparing Haskell with Haskell, namely Megaparsec 6 (still in making)
with the gold standard of fast parsing in the Haskell world—Attoparsec.

The post is about a modification to Megaparsec extending its `Stream` type
class to achieve four goals:

1. Allow to return more natural types from things like `string`. If you
   parse `Text` stream, you should get `Text` from there, with minimal
   repacking/overhead. (Megaparsec 5 returns `String ~ [Char]` for any
   stream of tokens `Char`.)
2. Reduce allocations and increase speed. Consuming input token by token and
   repacking them into `String` for example is not most efficient way to
   parse a row of tokens. Surely we can do better.
3. Add more combinators like `takeWhile`, which should return “chunks” of
   input stream with correct type (e.g. `ByteString` or `Text`, depending on
   the type of input stream).
4. Make code simpler by moving the complex position-updating logic that we
   keep for custom streams of tokens into the methods of the `Stream` type
   class. This way if you have a custom stream of tokens, you'll have to
   write the correct position updating code. While default streams
   (`String`, `Text`, `ByteString`) will enjoy simplified, and faster
   processing.

I have succeeded in reaching all these goals, and I'll explain how.

## Benchmarks

We're going to need some benchmarks. I have made the repo with code I used
for comparison public, it can be found here: https://github.com/mrkkrp/parsers-bench.

It contains 3 pairs of parsers:

* CSV parser (original code)
* JSON parser (stolen from Attoparsec's `Aeson.hs` benchmark with simplifications)
* Log parser (stolen from School of Haskell [tutorial](https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec))

I do not want to focus on microbenchmarks, instead I want to compare “common
place”, average parsing code people usually write with the libraries
(fortunately the code written using Attoparsec is easily convertable to
Megaparsec). To make the comparison fair, I also do not use complex monadic
stacks with Megaparsec, as it would surely make it slower (with Attoparsec
you just can't do that after all).

Running the benchmark with Megaparsec before any `Stream`-related
optimizations gives us the starting point:

![Megaparsec vs Attoparsec (beginning)](/static/img/megaatto-beginning.png)

Honestly, this is better than I expected. Average Megaparsec parser is just
twice as slow as average Attoparsec parser. Now what is really interesting
is how close we can get to performance of the venerable library.

## Extending `Stream`

Here are the allocations:

Case                           | Allocated  | GCs |    Max
-------------------------------|------------|-----|-------
CSV (Attoparsec)/csv-5.csv     |    73,760  | 0   |  9,040
CSV (Attoparsec)/csv-10.csv    |   125,200  | 0   |  9,040
CSV (Attoparsec)/csv-20.csv    |   232,112  | 0   |  9,040
CSV (Attoparsec)/csv-40.csv    |   437,832  | 0   |  9,040
CSV (Megaparsec)/csv-5.csv     |   162,184  | 0   |  9,040
CSV (Megaparsec)/csv-10.csv    |   301,944  | 0   |  9,040
CSV (Megaparsec)/csv-20.csv    |   586,624  | 1   | 10,120
CSV (Megaparsec)/csv-40.csv    | 1,149,704  | 2   | 10,120
Log (Attoparsec)/log-5.log     |   144,760  | 0   | 12,592
Log (Attoparsec)/log-10.log    |   259,856  | 0   | 12,592
Log (Attoparsec)/log-20.log    |   490,016  | 0   | 12,592
Log (Attoparsec)/log-40.log    |   951,480  | 1   | 13,672
Log (Megaparsec)/log-5.log     |   256,712  | 0   | 13,976
Log (Megaparsec)/log-10.log    |   480,280  | 0   | 13,976
Log (Megaparsec)/log-20.log    |   928,528  | 1   | 15,056
Log (Megaparsec)/log-40.log    | 1,822,736  | 3   | 15,056
JSON (Attoparsec)/json-5.json  |    43,872  | 0   |  8,944
JSON (Attoparsec)/json-10.json |    76,728  | 0   |  8,944
JSON (Attoparsec)/json-20.json |   154,464  | 0   |  9,016
JSON (Attoparsec)/json-40.json |   300,032  | 0   |  9,016
JSON (Megaparsec)/json-5.json  |    67,600  | 0   | 10,528
JSON (Megaparsec)/json-10.json |   126,944  | 0   | 10,528
JSON (Megaparsec)/json-20.json |   280,080  | 0   | 11,232
JSON (Megaparsec)/json-40.json |   552,768  | 1   | 12,312

It's probably obvious that Megaparsec allocates a lot more, although
residency stays more or less the same. My first thought was that if we avoid
repacking to `String` the numbers should go down, and performance is likely
to improve. Making things like `string` return types closer to what is being
parsed was one of my original goals anyway, so I started on that.

I'll save us a slight detour by saying that we cannot quite return `s` from
`string`:

```haskell
string :: (MonadParsec e s m, Token s ~ Char) => String -> m s -- bad
```

Because as we'll learn later `Stream` may be a bit more complex, more like a
buffer, so we need another associated type function in `Stream`, let's call
it `Tokens`:

TODO

Another cause of excessive allocations may be in how we “uncons” the stream,
but more about that later.

…

## Learning from Attoparsec

TODO: changing the state to `TextStream` and `ByteStream` with internal
cursors.

## Conclusion

…
