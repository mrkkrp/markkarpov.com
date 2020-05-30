---
title: The story with data-default
desc: The post explains why data-default should not be used.
date:
  published: May 30, 2020
tag: haskell
---

Using [`data-default`][data-default] or
[`data-default-class`][data-default-class] in Haskell is always a mistake.
Why?

* *It adds no value.* For simple types like `Int` or `Bool` there is no
  universal default value because the concepts they represent are too
  general. For more complex and specific types nothing beats a simple
  non-polymorphic value. Such a value can also build on other
  non-polymorphic values.

* *It makes incorrect code typecheck.* When you have your defaults as
  non-polymorphic values, every time you use a default you are being
  explicit about the type you mean. With `def`, as long as you happen to put
  it in a place where GHC expects something that happens to have a `Default`
  instance it will be happily accepted. It does not help that `Default` has
  instances like `Default r => Default (e -> r)`.

* *There may be more than one default value per type.*

* *It is an extra dependency.* However light, no dependency is better than
  one.

* *The last commit was in 2016.*

[data-default]: http://hackage.haskell.org/package/data-default
[data-default-class]: http://hackage.haskell.org/package/data-default-class
