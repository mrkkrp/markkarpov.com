---
title: Lisp and Haskell
desc: How I finally decided what is better (for me), Lisp or Haskell?
date:
  published: October 23, 2015
  updated: November 23, 2019
tag: haskell
---

Lisp and Haskell are arguably some of the more peculiar languages out there.
It is always interesting to compare languages, so let me entertain you with
a story how I finally decided which of them is better.

When I first found out about Common Lisp it took my breath away. Seriously,
Lisp has consistent syntax, good design, and unique metaprogramming
capabilities. After Common Lisp, I learned a few other languages, some of
them out of necessity, others because of curiosity: Python, JavaScript,
Prolog, Clojure, and Haskell. I also was doing C and C++ in the past, but I
don't touch them now. Until recently I considered Common Lisp the best
language I know, and probably the most powerful language in existence.

The fact is, I know what Common Lisp is and what it can do, but the days
when I actually hacked Lisp (more or less) regularly are long gone, and I'm
mainly doing Haskell these days.

## Goodbye, Lisp

Today I actually have had a chance to compare my productivity with Common
Lisp and Haskell. I decided to spend a few hours on my open source projects.
First, I refactored [Megaparsec][megaparsec], and that was nice and easy,
but I didn't notice this because I'm already used to the level of efficiency
Haskell gives me.

Next, a user of one of my Common Lisp libraries opened an issue asking to
improve one thing a bit. I estimated the required work in 15 minutes of time
and started Common Lisp hacking for the first time in a couple of months.

It took about 1 hour to write about 20 lines of trivial code. Of course one
might say that I just forgot the details. Yet, from my point of view the
real reasons are:

* Common Lisp is dynamically typed and the compiler cannot help you when you
  write your code. (Well, it can help you a bit, making sure that your code
  is syntactically correct and all your declared variables are used for
  something.)

* Common Lisp mixes functional code with code that has side effects. To
  write idiomatic Common Lisp, you usually have to mix functional code with
  not-so-functional approaches. See how this works below.

* Common Lisp's standard library (the functions that are available to you as
  part of the ANSI Common Lisp standard) is quite poor by modern standards.
  A lot of useful functions are missing. There are libraries, but I'll get
  to them.

It's essential for that library I was working on to have minimal
dependencies, so I came up with this function in bare Common Lisp to add
padding to every line of text except for the first line:

```commonlisp
(defun add-text-padding (str &key padding newline)
  "Add padding to text STR. Every line except for the first one, will be
prefixed with PADDING spaces. If NEWLINE is non-NIL, newline character will
be prepended to the text making it start on the next line with padding
applied to every single line."
  (let ((str (if newline
                 (concatenate 'string (string #\Newline) str)
                 str)))
    (with-output-to-string (s)
      (map 'string
           (lambda (x)
             (princ x s)
             (when (char= x #\Newline)
               (dotimes (i padding)
                 (princ #\Space s))))
           str))))
```

In case you don't speak Common Lisp, let me highlight some parts of the
code:

* `concatenate` needs to know the type of its output, so we pass it a symbol
  specifying type of desired result as the first argument.

* `(string #\Newline)` constructs a line containing a single newline
  character. There is no syntax in Common Lisp to write something like
  `"\n"`. The alternative approach would be `(format nil "~%")`. There is no
  syntax for all other special characters if you want to put them into
  string. To be fair, you have multi-line string literals without funny
  escaping instead, which is vital for doc-strings and the like.

* `(map 'string …)` is used to loop through characters in a string. Note
  that here we use `map` function as a helper for a rather imperative
  procedure—printing results to new string using a temporarily created
  stream `s` (with the help of `with-output-to-string`). But that's
  idiomatic in Common Lisp.

When I ran this in the REPL, I got the following:

```commonlisp
; SLIME 2015-10-18
CL-USER> (asdf:load-system :unix-opts)
T
CL-USER> (in-package :unix-opts)
#<PACKAGE "UNIX-OPTS">
OPTS> (defvar *foo* (format nil "first line~%second line~%third line"))
*FOO*
OPTS> *foo*
"first line
second line
third line"
; compiling (DEFUN ADD-TEXT-PADDING ...)
OPTS> (add-text-padding *foo* :padding 10)
; Evaluation aborted on #<TYPE-ERROR expected-type: CHARACTER datum: NIL>.
```

The debugger popped up and told me in plain English:

> The value NIL is not of type CHARACTER.

It is difficult to argue with, `nil` is definitely not a character. But why
the heck do I get this? Can you tell? Please try as hard as you can! *(The
answer is at the end of the blog post.)*

I decided that I won't hack Common Lisp anymore. That's great and expressive
language, but I want to write in something I'm efficient with.

## Productivity of Haskell programmer

I use [Emacs](https://gnu.org/software/emacs/) for almost everything that is
related to text. One package I love in particular is
[Flycheck](http://www.flycheck.org/). When I edit Haskell source code,
Flycheck is running GHC with `-Wall` flag and
[HLint](https://github.com/ndmitchell/hlint) in the background and displays
warnings and errors interactively underlining my source code. This is a
convenient feature for any language, but only Haskell with its type system
takes this sort of tool to its limits.

In fact, this non-stop interactive conversation with compiler is the most
efficient programming workflow I've ever used. Combined with the fact that
*if your code compiles, it probably works*, Haskell must be the most
efficient (with respect to human resources) programming language in
existence just because of the static type system that works as a powerful
ally for the programmer. Of course, bugs can live in Haskell code too, but
I'm not saying we should abandon writing tests.

## Problems of Common Lisp

Speaking of tests, recently I discovered that Zach Beane AKA Xach, an
über-level Common Lisp hacker [doesn't usually write tests][does-not-test].
FYI, he is the author of [Quicklisp][quicklisp], that is something like (but
not quite) Cabal or Stack. Quicklisp is de-facto the only widely used
library manager in Common Lisp world, and so it's written in Common Lisp and
[doesn't have any tests][quicklisp-client]. It is a wonder for me how it
works. Usually when a project is big enough I start to have doubts whether
all parts of it still work after some changes, so I cannot imagine you can
do a thing like Quicklisp without tests and be confident about the result.

But you know what, Lisp, and its most advanced dialect (IMO), Common Lisp is
really cool. If you don't believe me, you can read [Paul Graham][avg] at any
time. The author can tell you what a great language Common Lisp is on
many-many pages. I don't remember where I read this, but he has something
like “There is the problem of lacking libraries, but on a big enough project
benefits of the language itself outweigh the lack of libraries.”

*Well, take any high-level language like Python, which have all the nice
libraries, and for project of any size it will be better than Common Lisp.
Macros are missing, but you can live without macros after all.*

Common Lisp doesn't have enough high-quality, actively maintained libraries.
The fact is, there are some pearls like [caveman][caveman] or
[stumpwm][stumpwm], but most libraries don't look good enough. Sometimes you
start thinking that if you want to end up with a great project you'll need
to write your own libraries (which you'll probably do, like many people
before you, not that it has improved the situation though).

Another problem is that some widely-used Common Lisp libraries have no
documentation at all. If you're to understand how to use them, *read the
source code*. I can name a couple of them, but I don't want to do so,
because I don't think it's polite. I've opened an issue on GitHub of one
quite popular library, asking the maintainer to write documentation. After 6
months it's still not written (strange, right?). In my opinion, this is not
a serious approach to maintaining your code.

When I was interested in Common Lisp, I had an idea of a pet project to help
me remember all sorts of French words and verbs in particular. Of course I
wanted to do the whole thing decently, even though it's console app, it
should have decent interface and work smoothly in general. I succeeded, but
I had to do a lot more than I would need to do if I wrote it in, say Python.
This is how (in retrospect I understand) less powerful Python would be
better fit for this (or almost any) project.

## The curse of dynamic languages

There is a blog post called [*Dynamic Languages are Static
Languages*][dynamic-static]. In short, the author makes the point that
dynamic langauges are static languages but with one huge type including all
possible values. Here is a paragraph I find important:

> And this is precisely what is wrong with dynamically typed languages:
  rather than affording the *freedom* to ignore types, they instead impose
  the *bondage* of restricting attention to a *single* type! Every single
  value has to be a value of that type, you have no choice! Even if in a
  particular situation we are absolutely certain that a particular value is,
  say, an integer, we have no choice but to regard it as a value of the “one
  true type” that is *classified*, not typed, as an integer. Conceptually,
  this is just rubbish, but it has serious, tangible penalties. For one, you
  are depriving yourself of the ability to state and enforce the *invariant*
  that the value at a particular program point must be an integer. For
  another, you are imposing a serious bit of run-time overhead to represent
  the class itself (a tag of some sort) and to check and remove and apply
  the class tag on the value each time it is used.

The lack of the power to express meaning of your program on type level is
another downside of Lisp. (You can add types in Common Lisp too, but that's
used solely for optimization. Common Lisp can be almost as fast as C, by the
way.) Unfortunately most statically typed languages don't have powerful
enough type system that would work as your ally, not enemy that exists only
to tell you that your code won't compile. Once you have strong and flexible
type system, it is very addictive.

Of course programming is (mostly) not math. There are things that
“conceptually just rubbish” for some, but then they suddenly give you
something like Lisp macros from practical point of view. What I can say for
sure is that I cannot be sure about my code in dynamic languages anymore.

## A better Lisp?

Now, it's tempting to think that Lisp can be somehow enhanced with strong
static typing. Racket has [statically typed
dialect][statically-typed-racket]. The question is whether its type system
is advanced enough? I mean, does it allow to express non-trivial invariants?
How does it play with macros, etc? I don't know, contact me if you used both
Racket and Haskell and you can make a comparison.

The selling features of Lisp, such as reprogramming on the fly and Lisp
macros are tightly connected to the dynamic nature of the language. Lisp is
like a living organism, it's full of reflection and there is no difference
between compile and run time. That's why you can process Lisp code with Lisp
dynamically. There is [Template Haskell][haskell-th] for example, which
allows you to do metaprogramming and it's type-safe, but this is *static
metaprogramming*, which is not quite the same.

I think it is difficult to have entire Lisp statically typed. From the very
basic things like cons cells it's all dynamic. One thing is clear, Lisp with
strong and powerful static typing system can be very different from what our
familiar Lisps are.

## Hackish vs protecting languages

I've come to the idea that there are two camps of programming languages:

* Hacking languages where everything is possible and you can do whatever you
  want (potentially bad things).

* Languages that try to protect you from your own mistakes.

When I was younger I liked hacking languages. C is definitely a hacking
language. If you want an example of protecting language of roughly the same
level, that's [Pascal][pascal].

In Russia, Pascal is especially popular as the first language you're taught
in school, institute, or university. I hated Pascal. I learned C on my own
because it seemed more hardcore and real. Then I learned C++ to do my
assignments (usually we were permitted to write *in anything*, I abused this
when I started to do my assignments in Common Lisp, a language that my
examiners could not read at all).

Now I appreciate the protecting languages, maybe because I know that I'll
definitely make mistakes and I want something to catch them. Haskell has
changed me as a programmer. Now I think there is something good (apart from
the strings that know their length) in Pascal, at least I don't hate it
anymore.

## Fix for that code snippet

If you're still curious, [`map`][hs-map] tries to assemble the specified (as
its first argument) data-type (`string` in my case) using values returned by
its second argument when called with an element from sequence supplied as
third argument. Since the `lambda` has [`dotimes`][hs-dotimes] as the last
form in its body, the whole function always returns `nil`. `nil` is not a
character and we cannot build a string from `nil`s.

So the fix is to pass `nil` as the first argument of `map` indicating that
we just want to return `nil` without building any string.

I put there `string` thinking about the string I wanted to traverse. Stupid
mistake? Maybe so, I'm just a human. For me, the cause of the problem was
not obvious until I found out what's going on, and that took a while. Think
about Haskell where this problem would have no place to hide in.

[megaparsec]: https://github.com/mrkkrp/megaparsec
[does-not-test]: http://xach.livejournal.com/278047.html?thread=674335#t674335
[quicklisp]: https://www.quicklisp.org/beta/
[quicklisp-client]: https://github.com/quicklisp/quicklisp-client
[avg]: http://www.paulgraham.com/avg.html
[caveman]: https://github.com/fukamachi/caveman
[stumpwm]: https://github.com/stumpwm/stumpwm
[dynamic-static]: https://existentialtype.wordpress.com/2011/03/19/dynamic-languages-are-static-languages/
[statically-typed-racket]: http://docs.racket-lang.org/ts-guide/
[haskell-th]: /tutorial/th.html
[pascal]: https://en.wikipedia.org/wiki/Pascal_%28programming_language%29
[hs-map]: http://www.lispworks.com/documentation/HyperSpec/Body/f_map.htm
[hs-dotimes]: http://www.lispworks.com/documentation/HyperSpec/Body/m_dotime.htm
