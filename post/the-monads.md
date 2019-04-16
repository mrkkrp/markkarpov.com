---
title: The monads of Haskell
desc: Let's talk about monads in Haskell.
date:
  published: April 16, 2019
---

*This one reads almost as if it's a 1st April joke, but as a matter of fact
it's not. ~~It's 16th April joke.~~ It's serious.*

If we look at Haskell from outside of its community, the first word that
comes to mind of people who hear the name of the language is *monads*. If we
look from inside of the community… well, it's probably also monads. In a
typical Haskell program, there are quite a few.

In tutorials, Haskell beginner will find the wisdom of modelling various
things with monads such as state, writer, reader, etc. There are also *monad
transformers*, and they sort of compose. Except that they don't compose well
enough in practice. Some of them leak. If you're not knowledgeable about the
subject, you'll most likely write a program that leaks memory. If you are
knowledgeable, you'll most likely also write a program that leaks memory.
Finally, if you're knowledgeable enough and explicitly optimize what you
write not to leak, you *may leak less than usual*.

All too easy we get into the mindset “but this is how things work, you have
to care if you want to be a cool Haskeller and produce high-quality code”.
And here we go, remember the rules how to write code that doesn't leak.

*I would rather not.* I don't want to care about space leaks. I could care,
on special occasions, in fact I'm paid to, but I don't want to. When I do, I
feel so acutely the limitations of the technology that I use. If I have to
pay attention so much to how it's going to work *operationally*, then it's
not really *declarative* code, right?

Even if you don't leak, a complex monadic stack is not handy with
exceptions. The gold standard is often said to be something like `ReaderT
Context IO`. You just need the ability execute and mutate. As a Haskeller
I'll favor this simple stack over almost anything else. Sure. But again, is
it actually still *declarative* programming?

Where are we going? Are we just converging to a much simpler approach which
actually *does the trick* (tm)? Non-declarative code is OK. It's well-known
to humanity and works well. The only problem is that we wanted to have
declarative code instead.

What about all the lies you told to that young Haskell apprentice about
`WriterT`? Look in his eyes, he still believes it. He still tells you how he
would design the right monadic stack for a problem. Beautiful stack, which
makes sense, not your discovered-through-pains-and-experience reader-over-IO
thingy. Put it away. I don't wanna see it this evening.

This is a *bad* post, but in a sense, I'm innocent. Deep down inside people
know something is wrong. And they seek to recover the beauty of proper
declarative programming.

Some of these people deem monad transformers dim. They use free and freer
monads instead. And then they regret that and publish posts about their
regrets on Haskell sub-reddit. Sure,

> there are just some problems with exceptions, and concurrency and a few
  other things. So we spent most of the time actually \<censored\> instead
  of just doing the stuff that works.

Doesn't seem to work that way either?

So what is the conclusion regarding monads and monadic effects? They've been
around for a while, but still the experience is far from perfect. The only
reasonable thing that works is so rudimentary that I'm embarrassed when I
talk about it.
