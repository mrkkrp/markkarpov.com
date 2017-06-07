---
title: Working with text without pain
desc: Hints how to make your typing experience more pleasant and protect yourself from RSI in the future.
date:
  published: June 7, 2017
---

For some time I wanted to write about how I do typing and text editing in
general. This is because I've noticed that many people I know (mainly
software developers) started to complain about pains in hands. I have to
admit that also faced this issue a couple of years ago. Since then though, I
managed to eliminate the problem completely and currently I type as long and
as much as I want and do not experience any pain at all. Here I describe the
combination of hardware/settings/techniques what helped me.

## The keyboard

In 2015 I still was doing all my work using a laptop, so my typing
experience was that of a man who has to use a built-in laptop keyboard. At
that time it did not occur to me that the built-in keyboard is outright
dangerous for health. In retrospect, I can name the following flaws:

* One has to keep hands in an unnatural position—too close to each other. In
  addition to that, the position is not symmetric: while the left hand can
  be kept in more-or-less natural position, the right hand is usually
  shifted to the left because of insufficient distance between the resting
  positions of the hands.

* Pinkies are overloaded and thumbs (the strongest fingers) are only used to
  press the spacebar. One might argue that remapping could be used to
  alleviate the problem, but in my experience it is not true, as the
  important, non-alphabetic keys are all placed in positions that can be
  reached only by pinkies. There is nothing in the middle of the keyboard
  that could be comfortably operated by thumbs: only the spacebar.

* The keys are staggered so one has to stretch his/her fingers in weird ways
  to reach keys that are not in the home row. Why do those completely
  unnatural movements? This decreases accuracy too.

* As an Emacs user, I had to use the <kbd>⎈ Ctrl</kbd> key a lot and so I
  followed the popular advice to make <kbd>⇪ Caps Lock</kbd> to function as
  the left <kbd>⎈ Ctrl</kbd>. This looked like a win at first (certainly
  better than the default position of the left <kbd>⎈ Ctrl</kbd>). Only
  later I realized that it makes my poor left pinkie do even more work.
  Also, this introduces an additional asymmetry because I did not make my
  <kbd>↵ Enter</kbd> to function as the right <kbd>⎈ Ctrl</kbd>. So I
  quickly started to use the <kbd>⇪ Caps Lock</kbd> in lieu of both <kbd>⎈
  Ctrl</kbd> keys: left and right.

It did not take long before strange sensations in my left pinkie started to
undermine my productivity as a keyboard user. Because of all those
asymmetries my posture wasn't quite straight too.

When I realized that I may lose my ability to work as efficiently as I used
to, I started to look for a better keyboard. From the previous points it
follows that “a better keyboard” should:

* Be either split or have enough space between the clusters for left and
  right hand, so a straight, natural posture could be maintained without
  effort.

* Should have some keys that could be operated by thumbs to unload pinkies.

* Should have straight layout of keys for each hand so they form a “matrix”.

* Should have all modifier keys (<kbd>⇧ Shift</kbd>, <kbd>⎈ Ctrl</kbd>, and
  <kbd>⎇ Alt</kbd>) in symmetric positions for each hand, to help divide
  work load evenly between the hands.

* It should be mechanical, since mechanical keyboards feel better and
  hopefully, last longer.

To solve the problem once and for all, I decided that I'd buy a keyboard
that satisfies those criteria no matter the price. After some research I
bought **Kinesis Advantage**, which is now discontinued because Kinesis came
up with a newer model, **Kinesis Advantage 2 QD**
([hi-res image](https://www.kinesis-ergo.com/wp-content/uploads/2016/07/kb600qd-oh-1977x1024.png)):

![Kinesis Advantage 2 QD](/static/img/kinesis-advantage-2-qd.png)

I bought this one too as soon as it became available. The “QD” part in the
name tells us that this is a version with dual-legended QWERTY/Dvorak key
caps, which is exactly what I need as a Dvorak typist.

This keyboard satisfies all my requirements. All the keys are mechanical,
including those smaller function keys. In addition to that it's fully
programmable, with bells and whistles, stores all configuration in its own
memory, works nicely out-of-the-box on Linux (and can be reprogrammed
without any additional software that usually requires access to a Windows
machine with other keyboards).

Being a professional software developer, spending $369 for a thing as vital
as keyboard is absolutely justified. These keyboards are known to work
without any issues for 10 years straight, so it's a nice investment too.

## Sequences of keys vs chords

To make typing as easy and comfortable as possible, it makes sense to avoid
key chords and prefer sequences of keys. This follows from the simple fact
that typing several keys in sequence is easier (and safer in long term) than
holding several keys. Many Emacs users suffer RSI because the default Emacs
shortcuts are quite brain-damaged in this regard.

I have replaced most shortcuts in Emacs with key sequences starting with an
“introducing key”. We can choose a single key, whose seul rôle will be
starting key sequences. How long should every such a key sequence be? Of
course, we want it to be as short as possible, but we cannot use only one
key after the introducing key, because the total number of combinations
won't be satisfactory. But we can use two keys after the introducing key
(<kbd>Page Down</kbd> in my case), then we get 26 × 26 = 676 combinations!
(In practice, we get even more because of punctuation and numbers.) Not bad
at all. There are enough combinations for us to prefer those that have some
mnemonic value. For example, when I need to invoke `grep` from Emacs I just
type <kbd>Page Down</kbd> <kbd>g</kbd> <kbd>r</kbd>.

## Sticky keys

As a continuation of the previous point about key sequences being better
than key chords, here comes the “sticky keys” feature. This feature is
provided by all major operating systems and desktop environments and allows
to type modifier keys like <kbd>⇧ Shift</kbd> and <kbd>⎈ Ctrl</kbd> *before*
the key you want to add them to. So to type a capital “A” I hit <kbd>⇧
Shift</kbd> without holding it I then type “a” and here we go—an easier,
better typing experience. Another benefit is *increased accuracy*, as it's
virtually impossible to “apply” <kbd>⇧ Shift</kbd> to more key strokes than
intended.

I started to use this technique in 2015 and now if it's disabled for some
reason I just don't bother to type capital letters at all because it
requires holding shift, which is an unnatural movement for me now.

The common complaint from people who do not use this is that they don't want
to have “state” in their typing. I can only say that after a while it
becomes very, very rare to leave a “hanging” modifier so that it gets
applied to a keystroke you don't want to apply it to. On the other hand,
typing with “sticky keys” is a lot easier and feels more natural. Give it a
try!

## Modal editing

Even with “sticky keys” and sequential key bindings for most tasks, common,
intensive editing commands like “go to beginning of the line” (<kbd>⎈
Ctrl</kbd>+<kbd>a</kbd> by default in Emacs) still require holding a
modifier key. “Sticky keys” is not exactly a win in this case, as
combinations like <kbd>⎈ Ctrl</kbd>+<kbd>n</kbd> may need to be pressed
several times in a row. The fact that the number of keys located in
convenient positions is limited suggests that we must use different “layers”
for editing: one layer would input keys as usual while another layer would
run various common editing commands using the same keys.

The idea is nothing new, of course, this is how Vi works. The technique is
called *modal editing* because we sort of have (at least) two modes of
operation: *insert mode* and *normal mode* (if we choose to follow the Vi
terminology). This allows to avoid holding any modifier keys.

Vi users already edit modally, while in the Emacs world there are several
ways to do that. For more information on the topic I suggest reading
[this article](https://github.com/mrkkrp/modalka/blob/master/README.md),
which is actually the `README.md` file for the package I've written and use
personally—Modalka. The `README.md` compares all packages in existence for
modal editing in Emacs and describes why you might want to try Modalka.

That's it, currently I *never* hold modifier keys, or any keys at all for
that matter. All my editing happens through typing sequences of keys—that
is, normal typing experience.

## Keyboard layout

QWERTY is a terrible historical accident, but switching to an alternative
keyboard layout such as Dvorak is still optional in my opinion, provided you
have a good keyboard and do not ever hold any keys/use key chords.

However, Dvorak will make typing more comfortable. I can confirm this as a
person [who switched a year ago](/post/dvorak-rocks.html). I still remember
the difference. If you don't want to take my word for it, here
is [this article](http://infohost.nmt.edu/~shipman/ergo/parkinson.html)
which explains why Dvorak is objectively better for typing English.

## Conclusion

Applying all these recommendations in practice does take will power and may
be inconvenient at first. Nevertheless I'm quite convinced that it is this
combination of hardware and settings that allows me to be an efficient
keyboard user and avoid any discomfort now and later. Hearing stories about
pains in hands from others, I usually try to convince them to at least buy
an ergonomic keyboard. Now I have the whole article to send them to!
