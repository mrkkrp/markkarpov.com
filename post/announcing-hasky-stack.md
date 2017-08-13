---
title: Announcing Hasky Stack
desc: Announing my new package for working with Haskell Tool Stack — Hasky Stack.
date:
  published: August 13, 2017
---

It's been a while since I released an Emacs package. The last one was
[`hasky-extensions`](https://github.com/hasky-mode/hasky-extensions) for
inserting/removing Haskell language extensions. This time I publish
[`hasky-stack`](https://github.com/hasky-mode/hasky-stack), which provides a
[Magit](https://magit.vc/)-like interface to [Stack](https://haskellstack.org/).

For those who want to give it a try, here is how:

1. Execute <kbd>M-x package-install RET hasky-stack RET</kbd> (assuming you
   have setup [MELPA](https://melpa.org/#/getting-started) in your Emacs,
   which you probably have).

2. Globally bind two commands: `hasky-stack-execute` and `hasky-stack-new`,
   for example (yes your key bindings may be less strange):

```emacs-lisp
(global-set-key (kbd "<next> h e") #'hasky-stack-execute)
(global-set-key (kbd "<next> h i") #'hasky-stack-new)
```

Now when you are inside a Haskell project directory or file, you can run
`hasky-stack-execute` to get a popup with all commands currently available:

![Root popup](/static/img/hasky-stack-root.png)

If you hit <kbd>b</kbd> now, a sub-popup for the build command appears:

![Build popup](/static/img/hasky-stack-build.png)

Next you can toggle the switches and set options, then hit <kbd>b</kbd>
again to build. You'll be presented with all build targets in your project
to choose from, etc. Note that if you would like to always build the default
target (i.e. your entire current project), set `hasky-stack-auto-target` to
`t`. When it's not `nil`, `hasky-stack` won't ask you build/test target
using the default automatically (this is what you almost always want
anyway).

The `hasky-stack-new` (which should be called from a new directory created
for the project to initialize) asks for project name and allows to choose
from a list of all available templates (which it detects for you
automatically).

Right now the wrapper is rather basic, I've put it together in just several
hours. I plan to improve and extend the package iteratively as I use it
taking into account user feedback. Feel free to request features!
