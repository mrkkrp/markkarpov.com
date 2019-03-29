# Mark Karpov's personal web site

[![CircleCI](https://circleci.com/gh/mrkkrp/markkarpov.com/tree/master.svg?style=svg&circle-token=b1e49c26f2aa87ebc0c7884108ed7cfc866d24d0)](https://circleci.com/gh/mrkkrp/markkarpov.com/tree/master)

Here I have a `shake`-powered custom generator of static pages. I serve them
via Nginx from a DigitalOcean droplet.

# How to generate a new image

```console
$ docker build . -t mrkkrp/mk-com:X.X.X
```

Where `X.X.X` is the version of image we're generating.

Then push it:

```console
$ docker push mrkkrp/mk-com:X.X.X
```

Then update image names in `.circleci/config.yml`.

## License

Copyright © 2017–2019 Mark Karpov

All rights reserved.
