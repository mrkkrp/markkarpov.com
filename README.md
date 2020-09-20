# Mark Karpov's personal web site

![CI](https://github.com/mrkkrp/markkarpov.com/workflows/CI/badge.svg?branch=master)

To build the site:

```shell
$ nix-build -A site
```

The site will be in `result/`, you'll need to start an HTTP server to browse
it locally.

For interactive editing of articles:

```shell
$ ./watch.fish
```

## License

Copyright © 2017–present Mark Karpov

All rights reserved.
