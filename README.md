# Mark Karpov's personal web site

[![CI](https://github.com/mrkkrp/markkarpov.com/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/markkarpov.com/actions/workflows/ci.yaml)

To build the site:

```shell
$ nix build .#site
```

The site will be in `result/`, you'll need to start an HTTP server to browse
it locally.

For interactive editing of articles:

```shell
$ ./watch.sh
```

## License

Copyright © 2017–present Mark Karpov

All rights reserved.
