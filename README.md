# Mark Karpov's personal web site

![CI](https://github.com/mrkkrp/markkarpov.com/workflows/CI/badge.svg?branch=master)
[![Netlify Status](https://api.netlify.com/api/v1/badges/49aff031-4e12-4cb3-b539-bd5ce3e8f8c4/deploy-status)](https://app.netlify.com/sites/markkarpov/deploys)

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
