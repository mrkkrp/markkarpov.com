# Mark Karpov's personal web site

[![CI](https://github.com/mrkkrp/markkarpov.com/actions/workflows/ci.yaml/badge.svg)](https://github.com/mrkkrp/markkarpov.com/actions/workflows/ci.yaml)

To build the site:

```shell
$ nix build
```

The site will be in `result/`, you'll need to start an HTTP server to browse
it locally.

## Nix targets

The `flake.nix` exposes several targets:

* `site` — the full production build. It runs the site generator, compiles
  the Tailwind CSS, validates every generated HTML page with `vnu` (the
  Nu Html Checker), and includes the résumé PDF. This is the default package,
  so plain `nix build` produces it.

* `site-quick` — the same site but optimized for fast local iteration: HTML
  validation is skipped and the résumé PDF (which requires a full XeLaTeX
  toolchain) is not built. Prefer this while working on content or templates.

* `site-preview` — like `site`, with validation and the résumé PDF, but the
  generated `robots.txt` disallows all crawling (`Disallow: /`). Intended for
  deploy previews that should not be indexed by search engines.

* `app` — just the `mk-com` executable, the Haskell program that generates
  the site from the Markdown sources and templates. Building this alone is
  useful when hacking on the generator itself.

* `styles` — the compiled, minified `styles.css` produced from `styles/`
  by Tailwind, without building the rest of the site.

* `resume` — the résumé as a standalone PDF, rendered from the `site-quick`
  build via pandoc and XeLaTeX.

* `netlify-cli` — the Netlify CLI. The `netlify` app (`nix run .#netlify`)
  runs it directly, and is used for deployment.

To build a specific target, pass it to `nix build`, e.g. `nix build
.#site-quick` or `nix build .#app`.

## License

Copyright © 2017–present Mark Karpov

All rights reserved.
