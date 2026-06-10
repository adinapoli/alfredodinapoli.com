# alfredodinapoli.com

Static site for [alfredodinapoli.com](https://www.alfredodinapoli.com),
generated with [Hakyll](https://jaspervdj.be/hakyll/).

## Build

Requires GHC ≥ 9.6 and cabal ≥ 3.10.

```bash
cabal build                    # compile
cabal run site-ctl build       # produce _site/
cabal run site-ctl watch       # local dev server at :8000
cabal run site-ctl clean       # nuke _site/ and Hakyll cache
```

`site-ctl` is a thin wrapper around `cabal run site --` (see
`SiteCtl.hs`). Both work; `site-ctl` is shorter.

## Layout

* `app/Main.hs` — Hakyll rules (routes, pandoc, RSS, tag pages,
  custom tag-chip render via blaze-html).
* `content/` — static pages (`index.html`, `contacts.html`,
  `oss.html`, `404.html`).
* `posts/` — blog posts (markdown / lhs). Front matter uses `tags:`
  to attach one or more tag labels, each rendered as a color-coded
  chip on the archive and the post page.
* `templates/` — page templates (`default.html` is the chrome:
  glass-blur nav, footer, meta. `page.html` is a thin shell for
  static content. `post.html` and `posts.html` are the post
  layouts. `postitem.html` renders one archive row).
* `css/` — hand-written design system (`screen.css`) and pandoc
  syntax highlighting (`syntax.css`). Web fonts self-hosted as
  woff2 under `css/fonts/`.
* `img/` — images, the inline `enso.svg` favicon, and the
  hero photo `alfredo.jpg`.

## Theme

Light, modern developer portfolio. Inter (sans, 400/500/700) +
JetBrains Mono (mono). Light cool off-white base, gradient blue →
purple → pink accent, per-tag chip colors (haskell = purple,
ai = pink, types = blue, compilers = orange, security = green).
Sticky glass-blur nav. Gradient-text hero name. Tag chips on the
post archive.

The 3-link structure of the live site (Home, Posts, OSS) is
preserved verbatim. The predecessor's added Talks and CV pages
are gone.

Spec: `docs/superpowers/specs/2026-06-10-website-overhaul-design.md`
Plan: `docs/superpowers/plans/2026-06-10-website-overhaul.md`
