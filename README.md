# alfredodinapoli.com

Static site for [alfredodinapoli.com](https://www.alfredodinapoli.com),
generated with [Hakyll](https://jaspervdj.be/hakyll/).

## Build

Requires GHC ≥ 9.6 and cabal ≥ 3.10.

```bash
cabal build
cabal run site -- build       # produce _site/
cabal run site -- watch       # local dev server at :8000
cabal run site -- clean       # nuke _site/ and Hakyll cache
```

## Layout

* `app/Main.hs` — Hakyll rules (routes, pandoc, RSS, tags).
* `content/*.html` — static pages (index, contacts, oss, talks, 404).
* `posts/` — blog posts (markdown / lhs).
* `cv_eu/cv_eu.md` — CV source.
* `templates/` — page templates.
* `css/` — hand-written design system (`screen.css`) and pandoc syntax
  highlighting (`syntax.css`). Web fonts are loaded from Google Fonts in
  `templates/default.html`.
* `img/` — images and the inline-able `enso.svg` mark.

## Theme

2026 overhaul: Bauhaus geometry with wabi-sabi warmth — warm paper
background, EB Garamond body, Inter headings, JetBrains Mono code,
single terracotta accent. See
`docs/superpowers/specs/2026-06-10-website-overhaul-design.md`.
