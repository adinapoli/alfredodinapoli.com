# 2026 Website Overhaul — Design

**Status:** Draft for review
**Author:** Alfredo Di Napoli (with Claude)
**Date:** 2026-06-10
**Branch:** `adinapoli/2026-overhaul`

## Goal

Bring `alfredodinapoli.com` from its 2012-era Hakyll/Bootstrap shell to a 2026
visual + content state. Preserve the "Zen" spirit (simplicity, restraint,
enso mark) while modernizing typography, color, layout, and information
architecture. Update CV to reflect current role and experience. Build system
modernized; content URLs preserved.

Scope (confirmed): visual restyle + IA rework + content rewrite + CV
overhaul. Photo swap deferred (placeholder until owner supplies).

## Constraints

- Hakyll stays as the static site generator (Haskell-eats-own-dogfood).
- Existing post URLs MUST NOT change (RSS, inbound links, SEO).
- Light theme only — no dark mode this iteration.
- No JavaScript framework. KaTeX (already opt-in per post) is the only JS.
- Self-hosted web fonts (no Google Fonts CDN at runtime).

## Visual direction

**Aesthetic:** Bauhaus + wabi-sabi. Geometric grid, strong typographic
hierarchy, restrained palette, warm imperfection via paper texture.

**Palette (light only):**

| Token         | Hex       | Role                                      |
|---------------|-----------|-------------------------------------------|
| `--paper`     | `#f6f1e7` | Body background (warm rice paper)         |
| `--paper-soft`| `#efe8d8` | Code block, blockquote background         |
| `--ink`       | `#1a1a1a` | Primary text                              |
| `--ink-soft`  | `#4a4a4a` | Secondary text (dates, meta)              |
| `--rule`      | `#d8d0bc` | Hairline rules, borders                   |
| `--accent`    | `#c8553d` | Terracotta accent (links, active nav)     |
| `--accent-soft`| `#e08a78`| Hover states                              |

**Typography:**

- Body: **EB Garamond** 17px, line-height 1.65, measure 68ch.
- Headings: **Inter** 600, tracking tight, sentence case.
- Code: **JetBrains Mono** 0.92em.

Modular type scale, ratio 1.25:
`--fs-base 17px`, `--fs-sm 14px`, `--fs-lg 1.333rem`, `--fs-xl 1.777rem`,
`--fs-2xl 2.369rem`, `--fs-3xl 3.157rem`.

**Texture:** subtle SVG noise as data-URI on `body`, ~3% opacity. Wabi-sabi
warmth without skeuomorphism.

**Accent usage:** sparing — link underline-on-hover, active nav, blockquote
left border, post date. Never as backgrounds.

**Enso mark:** vectorize current `img/enso.png` to SVG, inline in header,
themeable via `currentColor`. Photo deferred.

## Information architecture

Navigation (6 items, unchanged):
`Home · Blog · Projects · Talks · CV · Contacts`

Home is intentionally minimal: hero only (no curated post list, no project
cards). Other content lives on its dedicated page.

Pages:
- `/` — hero (name, role, 1-line bio, photo placeholder, quote, 3-link row)
- `/posts.html` — full archive, grouped by year, one entry per line
- `/oss.html` — open source projects
- `/talks.html` — talks + publications
- `/cv_eu/cv_eu.html` — CV
- `/contacts.html` — contact info
- `/404.html` — new, simple

Routes preserved verbatim for every existing post (`posts/*.markdown`,
`posts/*.lhs`). RSS feed at `/rss.xml` unchanged.

## Build system

- Drop `stack.yaml`, `stack.yaml.lock`.
- Add `cabal.project` and `cabal.project.freeze` (pinned deps for repro).
- Bump GHC + `hakyll` + `pandoc` to current releases compatible with
  Haskell 2024 ecosystem.
- Update `website.cabal`: cabal-version 3.0, modern field syntax, drop
  unused deps (`aeson`, `shelly`, `string-conv` if Paginator no longer
  needed — review at impl time).
- Build commands documented in README:
  - `cabal run site -- build`
  - `cabal run site -- watch`
  - `cabal run site -- clean`

## Templates

**`templates/default.html`** — outer chrome only:
- Strip all 2012-era SEO/robots meta (Aladin, Lycos, REVISIT-AFTER,
  document-rating, etc.). Keep: charset, viewport, title, description,
  og:title, og:description, og:image, twitter:card.
- Add `<meta name="viewport" content="width=device-width, initial-scale=1">`.
- Header: enso SVG (~48px, left), name + role (right), nav row under
  hairline rule.
- Footer: copyright, RSS link, GitHub link, build year, hairline above.

**`templates/page.html`** (NEW) — thin shell for static content pages
(index, contacts, oss, talks). Lets `default.html` stay pure chrome.

**`templates/post.html`** — rewrite:
- Title (Inter 2xl) → date + tags row (Inter sm, ink-soft) → hairline →
  body (Garamond).
- Prev/next post links at bottom (by date).
- KaTeX block kept (existing opt-in via `katex: true` frontmatter).

**`templates/posts.html`** — rewrite:
- Group by year (h2 Inter). Each entry: mono date | serif title (link) |
  small tag list. Full archive, no pagination.

**`templates/cv_eu.html`** — rewrite:
- Sections: Header (name, role, Rome, contact), Summary, Experience
  (reverse chrono), Selected projects, Open source, Talks/publications,
  Education, Skills.

## Content rewrite

### `content/index.html` — hero

- Name (Inter 3xl display).
- 1-line bio: "Senior Haskell engineer in Rome. AI architect. Functional
  programming since 2012."
- Photo placeholder: 240×240 square, circle-cropped, neutral gray fill.
  Swap deferred.
- Quote (small italic, under bio): "Simplicity is the ultimate
  sophistication."
- 3-link row at bottom: Writing · Projects · CV.

### `content/contacts.html`

- Location: **Rome, Italy** (was Manchester / Sicily — both stale).
- Email: alfredo.dinapoli@gmail.com (mailto).
- GitHub: adinapoli.
- Mastodon/X: to be confirmed by owner at impl time.
- Phone removed unless owner requests keeping it.

### `content/oss.html`

Refresh project list. Existing repos to review: mandrill, rncryptor,
snaplet-purescript, threads-supervisor, etc. Owner to confirm which stay
featured at impl time.

Add prominent mention of **GHC diagnostic infrastructure** contribution
with link to Well-Typed blog post:
https://well-typed.com/blog/2021/08/the-new-ghc-diagnostic-infrastructure/

Centaur framework: include if public, else skip from OSS page (CV
mentions it via the published paper).

### `content/talks.html`

Keep past talks. Owner to supply any 2020–2026 talks at impl time.

### `cv_eu/cv_eu.md` — full rewrite

**Header**
- Alfredo Di Napoli — Senior Haskell Engineer — Rome, Italy — email — site.

**Summary**
2–3 sentences, 2026 voice. Drop the "beauty-driven developer / Gelernter"
opening from CV (lovely but dated; could migrate to home if desired —
owner choice at impl time).

**Skills** (compact, ordered by preference)
Haskell (expert), Zig (hobby), Scala, Python, C++, OCaml, Nix, AI/LLM
system architecture, distributed systems, type-driven design.

**Experience** (reverse chrono)
- **2019 — present** — Senior Haskell Engineer, **Well-Typed LLP**.
  Production Haskell engineering. (Clients omitted by owner request.)
- **2013 — 2019** — **IRIS Connect** (UK). Senior Developer; Cloud Lead
  Engineer in the final years.
- **2012 — 2013** — **Cake Solutions Ltd** (Manchester, UK). Scala
  developer.
- **Earlier** (collapsed): MBDA Missile Systems intern (2012); Laetus
  framework (master thesis lead developer, 2011–2012); Expiweb freelance.

**AI architecture** (new section)
- **Centaur** — framework for architecting systems via AI. Link to paper
  (owner to supply URL at impl time).
- AI-as-architect work in current role.

**Open source** (link list, top 5–8 repos)

**Talks & publications** (condensed)

**Education** — unchanged: "Roma Tre" College of Engineering, Master in
Computer Science, summa cum laude, 29.4/30.

## Error handling & edge cases

- Web fonts: `font-display: swap`. Fallback stacks: Georgia for serif,
  system-ui for sans, ui-monospace for mono. No FOIT.
- Old post URLs preserved (no Hakyll route changes).
- RSS feed (`/rss.xml`) URL unchanged.
- KaTeX opt-in per post (existing `katex:` frontmatter mechanism kept).
- Image paths `/img/2013/...`, `/img/2014/...` retained.
- `posts/*.hi` `*.o` cleanup preprocess hook kept.
- New `content/404.html` — simple text + home link.
- `relativizeUrls` kept for portability.
- `drafts.html` route stays but unlinked from nav (status quo).
- `cv/cv.markdown` and the `cv/` route: **DELETE**. Single canonical CV
  at `/cv_eu/cv_eu.html`.

## Testing & verification

- `cabal build` clean on fresh checkout (verify in CI or fresh sandbox).
- `cabal run site -- build` produces `_site/` with zero errors/warnings.
- Local watch mode: `cabal run site -- watch`. Visual smoke-check of:
  - `_site/index.html`
  - `_site/posts.html`
  - one random post per year (2012, 2014, 2017, 2018)
  - `_site/cv_eu/cv_eu.html`
  - `_site/contacts.html`
  - `_site/oss.html`, `_site/talks.html`
  - `_site/404.html`
- KaTeX renders: `_site/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html`
- RSS validates (W3C feed validator).
- Diff old vs new `_site/` for unexpected route changes (post URLs must
  match 1:1).
- Lighthouse mobile (manual): target ≥95 perf, ≥95 a11y, no console
  errors.
- Responsive check: 360px, 768px, 1280px viewports.

## Phasing

1. **Foundation** — drop stack, add `cabal.project` + freeze, bump deps,
   verify `cabal build` green. Templates/CSS unchanged.
2. **CSS system** — new `css/screen.css` with tokens + base styles. Drop
   `bootstrap.css`, `reset-fonts-grids.css`, `FontAwesome.ttf`. Self-host
   fonts under `css/fonts/`.
3. **Templates** — rewrite default, page, post, posts, cv_eu. New header,
   footer, nav, grid. Drop bootstrap classes everywhere.
4. **Content** — rewrite `content/index.html`, `content/contacts.html`,
   `content/oss.html`, `content/talks.html`. Add `content/404.html`.
5. **CV** — full rewrite of `cv_eu/cv_eu.md`.
6. **Polish** — paper texture, accent tuning, font-loading optimization,
   Lighthouse pass, validate links/RSS.

Each phase = its own commit. Owner decides single PR (phased commits) vs
PR-per-phase at plan time.

## Open items (resolved at impl time, not blockers for spec)

- Mastodon/X handles for contacts page.
- Phone number: keep or drop.
- Centaur paper URL.
- 2020–2026 talks list.
- OSS repos to feature (confirm/prune existing list).
- "Gelernter quote" — move to home, keep in CV, or drop entirely.
- New photo (owner supplies later — placeholder until then).

## Out of scope

- Dark mode.
- New CMS / migration off Hakyll.
- Newsletter / subscription.
- Comments system.
- Analytics.
- New domain or hosting move.
- Search.
