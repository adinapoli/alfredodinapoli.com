# 2026 Website Overhaul — Design

**Status:** Draft for review
**Author:** Alfredo Di Napoli (with Claude)
**Date:** 2026-06-10
**Branch:** `adinapoli/2026-overhaul`

> Replaces the earlier 2026-06-10 Florentine-codex spec at this same path.
> The previous design (Bauhaus / wabi-sabi, Marcellus caps, parchment, "Folio I"
> captions) was over-corrective: it replaced a spartan 2012 site with a museum
> exhibit rather than a modern developer portfolio. This spec goes back to the
> live site's original 3-link structure and applies a 2026 design language.

## Goal

Bring `alfredodinapoli.com` up to a confident 2026 visual state while
preserving the **structure of the live site** (`adinapoli.github.io/alfredodinapoli.com/`):
three top-of-page links (Home, Posts, OSS), a header motto, a tagline-style
hero on the home page, and a flat post archive. Bring the content up to 2026
(current role at Well-Typed, AI architecture work, Rusholme, GHC diagnostic
infrastructure contribution) and modernize every page with a single coherent
design system that reads as "modern developer portfolio" — not as a museum
exhibit, and not as a 2012 page.

## Constraints

- Hakyll stays as the static site generator (Haskell eats its own dogfood).
- Existing post URLs MUST NOT change (`posts/*.html`, RSS, inbound links, SEO).
- Light theme only — no dark mode this iteration.
- No JavaScript framework. No Tailwind. KaTeX (already opt-in per post) is the
  only client-side JS. Single hand-written `css/screen.css`.
- Self-hosted web fonts (no Google Fonts CDN at runtime).
- Photo: reuse `img/alfredo.jpg` as-is. New photo is out of scope; layout
  accommodates a square crop without changes.
- The 3-link structure of the live site is preserved. No new top-level
  navigation items added (no Talks, no CV, no Projects page in the nav).
- The `cv/` directory and `cv_eu/cv_eu.md` are dropped this iteration. The
  CV does not get a page; if needed, it lives at the bottom of `oss.html` as
  a `// resume` section (decided at impl time, default: omit).
- The predecessor's design spec at this same path is being overwritten —
  intentional, not a coincidence.

## Visual direction

**Aesthetic:** Light, modern developer portfolio. Inspired by rusholme's
project site (`adinapoli.github.io/rusholme/`) but light-themed and applied to
a *person*, not a project. Same techniques: glass-blur nav, gradient text,
soft glow, `// mono-comment` section labels, soft card lift on hover, custom
scrollbar. No emojis. No "vibe coding" decorations.

**Palette (light only):**

| Token              | Hex       | Role                                          |
|--------------------|-----------|-----------------------------------------------|
| `--bg`             | `#fbfbfd` | Page base (cool off-white)                    |
| `--bg-2`           | `#f4f4f8` | Slightly deeper, for soft gradients           |
| `--surface`        | `#ffffff` | Card backgrounds (translucent over `--bg`)    |
| `--border`         | `#e0e0ea` | 1px borders, card edges                       |
| `--text`           | `#111118` | Primary text                                  |
| `--text-soft`      | `#4a4a55` | Secondary text (bio, captions)                |
| `--text-faint`     | `#8a8a98` | Meta (dates, section labels)                  |

**Accent gradient** (the name, featured card title, and any decorative use):

```css
background: linear-gradient(90deg, #3b82f6 0%, #8b5cf6 50%, #ec4899 100%);
```

Three stops. Used sparingly: never as a background fill, only on text and on
the brand-mark monogram tile. Hover underline on nav links uses the same
gradient.

**Per-topic tag accent colors:**

| Tag              | Hex       | Notes                              |
|------------------|-----------|------------------------------------|
| `haskell`        | `#8b5cf6` | Purple                             |
| `types`          | `#3b82f6` | Blue                               |
| `ai`             | `#ec4899` | Pink                               |
| `compilers`      | `#f7a41d` | Zig-orange (matches Rusholme)      |
| `security`       | `#10b981` | Emerald                            |
| default / `misc` | `#6b7280` | Zinc                               |

Tag chip: 1px border in the topic color at 18% opacity, text in the topic
color at full saturation, 6px radius, 0.7rem JetBrains Mono.

**Typography:**

- Display: **Inter** 700, `letter-spacing: -0.04em`. The name, page titles.
- Body: **Inter** 400, 16px, line-height 1.6, `--text` color.
- Mono: **JetBrains Mono** 400, used for code, dates, section labels,
  tags, nav active state.
- No italic for body. Italic reserved for the Gelernter quote.
- Self-hosted under `css/fonts/` (woff2, `font-display: swap`,
  preloaded via `<link rel="preload">`).

**Rounding & shadows:**

- Cards: 14px radius
- Nav pills: 999px (full round)
- Section rules: 1px, gradient-fade horizontal
- Card shadow: `0 1px 2px rgba(17,17,24,0.04)` at rest
- Hero monogram tile: `0 20px 50px -12px rgba(139,92,246,0.45)`,
  `inset 0 1px 0 rgba(255,255,255,0.25)` for a soft top highlight
- Hover card lift: `translateY(-4px)`, shadow deepens to
  `0 12px 30px -8px rgba(17,17,24,0.12)`

**Glass nav:**

```css
background: rgba(251, 251, 253, 0.7);
backdrop-filter: blur(14px);
-webkit-backdrop-filter: blur(14px);
border-bottom: 1px solid var(--border);
```

Sticky, 64px tall on desktop, 56px on mobile. Brand mark on the left, three
links on the right (Home · Posts · OSS), active page as a black pill
(`background: #111118; color: #fff;`). Other links get a gradient underline
that grows from center on hover (rusholme's `nav-link::after` pattern).

**Animations (subtle, all gated by `@media (prefers-reduced-motion: no-preference)`):**

- Hero monogram: gentle 6s float (`translateY` 0 → −6px → 0)
- Nav link underline: width 0 → 100% on hover, 240ms ease
- Card hover: `translateY(-4px)` + shadow, 220ms ease
- Tag chip hover: `background` opacity 0.1 → 0.18
- Section dividers: static gradient line, no animation

**Custom scrollbar:** 8px wide, `--bg-2` track, `--border` thumb, accent
thumb on hover.

**Texture:** none. The design relies on color, gradient, and shadow for
depth. No noise, no paper texture, no grain — that was the Florentine codex
and it's gone.

**Brand mark:** a 26×26 square tile, 7px radius, the accent gradient as
background, with a 14×14 white ring inside (using a transparent right border
to suggest an enso / C-shape — pays homage to the original site's enso mark
without literally being one). Sits in the nav, left of "alfredo".

## Information architecture

**Navigation (3 items, unchanged from live site):**

```
Home · Posts · OSS
```

Active page is a black pill; inactive pages are text links with a gradient
underline on hover. No hamburger menu needed at 3 items.

**Pages:**

- `/` — hero home
  - Glass nav (sticky)
  - Hero: gradient monogram tile (with `alfredo.jpg` inside), gradient-text
    name, tagline bio, tag chips, Gelernter quote box
  - `// what i'm building` — 3 cards (Rusholme featured, Centaur, GHC
    diagnostic infrastructure contribution)
  - `// writing` — latest 5 posts, with tag chips; `all posts →` link
  - Footer: thin, "© MMXXVI · Alfredo Di Napoli · RSS · GitHub"
- `/posts.html` — flat list grouped by year. Every post one line. Tag chip
  in the right column. Click a tag → `/tags/<tag>.html` (Hakyll `buildTags`
  is already wired, see `app/Main.hs`).
- `/oss.html` — three sections: **Independent Projects** (Rusholme first as
  a featured card with the gradient title, then the rest of the live site's
  list), **Haskell Contributions** (GHC diagnostic infrastructure linked to
  the Well-Typed blog post, plus existing Snap-era items), **Other
  Contributions**.
- `/contacts.html` — short modernized. Email, GitHub, location. Pull quote
  at the bottom (which quote is your call at impl time).
- `/404.html` — minimal. Glass nav, "404" in big gradient-text, "This page
  doesn't exist. Try the home page." and a `→ home` link.

**Routes preserved verbatim** for every existing post. RSS at `/rss.xml`
unchanged. `/tags/*.html` is already produced by the existing `buildTags`
setup in `app/Main.hs` — verify it works at impl time and fix if broken.

**Removed this iteration:**

- `/cv/cv.html` (the old `cv/` route) — dropped.
- `/cv_eu/cv_eu.html` (the predecessor's CV) — dropped.
- `/talks.html` (added by the predecessor) — dropped.

**Photo:** uses `img/alfredo.jpg` as-is. The monogram tile is a 96×96
rounded square (14px radius), the photo is `object-fit: cover` inside, with
the gradient as a fallback background if the image is missing. New photo is
out of scope; swapping `alfredo.jpg` later requires no template change.

## Build system

Unchanged from the predecessor's setup:

- Hakyll (>= 4.16), GHC 9.6+, cabal 3.10+.
- `cabal.project` + `cabal.project.freeze` for reproducible builds.
- `site-ctl build | watch | clean` wrapper (defined in `SiteCtl.hs`).
- No new deps. No JS framework, no Tailwind, no build step beyond Hakyll.

**Routes that must be removed from `app/Main.hs`:**

```haskell
match "cv_eu/*.md" $ do ...   -- remove
```

**Search `app/Main.hs` for any remaining `cv` references** (e.g. the old
`cv/cv.markdown` route, if it ever existed as a separate match) and remove
them. The current `app/Main.hs` only has the `cv_eu/*.md` match — verify
there's no `cv/cv.markdown` match elsewhere. The `cv/` directory at the
project root can then be deleted; `cv_eu/cv_eu.md` too.

**Tag pages must be verified working.** `app/Main.hs` already has a
`buildTags "posts/*" (fromCapture "tags/*.html")` and `tagsRules` block.
Impl step: render `_site/tags/haskell.html` after a build and confirm it
exists and renders. If broken, fix the `tagsRules` block. (No new logic
required — just verify.)

## Templates

**`templates/default.html`** — outer chrome:

- Strips all SEO cruft except: charset, viewport, title, description, og:*,
  twitter:card. Same as predecessor.
- Self-hosted `<link rel="preload" as="font" type="font/woff2" crossorigin>`
  for Inter (400, 700) and JetBrains Mono (400).
- `<link rel="stylesheet" href="/css/screen.css">`
- `<link rel="stylesheet" href="/css/syntax.css">`
- `<link rel="icon" type="image/svg+xml" href="/img/enso.svg">` (the enso
  favicon from the original site is still fine — favicons don't need to
  match the new design language strictly)
- Glass `<header>` with brand mark + nav
- `<main>$body$</main>`
- Thin footer with copyright + RSS + GitHub
- `$if(katex)$ $katex$ $endif$` block at the end (unchanged)

**`templates/page.html`** (NEW) — thin shell for static content pages
(index, contacts, oss, 404). Lets `default.html` stay pure chrome. Wraps
the page body in a `<article class="page">` with appropriate top spacing.

**`templates/post.html`** — single-post layout:

- Title (Inter 700, 2rem, gradient *only* on the home page header, not on
  individual post titles)
- Date + tags row (mono, faint)
- Hairline
- Body (Garamond → Inter for body; pandoc renders the content; KaTeX
  opt-in via `katex: true` frontmatter kept)
- Prev/next post links at the bottom (existing behavior)

**`templates/posts.html`** — archive:

- `// writing` mono section label
- `<h1>` "Posts"
- Year groups: each year is a `<section>` with `<h2>{year}</h2>` (mono,
  faint), then a list of post items
- Each post item: date (mono) · title (Inter, link) · tag chip (right)
- Pagination: none (flat archive, matches live site)

**`templates/postitem.html`** — single row in the archive:

- `<li class="postitem">` with grid `7rem 1fr 4rem` (date, title, tag)
- Hover: subtle background tint

**`templates/cv_eu.html`** — REMOVED. (See IA section.)

**`templates/page-home.html`** (NEW) — the home page composition. Wraps
the home-specific sections (hero, `// what i'm building`, `// writing`) in
a single template that `default.html` can `$include$`. Keeps the home
page's special structure out of `default.html`.

## Content

### `content/index.html` — home

**Hero block:**

- Brand mark: monogram tile, 96×96, `alfredo.jpg` inside (`object-fit: cover`),
  gradient background as fallback
- Name (Inter 700, 3.4rem desktop / 2.4rem mobile, gradient text)
- Tagline bio (Inter 400, 1rem, `--text-soft`, max 42ch):
  > Beauty-driven developer. Haskell since 2010. AI-augmented architect at
  > Well-Typed. Open source contributor and public speaker based in Rome.
- Tag chips (5): `haskell`, `type-driven design`, `ai systems`,
  `distributed systems`, `compilers`
- Gelernter quote box (rounded, glass-blur, max 56ch):
  > "Beauty is more important in computing than anywhere else in technology
  > because software is so complicated. Beauty is the ultimate defense
  > against complexity."
  > — David Gelernter

**`// what i'm building` block** — 3 cards:

1. **Rusholme** *(featured)* — gradient text on the title, a soft corner
   accent (radial gradient in the top-right corner of the card), label
   `// featured` in mono purple. Body: "A toy Haskell compiler in Zig.
   Frontend, Core, GRIN, LLVM/WASM/C backends. Open source, in
   development." Tags: `zig`, `llvm`, `grin`. Link to
   `https://github.com/adinapoli/rusholme`.
2. **Centaur** — standard card, label `// production` in mono blue. Body:
   "AI-as-architect framework for designing complex distributed systems."
   Tags: `ai`, `architecture`. Link to the published paper (URL supplied
   at impl time; if no public URL, link to the GitHub repo or omit the
   link).
3. **GHC diagnostic infrastructure** — standard card, label
   `// open source` in mono green. Body: "Contributor to the new GHC
   diagnostic infrastructure at Well-Typed." Tags: `haskell`, `ghc`. Link
   to the Well-Typed blog post:
   `https://well-typed.com/blog/2021/08/the-new-ghc-diagnostic-infrastructure/`.

**`// writing` block** — latest 5 posts. Each row: date (mono faint) ·
title (Inter link) · tag chip (right). Below the list, an
`all posts →` link to `/posts.html`.

### `content/oss.html` — open source

Three sections, with the same content as the live site (Independent /
Haskell / Other), restructured so Rusholme is the first "Independent
Project" as a featured card (matches the home page treatment), and the
GHC diagnostic infrastructure contribution is in "Haskell Contributions"
with a link to the Well-Typed blog post.

### `content/contacts.html` — contacts

- Page title: "Contacts" (Inter 700, gradient text, smaller than home)
- Email: `alfredo.dinapoli@gmail.com` (mailto)
- GitHub: `https://github.com/adinapoli`
- Location: Rome, Italy
- Pull quote at the bottom (which quote is your call at impl time).
  Default: the Lao Tzu quote from the live site, since it's what the
  current site has and you didn't ask to change it. Easy to swap at impl.

### `content/404.html` — 404

- "404" in big gradient text
- "This page doesn't exist. Try the home page."
- `→ home` link

### `content/talks.html` — REMOVED.

(If talks are wanted later, the page can come back; the design language
will handle it without changes.)

### CV / `cv_eu/` — REMOVED.

## Error handling & edge cases

- Web fonts: `font-display: swap`. Fallback stacks: `system-ui` for sans,
  `ui-monospace` for mono. No FOIT.
- Old post URLs preserved (no Hakyll route changes that affect posts).
- RSS feed (`/rss.xml`) URL unchanged.
- KaTeX opt-in per post (existing `katex:` frontmatter mechanism kept).
- Image paths `/img/2013/...`, `/img/2014/...` retained.
- `posts/*.hi` `*.o` cleanup preprocess hook kept.
- `relativizeUrls` kept for portability.
- `drafts.html` route stays but unlinked from nav (status quo).
- `cv/cv.markdown` and the `cv/` directory: **DELETE**. Single canonical
  `cv_eu/cv_eu.md` route: **REMOVED** from `app/Main.hs`.
- Tag pages (`/tags/*.html`) already wired — verify and fix if broken.
- `prefers-reduced-motion: reduce` — disable all animations.
- `prefers-color-scheme: dark` — out of scope, not styled. (Site stays
  light; dark mode is a future iteration.)

## Testing & verification

- `site-ctl build` clean. No template errors, no missing identifiers.
- `cabal build` clean (with `-Wall -Wcompat` from `cabal.project`).
- Local watch mode: `site-ctl watch`. Visual smoke-check of:
  - `_site/index.html`
  - `_site/posts.html`
  - one random post per year (2012, 2014, 2017, 2018)
  - `_site/oss.html`
  - `_site/contacts.html`
  - `_site/404.html`
  - `_site/tags/haskell.html` (verify tag pages work)
- KaTeX renders: `_site/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html`
- RSS validates (W3C feed validator) at `_site/rss.xml`.
- Diff old vs new `_site/` for unexpected route changes — post URLs and
  `/rss.xml` MUST match 1:1.
- Tag page smoke check: `/tags/haskell.html`, `/tags/compilers.html`.
- Responsive check: 360px, 768px, 1280px viewports. Nav collapses to
  wrapped pills on mobile (no hamburger at 3 items).
- Visual sanity check against the mockup in
  `.superpowers/brainstorm/*/content/hero-crazy.html`. Not pixel-perfect —
  same techniques applied, real font metrics, real content.
- Browser DevTools: no console errors, no 404s on assets, fonts load
  with `font-display: swap` (no FOIT > 100ms).

## Phasing

1. **Foundation** — verify `site-ctl build` green on a fresh checkout.
   Remove `cv_eu/cv_eu.md` and the `cv/` directory. Remove the
   `match "cv_eu/*.md"` route from `app/Main.hs`. Verify nothing else
   breaks. (No CSS or template changes yet.)
2. **CSS system** — rewrite `css/screen.css` from scratch with the
   design tokens, base typography, monogram tile, glass nav, hero layout,
   card styles, tag chips, post list, OSS page styles, contacts styles,
   404, print styles, `prefers-reduced-motion` block. Self-host
   Inter + JetBrains Mono under `css/fonts/`. (Templates still using
   old markup, so this CSS matches the *new* HTML added in step 3 — the
   site is broken between step 2 and step 3 by design, both committed
   together.)
3. **Templates** — rewrite `templates/default.html`, `templates/post.html`,
   `templates/posts.html`, `templates/postitem.html`. Add
   `templates/page.html` and `templates/page-home.html`. Remove
   `templates/cv_eu.html`. Update `templates/default.html` to use the
   new brand mark and glass nav.
4. **Content** — rewrite `content/index.html`, `content/contacts.html`,
   `content/oss.html`. Add `content/404.html`. Delete `content/talks.html`.
   Verify `app/Main.hs` doesn't reference `cv_eu` or `talks` anywhere
   after this step.
5. **Polish** — manual smoke test, Lighthouse pass, tag-page fix-if-needed,
   verify RSS, no console errors. Final visual check against the mockup.
6. **Spec housekeeping** — overwrite this file (done — this iteration
   replaces the Florentine-codex spec that lived at this same path
   earlier today).

Each phase = its own commit on `adinapoli/2026-overhaul`. Steps 2 and 3
land as a single commit (the CSS is meaningless without the templates).
Owner can choose single PR or per-phase PRs at impl time.

## Open items (resolved at impl time, not blockers for spec)

- Rusholme description, Centaur description, GHC description text — you
  may tweak at impl time. The placeholder copy in this spec is fine as
  a default.
- Centaur paper URL — supply or omit the link.
- Contacts pull quote — Lao Tzu (live site default) or swap.
- Phone number: dropped (not in live site, not requested).
- Mastodon / X: explicitly dropped per your direction.
- "What I'm building" cards' content — the three (Rusholme / Centaur /
  GHC) are locked, but copy can be adjusted.
- Whether the home page should show the `// writing` latest-posts
  preview at all (small new addition, not in the live site) — kept by
  default, easy to remove at impl.

## Out of scope

- Dark mode.
- New CMS / migration off Hakyll.
- Newsletter / subscription.
- Comments system.
- Analytics.
- New domain or hosting move.
- Search.
- New photo (use `alfredo.jpg` as-is).
- Talks page.
- CV page.
- Multilingual content.
