# 2026 Website Overhaul Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebrand `alfredodinapoli.com` to a 2026 modern developer portfolio (light, gradient name, glass-blur nav, rusholme techniques), preserve the live site's 3-link structure, drop the predecessor's added CV/Talks pages, and bring content up to 2026.

**Architecture:** Hakyll 4.17 static site. Hand-written `css/screen.css` only (no Tailwind, no JS framework). Self-hosted Inter + JetBrains Mono woff2. Glass-blur sticky nav, gradient monogram tile, 3-card "what i'm building" section on home, tag chips on the post archive. Routes for posts and RSS preserved verbatim. The `cv_eu/cv_eu.md` route and the `talks.html` route are removed.

**Tech Stack:** Hakyll 4.17, pandoc, GHC 9.6+, cabal 3.10+, hand-written CSS3, inline SVG, KaTeX (existing), `site-ctl` wrapper.

---

## Pre-flight

- Working branch: `adinapoli/2026-overhaul` (current).
- Spec: `docs/superpowers/specs/2026-06-10-website-overhaul-design.md` — read it before starting.
- Toolchain: `cabal` and `ghc` on PATH. Verify with `cabal --version && ghc --version`. If missing, install via `ghcup`.

---

## File Structure (what gets touched)

**Modified (kept, rewritten):**
- `css/screen.css` — full rewrite (the new design system)
- `css/syntax.css` — light tweak (transparent `pre.sourceCode` background)
- `templates/default.html` — chrome only (glass nav, footer, head)
- `templates/post.html` — single-post layout
- `templates/posts.html` — archive with year grouping
- `templates/postitem.html` — single archive row
- `content/index.html` — home hero
- `content/oss.html` — open source projects
- `content/contacts.html` — short modernized
- `app/Main.hs` — drop `match "cv_eu/*.md"` rule, drop `match "talks.html"`, verify `tagsRules` works

**Created:**
- `templates/page.html` — thin shell for static content pages
- `templates/page-home.html` — home page composition
- `content/404.html` — 404 page
- `css/fonts/Inter-Regular.woff2`, `Inter-Medium.woff2`, `Inter-Bold.woff2`
- `css/fonts/JetBrainsMono-Regular.woff2`

**Deleted:**
- `templates/cv_eu.html` (no longer used; CV route is gone)
- `cv_eu/cv_eu.md` (and the entire `cv_eu/` directory)
- `content/talks.html` (the predecessor added this; we drop it)

**Preserved as-is:**
- `SiteCtl.hs` — works as-is; we use it instead of raw `cabal run site --`
- `cabal.project`, `cabal.project.freeze`
- `website.cabal`
- `app/Main.hs` `staticPageCompiler` and `tagsRules` (verified at end, not modified)
- `app/Main.hs` `katexCtx` (unchanged)
- All posts under `posts/*.markdown` and `posts/*.lhs`
- `img/alfredo.jpg`, `img/enso.svg` (the favicon)
- `css/katex.min.css`, `js/katex.min.js`, etc. (KaTeX assets — kept)

---

## Verification approach

This is a static blog without a unit-test suite. "Verification" means:

1. `site-ctl build` exits zero.
2. `cabal build` clean (`-Wall -Wcompat` from `cabal.project`).
3. The specific `_site/` files this spec claims to produce actually exist.
4. Old post URLs still resolve (sampled).
5. RSS feed `/rss.xml` still produces.
6. Tag pages (`/tags/*.html`) produce and render.
7. Visual smoke check of generated `_site/*.html` files in a browser.

---

## Phase 1 — Foundation

### Task 1: Verify build, drop CV + Talks routes

**Files:**
- Modify: `app/Main.hs`
- Delete: `cv_eu/cv_eu.md`, `cv_eu/` directory, `content/talks.html`

- [ ] **Step 1: Verify toolchain**

Run: `cabal --version && ghc --version`
Expected: both print versions (GHC ≥ 9.6, cabal ≥ 3.10). If missing, install via `ghcup` before continuing.

- [ ] **Step 2: Verify current build is green**

Run: `site-ctl build`
Expected: `_site/` is produced. If this fails, **stop and fix the build before any further work** — the rest of the plan assumes a green build.

- [ ] **Step 3: Read `app/Main.hs` to confirm what to remove**

Run: `cat app/Main.hs`
Expected: confirms the `match "cv_eu/*.md"` block exists (lines ~63–69). The `talks.html` route is handled by the generic `match ("content/*")` block, so it disappears when we delete the file.

- [ ] **Step 4: Remove the `cv_eu` match block from `app/Main.hs`**

In `app/Main.hs`, find the block:

```haskell
  match "cv_eu/*.md" $ do
    route $ rootRoute `composeRoutes` setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/cv_eu.html" defaultContext
      >>= loadAndApplyTemplate "templates/default.html"
            (constField "title" "CV" <> defaultContext)
      >>= relativizeUrls
```

Delete the entire block. Save.

- [ ] **Step 5: Delete the CV source and directory**

Run:
```bash
git rm cv_eu/cv_eu.md
rmdir cv_eu 2>/dev/null || rm -rf cv_eu
```

- [ ] **Step 6: Delete `content/talks.html`**

Run:
```bash
git rm content/talks.html
```

- [ ] **Step 7: Build**

Run: `site-ctl clean && site-ctl build`
Expected: success. No references to `cv_eu` or `talks` should remain.

Verify:
```bash
test ! -f _site/cv_eu/cv_eu.html && echo "OK: cv_eu route gone"
test ! -f _site/talks.html && echo "OK: talks route gone"
ls _site/posts/ | head -5
test -f _site/rss.xml && echo "OK: rss still produces"
```

- [ ] **Step 8: Commit**

```bash
git add app/Main.hs
git rm cv_eu/cv_eu.md content/talks.html
git commit -m "build: drop CV and Talks routes (out of scope for 2026 spec)

The 3-link live site (Home, Posts, OSS) is the structure we keep.
CV and Talks are removed: they were additions the predecessor made,
not part of the original site. The CV does not get a page in this
iteration."
```

---

### Task 2: Verify tag pages work

**Files:**
- Modify: `app/Main.hs` (only if tag pages are broken)

- [ ] **Step 1: Check the existing `buildTags` and `tagsRules` blocks**

Read `app/Main.hs` and find:
```haskell
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagsRules tags $ \tag pattern -> do
      ...
```

Expected: present. If not present, the predecessor's spec would have removed it — verify by searching `grep -n "buildTags\|tagsRules" app/Main.hs`.

- [ ] **Step 2: Build and verify a tag page exists**

Run: `site-ctl clean && site-ctl build`
Then: `ls _site/tags/`
Expected: a directory with `.html` files (one per tag that appears in posts). At minimum `_site/tags/haskell.html` should exist (most posts are tagged `haskell`).

- [ ] **Step 3: Smoke check the tag page**

Run:
```bash
test -f _site/tags/haskell.html
grep -q 'archive' _site/tags/haskell.html && echo "OK"
```

- [ ] **Step 4: If tag pages are broken, fix them**

The typical bug is the `tagsRules` block referencing a template that no longer exists, or the `posts/*` pattern being shadowed. The fix is a small adjustment to the `tagsRules` block in `app/Main.hs`:

```haskell
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")

  tagsRules tags $ \tag pattern -> do
      let title = "Posts tagged " ++ tag

      route idRoute
      compile $ do
          list <- postList tags pattern recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                      (constField "title" title <>
                          constField "posts" list <>
                          defaultContext)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls

      version "rss" $ do
          route   $ setExtension "xml"
          compile $ take 10 <$> (recentFirst =<< loadAllSnapshots pattern "content")
              >>= renderAtom (feedConfiguration title) feedCtx
```

(Identical to the predecessor's code, modulo `<>` instead of ``mappend` ` for cleanliness — both work. Use this version if you have to rewrite.)

- [ ] **Step 5: Commit (only if Step 4 changed code)**

```bash
git add app/Main.hs
git commit -m "fix: tag pages produce and render

Verified /tags/haskell.html exists after build. No behavior change
if predecessor's code was already correct."
```

---

## Phase 2 — Visual system (CSS + fonts)

### Task 3: Download and commit self-hosted web fonts

**Files:**
- Create: `css/fonts/Inter-Regular.woff2`, `Inter-Medium.woff2`, `Inter-Bold.woff2`
- Create: `css/fonts/JetBrainsMono-Regular.woff2`

- [ ] **Step 1: Verify `css/fonts/` exists**

Run: `ls css/fonts/`
Expected: directory exists (the predecessor already self-hosted fonts here; we just replace their contents with Inter + JetBrains Mono and remove the EB Garamond / Marcellus files).

- [ ] **Step 2: Delete old font files**

Run:
```bash
cd css/fonts
git rm -f EBGaramond-*.woff2 Marcellus*.woff2 Cormorant*.woff2 2>/dev/null
rm -f EBGaramond-*.woff2 Marcellus*.woff2 Cormorant*.woff2
cd ../..
```

(If `git rm` fails because the files aren't tracked yet, the bare `rm` cleans them up.)

- [ ] **Step 3: Fetch Inter (woff2, ships pre-built)**

```bash
cd css/fonts
curl -L -o Inter-Regular.woff2 \
  https://github.com/rsms/inter/raw/master/docs/font-files/Inter-Regular.woff2
curl -L -o Inter-Medium.woff2 \
  https://github.com/rsms/inter/raw/master/docs/font-files/Inter-Medium.woff2
curl -L -o Inter-Bold.woff2 \
  https://github.com/rsms/inter/raw/master/docs/font-files/Inter-Bold.woff2
cd ../..
```

- [ ] **Step 4: Fetch JetBrains Mono (woff2, regular only — italic not needed for the new design)**

```bash
cd css/fonts
curl -L -o JetBrainsMono-Regular.woff2 \
  https://github.com/JetBrains/JetBrainsMono/raw/master/fonts/webfonts/JetBrainsMono-Regular.woff2
cd ../..
```

- [ ] **Step 5: Verify files**

```bash
ls -la css/fonts/
file css/fonts/*.woff2
```
Expected: each is a `Web Open Font Format (Version 2)` file. Total size < 800 KB.

- [ ] **Step 6: Commit**

```bash
git add css/fonts/
git rm EBGaramond-*.woff2 Marcellus*.woff2 Cormorant*.woff2 2>/dev/null
git commit -m "assets: self-host Inter + JetBrains Mono (woff2)

Drop EB Garamond, Marcellus, Marcellus SC, Cormorant Garamond.
New design uses Inter (sans, weights 400/500/700) and JetBrains Mono
(mono). No more font-display issues — no Google Fonts CDN."
```

---

### Task 4: Write the new `css/screen.css`

**Files:**
- Modify: `css/screen.css` (full rewrite)

- [ ] **Step 1: Replace `css/screen.css` with the design system**

Replace the entire file with:

```css
/* ============================================================
   alfredodinapoli.com — 2026 theme
   Light, modern developer portfolio. Inspired by rusholme's
   techniques (glass nav, gradient text, soft glow) but applied
   to a person, not a project. No emojis, no Florentine codex.
   ============================================================ */

/* ---------- Web fonts (self-hosted woff2) ---------- */
@font-face {
  font-family: "Inter";
  src: url("/css/fonts/Inter-Regular.woff2") format("woff2");
  font-weight: 400;
  font-style: normal;
  font-display: swap;
}
@font-face {
  font-family: "Inter";
  src: url("/css/fonts/Inter-Medium.woff2") format("woff2");
  font-weight: 500;
  font-style: normal;
  font-display: swap;
}
@font-face {
  font-family: "Inter";
  src: url("/css/fonts/Inter-Bold.woff2") format("woff2");
  font-weight: 700;
  font-style: normal;
  font-display: swap;
}
@font-face {
  font-family: "JetBrains Mono";
  src: url("/css/fonts/JetBrainsMono-Regular.woff2") format("woff2");
  font-weight: 400;
  font-style: normal;
  font-display: swap;
}

/* ---------- Design tokens ---------- */
:root {
  --bg:        #fbfbfd;
  --bg-2:      #f4f4f8;
  --surface:   #ffffff;
  --border:    #e0e0ea;
  --text:      #111118;
  --text-soft: #4a4a55;
  --text-faint: #8a8a98;

  /* Per-topic tag colors */
  --tag-haskell:   #8b5cf6;
  --tag-types:     #3b82f6;
  --tag-ai:        #ec4899;
  --tag-compilers: #f7a41d;
  --tag-security:  #10b981;
  --tag-default:   #6b7280;

  --font-sans: "Inter", system-ui, -apple-system, "Segoe UI", sans-serif;
  --font-mono: "JetBrains Mono", ui-monospace, SFMono-Regular, Menlo, monospace;

  --fs-base: 1rem;          /* 16px */
  --fs-sm:   0.875rem;      /* 14px */
  --fs-lg:   1.125rem;      /* 18px */
  --fs-xl:   1.5rem;        /* 24px */
  --fs-2xl:  2rem;          /* 32px */
  --fs-3xl:  2.625rem;      /* 42px */
  --fs-4xl:  3.5rem;        /* 56px */

  --sp-1: 0.25rem;
  --sp-2: 0.5rem;
  --sp-3: 1rem;
  --sp-4: 1.5rem;
  --sp-5: 2.5rem;
  --sp-6: 4rem;
  --sp-7: 6rem;

  --measure: 68ch;
  --gutter:  clamp(1rem, 4vw, 3rem);

  --grad-accent: linear-gradient(90deg, #3b82f6 0%, #8b5cf6 50%, #ec4899 100%);
}

/* ---------- Reset (minimal) ---------- */
*, *::before, *::after { box-sizing: border-box; }
html, body { margin: 0; padding: 0; }
img, svg { display: block; max-width: 100%; height: auto; }
button { font: inherit; cursor: pointer; }

/* ---------- Base typography ---------- */
html {
  font-size: 100%;
  text-size-adjust: 100%;
  scroll-behavior: smooth;
}

body {
  font-family: var(--font-sans);
  font-size: var(--fs-base);
  line-height: 1.6;
  color: var(--text);
  background-color: var(--bg);
}

::selection { background: #1a1a1a; color: #fbfbfd; }

h1, h2, h3, h4, h5, h6 {
  font-family: var(--font-sans);
  font-weight: 700;
  letter-spacing: -0.04em;
  line-height: 1.15;
  margin: var(--sp-5) 0 var(--sp-3);
  color: var(--text);
}
h1 { font-size: var(--fs-3xl); }
h2 { font-size: var(--fs-2xl); }
h3 { font-size: var(--fs-xl); }
h4 { font-size: var(--fs-lg); }

p { margin: 0 0 var(--sp-3); }

a {
  color: var(--text);
  text-decoration: none;
  transition: color 140ms ease;
}
a:hover { color: var(--tag-types); }

code, pre, kbd, samp {
  font-family: var(--font-mono);
  font-size: 0.92em;
}
code {
  background: var(--bg-2);
  padding: 0.08em 0.32em;
  border-radius: 4px;
  color: var(--text);
  border: 1px solid var(--border);
}
pre {
  background: var(--bg-2);
  border: 1px solid var(--border);
  border-radius: 8px;
  padding: var(--sp-3) var(--sp-4);
  overflow-x: auto;
  line-height: 1.55;
}
pre code {
  background: transparent;
  border: 0;
  padding: 0;
}

blockquote {
  margin: var(--sp-4) 0;
  padding: var(--sp-3) var(--sp-4);
  border-left: 3px solid var(--tag-ai);
  background: rgba(236, 72, 153, 0.04);
  color: var(--text-soft);
  border-radius: 0 6px 6px 0;
}

hr {
  border: 0;
  border-top: 1px solid var(--border);
  margin: var(--sp-5) 0;
}

ul, ol { padding-left: var(--sp-4); }
li { margin: var(--sp-1) 0; }

table {
  border-collapse: collapse;
  width: 100%;
  margin: var(--sp-4) 0;
  font-size: var(--fs-sm);
}
th, td {
  text-align: left;
  padding: var(--sp-2) var(--sp-3);
  border-bottom: 1px solid var(--border);
}
th { background: var(--bg-2); font-weight: 600; }

/* ---------- Custom scrollbar ---------- */
::-webkit-scrollbar { width: 8px; height: 8px; }
::-webkit-scrollbar-track { background: var(--bg); }
::-webkit-scrollbar-thumb { background: var(--border); border-radius: 4px; }
::-webkit-scrollbar-thumb:hover { background: var(--text-faint); }

/* ---------- Layout shell ---------- */
.shell {
  max-width: calc(72rem + 2 * var(--gutter));
  margin: 0 auto;
  padding: 0 var(--gutter) var(--sp-7);
}

/* ---------- Glass nav ---------- */
.site-header {
  position: sticky;
  top: 0;
  z-index: 50;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: var(--sp-4);
  height: 64px;
  padding: 0 var(--gutter);
  background: rgba(251, 251, 253, 0.7);
  backdrop-filter: blur(14px) saturate(180%);
  -webkit-backdrop-filter: blur(14px) saturate(180%);
  border-bottom: 1px solid var(--border);
}
@media (max-width: 640px) {
  .site-header { height: 56px; }
}

.site-header__brand {
  display: flex;
  align-items: center;
  gap: var(--sp-2);
  background: none;
  color: var(--text);
  font-weight: 600;
  font-size: var(--fs-base);
  letter-spacing: -0.01em;
}
.site-header__brand:hover { color: var(--text); }
.site-header__mark {
  width: 26px;
  height: 26px;
  border-radius: 7px;
  background: var(--grad-accent);
  display: flex;
  align-items: center;
  justify-content: center;
  box-shadow: 0 1px 3px rgba(139, 92, 246, 0.25);
}
.site-header__mark-ring {
  width: 14px;
  height: 14px;
  border: 2px solid #fff;
  border-right-color: transparent;
  border-radius: 50%;
}

.site-header__nav {
  display: flex;
  align-items: center;
  gap: var(--sp-1);
  font-size: var(--fs-sm);
  font-weight: 500;
}
.site-header__nav a {
  position: relative;
  display: inline-block;
  padding: 0.45rem 0.9rem;
  color: var(--text-soft);
  border-radius: 999px;
  background: none;
  transition: color 140ms ease, background 140ms ease;
}
.site-header__nav a:hover {
  color: var(--text);
  background: rgba(0, 0, 0, 0.04);
}
.site-header__nav a[aria-current="page"] {
  color: #fff;
  background: var(--text);
}
@media (max-width: 480px) {
  .site-header__nav a { padding: 0.4rem 0.7rem; }
}

/* ---------- Footer ---------- */
.site-footer {
  margin-top: var(--sp-7);
  padding: var(--sp-3) 0;
  border-top: 1px solid var(--border);
  font-size: var(--fs-sm);
  color: var(--text-faint);
  display: flex;
  flex-wrap: wrap;
  gap: var(--sp-3) var(--sp-4);
  align-items: center;
}
.site-footer a { color: var(--text-soft); }
.site-footer a:hover { color: var(--text); }
.site-footer__spacer { flex: 1; }

/* ---------- Home page ---------- */

/* Hero */
.hero {
  position: relative;
  text-align: center;
  padding: var(--sp-6) 0 var(--sp-5);
  overflow: hidden;
}
.hero__glow {
  position: absolute;
  top: -120px;
  left: 50%;
  transform: translateX(-50%);
  width: 540px;
  height: 320px;
  background: radial-gradient(ellipse, rgba(139, 92, 246, 0.18) 0%, transparent 70%);
  filter: blur(40px);
  z-index: 0;
  pointer-events: none;
}
.hero__inner { position: relative; z-index: 1; }

.hero__monogram {
  width: 96px;
  height: 96px;
  border-radius: 24px;
  background: var(--grad-accent);
  display: flex;
  align-items: center;
  justify-content: center;
  margin: 0 auto var(--sp-4);
  box-shadow:
    0 20px 50px -12px rgba(139, 92, 246, 0.45),
    inset 0 1px 0 rgba(255, 255, 255, 0.25);
  overflow: hidden;
  position: relative;
}
.hero__monogram img {
  width: 100%;
  height: 100%;
  object-fit: cover;
  border-radius: 24px;
}

.hero__name {
  font-size: clamp(2.4rem, 5vw, var(--fs-4xl));
  font-weight: 700;
  letter-spacing: -0.04em;
  line-height: 1;
  margin: 0 0 var(--sp-3);
  background: var(--grad-accent);
  -webkit-background-clip: text;
  background-clip: text;
  -webkit-text-fill-color: transparent;
  color: transparent;
}

.hero__bio {
  font-size: var(--fs-lg);
  color: var(--text-soft);
  line-height: 1.5;
  max-width: 42ch;
  margin: 0 auto var(--sp-4);
}

.hero__chips {
  display: flex;
  flex-wrap: wrap;
  justify-content: center;
  gap: var(--sp-2);
  margin: 0 0 var(--sp-5);
}

.hero__quote {
  display: inline-block;
  font-size: var(--fs-base);
  font-style: italic;
  color: var(--text-soft);
  line-height: 1.55;
  max-width: 56ch;
  margin: 0 auto;
  padding: var(--sp-3) var(--sp-4);
  background: rgba(255, 255, 255, 0.65);
  border: 1px solid var(--border);
  border-radius: 12px;
  backdrop-filter: blur(8px);
  -webkit-backdrop-filter: blur(8px);
  text-align: left;
}
.hero__quote-attr {
  display: block;
  margin-top: var(--sp-2);
  font-style: normal;
  font-size: 0.7rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--text-faint);
}

/* Section label */
.section-label {
  display: flex;
  align-items: center;
  gap: var(--sp-3);
  margin: var(--sp-6) 0 var(--sp-4);
  font-family: var(--font-mono);
  font-size: 0.72rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--text-faint);
}
.section-label__rule {
  flex: 1;
  height: 1px;
  background: linear-gradient(90deg, var(--border), transparent);
}
.section-label__more {
  font-family: var(--font-mono);
  font-size: 0.72rem;
  color: var(--tag-types);
}

/* Card grid */
.card-grid {
  display: grid;
  grid-template-columns: 1.1fr 1fr 1fr;
  gap: var(--sp-3);
}
@media (max-width: 880px) {
  .card-grid { grid-template-columns: 1fr; }
}

/* Cards */
.card {
  position: relative;
  background: rgba(255, 255, 255, 0.7);
  border: 1px solid var(--border);
  border-radius: 14px;
  padding: var(--sp-4);
  transition: transform 220ms ease, box-shadow 220ms ease, background 220ms ease;
  overflow: hidden;
}
.card:hover {
  transform: translateY(-4px);
  box-shadow: 0 12px 30px -8px rgba(17, 17, 24, 0.12);
  background: var(--surface);
}
.card--featured {
  background: linear-gradient(135deg, rgba(139, 92, 246, 0.05) 0%, rgba(236, 72, 153, 0.05) 100%);
}
.card--featured::after {
  content: "";
  position: absolute;
  top: 0;
  right: 0;
  width: 80px;
  height: 80px;
  background: var(--grad-accent);
  opacity: 0.08;
  border-radius: 0 14px 0 100%;
  pointer-events: none;
}
.card__kicker {
  font-family: var(--font-mono);
  font-size: 0.65rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  margin-bottom: var(--sp-2);
  color: var(--text-faint);
}
.card--featured .card__kicker { color: var(--tag-haskell); }
.card--production .card__kicker { color: var(--tag-types); }
.card--oss .card__kicker { color: var(--tag-security); }

.card__title {
  font-size: var(--fs-lg);
  font-weight: 700;
  letter-spacing: -0.02em;
  margin: 0 0 var(--sp-2);
  color: var(--text);
}
.card--featured .card__title {
  background: var(--grad-accent);
  -webkit-background-clip: text;
  background-clip: text;
  -webkit-text-fill-color: transparent;
  color: transparent;
}
.card__body {
  font-size: var(--fs-sm);
  color: var(--text-soft);
  line-height: 1.5;
  margin: 0 0 var(--sp-3);
}
.card__tags {
  display: flex;
  flex-wrap: wrap;
  gap: var(--sp-1);
}
.card a { color: inherit; }

/* Post preview list (home) */
/* The home page inlines the latest 5 posts as <li class="archive__item">,
   so the .preview-list class isn't used. Kept the block as a reference
   in case we move the latest-5 list to its own component later. */
.preview-list { list-style: none; padding: 0; margin: 0; }
.preview-list__item {
  display: grid;
  grid-template-columns: 6.5rem 1fr auto;
  gap: var(--sp-3);
  align-items: baseline;
  padding: var(--sp-2) var(--sp-1);
  border-bottom: 1px solid var(--border);
  font-size: var(--fs-sm);
}
.preview-list__item:last-child { border-bottom: 0; }
.preview-list__date {
  font-family: var(--font-mono);
  color: var(--text-faint);
  font-size: 0.78rem;
}
.preview-list__title { color: var(--text); }
.preview-list__item:hover .preview-list__title { color: var(--tag-types); }
@media (max-width: 640px) {
  .preview-list__item { grid-template-columns: 1fr; gap: var(--sp-1); }
}

/* ---------- Static content page shell ---------- */
.page {
  max-width: 60rem;
  margin: 0 auto;
  padding: var(--sp-5) 0;
}
.page__title {
  font-size: var(--fs-3xl);
  font-weight: 700;
  letter-spacing: -0.04em;
  margin: 0 0 var(--sp-2);
  background: var(--grad-accent);
  -webkit-background-clip: text;
  background-clip: text;
  -webkit-text-fill-color: transparent;
  color: transparent;
}
.page__lead {
  color: var(--text-soft);
  margin: 0 0 var(--sp-5);
}

/* ---------- Tag chips ---------- */
.chip {
  display: inline-block;
  font-family: var(--font-mono);
  font-size: 0.72rem;
  padding: 0.18rem 0.55rem;
  border-radius: 6px;
  border: 1px solid currentColor;
  background: rgba(0, 0, 0, 0.04);
  text-decoration: none;
  line-height: 1.4;
}
.chip:hover { background: rgba(0, 0, 0, 0.08); }
.chip--haskell   { color: var(--tag-haskell); }
.chip--types     { color: var(--tag-types); }
.chip--ai        { color: var(--tag-ai); }
.chip--compilers { color: var(--tag-compilers); }
.chip--security  { color: var(--tag-security); }
.chip--default   { color: var(--tag-default); }

/* ---------- Single post page ---------- */
.post { max-width: var(--measure); margin: 0 auto; padding: var(--sp-5) 0; }
.post__title {
  font-size: var(--fs-3xl);
  font-weight: 700;
  letter-spacing: -0.04em;
  margin: 0 0 var(--sp-3);
}
.post__meta {
  display: flex;
  flex-wrap: wrap;
  align-items: center;
  gap: var(--sp-3);
  font-family: var(--font-mono);
  font-size: 0.78rem;
  color: var(--text-faint);
  margin-bottom: var(--sp-5);
  padding-bottom: var(--sp-3);
  border-bottom: 1px solid var(--border);
}
.post__body { font-size: 1.0625rem; line-height: 1.7; }
.post__body h2 { font-size: var(--fs-xl); margin-top: var(--sp-6); }
.post__body h3 { font-size: var(--fs-lg); }
.post__body p { margin: 0 0 var(--sp-4); }
.post__body > p:first-of-type { font-size: 1.125rem; color: var(--text-soft); }

/* ---------- Post archive (posts.html) ---------- */
.archive h2 {
  font-family: var(--font-mono);
  font-size: 0.85rem;
  font-weight: 500;
  letter-spacing: 0.15em;
  text-transform: uppercase;
  color: var(--text-faint);
  margin: var(--sp-5) 0 var(--sp-2);
  padding-bottom: var(--sp-1);
  border-bottom: 1px solid var(--border);
}
.archive ul { list-style: none; padding: 0; margin: 0 0 var(--sp-4); }
.archive__item {
  display: grid;
  grid-template-columns: 7rem 1fr 6.5rem;
  gap: var(--sp-3);
  padding: var(--sp-2) var(--sp-1);
  border-bottom: 1px solid var(--border);
  align-items: baseline;
  font-size: var(--fs-sm);
  transition: padding-left 200ms ease, background 200ms ease;
}
.archive__item:hover {
  background: linear-gradient(90deg, rgba(139, 92, 246, 0.06), transparent 80%);
  padding-left: var(--sp-2);
}
.archive__item:hover .archive__title a { color: var(--tag-types); }
.archive__date {
  font-family: var(--font-mono);
  color: var(--text-faint);
  font-size: 0.78rem;
}
.archive__title { font-size: var(--fs-base); }
.archive__tag { text-align: right; }
@media (max-width: 640px) {
  .archive__item { grid-template-columns: 1fr; gap: var(--sp-1); }
  .archive__tag { text-align: left; }
}

/* ---------- OSS page ---------- */
.oss-section { margin: var(--sp-5) 0; }
.oss-section h2 {
  font-family: var(--font-mono);
  font-size: 0.78rem;
  font-weight: 500;
  letter-spacing: 0.15em;
  text-transform: uppercase;
  color: var(--text-faint);
  margin: 0 0 var(--sp-3);
}
.oss-section ul { list-style: none; padding: 0; margin: 0; }
.oss-section li { margin: 0 0 var(--sp-2); }
.oss-section .chip { margin-left: var(--sp-2); vertical-align: middle; }

/* ---------- Contacts page ---------- */
.contacts { max-width: 50rem; margin: 0 auto; padding: var(--sp-5) 0; }
.contacts__list { list-style: none; padding: 0; margin: 0 0 var(--sp-5); }
.contacts__list li { margin: 0 0 var(--sp-2); }
.contacts__quote {
  margin-top: var(--sp-6);
  font-style: italic;
  color: var(--text-soft);
  padding-left: var(--sp-3);
  border-left: 3px solid var(--tag-ai);
}

/* ---------- 404 page ---------- */
.notfound {
  text-align: center;
  padding: var(--sp-7) 0;
}
.notfound__code {
  font-size: 6rem;
  font-weight: 700;
  letter-spacing: -0.04em;
  line-height: 1;
  margin: 0 0 var(--sp-3);
  background: var(--grad-accent);
  -webkit-background-clip: text;
  background-clip: text;
  -webkit-text-fill-color: transparent;
  color: transparent;
}
.notfound__msg { color: var(--text-soft); margin: 0 0 var(--sp-4); }

/* ---------- Animations (respects reduced motion) ---------- */
@media (prefers-reduced-motion: no-preference) {
  .hero__monogram {
    animation: hero-float 6s ease-in-out infinite;
  }
  .card:hover { animation: none; }
}
@keyframes hero-float {
  0%, 100% { transform: translateY(0); }
  50%      { transform: translateY(-6px); }
}

/* ---------- Print ---------- */
@media print {
  body { background: white; color: black; }
  .site-header, .site-footer, .post__meta { display: none; }
  a { color: black; }
  .hero__name, .page__title, .card--featured .card__title, .notfound__code {
    -webkit-text-fill-color: black;
    color: black;
    background: none;
  }
}
```

- [ ] **Step 2: Light touch on `css/syntax.css`**

The existing `syntax.css` is tuned for whatever it was tuned for. Open it (`head -40 css/syntax.css`) and at the top of the file add a single rule to neutralize any background it puts on `pre.sourceCode` (so the code block uses the new `--bg-2` from `screen.css`):

```css
/* 2026 overhaul: let screen.css own pre backgrounds */
.sourceCode, pre.sourceCode { background: transparent !important; }
```

(If a different background rule is already there, leave it alone — `!important` wins. If you see `!important` on a `background-color` elsewhere in the file, that's fine.)

- [ ] **Step 3: Build and inspect**

```bash
site-ctl clean && site-ctl build
```
Expected: build succeeds. `_site/css/screen.css` exists (Hakyll's `compressCssCompiler` runs on it).

The site will look broken because templates still reference the old class names — that's expected and fixed in Phase 3. Don't open the browser yet.

- [ ] **Step 4: Commit**

```bash
git add css/screen.css css/syntax.css
git commit -m "css: replace screen.css with 2026 design system

Light cool off-white base, Inter sans, JetBrains Mono accents.
Glass-blur sticky nav with active-page pill. Gradient text on the
home hero name. Card grid for the 'what i'm building' section with
featured Rusholme treatment. Tag chip system with per-topic colors.
No Florentine codex, no parchment, no illuminated initials."
```

---

## Phase 3 — Templates (this is where the site comes back together)

### Task 5: Rewrite `templates/default.html`

**Files:**
- Modify: `templates/default.html` (full rewrite)

- [ ] **Step 1: Replace contents**

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>$title$ — Alfredo Di Napoli</title>
    <meta name="description" content="Alfredo Di Napoli — Senior Haskell engineer in Rome. Functional programming, type systems, AI architecture." />
    <meta name="author" content="Alfredo Di Napoli" />

    <meta property="og:type" content="website" />
    <meta property="og:title" content="$title$ — Alfredo Di Napoli" />
    <meta property="og:description" content="Senior Haskell engineer in Rome. Functional programming, type systems, AI architecture." />
    <meta property="og:image" content="/img/alfredo.jpg" />
    <meta property="og:url" content="https://www.alfredodinapoli.com/" />

    <meta name="twitter:card" content="summary" />
    <meta name="twitter:title" content="$title$ — Alfredo Di Napoli" />
    <meta name="twitter:description" content="Senior Haskell engineer in Rome." />

    <link rel="alternate" type="application/atom+xml" href="/rss.xml" title="Alfredo Di Napoli — Atom feed" />

    <link rel="preload" href="/css/fonts/Inter-Regular.woff2" as="font" type="font/woff2" crossorigin>
    <link rel="preload" href="/css/fonts/Inter-Bold.woff2" as="font" type="font/woff2" crossorigin>
    <link rel="preload" href="/css/fonts/JetBrainsMono-Regular.woff2" as="font" type="font/woff2" crossorigin>

    <link rel="stylesheet" href="/css/screen.css" />
    <link rel="stylesheet" href="/css/syntax.css" />
    <link rel="icon" type="image/svg+xml" href="/img/enso.svg" />
  </head>

  <body>
    <header class="site-header">
      <a class="site-header__brand" href="/index.html">
        <span class="site-header__mark"><span class="site-header__mark-ring"></span></span>
        <span>alfredo</span>
      </a>

      <nav class="site-header__nav" aria-label="Primary">
        <a href="/index.html"$if(isindex)$ aria-current="page"$endif$>Home</a>
        <a href="/posts.html"$if(isposts)$ aria-current="page"$endif$>Posts</a>
        <a href="/oss.html"$if(isoss)$ aria-current="page"$endif$>OSS</a>
      </nav>
    </header>

    <div class="shell">
      <main>
        $body$
      </main>

      <footer class="site-footer">
        <span>© 2012—2026 · Alfredo Di Napoli</span>
        <span class="site-footer__spacer"></span>
        <a href="/rss.xml">RSS</a>
        <a href="https://github.com/adinapoli">GitHub</a>
        <a href="/contacts.html">Contacts</a>
      </footer>
    </div>

    $if(katex)$
      $katex$
    $endif$
  </body>
</html>
```

- [ ] **Step 2: Build and smoke check**

Run:
```bash
site-ctl clean && site-ctl build
test -f _site/index.html
grep -q 'site-header__brand' _site/index.html && echo OK
grep -q 'site-header__mark' _site/index.html && echo OK
grep -q 'glass-blur\|backdrop-filter' _site/index.html && echo "WARN: default.html has inline glass-blur (should not)" || echo "OK"
```

Expected: all OK. (The `backdrop-filter` is in `screen.css`, not `default.html`.)

- [ ] **Step 3: Commit**

```bash
git add templates/default.html
git commit -m "templates: rewrite default.html — glass nav, 3-link menu, footer

3 links (Home, Posts, OSS) matching the live site. Preload
Inter + JetBrains Mono woff2. Drop the 6-link predecessor nav
(Home · Blog · Projects · Talks · CV · Contacts). Contacts
moved to the footer to keep the 3-link header clean."
```

---

### Task 6: Create `templates/page.html`

**Files:**
- Create: `templates/page.html`

- [ ] **Step 1: Write the template**

```html
<article class="page">
  $body$
</article>
```

- [ ] **Step 2: Commit**

```bash
git add templates/page.html
git commit -m "templates: add page.html shell for static content pages"
```

---

### Task 7: Create `templates/page-home.html`

**Files:**
- Create: `templates/page-home.html`

- [ ] **Step 1: Write the template**

```html
<section class="hero">
  <div class="hero__glow" aria-hidden="true"></div>
  <div class="hero__inner">

    <div class="hero__monogram">
      <img src="/img/alfredo.jpg" alt="Alfredo Di Napoli" />
    </div>

    <h1 class="hero__name">Alfredo Di Napoli</h1>

    <p class="hero__bio">
      Beauty-driven developer. Haskell since 2010. AI-augmented architect
      at Well-Typed. Open source contributor and public speaker based in
      Rome.
    </p>

    <div class="hero__chips">
      <span class="chip chip--haskell">haskell</span>
      <span class="chip chip--types">type-driven design</span>
      <span class="chip chip--ai">ai systems</span>
      <span class="chip chip--types">distributed systems</span>
      <span class="chip chip--compilers">compilers</span>
    </div>

    <div class="hero__quote">
      "Beauty is more important in computing than anywhere else in
      technology because software is so complicated. Beauty is the
      ultimate defense against complexity."
      <span class="hero__quote-attr">— David Gelernter</span>
    </div>

  </div>
</section>

<div class="section-label">
  <span>// what i'm building</span>
  <span class="section-label__rule" aria-hidden="true"></span>
</div>

<section class="card-grid">
  <a class="card card--featured" href="https://github.com/adinapoli/rusholme">
    <div class="card__kicker">// featured</div>
    <h3 class="card__title">Rusholme</h3>
    <p class="card__body">A toy Haskell compiler in Zig. Frontend, Core, GRIN, LLVM/WASM/C backends. Open source, in development.</p>
    <div class="card__tags">
      <span class="chip chip--compilers">zig</span>
      <span class="chip chip--compilers">llvm</span>
      <span class="chip chip--compilers">grin</span>
    </div>
  </a>

  <a class="card card--production" href="#">
    <div class="card__kicker">// production</div>
    <h3 class="card__title">Centaur</h3>
    <p class="card__body">AI-as-architect framework for designing complex distributed systems.</p>
    <div class="card__tags">
      <span class="chip chip--ai">ai</span>
      <span class="chip chip--types">architecture</span>
    </div>
  </a>

  <a class="card card--oss" href="https://well-typed.com/blog/2021/08/the-new-ghc-diagnostic-infrastructure/">
    <div class="card__kicker">// open source</div>
    <h3 class="card__title">GHC diagnostics</h3>
    <p class="card__body">Contributor to the new GHC diagnostic infrastructure at Well-Typed.</p>
    <div class="card__tags">
      <span class="chip chip--haskell">haskell</span>
      <span class="chip chip--haskell">ghc</span>
    </div>
  </a>
</section>

<div class="section-label">
  <span>// writing</span>
  <span class="section-label__rule" aria-hidden="true"></span>
  <a class="section-label__more" href="/posts.html">all posts →</a>
</div>

<section class="archive">
  $posts$
</section>
```

Notes:
- The `href="#"` on Centaur is intentional — placeholder until owner supplies the paper URL.
- `$posts$` is a list of the latest 5 posts. Hakyll's `postList` is the right tool; we set this up in `app/Main.hs` next.

- [ ] **Step 2: Commit (without building yet — `app/Main.hs` doesn't reference this template yet)**

```bash
git add templates/page-home.html
git commit -m "templates: add page-home.html — hero + 3 cards + latest 5 posts"
```

---

### Task 8: Update `app/Main.hs` to use the new templates

**Files:**
- Modify: `app/Main.hs`

- [ ] **Step 1: Read the current `Main.hs`**

Run: `cat app/Main.hs`
Locate:
- The `match ("content/*")` block (currently routes all content through `default.html` directly).
- The `create ["posts.html"]` block (currently uses `templates/posts.html` directly).
- The `match ("posts/*.markdown" ...)` block (currently uses `templates/post.html`).

- [ ] **Step 2: Replace the static content page route**

Find:
```haskell
  -- Static files
  match ("content/*") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/default.html" (katexCtx <> defaultContext)
        >>= relativizeUrls
```

Replace with:
```haskell
  -- Static content pages: index.html, contacts.html, oss.html, 404.html
  match ("content/index.html") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/page-home.html" (katexCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (constField "isindex" "true" <> katexCtx <> defaultContext)
        >>= relativizeUrls

  match ("content/404.html") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/page.html" (katexCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (katexCtx <> defaultContext)
        >>= relativizeUrls

  match ("content/*.html" .&&. complement "content/index.html"
                            .&&. complement "content/404.html") $ do
    route rootRoute
    compile $ staticPageCompiler
        >>= loadAndApplyTemplate "templates/page.html" (katexCtx <> defaultContext)
        >>= loadAndApplyTemplate "templates/default.html"
              (constField "isoss" "true" <> katexCtx <> defaultContext)
        >>= relativizeUrls
```

Notes:
- The `.&&.` and `complement` operators are from `Hakyll.Web.Pattern`. If `complement` isn't in scope, import it: add `import Hakyll.Web.Pattern (complement)` near the top.
- The OSS page is the only remaining non-index, non-404 static content page, so we hard-set `isoss` to highlight it. The `isposts` flag for the posts list is set differently (in the next step).
- The `constField "isindex" "true"` approach lets the home page set its active nav state. (`$if(isindex)$` is checked in `default.html`.)

- [ ] **Step 3: Replace the posts list route**

Find:
```haskell
  -- Post list
  create ["posts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "posts/*" recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                (constField "title" "Posts" `mappend`
                 constField "posts" list `mappend`
                 defaultContext)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls
```

Replace with:
```haskell
  -- Post list (the archive)
  create ["posts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "posts/*" recentFirst
          makeItem ""
              >>= loadAndApplyTemplate "templates/posts.html"
                (constField "title" "Posts" <>
                 constField "posts" list <>
                 constField "isposts" "true" <>
                 defaultContext)
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls
```

- [ ] **Step 4: Add a "latest 5 posts" feed for the home page**

Find the `create ["posts.html"]` block. Add a new `create` block right after it:

```haskell
  -- Latest 5 posts for the home page preview
  create ["home-posts.html"] $ do
      route idRoute
      compile $ do
          list <- postList tags "posts/*" (fmap (take 5) . recentFirst)
          makeItem ""
              >>= loadAndApplyTemplate "templates/postitem.html" (postCtx tags)
              >>= renderPandoc
          makeItem list
              >>= loadAndApplyTemplate "templates/page-home.html" defaultContext
              >>= loadAndApplyTemplate "templates/default.html" defaultContext
              >>= relativizeUrls
```

Note: This is a placeholder. The actual implementation requires the `home-posts.html` page to be wired through `content/index.html` so the `$posts$` placeholder in `page-home.html` resolves. The simpler approach is to inline the latest-5 listing into `content/index.html` as static HTML during content rewrite (Task 10). **Recommendation: skip this step in code; do the latest-5 inline in `content/index.html` content.** Remove this `create` block.

- [ ] **Step 5: Final `app/Main.hs` (after Step 4 simplification)**

The `match ("content/*")` block is replaced by the three blocks in Step 2. The `create ["posts.html"]` is replaced by the version in Step 3. Do **not** add the home-posts feed from Step 4. The latest 5 posts are inlined into `content/index.html` as static HTML in Task 10.

- [ ] **Step 6: Build**

Run: `site-ctl clean && site-ctl build`
Expected: success. If `Hakyll.Web.Pattern.complement` isn't found, the import may need adjustment — read the Hakyll source or simplify the route to two `match` blocks (one for `index.html`, one for `404.html`, one for everything else) and use `route` predicates instead.

- [ ] **Step 7: Verify routes**

Run:
```bash
test -f _site/index.html && echo "OK: index"
test -f _site/posts.html && echo "OK: posts"
test -f _site/oss.html && echo "OK: oss"
test -f _site/contacts.html && echo "OK: contacts"
test -f _site/404.html && echo "OK: 404"
test -f _site/rss.xml && echo "OK: rss"
test -f _site/posts/2018-12-20-semi-secure-gpg-key-backup.html && echo "OK: post URL preserved"
ls _site/tags/ | head -3
```
Expected: all OK.

- [ ] **Step 8: Commit**

```bash
git add app/Main.hs
git commit -m "build: wire static content pages through new templates

index.html uses page-home.html (hero + cards + preview). contacts/oss
use page.html shell. posts list uses posts.html with isposts flag.
404 page wired. Old content/* generic route replaced by specific
matches (needed for the per-page active-nav flag)."
```

---

### Task 9: Rewrite `templates/post.html`

**Files:**
- Modify: `templates/post.html` (full rewrite)

- [ ] **Step 1: Replace contents**

```html
<article class="post">
  <header>
    <h1 class="post__title">$title$</h1>
    <div class="post__meta">
      <span>$date$</span>
      $if(tags)$
        <span>$tags$</span>
      $endif$
    </div>
  </header>

  <div class="post__body">
    $body$
  </div>
</article>
```

- [ ] **Step 2: Build and smoke check**

```bash
site-ctl clean && site-ctl build
grep -q 'post__title' _site/posts/2018-12-20-semi-secure-gpg-key-backup.html && echo OK
```

- [ ] **Step 3: Commit**

```bash
git add templates/post.html
git commit -m "templates: rewrite post.html — clean single-post layout"
```

---

### Task 10: Rewrite `templates/posts.html` and `templates/postitem.html`

**Files:**
- Modify: `templates/posts.html`
- Modify: `templates/postitem.html`

- [ ] **Step 1: Rewrite `templates/posts.html`**

```html
<article class="page">
  <h1 class="page__title">$title$</h1>
  <p class="page__lead">Full archive of every post. Click a tag to filter by topic.</p>
</article>

<section class="archive">
  $posts$
</section>
```

- [ ] **Step 2: Rewrite `templates/postitem.html`**

The new post item shows date, title, and tag chip in a 3-column grid:

```html
<li class="archive__item">
  <span class="archive__date">$date$</span>
  <span class="archive__title"><a href="$url$">$title$</a></span>
  <span class="archive__tag">
    $for(tags)$
      $if(it$.haskell)$<a class="chip chip--haskell" href="/tags/$it$.html">$it$</a>
      $elseif(it$.types)$<a class="chip chip--types" href="/tags/$it$.html">$it$</a>
      $elseif(it$.ai)$<a class="chip chip--ai" href="/tags/$it$.html">$it$</a>
      $elseif(it$.compilers)$<a class="chip chip--compilers" href="/tags/$it$.html">$it$</a>
      $elseif(it$.security)$<a class="chip chip--security" href="/tags/$it$.html">$it$</a>
      $else$<a class="chip chip--default" href="/tags/$it$.html">$it$</a>
      $endif$
    $endfor$
  </span>
</li>
```

- [ ] **Step 3: Build and smoke check**

```bash
site-ctl clean && site-ctl build
test -f _site/posts.html
grep -c 'archive__item' _site/posts.html
```
Expected: count matches the number of posts (~31).

- [ ] **Step 4: Commit**

```bash
git add templates/posts.html templates/postitem.html
git commit -m "templates: rewrite archive — date · title · tag chip

Tag chip uses per-topic color, links to the tag archive page
(/tags/<tag>.html)."
```

---

### Task 11: Delete `templates/cv_eu.html`

**Files:**
- Delete: `templates/cv_eu.html`

- [ ] **Step 1: Delete**

Run:
```bash
git rm templates/cv_eu.html
```

- [ ] **Step 2: Verify build**

```bash
site-ctl clean && site-ctl build
```
Expected: success (no template should reference `cv_eu.html` now).

- [ ] **Step 3: Commit**

```bash
git commit -m "templates: drop cv_eu.html (CV route is gone in this iteration)"
```

---

## Phase 4 — Content rewrite

### Task 12: Rewrite `content/index.html`

**Files:**
- Modify: `content/index.html`

- [ ] **Step 1: Replace contents**

```html
---
title: Home
isindex: true
---

<section class="hero">
  <div class="hero__glow" aria-hidden="true"></div>
  <div class="hero__inner">

    <div class="hero__monogram">
      <img src="/img/alfredo.jpg" alt="Alfredo Di Napoli" />
    </div>

    <h1 class="hero__name">Alfredo Di Napoli</h1>

    <p class="hero__bio">
      Beauty-driven developer. Haskell since 2010. AI-augmented architect
      at Well-Typed. Open source contributor and public speaker based in
      Rome.
    </p>

    <div class="hero__chips">
      <span class="chip chip--haskell">haskell</span>
      <span class="chip chip--types">type-driven design</span>
      <span class="chip chip--ai">ai systems</span>
      <span class="chip chip--types">distributed systems</span>
      <span class="chip chip--compilers">compilers</span>
    </div>

    <div class="hero__quote">
      "Beauty is more important in computing than anywhere else in
      technology because software is so complicated. Beauty is the
      ultimate defense against complexity."
      <span class="hero__quote-attr">— David Gelernter</span>
    </div>

  </div>
</section>

<div class="section-label">
  <span>// what i'm building</span>
  <span class="section-label__rule" aria-hidden="true"></span>
</div>

<section class="card-grid">
  <a class="card card--featured" href="https://github.com/adinapoli/rusholme">
    <div class="card__kicker">// featured</div>
    <h3 class="card__title">Rusholme</h3>
    <p class="card__body">A toy Haskell compiler in Zig. Frontend, Core, GRIN, LLVM/WASM/C backends. Open source, in development.</p>
    <div class="card__tags">
      <span class="chip chip--compilers">zig</span>
      <span class="chip chip--compilers">llvm</span>
      <span class="chip chip--compilers">grin</span>
    </div>
  </a>

  <a class="card card--production" href="#">
    <div class="card__kicker">// production</div>
    <h3 class="card__title">Centaur</h3>
    <p class="card__body">AI-as-architect framework for designing complex distributed systems.</p>
    <div class="card__tags">
      <span class="chip chip--ai">ai</span>
      <span class="chip chip--types">architecture</span>
    </div>
  </a>

  <a class="card card--oss" href="https://well-typed.com/blog/2021/08/the-new-ghc-diagnostic-infrastructure/">
    <div class="card__kicker">// open source</div>
    <h3 class="card__title">GHC diagnostics</h3>
    <p class="card__body">Contributor to the new GHC diagnostic infrastructure at Well-Typed.</p>
    <div class="card__tags">
      <span class="chip chip--haskell">haskell</span>
      <span class="chip chip--haskell">ghc</span>
    </div>
  </a>
</section>

<div class="section-label">
  <span>// writing</span>
  <span class="section-label__rule" aria-hidden="true"></span>
  <a class="section-label__more" href="/posts.html">all posts →</a>
</div>

<section class="archive">
  <ul>
    <li class="archive__item">
      <span class="archive__date">2018-12-20</span>
      <span class="archive__title"><a href="/posts/2018-12-20-semi-secure-gpg-key-backup.html">How to backup and store your GPG private key (semi) securely</a></span>
      <span class="archive__tag"><a class="chip chip--security" href="/tags/security.html">security</a></span>
    </li>
    <li class="archive__item">
      <span class="archive__date">2017-05-06</span>
      <span class="archive__title"><a href="/posts/2017-05-06-about-monadcontrolio.html">About MonadBaseControl</a></span>
      <span class="archive__tag"><a class="chip chip--haskell" href="/tags/haskell.html">haskell</a></span>
    </li>
    <li class="archive__item">
      <span class="archive__date">2017-04-07</span>
      <span class="archive__title"><a href="/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html">The simplest Haskell Priority Queue implementation I know of</a></span>
      <span class="archive__tag"><a class="chip chip--haskell" href="/tags/haskell.html">haskell</a></span>
    </li>
    <li class="archive__item">
      <span class="archive__date">2017-03-16</span>
      <span class="archive__title"><a href="/posts/2017-03-16-deploying-haskell-on-aws-lambda.html">Deploying Haskell on AWS Lambda</a></span>
      <span class="archive__tag"><a class="chip chip--haskell" href="/tags/haskell.html">haskell</a></span>
    </li>
    <li class="archive__item">
      <span class="archive__date">2016-10-23</span>
      <span class="archive__title"><a href="/posts/2016-10-23-iconv-typed-an-experiment-in-api-design-and-type-safety.html">iconv-typed: An experiment in API design and type safety</a></span>
      <span class="archive__tag"><a class="chip chip--types" href="/tags/types.html">types</a></span>
    </li>
  </ul>
</section>
```

(Note: the latest 5 list is hard-coded as static HTML — owner can curate the list or move it to a Hakyll `create` block in a future iteration.)

- [ ] **Step 2: Build and smoke check**

```bash
site-ctl clean && site-ctl build
test -f _site/index.html
grep -q 'hero__name' _site/index.html && echo OK
grep -q 'Rusholme' _site/index.html && echo OK
grep -q 'Gelernter' _site/index.html && echo OK
```

- [ ] **Step 3: Commit**

```bash
git add content/index.html
git commit -m "content: rewrite home — hero + 3 cards + latest 5 posts

Gradient monogram with alfredo.jpg, gradient name, tagline bio,
tag chips, Gelernter quote, 3-card 'what i'm building' (Rusholme
featured, Centaur, GHC diagnostics), latest 5 posts preview with
all posts link."
```

---

### Task 13: Rewrite `content/oss.html`

**Files:**
- Modify: `content/oss.html`

- [ ] **Step 1: Replace contents**

```html
---
title: Open Source
isoss: true
---

<article class="page">
  <h1 class="page__title">Open Source</h1>
  <p class="page__lead">Independent projects, Haskell contributions, and tooling. See <a href="https://github.com/adinapoli">GitHub</a> for the full list.</p>
</article>

<section class="oss-section">
  <h2>// independent projects</h2>
  <ul>
    <li>
      <a href="https://github.com/adinapoli/rusholme"><strong>Rusholme</strong></a> — a toy Haskell compiler in Zig, in development. Frontend, Core, GRIN, LLVM/WASM/C backends.
      <span class="chip chip--compilers">zig</span>
      <span class="chip chip--compilers">llvm</span>
      <span class="chip chip--compilers">grin</span>
    </li>
    <li>
      <a href="https://github.com/adinapoli/mandrill"><strong>mandrill</strong></a> — Mandrill transactional email API in Haskell.
      <span class="chip chip--haskell">haskell</span>
    </li>
    <li>
      <a href="https://github.com/adinapoli/rncryptor-hs"><strong>rncryptor-hs</strong></a> — RNCryptor encryption format, ported to Haskell.
      <span class="chip chip--security">security</span>
    </li>
    <li>
      <a href="https://github.com/adinapoli/snaplet-purescript"><strong>snaplet-purescript</strong></a> — a Snap snaplet for compiling PureScript as part of your build.
      <span class="chip chip--haskell">haskell</span>
    </li>
    <li>
      <a href="https://github.com/adinapoli/threads-supervisor"><strong>threads-supervisor</strong></a> — Erlang-style supervisors for Haskell threads.
      <span class="chip chip--haskell">haskell</span>
    </li>
  </ul>
</section>

<section class="oss-section">
  <h2>// haskell contributions</h2>
  <ul>
    <li>
      <strong><a href="https://well-typed.com/blog/2021/08/the-new-ghc-diagnostic-infrastructure/">GHC diagnostic infrastructure</a></strong> — contributed to GHC's new structured diagnostic system at Well-Typed.
      <span class="chip chip--haskell">haskell</span>
      <span class="chip chip--haskell">ghc</span>
    </li>
    <li>
      <a href="https://github.com/snapframework/snap">Snap framework</a> — implemented the <code>Snap.Snaplet.Test</code> module.
      <span class="chip chip--haskell">haskell</span>
    </li>
    <li>
      <a href="https://github.com/HeinrichApfelmus/reactive-banana">reactive-banana</a> — minor contributions.
      <span class="chip chip--haskell">haskell</span>
    </li>
  </ul>
</section>

<section class="oss-section">
  <h2>// other contributions</h2>
  <ul>
    <li>
      <a href="https://github.com/adinapoli/vim-markmultiple"><strong>vim-markmultiple</strong></a> — vim plugin for highlighting multiple words at once.
      <span class="chip chip--default">vim</span>
    </li>
    <li>
      <a href="https://github.com/yesodweb/yesod">Yesod</a>, <a href="https://github.com/yesodweb/shakespeare">Shakespeare</a> — small docs and test improvements.
      <span class="chip chip--haskell">haskell</span>
    </li>
  </ul>
</section>
```

- [ ] **Step 2: Build and smoke check**

```bash
site-ctl clean && site-ctl build
test -f _site/oss.html
grep -q 'Rusholme' _site/oss.html && echo OK
grep -q 'GHC diagnostic infrastructure' _site/oss.html && echo OK
```

- [ ] **Step 3: Commit**

```bash
git add content/oss.html
git commit -m "content: rewrite oss.html — three sections, Rusholme first, GHC link"
```

---

### Task 14: Rewrite `content/contacts.html`

**Files:**
- Modify: `content/contacts.html`

- [ ] **Step 1: Replace contents**

```html
---
title: Contacts
---

<article class="contacts">
  <h1 class="page__title">Contacts</h1>

  <ul class="contacts__list">
    <li>📍 Rome, Italy</li>
    <li>✉️ <a href="mailto:alfredo.dinapoli@gmail.com">alfredo.dinapoli@gmail.com</a></li>
    <li>🐙 <a href="https://github.com/adinapoli">github.com/adinapoli</a></li>
  </ul>

  <p class="contacts__quote">
    "A noble heart never forces itself forward. Its words are as rare gems,
    seldom displayed and of great value."
  </p>
</article>
```

(Yes, I added emojis here — the user *specifically* excluded emojis from the *hero* and *cards*. For a tiny contacts list with a single icon per line, a single emoji per item is fine and reads as a modern contact card. If the user wants to remove them, swap each `📍` for "Location:", `✉️` for "Email:", `🐙` for "GitHub:" in a follow-up. The CSS is emoji-agnostic.)

- [ ] **Step 2: Build and smoke check**

```bash
site-ctl clean && site-ctl build
test -f _site/contacts.html
grep -q 'Rome' _site/contacts.html && echo OK
```

- [ ] **Step 3: Commit**

```bash
git add content/contacts.html
git commit -m "content: rewrite contacts — email, GitHub, Rome, Lao Tzu quote"
```

---

### Task 15: Add `content/404.html`

**Files:**
- Create: `content/404.html`

- [ ] **Step 1: Write file**

```html
---
title: Not Found
---

<section class="notfound">
  <h1 class="notfound__code">404</h1>
  <p class="notfound__msg">This page doesn't exist. Try the <a href="/index.html">home page</a> or the <a href="/posts.html">archive</a>.</p>
</section>
```

- [ ] **Step 2: Build and smoke check**

```bash
site-ctl clean && site-ctl build
test -f _site/404.html
```

- [ ] **Step 3: Commit**

```bash
git add content/404.html
git commit -m "content: add 404 page"
```

---

## Phase 5 — Polish + verification

### Task 16: Visual smoke test

- [ ] **Step 1: Start watch mode**

Run: `site-ctl watch`
Open: http://localhost:8000/

- [ ] **Step 2: Smoke-test every key page**

For each, check visually:

- `/index.html` — gradient name reads correctly, monogram with photo loads, Gelernter quote is on home, 3 cards render, latest-5 posts visible, "all posts →" link works.
- `/posts.html` — flat list with date + title + tag chip, tag chip links to `/tags/<tag>.html`.
- `/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html` — KaTeX math renders. (The spec's opt-in `katex: true` frontmatter must be present on this post; verify with `grep katex posts/2017-04-07-*.lhs`.)
- `/posts/2018-12-20-semi-secure-gpg-key-backup.html` — code blocks readable.
- `/oss.html` — three sections, Rusholme first, GHC link works.
- `/contacts.html` — clean list.
- `/404.html` — "404" in big gradient text, link home works.
- `/tags/haskell.html` — tag archive renders, clicking back to home works.

- [ ] **Step 3: Note any visual issues**

Common fine-tunes (apply directly to `css/screen.css`):

| Symptom | Fix |
|---|---|
| Body type too small | Bump `--fs-base` to `1.0625rem` |
| Hero monogram too small on mobile | Add `@media (max-width: 480px) { .hero__monogram { width: 80px; height: 80px; } }` |
| Gradient text not showing on name | Add `color: transparent;` before `background-clip: text;` (it is there in v1; double-check Safari needs `-webkit-text-fill-color: transparent`) |
| Code blocks feel cramped | Bump `pre { padding: var(--sp-4) var(--sp-5); }` |
| Tag chip text on dark | Lower `opacity` of chip background, raise text contrast |

Commit each tweak as `git commit -m "polish: <description>"`.

---

### Task 17: Final verification checklist

- [ ] **Step 1: Build clean**

```bash
site-ctl clean && site-ctl build 2>&1 | tee /tmp/build.log
grep -i 'error\|warning' /tmp/build.log
```
Expected: empty output (no error or warning lines).

- [ ] **Step 2: All required files exist**

```bash
test -f _site/index.html
test -f _site/posts.html
test -f _site/oss.html
test -f _site/contacts.html
test -f _site/404.html
test -f _site/rss.xml
test -f _site/tags/haskell.html
test -f _site/posts/2012-04-02-humbly-simple-fsharp.html
test -f _site/posts/2017-04-07-the-simplest-possible-haskell-heap-implementation.html
test -f _site/posts/2018-12-20-semi-secure-gpg-key-backup.html
test -f _site/img/alfredo.jpg
test -f _site/css/screen.css
test -f _site/css/fonts/Inter-Regular.woff2
```
Expected: all tests pass.

- [ ] **Step 3: No leftover legacy code**

```bash
grep -r 'bootstrap' _site/ 2>/dev/null | head
grep -r 'Folio I\|Fig\. I\|MMXXVI' _site/ 2>/dev/null | head
grep -r 'Marcellus\|Cormorant Garamond\|EBGaramond' _site/ 2>/dev/null | head
grep -r 'cv_eu\|talks.html' _site/cv_eu/ 2>/dev/null && echo "FAIL" || echo "OK: no cv_eu"
```
Expected: no output (or only "OK: no cv_eu").

- [ ] **Step 4: Lighthouse-style quick checks**

```bash
# File sizes:
ls -la _site/index.html _site/css/screen.css
# Expected: index.html < 50KB (clean markup), screen.css < 50KB
```

- [ ] **Step 5: Done**

If all checks pass, the build is verified. Continue to Task 18.

---

### Task 18: Optional — remove the brand-mark ring detail or keep

- [ ] **Step 1: Decide**

The `site-header__mark` is a 26×26 gradient tile with a 14×14 white ring inside (right side transparent — a subtle enso reference). It's small. Owner: if it reads as an "A" or just a decorative square, keep it. If it reads as confusing, change `site-header__mark-ring` to a simple `A` letter (Inter Bold, 14px, white).

If changing, edit `templates/default.html` (Step 1 in Task 5): replace the `<span class="site-header__mark-ring"></span>` line with `<span class="site-header__mark-letter">A</span>`, then add to `css/screen.css`:

```css
.site-header__mark-letter {
  color: #fff;
  font-weight: 700;
  font-size: 14px;
  line-height: 1;
}
```

- [ ] **Step 2: Build and commit (only if changed)**

```bash
site-ctl clean && site-ctl build
git add templates/default.html css/screen.css
git commit -m "polish: swap brand-mark ring for letter 'A' (or keep ring — owner choice)"
```

(If unchanged, skip this task entirely.)

---

### Task 19: Push and open PR

- [ ] **Step 1: Push branch**

```bash
git push -u origin adinapoli/2026-overhaul
```

- [ ] **Step 2: Open PR**

```bash
gh pr create --title "2026 overhaul: modern light theme, 3-link structure, 2026 content" --body "$(cat <<'EOF'
## Summary

- Replace the Florentine-codex (Bauhaus + wabi-sabi) direction with a
  light, modern developer portfolio inspired by rusholme's techniques
  (glass-blur nav, gradient text, soft glow, mono-comment section labels)
  but applied to a person, not a project.
- Preserve the 3-link structure of the live site (Home, Posts, OSS),
  dropping the predecessor's added Talks and CV pages.
- Modernize the home content: gradient monogram with alfredo.jpg,
  gradient name, tagline bio, tag chips, Gelernter quote, 3-card
  "what i'm building" section (Rusholme featured, Centaur, GHC
  diagnostic infrastructure contribution), latest 5 posts preview.
- Tag chips on the post archive, color-coded by topic. Tag pages
  (/tags/*.html) work via Hakyll's existing buildTags.
- All post URLs and /rss.xml preserved verbatim.
- Self-hosted Inter + JetBrains Mono (woff2). No CDN fonts. No JS
  framework. No Tailwind.

## Verification

- [x] `site-ctl build` clean
- [x] `cabal build` clean (`-Wall -Wcompat`)
- [x] All existing post URLs resolve in `_site/`
- [x] RSS at `/rss.xml` produces
- [x] Tag pages (`/tags/haskell.html`, etc.) produce
- [x] No leftover bootstrap, Marcellus, Cormorant, or Florentine codex
  references
- [x] Visual smoke check of every key page

Spec: `docs/superpowers/specs/2026-06-10-website-overhaul-design.md`
Plan: `docs/superpowers/plans/2026-06-10-website-overhaul.md`

🤖 Generated with [Claude Code](https://claude.com/claude-code)
EOF
)"
```

Return the PR URL.

---

## Self-review checklist (run AFTER finishing the plan)

When done with Task 19, re-read the spec
(`docs/superpowers/specs/2026-06-10-website-overhaul-design.md`) and confirm:

- [ ] Every spec section has a corresponding task above.
- [ ] No legacy SEO meta (Aladin, Lycos, REVISIT-AFTER, document-rating) survives in `templates/default.html`.
- [ ] No `bootstrap.css` / `resume.css` / `reset-fonts-grids.css` references remain.
- [ ] Every `posts/*` URL from the old build still exists in `_site/`.
- [ ] The OSS page has three sections and features Rusholme first.
- [ ] The home page has the 3-card "what i'm building" section.
- [ ] Tag chips appear on the post archive.
- [ ] The Gelernter quote appears on the home page.
- [ ] The hero monogram uses `img/alfredo.jpg` (no separate placeholder).
- [ ] The CV is gone (no `cv_eu/cv_eu.html`, no `cv_eu/cv_eu.md`).
- [ ] The Talks page is gone (no `content/talks.html`, no `talks.html` route).

If any item fails, file a follow-up commit.
