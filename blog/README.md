# meta-springs blog

A three-part interactive series on network meta-analysis as a spring network.
The pages are **generated from Markdown** — you edit Markdown, rebuild, and serve
the static HTML locally.

---

## 1. Launch and watch

From the repository root, run one command:

```bash
bash blog/dev.sh
```

Open **http://macas.lan:8000/**. The command builds the pages, serves the blog,
and rebuilds when Markdown or animation sources change. Save your edits; the
browser refreshes automatically after a successful build. **Ctrl-C stops both serving and watching.**

The server listens on all IPv4 interfaces for LAN access. `macas.lan` must
resolve to this computer on your network.

Requires Python 3 and **pandoc** (`brew install pandoc`). If port 8000 is
already occupied, stop the earlier server or use `bash blog/dev.sh 8001`.
From inside `blog/`, use `bash dev.sh`.

---

## 2. Edit the content

Edit the Markdown in **`content/*.md`** — one file per page. Each file is
front-matter (metadata) + prose:

```
---
title:        Full page title (the <h1>)
animation:    re-tau-springs.html     # interactive widget inlined at the top
hero_title:   small label above the animation
hero_caption: one-line caption under the animation
nav_label:    short label for the "in this part" menu + series lists
blurb:        one-line summary on the hub / index lists
---

Prose here. Use $...$ and $$...$$ for math, plus normal Markdown
(**bold**, *italics*, `code`, [links](x.html), ## headings, tables).
```

Special blocks:

- **Page FAQ** — add `## FAQ` at the end of the page, with `### Question`
  headings and answers underneath. Each FAQ appears only on its own page.

- **Callout box** — `::: callout` … `:::`
- **Side-by-side code** (meta-springs vs multinma) — `::: codecompare` … (see
  `content/README.md` for the exact shape)

**Generated for you** (don't hand-write): breadcrumbs, prev/next pager, the "in
this part" menu, hub series lists, the top nav, and the inlined animation +
"open in own page" link. Page order lives in `ORDER`/`PARTS` at the top of
`tools/mdblog.py`.

> ⚠️ **Never edit `index.html` or `posts/*.html` directly** — they are generated
> and overwritten on every build. Edit `content/*.md` instead.

---

## 3. Rebuild

```bash
./rebuild.sh            # rebuild the HTML once
./rebuild.sh --watch    # auto-rebuild on every save (needs no install; polls, or uses fswatch if present)
```

`rebuild.sh` just runs `python3 tools/mdblog.py build`. Requires **pandoc**
(`brew install pandoc`).

With `bash blog/dev.sh` running, the normal loop is **edit → save → automatic browser refresh**.
The rebuild-only commands above remain available for use without a server.

---

## What lives where

Every animation includes a collapsed **Study data** panel, including on standalone
animation pages. Tables read the animation's own data through `study-data.js`;
edit the dataset in the animation rather than maintaining a separate table.
The binomial demo's table follows its editable event counts, and the grand
canonical animation lists both likelihood datasets.

| To change… | Edit… |
|---|---|
| Page prose, math, code, callouts | `content/<slug>.md` |
| An animation's behavior/data | the standalone `blog/<name>.html` (e.g. `binomial-spring.html`), then rebuild |
| Colors / fonts / spacing | `style.css` (CSS variables at the top) |
| Page order, or add/remove a page | `ORDER` / `PARTS` in `tools/mdblog.py` (+ a new `content/<slug>.md`) |

## Layout

```
blog/
├── README.md              ← this file
├── dev.sh                 ← launch server and watch sources
├── rebuild.sh             ← build shortcut ( ./rebuild.sh [--watch] )
├── style.css              ← all shared styling
├── content/               ← EDIT THESE  (Markdown source, one per page)
│   ├── README.md          ← Markdown syntax cheatsheet
│   ├── index.md
│   ├── part1.md … 1a/1b/1c/1d
│   ├── part2.md … 2a/2b/2c
│   └── part3.md … 3a
├── tools/mdblog.py        ← the generator (extract / build)
├── *.html                 ← the interactive animation widgets (standalone)
├── index.html             ← GENERATED — do not edit
└── posts/*.html           ← GENERATED — do not edit
```

## One-time re-seed (rarely needed)

`content/*.md` was seeded from the HTML once via `python3 tools/mdblog.py extract`.
You won't normally run this again — Markdown is the source of truth now, and
`extract` would overwrite your Markdown from the current HTML.
