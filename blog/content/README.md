# Editing the blog in Markdown

The blog HTML is **generated** from the Markdown files in this folder. Edit the
Markdown, then rebuild — never hand-edit the `.html` files (they get overwritten).

## Workflow

```bash
bash blog/dev.sh    # from the repository root: build, serve, and watch
```

Open http://localhost:8000/. Save your edits; the browser refreshes automatically after a successful rebuild.
Ctrl-C stops the server and watcher.

## What each `.md` file is

One file per page. Front-matter (between the `---` fences) holds the editable
metadata; the body below is the prose.

```
---
title:        Full page title (shows as the <h1>)
animation:    re-tau-springs.html      # which interactive widget to inline at the top
hero_title:   small label above the animation
hero_caption: one-line caption under the animation
nav_label:    short label for the "in this part" menu + series lists
blurb:        one-line summary shown on the hub / index lists
---

Prose goes here...
```

You can use **`$...$`** and **`$$...$$`** for math (MathJax), normal Markdown for
**bold**, *italics*, `code`, [links](x.html), `##` headings, and tables.

### Special blocks

- **Callout box:**
  ```
  ::: callout
  **Note.** text...
  :::
  ```
- **Side-by-side code (meta-springs vs multinma):**
  ```
  ::::::: codecompare
  :::: codeblock
  ::: code-label
  meta-springs (R)
  :::
  ` ``code
  library(springs)
  ` ``
  ::::
  :::: codeblock
  ::: code-label
  multinma (R)
  :::
  ` ``code
  library(multinma)
  ` ``
  ::::
  :::::::
  ```
  (Use real triple-backticks — the spaces above are only to show it in this README.)

## What is generated for you (don't hand-write it)

Breadcrumbs, the prev/next pager, the "in this part" menu, the hub series lists,
the top nav, and the inlined animation + "open in own page" link. All driven by
the page order in `tools/mdblog.py` (the `ORDER` and `PARTS` lists at the top).

## Page-specific FAQs

Add `## FAQ` at the end of a page's Markdown, then use `### Question` followed
by the answer for each item. The builder renders these as a collapsible FAQ
on that page only. Pages without this section have no FAQ.

## Animations

The interactive widgets are the standalone `.html` files in `blog/` (e.g.
`re-tau-springs.html`, `binomial-spring.html`). Edit those directly to change an
animation; `build` re-inlines the current version into every page that uses it.

## Adding / reordering pages

Edit `ORDER` and `PARTS` in `tools/mdblog.py`, add a `content/<slug>.md`, set its
`animation:` in the front-matter, and rebuild.
