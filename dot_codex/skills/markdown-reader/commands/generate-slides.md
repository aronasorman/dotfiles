---
name: generate-slides
description: Generate a slide deck as a self-contained HTML page
---

Load the markdown-reader skill and generate a slide deck for: $@

Slides are only for an explicit request. Before writing HTML, read `templates/slide-deck.html`, `references/slide-patterns.md`, and only the shared CSS/library sections the source needs.

Plan the deck first: inventory the source, map every item to slides, choose a narrative arc, and assign a composition to each slide. Use the 10 slide types and the nav chrome from `slide-patterns.md` and `slide-deck.html`, including carousel dots, prev/next, slide count, and keyboard controls. Keep each slide to `100dvh`; split dense content across slides rather than scrolling or dropping content.

Use visual-first slides: diagrams, charts, tables, and SVG accents when they clarify the story. Vary compositions; three centered slides in a row is a smell. Keep the approved palette and native fonts - variety comes from treatment and composition, not from swapping palettes or typefaces.

Write to `~/.agent/diagrams/` and open in the browser.
