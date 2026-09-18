---
name: visual-explainer
description: Generate self-contained HTML pages that explain, synthesise, or design something. Use for architecture overviews, new system designs, diagrams, diff or plan reviews, project recaps, and slide decks. To render an already-written Markdown file as a readable page without changing it, use markdown-reader instead.
license: MIT
metadata:
  author: nicobailon
  version: "0.8.1"
  compatibility: "Requires a browser to view generated HTML files. Optional surf-cli for AI image generation."
---

# Visual Explainer

Generate self-contained HTML pages that explain systems, code changes, plans, data, and technical concepts. Use it when you are producing understanding: an architecture overview, a new design, a diff or plan review, a project recap, a slide deck.

## Routing

| Request | Skill |
|---|---|
| Read, preview, or open an existing `.md` file or Markdown document as a readable page | `markdown-reader` |
| Explain, synthesise, compare, or diagram content; design new architecture; review a diff or plan; recap a project | this skill |

If the input happens to be Markdown but the ask is to explain, restructure, or add diagrams, this skill is correct - say plainly that the source is being reinterpreted rather than rendered.

## Trigger and delivery rules

- Prefer an HTML page over terminal ASCII when the output is inherently visual.
- Write files to `~/.agent/diagrams/` or the explicit eval output path. Use descriptive filenames.
- Open generated pages in the browser when running normally. In Pi package installs, use `visual_explainer` with `prepare` for planning/context and `render` only after the complete HTML document exists.
- The final page must be a complete self-contained HTML document, including embedded CSS and any needed JS.

## Narrative-first hybrid default

Explanations default to a reading page rather than a grid of tiles. Start from `./templates/reader-hybrid.html` and choose the layout **per section** rather than applying one grid to the whole page. A request that genuinely is a dashboard, a single diagram, or a deck is a different medium - build that instead.

- Prose sits in a single column at `min(690px, 62ch)`. Visual regions may widen to 1040px when the content is genuinely wide. Do not widen prose to fill the screen.
- **Cards only for comparable items**: peers judged on the same attributes, so the reader can scan across them. Never wrap a single narrative, an intro, or a conclusion in a card. Prose is not a card.
- **Diagrams and sequences only when they help**: use one where a picture carries something sentences cannot - flow order, fan-out, retries, state transitions. A diagram that restates the paragraph above it is noise; drop it.
- **Searchable catalogue only when the content is a catalogue readers need to search**: same-shaped entries they will look things up in rather than read through. Otherwise no search box and no filter chips.
- **`<details>` for technical reference** a reader can skip: config, full command output, schema listings, edge cases.
- **No tables by default.** Only use a table when the user explicitly asks for one; then use a semantic `<table>` in a scroll container. Otherwise present tabular material as labeled entries that keep each value next to its column name.

Prefer fewer, better sections. Every region should answer "what does this let the reader see that the prose alone does not?"

Avoid filler UI: no legend explaining the layout, no counts or badges that restate what is already visible, no control that does nothing.

**Preserve the source while restructuring.** Reorganising material into sections is not licence to change it. Keep every supplied fact, number, name, link, and code snippet, and keep the qualifiers attached to them - status, dates, scope, ownership, "proposed", "draft", "estimated", "not yet decided". Never promote a proposal into a completed action, a target into an achieved result, or an estimate into a measurement. If something is missing from the source, leave the gap visible rather than filling it in.

## Approved reader appearance

These values come from the approved reader pages and apply to **reading and hybrid pages**. **They override the prose, font, palette, and theme guidance in `./references/*`** - those references describe generic aesthetic directions and are only advisory here. Do not substitute Google-font pairings, custom accent schemes, or OS-following themes for the values below.

Standalone dashboards, diagram-only pages, and slide decks are different media: they keep their own type scale and layout, and the 19px prose measure below does not apply to them. Match the palette and the no-web-font rule regardless.

- **Type**: 19px body, 1.5 line-height. Prose in the native serif stack `-apple-system-ui-serif, ui-serif, "Iowan Old Style", Palatino, Georgia, serif`; system sans for controls and small labels; monospace for code. No web-font downloads.
- **Measure and rhythm**: prose `min(690px, 62ch)`; wide regions up to 1040px; 20px page gutters; 8px paragraph bottom gap, 20px gap between blocks and entries, 0.3rem under headings.
- **Palette**: initial `html data-theme="night"` - bg `#131210`, surface `#1c1a17`, border `#33302a`, text `#e7e2d7`, dim `#aaa397`. Paper - bg `#faf7f0`, surface `#f4f0e6`, border `#e2dacb`, text `#211f1b`, dim `#6d665a`. Honour a saved explicit theme choice; never follow the OS setting and never pick a random initial theme.
- **Controls**: Contents in a native `<dialog>` (Close button, Escape, dismiss on link click, scroll reset and focus on open); A-/A+ over 18-28px in 1px steps from 19px; Paper/Night toggle. Quiet 44px targets with visible focus styling. Guard `localStorage` access.
- **Robustness**: anchor `scroll-margin-top`; no horizontal page overflow at 320px even at 28px text, with code keeping its own scroll; print resets every palette variable to paper and hides the chrome; with JavaScript off the page still reads and dead controls are hidden.

Reader chrome is only for reading pages. Slide decks keep their own navigation.

## Reference routing

Read only what the current output needs. The approved appearance above wins wherever these disagree.

| Need | Read |
|---|---|
| Reading page: narrow prose, wide comparison, sequence, reference regions, reader controls | `./templates/reader-hybrid.html` |
| Mermaid flowcharts, sequence, ER, state, class, C4, data flow | `./templates/mermaid-flowchart.html`, Mermaid sections in `./references/libraries.md` |
| A table the user explicitly asked for | `./templates/data-table.html` |
| Slide decks | `./templates/slide-deck.html`, `./references/slide-patterns.md` |
| CSS mechanics: overflow, collapsibles, SVG connectors, generated images | `./references/css-patterns.md` |
| Pages with 4+ major sections needing in-page nav beyond the contents dialog | `./references/responsive-nav.md` |
| Text-heavy card grids for comparable items | `./templates/architecture.html` |

## Choose the representation

| Content | Default representation |
|---|---|
| Explanation, rationale, narrative, decisions, caveats | Narrow prose |
| Comparable items on shared attributes | Wide card grid |
| Flowchart, pipeline, state machine, decision tree | Mermaid |
| Ordered interaction across participants, retries, failure paths | Mermaid sequence |
| ER/schema, class, C4, topology-focused architecture | Mermaid |
| 15+ element architecture | Hybrid: small Mermaid overview + detail cards |
| Config, command output, schema listings, edge cases | `<details>` reference region |
| Many same-shaped lookup entries | Labeled entries, with search only if readers will search them |
| Metrics, KPIs, status at a glance | CSS grid dashboard with charts/KPI tiles, at its own type scale |
| Timeline/roadmap | CSS timeline |
| Slide deck | `100dvh` slides using slide template patterns |

## Mermaid invariants

- Use `theme: 'base'` with custom `themeVariables` matching the page palette.
- For complex diagrams use ELK layout when available.
- Never use bare `<pre class="mermaid">`.
- Use the canonical `diagram-shell` pattern from `templates/mermaid-flowchart.html`: `.diagram-shell` > `.mermaid-wrap` > `.zoom-controls` + `.mermaid-viewport` > `.mermaid-canvas`.
- Every Mermaid diagram needs zoom in/out/reset/expand controls, Ctrl/Cmd+scroll zoom, drag panning, and click-to-expand.
- Prefer `flowchart TD` for complex diagrams. Use `LR` only for simple 3-4 node linear flows.
- Use `<br/>` in quoted flowchart labels. Do not use escaped `\n` labels.
- Never define page-level `.node`; Mermaid uses it internally. Use namespaced page classes such as `.ve-card`.
- For 15+ elements, do not cram everything into one Mermaid diagram. Use the hybrid overview + cards pattern.
- Diagrams live in a wide region, not in the prose column.

## Layout and style invariants

- Use semantic HTML where it helps accessibility and copy/paste: headings, lists, `<details>`, captions, and `<table>` when a table was requested.
- Use CSS custom properties for palette: `--bg`, `--surface`, `--border`, `--text`, `--dim`, plus one restrained accent for links and focus.
- Prevent overflow: `min-width: 0` on grid/flex children, `overflow-wrap: break-word` for long text, and scroll containers for wide diagrams, tables, and code.
- Do not set `display: flex` directly on `<li>` when list markers matter.
- Use borders and surface tint for separation. Reserve elevation for the one primary region, if any.
- Use animation only when it clarifies hierarchy. Respect `prefers-reduced-motion`. No continuous glow, pulse, or breathing effects on static content.

## Slide deck mode

Use slides only when explicitly requested or when a command asks for slides. Slides are a different medium, not a paginated article:

- Each slide is one viewport (`100dvh`) with no page-level scrolling.
- Use larger type, fewer objects per slide, varied compositions, and visible navigation.
- Include slide nav chrome from `slide-deck.html`: prev/next controls, slide count, keyboard navigation, and carousel dots/indicators.
- Before writing HTML, inventory the source and map every source item to slides.
- Do not drop content to fit a fixed slide count. Add slides instead.
- Use the 10 slide types from `slide-patterns.md`: Title, Section Divider, Content, Split, Diagram, Dashboard, Table, Code, Quote, Full-Bleed.

## Optional generated images

If `surf` is available, generated images may be embedded as base64 for hero banners, conceptual illustrations, or educational visuals. Skip images for data-heavy, structural, or Mermaid/CSS-suitable content. Pages must stand on typography and diagrams without images.

## Final checklist

Before delivery, verify:

- complete self-contained HTML document, written to the requested path;
- reading and hybrid pages use the approved appearance: 19px serif prose, night default, 690px measure, working Contents/A-/A+/Paper-Night controls;
- layout was chosen per section - prose is not in cards, no card grid without comparable peers, no diagram that restates its paragraph, no search box on a list nobody will search, no legend explaining the layout;
- every supplied fact, number, link, and qualifier survived the restructuring, and nothing proposed was written up as done;
- no table unless the user asked for one;
- no console errors when opened;
- no horizontal page overflow at 320px or at 28px text; code, wide tables, and diagrams keep their own scroll;
- Mermaid diagrams use `diagram-shell` with zoom/pan/expand and sit in a wide region;
- slides fit one viewport, include carousel dots, and preserve source coverage;
- the main idea is obvious in the first viewport.
