---
name: generate-web-diagram
description: Generate a standalone HTML diagram or dashboard and open it in the browser
---

Load the markdown-reader skill and generate a standalone visual page for: $@

This command is for pages whose content *is* the visual: a diagram, a topology, a dashboard, a timeline, a table the user asked for. If the ask is really an explanation, a recap, or a review, that is a reading page - author Markdown and render it with `scripts/render.py` instead.

Use the skill's reference routing: `templates/mermaid-flowchart.html` for connected flows and topologies, `templates/architecture.html` for card grids of comparable peers and KPI tiles, `templates/data-table.html` for a requested table, `references/css-patterns.md` for layout and connector mechanics, `references/libraries.md` for Mermaid and Chart.js.

Keep the approved palette: night by default, paper as the only alternate, native fonts, no web-font downloads. Every Mermaid diagram uses the `diagram-shell` pattern with zoom, pan, and expand.

Write a complete self-contained HTML document to `~/.agent/diagrams/` with a descriptive filename and open it in the browser.
