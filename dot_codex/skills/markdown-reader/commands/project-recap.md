---
name: project-recap
description: Generate a visual project recap for context switching
---

Load the markdown-reader skill and author a project recap, then render it with `scripts/render.py`.

## Data gathering before writing

Read project identity files (`README`, changelog, package/build files), top-level tree, current git status, recent commits, unmerged/stale branches, TODO/FIXME in recent files, progress/todo memory if present, and key entry points/source files. Focus on what a returning developer needs to rebuild the mental model.

## Verify before writing

Cite command output or file:line evidence for project state, module/function/type names, recent activity, current blockers, and next-step claims. Do not fabricate momentum or rationale.

## Required sections

1. Project identity: what this repo is, stack, entry points.
2. Architecture snapshot: the current conceptual modules in prose. Build a linked diagram page only when the shape is too large to hold in a paragraph.
3. Recent activity: grouped narrative, not raw log.
4. Current state: uncommitted work, branches, TODOs, known blockers.
5. Mental model map: key modules, data flow, command/test/deploy paths.
6. Risks and cognitive debt: hotspots and gotchas.
7. Useful commands and files.
8. Likely next steps, based only on evidence.

Keep file maps and commands as compact labeled entries in the reference section at the end, not tables.

Write the Markdown to `~/.agent/diagrams/<slug>-recap.md`, render it to `<slug>-recap-reader.html`, and open the page.
