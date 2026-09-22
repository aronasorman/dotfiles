---
name: generate-visual-plan
description: Generate a readable implementation plan
---

Load the markdown-reader skill and author an implementation plan for `$@`, then render it with `scripts/render.py`.

## Research first

Read relevant repo files before planning. Identify entry points, existing patterns, affected modules, public APIs, tests, config/schema/data model, similar features, and constraints from README/CHANGELOG/docs.

## Required sections

1. Goal and scope: what will change and what is intentionally out.
2. Current state: a short prose summary of the existing architecture.
3. Proposed design: architecture, data flow, control flow. Build a linked diagram page only when the structure is what the reader needs to see.
4. Implementation sequence: ordered phases with dependencies.
5. File map: files to create/edit/delete and why.
6. Interface and contracts: types, APIs, schemas, CLI flags, config, events.
7. Risks and decisions: correctness, tests, migration, release, UX, security/privacy, performance.
8. Test plan: unit/integration/e2e/edge cases mapped to files.
9. Acceptance checklist: observable done criteria.

Lead with the goal and the design. Keep file, test, and interface detail compact, in the reference section at the end. Everything here is proposed until it ships - keep it written that way.

Write the Markdown to `~/.agent/diagrams/<slug>-plan.md`, render it to `<slug>-plan-reader.html`, and open the page.
