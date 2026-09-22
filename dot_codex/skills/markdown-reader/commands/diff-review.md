---
name: diff-review
description: Generate a visual diff review for code changes
---

Load the markdown-reader skill and author a diff review, then render it with `scripts/render.py`.

## Scope detection

Interpret `$@` as a branch, commit, range, PR, or `HEAD`. If no argument is given, compare the working tree against `main`/`master`.

## Data gathering before writing

Run the relevant git commands for: diff stats, name-status, changed files, line counts, public API/type/function changes, added/removed files, docs/changelog changes, tests touched, dependencies/config changes. Read changed files in full plus surrounding code paths needed to validate behavior. If reviewing committed work, read commit messages. If this session created the work, use available progress/plan notes for rationale.

## Source verification

Before writing, know and cite:

- exact changed files and line-count scope;
- each function/type/module name referenced;
- before/after behavior for important changes;
- likely coupling and test impact.

Use file paths, command outputs, or file:line evidence. Do not invent rationale or code paths.

## Required sections

1. Executive summary: intuition, problem solved, factual scope.
2. File map: the full tree, marked new/modified/deleted. Keep it compact; long trees go in the reference section at the end.
3. Architecture impact: describe it in prose. Build a linked diagram page only when the relationships are what the reader needs to see.
4. Before/after behavior for the important changes.
5. Risk review: correctness, tests, API compatibility, security/privacy, performance, maintainability.
6. Coupling map: dependencies, hidden coupling, migration/release concerns.
7. Review recommendation: merge/readiness, blockers, follow-ups.

Say "removed"/"before" and "added"/"after" in words - the reading page has no diff colours. Keep file maps and command output as labeled entries in the reference section, not tables.

Write the Markdown to `~/.agent/diagrams/<slug>-diff-review.md`, render it to `<slug>-diff-review-reader.html`, and open the page.
