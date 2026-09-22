---
name: plan-review
description: Compare an implementation plan against the current codebase
---

Load the markdown-reader skill and author a plan review, then render it with `scripts/render.py`.

## Inputs

Use `$@` as the plan path or plan text. If no path is given, ask for the plan.

## Data gathering before writing

Read the plan in full. Extract goals, assumptions, proposed files/functions/types, migrations, tests, rollout/release notes, and explicit risks. Read every referenced file, plus importers/dependents that may be affected. Use ripgrep for existing patterns, similar implementations, public API boundaries, config/schema files, and tests.

## Source verification

For each proposed change, verify whether referenced files/functions/types exist, whether current behavior matches the plan, what ripple effects are missing, and whether the proposed test coverage fits the current test style. Cite plan sections and file:line evidence.

## Required sections

1. Plan summary: problem, core idea, scope.
2. Accuracy verdict: correct, stale, risky, unsupported, missing.
3. Current architecture of the affected subsystem, and how the proposal changes it. Build a linked diagram page only when the structure is what the reader needs to see.
4. Gap and risk review: correctness, tests, API, data model, UX, security/privacy, performance, maintainability, release.
5. File-by-file review: proposed edit, current reality, recommendation - as labeled entries.
6. Better plan: concrete corrections or simplifications.
7. Decision: approve, revise, or reject with rationale.

Keep the plan's own qualifiers intact. A proposal stays a proposal.

Write the Markdown to `~/.agent/diagrams/<slug>-plan-review.md`, render it to `<slug>-plan-review-reader.html`, and open the page.
