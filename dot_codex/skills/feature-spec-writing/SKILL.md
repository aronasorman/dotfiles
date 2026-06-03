---
name: feature-spec-writing
description: Use when writing or materializing an implementation-ready feature spec before build, especially for Beads-tracked work that lacks a readable spec or needs scope, non-goals, architecture decisions, required behavior, and verification defined.
---

# Feature Spec Writing

Write implementation-ready feature specs that are detailed enough to guide build, review, and verification without becoming an implementation plan.

Use this skill when:

- A feature bead or task has no readable spec.
- A user asks to draft, materialize, or sync a feature spec.
- `agent-driven-pr-workflow` needs a spec before `spec-review-gates`.
- Existing requirements are scattered across bead comments, chat, docs, or prior PRs and need to become one file.

Do not use this for purely mechanical CakePHP-to-Go page, controller action, API, or admin workflow ports when `mechanical-port-spec` is a better fit. Do not treat the spec as approved until `spec-review-gates` passes.

## Workflow

1. Gather inputs from the bead, user request, repo instructions, existing docs, comments, linked PRs, and relevant code patterns.
2. Choose a repo-local path by convention. Prefer `history/port-specs/<feature-slug>.md` when the repo has no better feature-spec location.
3. Write the spec with every required section below. Add optional sections only when they clarify the feature.
4. Use tables for scope boundaries, decisions, behavior contracts, and verification when tables make the spec easier to scan.
5. Sync the spec to Beads when a bead id is available:

```bash
bd update <bead-id> --spec-id <spec-path> --design-file <spec-path> --json
bd comment <bead-id> "Spec materialized at <spec-path>; synced to bead design/spec-id."
```

6. Run `spec-review-gates` and iterate until PASS before implementation starts.

If a behavior or product decision cannot be inferred safely, stop with a concise open question instead of hiding the uncertainty inside the spec.

## Required Sections

Every feature spec must include these sections. Preserve repo heading style if it already exists; `Required Behaviour` and `Required Behavior` are equivalent.

| Section | Required content |
| --- | --- |
| Executive Summary | The problem, intended outcome, chosen approach, phase boundary, and main risk in a short reader-ready summary. |
| Quick Reference | Stable names, ids, files, commands, external systems, and vocabulary a reviewer needs before reading details. |
| Scope | What this feature changes and owns. Prefer an in-scope table with concrete deliverables. |
| Non-Goals | Explicit exclusions, deferred follow-up work, and behaviors that must not change. |
| Architecture Decisions | Decisions already made, alternatives rejected, tradeoffs, and boundaries the implementation must preserve. |
| Required Behaviour | Observable contracts, state transitions, edge cases, error handling, idempotency, persistence, and user/system-visible outcomes. |
| Verification | Required tests, scripts, gates, build commands, success lines, CI expectations, and any production smoke or no-smoke rationale. |

## Optional Sections

Add only sections that improve clarity for the feature:

- Data Model or Schema
- Workflow or State Machine
- Failure and Retry Semantics
- Migration and Compatibility
- Rollout and Operations
- Test Matrix
- Security, Privacy, or Abuse Considerations
- Performance Expectations
- Risks and Mitigations
- Open Questions
- References

## Quality Bar

- Make the spec table-driven where exact behavior matters.
- Separate product behavior from implementation mechanics.
- Use concrete states, commands, file paths, and success criteria.
- Keep non-goals and deferred work visible so scope does not drift during build.
- Avoid vague requirements such as "works correctly" or "handle errors" without defining the observable contract.
- Preserve exact verifier success lines when the user or repo provides them.
- Keep the spec concise enough for reviewers to read, but complete enough that an implementation agent can build without rediscovering requirements.
