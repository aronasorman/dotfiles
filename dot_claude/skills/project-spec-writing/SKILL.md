---
name: project-spec-writing
description: Use when writing or materializing a project-level spec, architecture spec, product/system spec, tracker spec, platform spec, or top-level project plan before feature specs or implementation work.
---

# Project Spec Writing

Write project-level specs through an iterative approval flow before saving the final artifact.

A project spec defines the system, its stable contracts, its non-goals, and the ordered feature-spec work needed to build it. It is not a code-level implementation plan.

## Use When

- A user asks to start, draft, write, materialize, or update a project spec.
- A project idea needs architecture, data structures, API shape, behaviors, and scope decisions.
- Requirements are being refined interactively before feature specs are ready.
- The next useful artifact is a top-level project spec, not a feature spec.

Do not use this when the task is already a bounded feature slice; use `feature-spec-writing` instead.

## Workflow

1. Gather context from the conversation, repo instructions, docs, notes, existing specs, and visible project constraints.
2. Separate decisions already made from assumptions and open questions.
3. Present draft sections, endpoint shapes, data structures, behavior decisions, and implementation-plan items to the user before writing files.
4. Iterate in chat until the user explicitly approves the project spec shape.
5. Write the approved spec to the user-requested path. If no path is given, choose the project or repo's existing spec location and state the path.
6. Run the local spec self-review: scan for placeholders, contradictions, vague contracts, missing non-goals, missing decision status, and implementation-plan items that are too broad for future feature specs.
7. Show a final summary of the written spec and ordered implementation items.
8. Hard stop and ask whether to start `feature-spec-writing` for the first item.

Do not begin feature-spec writing, implementation planning, coding, scaffolding, endpoint creation, or data-model edits until the user explicitly chooses the next item.

## Required Sections

Every project spec must include these sections. Preserve the destination document's existing heading style if updating an existing spec.

| Section | Required content |
| --- | --- |
| Executive Summary | What the project is, why it exists, chosen high-level shape, and main risk. |
| Project Goals | Concrete outcomes the project should achieve. |
| Users and Actors | Human users, public viewers, admins, agents, systems, and external sources. |
| Scope | What the project owns. Prefer a table of in-scope capabilities. |
| Non-Goals | Explicit exclusions, deferred complexity, and behaviors the project must not take on yet. |
| Core Data Structures | Main resources, fields, relationships, lifecycle ownership, persistence expectations, and queryability requirements. |
| API Endpoints | Endpoint groups, purpose, auth, request shape, response shape, writes, errors, and side effects. |
| Key Behaviors | Observable workflows, state transitions, publishing rules, validation, imports, previews, failure handling, and user/system-visible outcomes. |
| Auth and Access | Public/private surfaces, token model, roles, preview access, and agent permissions. |
| External Data and Integrations | Sources, import/update paths, provenance, trust boundaries, and refresh cadence. |
| QA and Publishing Flow | Preview, review, approval, publish, rollback, screenshots, smoke checks, and verification expectations. |
| Implementation Plan | Ordered feature-spec items with short summaries, dependencies, and intended handoff to `feature-spec-writing`. |
| Decision Ledger | Decided, assumed, rejected, and open decisions. |
| Open Questions | Remaining unresolved issues that block or shape implementation. |

## Implementation Plan Section

The `Implementation Plan` section is a project breakdown, not a detailed build plan. Each item should be small enough to become one feature spec.

Each item must include:

- Name
- Purpose
- Why it comes at this point in the sequence
- Dependencies
- Summary bullets for the future feature spec
- Acceptance shape or completion signal

Use this shape:

```markdown
### 1. <Item Name>

Purpose: <what this unlocks>

Order rationale: <why this comes now>

Depends on: <none | earlier item names>

Feature spec seed:
- <behavior or contract the feature spec must define>
- <data, API, UI, or operational surface it owns>
- <verification or acceptance shape>
```

## Decision Ledger

Use a table when decisions matter for implementation.

| Status | Meaning |
| --- | --- |
| Decided | The user approved this or the source material makes it explicit. |
| Assumed | The agent chose a conservative default that should be easy to revise. |
| Rejected | The option was considered and intentionally excluded. |
| Open | The decision is unresolved and should not be hidden in prose. |

Do not treat assumptions as approved decisions. If an open decision blocks a stable project spec, stop with a concise question.

## Endpoint And Data Contracts

Keep API, resources, behaviors, and auth cross-linked.

For each endpoint group, define:

- Route and method
- Actor
- Auth requirement
- Request body or query params
- Response body
- Resource writes or reads
- State transitions
- Errors
- Idempotency or immutability rules

For each core resource, define:

- Purpose
- Ownership boundary
- Key fields
- Relationships
- Lifecycle states
- Query requirements
- Public exposure
- Versioning or immutability rules

## Final Response

After writing the approved project spec, stop with this structure:

```markdown
Project spec written to <path>.

Summary:
- <short summary bullet>
- <short summary bullet>

Implementation items:
1. <item name> - <one-line purpose>
2. <item name> - <one-line purpose>

Should I start `feature-spec-writing` for item 1, `<item name>`?
```

Do not continue past this prompt without explicit user approval.

## Quality Bar

- Keep the spec decision-driven, not essay-driven.
- Use concrete resource names, states, routes, permissions, and verification signals.
- Make scope and non-goals visible enough to prevent later drift.
- Keep the implementation-plan items ordered and feature-spec sized.
- Preserve known user decisions even when they remove generalized machinery.
- Prefer simple stable contracts over premature CMS, workflow, or multi-agent abstractions.
