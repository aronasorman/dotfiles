---
name: second-pass
description: Use when the user explicitly invokes /second-pass or asks to extract durable learnings from a V1 implementation into a monolithic PER spec, update required test obligations and non-binding hints, then hard-reset the code worktree to the base branch. Do not use implicitly because the workflow performs a destructive worktree reset.
---

# /second-pass
Extract durable learnings from the current V1 implementation, update the monolithic PER spec, then hard-reset the code worktree back to the base branch.
Do not implement V2.
Do not create beads.
Do not create follow-up work items.
A higher-level workflow handles beads and V2 dispatch.

## Usage

```text
/second-pass

Examples:

/second-pass
/second-pass
```

## Goal

Use V1 as a discovery artifact.

Update the PER spec so durable learnings are preserved for a clean V2 implementation.

Then reset the current code worktree to the base branch so the next agent starts clean.

V1 is evidence, not a template.

## Inputs

Required:

* PER spec path from the first argument.
* Current repo/worktree containing the V1 implementation.
* Base branch from the second argument, or inferred default branch.

Optional context to inspect if present:

* current bead description
* review notes
* test results
* failing test logs
* QA notes
* existing spec changelog
* git commit history
* git diff against base branch

## Hard rules

1. Do not implement V2.
2. Do not create beads.
3. Do not rewrite the spec into multiple files.
4. Do not create separate discovery, constraint, trace, or planning files.
5. Only update the monolithic PER spec.
6. Preserve durable learnings from V1.
7. Do not promote incidental V1 implementation structure into the spec.
8. After the spec update is complete, hard-reset the code worktree to the base branch.
9. End with the code worktree clean and positioned at the base branch.
10. If the PER spec path is inside the code worktree, preserve the intended spec update across the reset by saving the final spec content outside the worktree before resetting, then reapply only the spec update after reset. Otherwise, do not preserve any worktree changes.

## Spec format

The PER spec is monolithic.

It should contain these top-level sections near the top:

## Second-Pass Changelog
## Required Test Obligations
## Non-Binding Implementation Hints
## Main Spec

If these sections do not exist, add them.

Do not add other top-level second-pass artifact sections unless explicitly instructed.

## What to add to the spec

1. Second-Pass Changelog

Add a new changelog entry at the top of Second-Pass Changelog.

Use this format:

### <YYYY-MM-DD> — V1 learnings
Status: proposed
Source: current V1 worktree
Base branch: <base branch>
V1 comparison: <git ref or diff summary>
#### Summary
<Brief summary of what V1 revealed.>
#### Main spec changes
- Updated `<section>` to clarify ...
- Added `<section>` requirement for ...
- Revised `<section>` because ...
#### Learnings folded into main spec
- <Durable learning now reflected in the main body.>
- <Durable learning now reflected in the main body.>
#### V1 details intentionally not promoted
These V1 details were observed but are not requirements for V2:
- <Incidental class/helper/file/control-flow detail not promoted.>
- <Temporary workaround not promoted.>

2. Required Test Obligations

Add or update required behavioral checks discovered during V1.

Use this format:

| ID | Obligation | Type | Notes |
|---|---|---|---|
| T1 | <Required behavior to verify> | unit / integration / e2e / regression / contract | <Notes> |

Test obligations should describe behavior to prove, not exact implementation structure.

3. Non-Binding Implementation Hints

Add only optional guidance.

These hints must not constrain V2 unless the V2 agent finds them useful.

Use language like:

- MAY reuse <existing system> if it remains the simplest clean approach.
- Consider <known useful pattern>, but do not force it if it complicates the implementation.

4. Main Spec

Fold all durable V1 learnings into the main spec body.

Durable learnings include:

* clarified behavior
* user-visible outcomes
* invariants
* API/interface contracts
* data semantics
* failure behavior
* permissions/security/privacy rules
* performance or operational expectations
* compatibility requirements
* required tests
* known traps that V2 could plausibly repeat

Do not create a separate "discoveries" or "constraints" section. Put durable content into the relevant main spec section.

## Implementation-detail rule

Do not add strong private implementation details to the spec unless the problem specifically requires them.

Avoid promoting:

* class names
* helper names
* private function names
* exact file layout
* incidental module decomposition
* exact internal control flow
* temporary workaround structure
* V1-specific abstractions

Prefer:

Failed delivery attempts must be recorded and visible in delivery history.

Avoid:

Use WebhookFailureCoordinator in src/webhooks/failureCoordinator.ts.

Prefer:

Retry scheduling should not happen inline in the request path if it can increase user-facing latency or duplicate side effects.

Avoid:

Call scheduleRetry() from WebhookController.create() after saveAttempt().

The promotion rule is:

Add a detail to the spec only if a clean V2 implementation could plausibly get the feature wrong without knowing it.

## Verify final state

* code worktree is at base branch
* code worktree is clean, except possibly the PER spec if it intentionally lives inside the repo
* PER spec contains the new changelog entry
* no V2 implementation was started

## Final response

Report only:

Second-pass complete.
Spec updated:
<PER spec path>
Base reset:
<base branch/ref>
V1 source:
<branch/ref before reset>
Spec changes:
- <brief bullet>
- <brief bullet>
- <brief bullet>
Required test obligations added/updated:
- <brief bullet>
Non-binding hints added/updated:
- <brief bullet>
Worktree status:
<clean / clean except PER spec>

Do not include a V2 implementation plan unless explicitly asked.
Do not start coding.
Do not create beads.
