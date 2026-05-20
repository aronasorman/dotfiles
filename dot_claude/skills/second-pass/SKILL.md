---
name: second-pass
description: Use when the user explicitly invokes /second-pass or asks to extract durable learnings from a V1 implementation into a writable monolithic spec file or existing Beads-persisted spec, update required test obligations and non-binding hints, then reset the code worktree to the base branch. Do not use implicitly because the workflow performs a destructive worktree reset.
---

# /second-pass
Extract durable learnings from the current V1 implementation, update the monolithic spec file, then reset the code worktree back to the base branch.
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

Update the PER, repo-local, or Beads-persisted spec so durable learnings are preserved for a clean V2 implementation.

Then reset the current code worktree to the base branch so the next agent starts clean.

V1 is evidence, not a template.

## Inputs

Required:

* Writable monolithic spec file path from the first argument, one unambiguous repo-local spec file, or one existing Beads-persisted monolithic spec plus its exact materialized file path.
* Current repo/worktree containing the V1 implementation.
* Base branch from the second argument, or inferred default branch.

A Bead is a valid second-pass target only when it already persists the monolithic spec and the workflow has the exact file path where that spec is materialized after reset.

An issue, chat summary, remote tracker, or Bead that does not itself persist the monolithic spec is not a valid second-pass target.

If no writable spec file or valid Beads-persisted spec can be identified, stop before reset and report that second-pass requires a spec source.

Optional context to inspect if present:

* current bead description
* review notes
* test results
* failing test logs
* QA notes
* existing spec changelog
* git commit history
* git diff against base branch

## Spec location and reset behavior

Resolve the spec to exactly one writable file, or one existing Beads-persisted spec plus its exact materialized file path, before editing or resetting.

| Spec location | What to update | Reset behavior |
|---|---|---|
| PER spec file outside the code worktree | That PER spec file | Update the spec, then hard-reset the entire code worktree to the base branch. Preserve no code worktree changes. |
| Repo-local spec file inside the code worktree | That exact spec file | Update the spec, save the final spec content outside the worktree, reset every other worktree change to the base branch, then reapply only the spec file update. |
| Beads-persisted spec with an exact materialized file path | The existing Bead-persisted spec, then the materialized file | Update the existing Bead-persisted spec, delete the materialized file version from the worktree, hard-reset the entire worktree to the base branch with no path exceptions, then write the updated spec file from the Bead-persisted content. |
| Issue, tracker, chat-only spec, or non-spec Bead | Nothing | Stop. Do not reset, do not fake a second pass, and surface the missing writable spec file or Beads-persisted spec. |

If more than one plausible repo-local spec exists and the user did not name one, stop and ask which file is the spec.

Do not preserve source, tests, generated files, build artifacts, lockfiles, issue metadata, or repo docs unless the named repo-local spec file or Beads materialized spec file is that exact file.

For a Beads-persisted spec:

1. Read the current spec from the existing Bead.
2. Write the second-pass updates back to that same Bead before resetting.
3. Delete the materialized file version from the code worktree if it exists.
4. Hard-reset the whole code worktree to the base branch with no pathspec exceptions.
5. Write the updated spec content from Beads back to the exact materialized file path.
6. Verify no worktree changes remain except that materialized spec file, unless the reset base already contains identical content.

## Hard rules

1. Do not implement V2.
2. Do not create beads. If a Bead already persists the spec, update only that existing Bead.
3. Do not rewrite the spec into multiple files.
4. Do not create separate discovery, constraint, trace, or planning files.
5. Only update the monolithic spec file.
6. Preserve durable learnings from V1.
7. Do not promote incidental V1 implementation structure into the spec.
8. After the spec update is complete, reset the code worktree to the base branch.
9. End with the code worktree positioned at the base branch.
10. If the spec is outside the worktree, end with the code worktree clean.
11. If the spec is inside the worktree, end with the code worktree clean except for that exact spec file.
12. Preserve a repo-local spec update across the reset by saving the final spec content outside the worktree before resetting, then reapply only that spec update after reset.
13. For a Beads-persisted spec, do not preserve the materialized file through reset; delete it, fully reset the worktree, then write it again from Beads.

## Spec format

The spec is monolithic.

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
* code worktree is clean when the spec is outside the repo
* code worktree is clean except for the named spec file when the spec lives inside the repo
* Beads-persisted spec content has been updated before the materialized file is rewritten
* spec contains the new changelog entry
* no V2 implementation was started

## Final response

Report only:

Second-pass complete.
Spec updated:
<spec path>
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
<clean / clean except repo-local spec / clean except Beads materialized spec>

Do not include a V2 implementation plan unless explicitly asked.
Do not start coding.
Do not create beads.
