---
name: human-driven-pr-workflow
description: Use when a developer using OpenSpec and Jujutsu wants to personally write and understand the behavior-defining parts of a PR-scoped change without doing every supporting edit.
---

# Human-Driven PR Workflow

## Overview

The human writes the parts whose design and behavior they need to own. The controller preserves velocity around that work by managing the spec, reviewing each human-authored slice, implementing bounded non-critical work, running proof, shaping the final JJ history, and preparing one ready-for-review PR.

**Core invariant:** if a section could surprise the human in design review, debugging, or on-call, the human writes it.

Writing and debugging the critical code is the comprehension gate. Do not add quizzes, recitation, or a separate “prove you understand it” ceremony.

## Required Route

Use this sequence:

1. Materialize and validate the OpenSpec change.
2. Run `spec-review-gates`, then obtain explicit human spec approval.
3. Create a dynamic criticality map and work contract.
4. Alternate human-owned critical slices with controller-owned supporting work and focused proof.
5. Assemble the full PR and run applicable E2E or runtime proof.
6. Only after that proof passes, propose and execute the final JJ split.
7. Propose the exact PR title and body; after approval, open one non-draft PR.
8. Use CodeRabbit as the first independent whole-PR reviewer and route its findings by criticality.

Do not invoke Fable, `verifier`, or `pr-review-gates` anywhere in this workflow. Final local verification is still required.

## Ownership Boundary

Classify decisions and hunks, not whole file types. One file may contain both ownership classes.

| Class | Default owner | Typical work |
| --- | --- | --- |
| `human-critical` | Human | Data structures and schemas; migrations; core algorithms; state/lifecycle/concurrency logic; architectural boundaries; public contracts; auth, security, destructive, rollback, recovery, transaction, retry, or failure semantics; behavior the human expects to explain later |
| `controller-bounded` | Controller | Call-site adaptation with no new semantic choice; imports, exports, registration, and wiring; small local tweaks required by an approved interface; tests and fixtures for already-approved behavior; docs; generated artifacts; formatting; mechanical rename or cleanup |

The human may escalate any slice to `human-critical` at any time. Downgrade a default-critical slice only after explicit discussion and evidence that no behavior-defining decision remains; deadline, diff size, or agent convenience is not sufficient. The controller should also reclassify work when new evidence shows it is more consequential than expected.

### Boundary red flags — stop and reclassify

The controller must not continue a supposedly bounded edit when it requires any of these:

- Choosing behavior not settled by the approved spec.
- Defining or changing a data shape, migration, invariant, public interface, or architecture boundary.
- Choosing error, retry, transaction, concurrency, authorization, privacy, destructive, rollback, or recovery semantics.
- Expanding blast radius beyond the named slice or touching an unexpected subsystem.
- Writing a test whose expected result introduces a new product or correctness decision.
- Making a change the controller cannot precisely explain as a mechanical consequence of an approved decision.

Return that decision or hunk to the human. Do not disguise semantic work as “just wiring,” “just a test,” or “a small tweak.”

## Operating Choices

Ownership mode defaults to `critical-pair`:

- `critical-pair`: the human writes `human-critical` code; the controller writes `controller-bounded` work.
- `strict-driver`: on explicit request, the human writes all production code and the controller limits edits to specs, tests, fixtures, and docs.

Separately record the testing strategy (`existing-suite` or `bdd-tdd`) and workflow shape (`feature` or `bootstrap`). BDD/TDD can layer onto either ownership mode. Bootstrap uses the same ownership boundary; the human owns consequential stack, architecture, data, auth, and deployment choices.

Do not switch the whole workflow merely because the controller is asked to make a bounded production edit. That is part of `critical-pair`.

## Orientation

Before changing anything:

1. Read applicable `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, repo docs, and runbooks.
2. Resolve the repository, target base, current JJ change/stack, publication bookmark, remote, tracking item, package manager, tests, entry points, and publication rules from evidence.
3. Use `jj status`, `jj log`, and `jj diff` in a JJ repository. Do not assume Git three-dot ranges or use Git commit commands to shape JJ history.
4. Preserve unrelated user changes. Stop if this work overlaps edits whose ownership cannot be resolved safely.
5. Identify whether E2E/runtime proof is applicable and what environment it would touch. Never infer authority to deploy or mutate an environment.

The controller may run safe inspection and normal in-scope implementation commands. Give the human exact commands for history changes and for any risky, credentialed, destructive, deploy, or publication step unless the human explicitly asks the controller to run them.

## OpenSpec Contract And Human Gate

This workflow requires an OpenSpec change before implementation.

1. Resolve the supplied root or store. Otherwise run `openspec doctor --json` from the coordination or target root. Do not initialize OpenSpec implicitly; stop for a root decision if none resolves.
2. For an existing change, run:

   ```bash
   openspec status --change "<name>" --json
   openspec validate "<name>" --type change --strict --json --no-interactive
   ```

   Add `--store <id>` where the resolved store requires it.
3. If the change is missing, use the repo-local OpenSpec proposal workflow when available. Otherwise use the installed OpenSpec schema, instructions, and templates to create the proposal, design, delta specs, and task checklist. Do not substitute a chat summary or issue title.
4. Treat the proposal, design, and every delta spec as one authoritative feature contract. Treat `tasks.md` as an execution checklist, not canonical tracking.
5. Run `spec-review-gates` over the whole bundle and iterate until `PASS`.
6. Rerun strict OpenSpec validation after every gate-driven edit. Record the final approved bundle identity using an immutable VCS revision or content digests for every authoritative artifact.
7. Present the change name, approved bundle identity, artifact paths, requirements/scenarios, important design decisions, non-goals, acceptance proof, and material gate-driven revisions.
8. Stop for the human's explicit approval. Immediately before implementation, confirm the bundle still matches the approved identity and strict validation still passes. Do not begin on silence, partial review, drift, or an unresolved decision.

For requested spec changes, use `receiving-spec-feedback`, update the OpenSpec bundle, rerun strict validation and `spec-review-gates`, then obtain approval again. Any material change to scope, behavior, data shape, architecture, risk, or acceptance proof invalidates the earlier approval.

If implementation already exists when this skill starts, freeze further edits, pin the pre-existing diff, materialize/reconcile the OpenSpec bundle from evidence, pass the same review and human approval gate, then classify every existing hunk before continuing. Any `human-critical` hunk not authored by the human must be replaced through the human-owned slice loop; reviewing or approving it does not convert its authorship. Approval is not retroactive permission to preserve code that contradicts the contract.

## Criticality Map And Work Contract

After spec approval, inspect the implementation surface and present:

```markdown
Human-driven workflow ready.
Ownership mode: critical-pair | strict-driver
Testing strategy: existing-suite | bdd-tdd
Workflow shape: feature | bootstrap
OpenSpec: <root/store, change name, approved identity, artifact paths>
Repo and base: <path and exact base>
Current JJ change: <change id and description>

| Slice | Class | Owner | Why | Focused proof |
| --- | --- | --- | --- | --- |
| <slice> | human-critical | human | <risk/decision> | <command/signal> |
| <slice> | controller-bounded | controller | <mechanical consequence> | <command/signal> |

E2E/runtime proof: <required command/environment/signal | not applicable with reason>
Publication boundary: one PR; exact title/body require approval
```

The map is a living agreement, not a one-time file allowlist. Refine it as the implementation reveals new decisions.

## Slice Loop

For each `human-critical` slice:

1. Give one coherent slice card:

   ```markdown
   Critical slice: <outcome>
   Why the human owns it: <decision or risk>
   Invariants and constraints: <approved contract>
   Relevant evidence: <paths, APIs, prior pattern>
   Allowed scope: <files/interfaces>
   Focused proof: <exact command and expected signal>
   Stop conditions: <when to discuss or update the spec>
   ```

2. Let the human write the code and ask questions in the same conversation. Answer directly from evidence with constraints, alternatives, targeted hints, or small illustrative examples. Illustrations must not accumulate into the target patch. Do not supply a paste-ready implementation of the target critical slice; if the human asks the controller to own it, stop this workflow for an explicit workflow change. On re-entry, pin and classify all intervening changes; every controller-authored `human-critical` hunk must be removed and reimplemented by the human before this workflow continues.
3. Pin and review the exact human-authored diff for accuracy against the approved OpenSpec contract and surrounding code. Report actionable findings; do not silently repair critical code.
4. The human fixes critical findings. Review the new exact diff until no blocking critical finding remains.
5. Implement the mapped `controller-bounded` consequences. Before editing, name the files/hunks and avoid files the human is actively changing.
6. Run the focused proof for the assembled slice. Diagnose failures from evidence and route the fix by criticality.
7. Keep the product and focused test suite working before moving to the next critical slice.

Use exact command cards for risky operations or when the human requests command guidance. Routine implementation does not need command-by-command interruption.

### Tests

The controller may write and run tests only for behavior settled by the approved OpenSpec contract. If the human's implementation reveals intended behavior absent from that contract, update, strictly validate, review, and reapprove OpenSpec before encoding the expectation. If a test exposes an unanswered behavioral decision, return that decision to the human.

For TDD/BDD, verify the focused test fails for the intended reason before implementation and passes afterward. Ownership of the test file does not grant ownership of the product decision.

### Supporting agent lanes

The controller may keep up to two bounded supporting lanes moving. In the human's active working copy, supporting lanes are read-only: they must not edit, mutate JJ state, run formatters/generators, or run commands that alter shared state. A lane may make a non-overlapping `controller-bounded` edit only in an isolated worktree/change with an explicit file scope; the controller inspects and deliberately integrates it into the single feature stack and PR after the human's active slice ends. Lanes have no publication authority and may not make critical decisions or create separate PRs. Batch ordinary results between human slices; interrupt immediately only for safety, scope invalidation, or collision.

## Verification Before History Shaping

Focused checks keep each slice healthy; they do not replace assembled behavior proof.

After all planned slices are assembled:

1. Review the aggregate diff for accidental scope and confirm the criticality map still matches reality.
2. Run all relevant local checks.
3. Run the applicable E2E, integration, migration, or runtime acceptance proof named in the work contract. Static validation, rendering, preflight, or a deployment preview alone is not behavioral proof.
4. If E2E fails, diagnose the cause before editing. A `human-critical` fix goes back to the human; a `controller-bounded` fix may be implemented by the controller. Rerun the focused proof and then E2E.
5. If E2E is genuinely not applicable, present the evidence for non-applicability, name the strongest observable acceptance proof, and obtain the human's explicit acceptance before omitting it.

Do not split the JJ change while behavior is still being assembled or while required E2E is failing. The mutable change is deliberate: integration feedback can be fixed without repeatedly rewriting a premature commit stack.

## Final JJ Narrative

Maintain one mutable JJ change through implementation, focused testing, and assembled E2E/runtime proof. A useful WIP description is allowed; narrative commits are not required per slice.

Only after required proof passes:

1. Inspect the final aggregate diff and propose the smallest coherent developer narrative. Prefer fewer meaningful commits. Split by behavior and reviewability, not by human versus controller authorship.
2. Record the current JJ operation ID and a tested rollback command before mutation. For JJ 0.40, inspect without snapshotting with `jj --at-op=@ --ignore-working-copy op log -n 1` and plan `jj op restore <operation-id>` as the rollback; recheck installed help when the version differs. Show the intended stack and exact `jj split -r <change-id>`/`jj describe -r <change-id>` commands derived from the actual diff and installed JJ version; do not rely on a movable `@`. Immediately before every mutating command, reread the operation ID, stack, target change, and target diff. Any drift invalidates the proposed command and rollback plan; rederive both. Never restore a stale operation across later work without inspecting the lost operations and obtaining explicit approval. The human runs history commands unless they explicitly ask the controller to do so.
3. Prefer fileset splits when boundaries are clean and interactive hunk selection when a file contains multiple narrative units. State what selected changes remain in the first revision and what becomes the child.
4. Verify the resulting `jj log`, each commit diff, and the aggregate base-to-tip diff. Confirm no content was lost, duplicated, or pulled in from unrelated work.
5. Rerun checks affected by the split. Where practical, keep each commit independently coherent; always re-prove the final aggregate tree.

Before the PR opens, history may be reshaped to improve the narrative. After review begins, append focused follow-up JJ revisions by default so CodeRabbit and human review anchors remain stable; do not force-push or rewrite reviewed history without explicit approval.

## Ready PR And CodeRabbit

After final local verification and JJ narrative checks:

1. Prepare the exact PR title and body. The body must include the OpenSpec change/artifact paths, behavior, human-authored critical slices, controller-authored bounded work, verification evidence, migration/rollback notes when applicable, tracking item, and residual risk.
2. Present the exact title and body as a publication boundary. Let the human edit them and wait for explicit approval.
3. Resolve and show the exact bookmark, remote, base revision/branch, and tip revision to publish. Push and open one non-draft PR. Do not split human and controller work into separate PRs merely because authorship differs.
4. Read the hosted PR back and verify its title, body, base, head/tip, and `isDraft=false` match the approved publication.
5. Let CodeRabbit be the first deliberately invoked independent whole-PR reviewer. Required CI and automatically triggered bots may run concurrently; Fable, `verifier`, and `pr-review-gates` remain out of scope, and no other whole-diff reviewer is deliberately invoked before CodeRabbit. Local testing remains evidence; CodeRabbit is not a substitute for it.
6. Route every actionable finding:
   - Semantic, architectural, data, security, concurrency, lifecycle, failure-mode, or otherwise critical finding → human fixes it.
   - Mechanical, documentation, fixture, or already-decided test/call-site finding → controller may fix it.
   - Ambiguous finding → discuss and classify before editing.
7. Wait for the CodeRabbit check/review to reach a terminal result and inspect every actionable finding. No critical finding may remain unresolved.
8. Rerun affected focused checks and applicable E2E after fixes. Push focused follow-up JJ revisions and let CodeRabbit review the updated PR. Repeat until CodeRabbit has no unresolved actionable finding; do not repeatedly recurate the pre-review stack.

If the implementation changes materially after title/body approval but before publication, regenerate the affected text and obtain approval again. After publication, propose exact PR-body edits for approval whenever review fixes make the published behavior, proof, migration/rollback notes, or residual-risk statements inaccurate.

If CodeRabbit is unavailable or cannot review, report that plainly and stop. This workflow cannot complete with a silently substituted reviewer; a different reviewer requires the human to choose a different publication workflow explicitly. Do not restore Fable, `verifier`, or `pr-review-gates`.

## Common Mistakes

| Mistake or rationalization | Correction |
| --- | --- |
| “Any production edit makes this agent-driven.” | Ownership follows criticality. Bounded production wiring is allowed; behavior-defining code remains human-owned. |
| “It is only a few lines, so it is non-critical.” | Diff size does not determine consequence. Reclassify using semantics and blast radius. |
| “The test is mechanical, so I can choose the expected behavior.” | Test syntax may be mechanical; the behavioral oracle is not. Return unresolved behavior to the human. |
| “Focused tests pass, so split commits now.” | Keep the mutable change until the assembled PR passes its applicable E2E/runtime proof. |
| “Separate authorship deserves separate PRs or commits.” | Use one PR and split commits by developer narrative, not authorship. |
| “Another local whole-diff reviewer is safer.” | The human already reviewed critical slices. Run final evidence, then use CodeRabbit as the independent whole-PR reviewer. |
| “Open a draft while waiting for approval.” | First obtain exact title/body approval, then open a non-draft PR. |

## Stop States

Stop and state the precise blocker when:

- No healthy OpenSpec root/store or complete feature contract can be resolved.
- `spec-review-gates` or human spec approval has not passed.
- A material spec change has not been updated, re-reviewed, and re-approved.
- Criticality is ambiguous and choosing an owner would itself choose behavior or risk.
- User changes overlap the intended edit and ownership cannot be resolved safely.
- Required E2E/runtime proof would touch an environment without authorization.
- A required critical fix is waiting for the human.
- The exact final JJ split cannot be derived safely from the current stack.
- PR title/body approval, credentials, push, PR creation, CodeRabbit, CI, or required runtime access is unavailable.

## Report Format

For checkpoints:

```markdown
Human-driven workflow checkpoint.
OpenSpec: <change and approval state>
Current critical slice: <slice and owner>
Human-authored: <current diff summary>
Controller-authored: <current diff summary>
Evidence: <focused signal>
Next: <human slice | bounded work | assembled E2E | final JJ split | PR approval | CodeRabbit>
```

For completion:

```markdown
Human-driven workflow complete.
OpenSpec: <change and artifact paths>
Repo/base: <path and exact base>
Human-authored critical implementation:
- <summary>
Controller-authored bounded work:
- <summary>
Behavior and design:
- <what changed and how it works>
Verification:
- <focused and E2E/runtime evidence>
JJ narrative:
- <final commits>
Ready PR: <URL>
CodeRabbit: <result and addressed findings>
Residual risk:
- <risk or none>
```

If stopped, replace the first line with `Human-driven workflow stopped.` and name the next required human action.
