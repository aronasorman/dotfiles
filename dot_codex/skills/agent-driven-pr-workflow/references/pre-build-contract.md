# Pre-Build Contract Selection and Approval

Load this reference whenever the workflow must resolve, create, revise, or
approve its implementation contract.

## Contents

- Route selection
- Passed spec route
- Small-fix route
- OpenSpec route
- Beads and publication
- Build contract and approval invalidation
- OpenSpec lifecycle during execution

## Route Selection

Choose exactly one route in this order:

1. **Passed readable spec**: use the supplied approved spec path.
2. **Passed OpenSpec change**: resolve its artifact bundle and validate it.
3. **Explicit small-fix request**: use the small-fix route only when the current
   request explicitly invokes `fix-explainer` and every eligibility condition
   below passes.
4. **No contract**: create an OpenSpec change and require approval.

Do not classify a fix from expected line count alone. If the user explicitly
requested `fix-explainer` but the diagnosis or source evidence is incomplete,
stop with the missing evidence instead of diagnosing inside the explainer. If
the evidence exists but the change is not small, use the OpenSpec route.

## Passed Spec Route

Confirm the supplied path is readable, feature-scoped, and approved. Run
`spec-review-gates` until PASS and sync the current spec text/path to the Bead.
An unchanged approved input does not need another approval. Any material change
made during review invalidates the prior approval; present the revised spec and
stop for explicit user approval before execution.

## Small-Fix Route

The route is eligible only when all of these observable conditions are true:

- The current workflow request explicitly invokes `fix-explainer` because the
  already-evidenced problem or proposed fix still needs explanation and
  approval.
- An already-evidenced diagnosis, repository root, exact source paths/ranges,
  and proposed fix exist. Log-dependent claims include the supporting logs.
- The fix restores one intended behavior through one localized root cause.
- It changes no public API or product requirement, data/config schema,
  architecture boundary, dependency, security/permission policy, migration, or
  cross-service contract.
- Acceptance can be proven with a focused regression test plus local behavioral
  verification. A remote rollout is not required to understand whether the fix
  is correct, although normal post-merge smoke may still apply.

When eligible:

1. **REQUIRED SUB-SKILL:** Use `fix-explainer`. It must create only its
   temporary annotated-source page and must prove the target repository stayed
   unchanged.
2. Ensure the page labels the candidate as `Proposed — not applied` and shows
   the diagnosis, exact source evidence, proposed change, and verification
   plan.
3. Present the page and stop for explicit user approval. Do not treat viewing,
   silence, or a request for clarification as approval.
4. On requested changes, revise only from supplied evidence, rerender, prove the
   repository is still unchanged, and ask again.
5. After approval, keep the approved manifest available outside the target
   repository through build and verification. Pass it to the builder as the
   authoritative small-fix contract. `fix-explainer` never implements the fix.

## OpenSpec Route

### Resolve the root and change

Use the supplied OpenSpec root/store when present. Otherwise run
`openspec doctor --json` from the coordination or target root and require a
healthy resolved root. Do not initialize OpenSpec in a new repository
implicitly; stop for an explicit root choice if none resolves. Run commands
from the resolved root; when using a registered store, add its `--store <id>`
flag to commands that support it.

For an existing change:

```bash
openspec status --change "<name>" --json
openspec validate "<name>" --type change --strict --json --no-interactive
```

Use `changeRoot` and `artifactPaths` from status output. The authoritative
feature contract is one bundle containing the existing `proposal`, `design`,
and every delta spec output. Include `tasks` as an execution checklist, not as
canonical task tracking.

For a missing change, **REQUIRED SUB-SKILL:** use `openspec-propose` to create a
feature-scoped change and all planning artifacts. Then resolve status and run
strict validation using the commands above.

### Review and approve

1. Run `spec-review-gates` over the proposal, design, and delta specs as one
   coherent review bundle. Iterate until PASS.
2. Present the change name, artifact paths, requirements/scenarios, important
   design decisions, non-goals, acceptance proof, and material gate-driven
   revisions.
3. Stop for explicit user approval before Fable planning or any implementation.
4. If the user requests changes, use `openspec-update-change`, rerun strict
   validation and `spec-review-gates`, then request approval again.
5. Any later revision that materially changes scope, behavior, requirements,
   architecture, risk, or acceptance proof invalidates approval. Update the
   OpenSpec bundle, rerun the gates, and obtain renewed approval before another
   build. Evidence clarifications that do not change the contract do not require
   renewed approval.

## Beads and Publication

Beads remains canonical for ownership, status, blockers, implementation work,
PR tracking, and post-merge smoke. Record the OpenSpec root, change name,
artifact paths, and current spec text on the relevant bead; do not rely on a
path alone. OpenSpec `tasks.md` may reference Bead IDs but never replaces Beads.

For `external/uncontrolled` repos, keep OpenSpec artifacts outside the target
worktree and out of the pushed branch. Pass absolute artifact paths to builders
and gates. Broader project or architecture documents stay in their established
documentation system; OpenSpec carries only the feature/PR-slice delta.

## Build Contract and Approval Invalidation

Before invoking Fable, Claude Opus, `subagent-driven-development`, a worktree
skill, or any implementation tool, the controller must have:

- A readable approved spec, approved OpenSpec bundle, or approved small-fix
  manifest.
- Explicit approval for any contract generated during this workflow.
- The architecture packet and material spec-gate findings when applicable.
- The publication boundary and behavioral acceptance/proof requirements.

Pass every authoritative contract path to the builder and verifier. Chat
history, a bead title, an issue summary, or inferred intent alone is never a
build contract. Any implementation change that alters the approved behavior
invalidates approval and returns to the applicable update/review/approval loop.

## OpenSpec Lifecycle During Execution

For OpenSpec-backed work:

1. After each build, use `openspec-verify-change` as a structural and
   spec-conformance precheck. Its PASS never replaces fresh runtime proof from
   `verifier` or code review from `pr-review-gates`.
2. Use `openspec-update-change` only for durable scope, requirement, design, or
   proof-contract changes found by Fable, verification, review, or user input.
3. After final gates pass, use `openspec-sync-specs` before diff annotation and
   PR-description handoff. If later code changes alter behavior, update,
   re-review, re-approve when material, re-verify, and sync again.
4. Keep the change active through draft PR review and required post-merge
   CI/deploy/smoke. Use `openspec-archive-change` only after the actual outcome
   is recorded, or after merge when no post-merge follow-through is required.
5. Do not archive a failed, blocked, unmerged, or behaviorally unverified
   change. Do not store raw gate transcripts or internal scores in OpenSpec.
