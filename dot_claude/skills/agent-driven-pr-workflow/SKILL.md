---
name: agent-driven-pr-workflow
description: Use when working from an approved spec and the user asks for the full or lite agent-driven build-to-draft-PR process.
---

# Agent-Driven PR Workflow

Orchestrate an approved spec through build, verification, hard review, optional second pass, and draft PR creation.

This skill assumes the spec already exists. It does not cover collaborative spec writing.

## Modes

Default to `full`.

Use `lite` only when the user explicitly asks for lite, quick, single-pass, or no second pass.

## Context Boundary

The current agent is the controller.

The controller owns:

- Workflow state and stop decisions.
- Invoking each skill or gate in order.
- Applying required spec edits if `spec-review-gates` returns actionable spec feedback.
- The `second-pass` stage in full mode.
- Rewriting the final commit history into a better developer narrative that is easy to review commit by commit before the final review gate and push.
- Final git hygiene, push, and draft PR creation.

The controller must not do the normal build implementation manually. Build work belongs to `subagent-driven-development`.

Fresh or external gates own:

- `spec-review-gates` review work.
- `subagent-driven-development` build work.
- `verifier` verification work.
- `pr-review-gates` opposite-family hard review work.

## Inputs

Resolve these before starting:

- Spec path.
- Repo/worktree path.
- Target base branch for reset, review, push, and draft PR.
- Mode: `full` or `lite`.
- Any repo instructions for Beads, issue tracking, PR body format, tests, or hosted CI.

If the spec path, repo path, or base branch cannot be resolved safely, stop before making changes.

## Full Mode

Run this sequence continuously. Do not pause between stages unless blocked, a gate returns `HUMAN DECISION`, or the next destructive operation cannot be made safe.

1. Run `spec-review-gates` on the spec. Iterate until PASS.
2. Build with `subagent-driven-development`.
3. Run `verifier` as a fresh-context gate. Iterate until PASS or `HUMAN DECISION`.
4. Run `pr-review-gates`. Iterate until PASS or an issue needs human judgment.
5. Run the second pass in the controller context.
   - Use the existing `second-pass` workflow if available.
   - Treat the V1 implementation as evidence, not a template.
   - Fold durable learnings into the spec.
   - Reset the code worktree to the target base branch.
   - Preserve the intended spec update if the spec lives inside the worktree.
6. Build again with `subagent-driven-development` from the updated spec.
7. Run `verifier` again as a fresh-context gate. Iterate until PASS or `HUMAN DECISION`.
8. Rewrite the final commit history into a better developer narrative that is easy to review commit by commit.
9. Run `pr-review-gates` again. Iterate until PASS or an issue needs human judgment.
10. Push the branch and create a draft PR.

## Lite Mode

Lite is a single-pass workflow. Run each build or gate once. If any gate returns a non-pass result, stop with the result and evidence instead of entering the second-pass loop.

1. Run `spec-review-gates` once.
2. Build once with `subagent-driven-development`.
3. Run `verifier` once as a fresh-context gate.
4. Rewrite the commit history into a better developer narrative that is easy to review commit by commit.
5. Run `pr-review-gates` once.
6. Push the branch and create a draft PR if all gates passed.

Lite never runs `second-pass`, a second build, or repeated gate iterations unless the user explicitly upgrades to full mode.

## Gate Semantics

`PASS` means proceed.

`FAIL` or `ITERATE` in full mode means the responsible implementation context fixes the issue, then the same gate is rerun.

`FAIL` or `ITERATE` in lite mode means stop and report the gate result.

`HUMAN DECISION` means stop and surface the decision needed. Do not reinterpret or override it.

For `verifier`, preserve the existing verifier contract: infer missing `success criteria` from the just-written implementation, mark inferred criteria as verifier-inferred, and rerun a fresh verifier subagent after fixes in full mode.

For `pr-review-gates`, use the opposite family reviewer from the implementation writer whenever possible. Do not push or create the draft PR until the required review gates have passed for the selected mode.

## Commit Narrative Stage

Before the final `pr-review-gates` run and before pushing, rewrite the commit history so it is easy to review commit by commit.

The commit stack should:

- Tell a coherent developer narrative from foundation to behavior to verification.
- Keep mechanical setup, behavior changes, tests, and documentation/spec updates in reviewable commits.
- Avoid one large "misc fixes" commit when the changes can be split by concern.
- Avoid tiny noise commits that force reviewers to reconstruct intent across many diffs.
- Preserve authorship and avoid rewriting unrelated user commits.

If the branch has already been pushed or is shared, do not force-push rewritten history unless the user has explicitly approved that for the branch.

## Draft PR Stage

After the commit narrative stage and final gates pass:

- Check `git status`.
- Ensure only intended files are included.
- Run required repo-local final checks if the gate output says they must be rerun after the last fix.
- Honor repo instructions for Beads, issue IDs, PR body content, and tracking.

Create a draft PR, not a ready-for-review PR.

The PR body should include:

- Spec path.
- Mode used: `full` or `lite`.
- Build summary.
- Verifier result or results.
- PR review gate result or results.
- Tracking issue or bead, when required by the repo.

## Stop States

Stop and report clearly when:

- Required input cannot be resolved.
- A destructive reset would be unsafe.
- A gate returns `HUMAN DECISION`.
- A full-mode gate cannot be made to pass after a technically valid fix path is exhausted.
- A lite-mode gate returns anything other than PASS.
- Authentication, credentials, network, hosted CI, or PR creation is unavailable.

## Report Format

Report:

```markdown
Agent-driven PR workflow complete.
Mode: full | lite
Spec: <path>
Repo: <path>
Base: <branch/ref>
Branch: <branch>
Build passes:
- <summary>
Verifier:
- <result summary>
PR review gates:
- <result summary>
Commit narrative:
<rewrite summary>
Second pass:
<run / skipped / not applicable>
Draft PR:
<url or not created with reason>
Residual risk:
- <risk or none>
```

If stopped early, replace the first line with:

```markdown
Agent-driven PR workflow stopped.
Reason: <specific stop state>
Next required action: <specific action>
```
