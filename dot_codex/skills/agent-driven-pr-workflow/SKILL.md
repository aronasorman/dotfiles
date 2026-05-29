---
name: agent-driven-pr-workflow
description: Use when working from an approved spec and the user asks for the full or lite agent-driven build-to-draft-PR process or autonomous post-merge main CI, deploy, and production-smoke follow-through for an agent-created PR.
---

# Agent-Driven PR Workflow

Orchestrate an approved spec through build, verification, hard review, optional second pass, draft PR creation, and required post-merge `main` CI/deploy monitoring plus production smoke follow-through.

Full and lite both use hard gates and iterate until they pass. Full mode adds
a second implementation pass; lite mode stops after one implementation pass.

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
- Post-merge `main` CI/deploy monitoring and production smoke tracking when repo instructions, the spec, or the user require it.

The controller must not do the normal build implementation manually. Build work belongs to `subagent-driven-development`.

Fresh or external gates own:

- `spec-review-gates` review work.
- `subagent-driven-development` build work.
- `verifier` verification work.
- `pr-review-gates` opposite-family review work.

## Inputs

Resolve these before starting:

- Spec path.
- Architecture model packet path or Beads attachment, if the spec includes one.
- Repo/worktree path.
- Target base branch for reset, review, push, and draft PR.
- Mode: `full` or `lite`.
- Any repo instructions for Beads, issue tracking, PR body format, tests, or hosted CI.
- Any repo-local production smoke skill or runbook, target `main` CI/deploy workflow, and noninteractive credential path needed to run it autonomously.

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
11. Create or confirm the post-merge `main` CI/deploy and production smoke Bead when required.

## Lite Mode

Lite is a single implementation pass with hard gates and iteration. Iterate
inside this one pass until each gate returns `PASS` or reaches a stop state.

1. Run `spec-review-gates` on the spec. Iterate until PASS.
2. Build with `subagent-driven-development`.
3. Run `verifier` as a fresh-context gate. Iterate until PASS or `HUMAN DECISION`.
4. Rewrite the commit history into a better developer narrative that is easy to review commit by commit.
5. Run `pr-review-gates`. Iterate until PASS or an issue needs human judgment.
6. Push the branch and create a draft PR.
7. Create or confirm the post-merge `main` CI/deploy and production smoke Bead when required.

Lite never runs `second-pass` or a second implementation build unless the user
explicitly upgrades to full mode.

## Gate Semantics

`PASS` means proceed.

`FAIL` or `ITERATE` in full or lite mode means the responsible implementation
context fixes the issue, then the same gate is rerun. Lite mode repeats gates
inside the same implementation pass; it does not trigger the full-mode second
pass.

`HUMAN DECISION` means stop and surface the decision needed. Do not reinterpret or override it.

For `verifier`, preserve the existing verifier contract: infer missing `success criteria` from the just-written implementation, mark inferred criteria as verifier-inferred, and rerun a fresh verifier subagent after fixes in full mode.

When a spec includes an architecture model packet, pass the packet to every verifier run. The verifier must check implementation shape against the modeled states, transitions, invariants, assumptions, and verifier obligations without requiring the code to mirror Quint syntax.

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
- If production smoke is required, create or identify the Bead that will track post-merge `main` CI/deploy monitoring and the smoke run before reporting the PR handoff complete.

Create a draft PR, not a ready-for-review PR.

The PR body should include:

- Spec path.
- Architecture model packet path or `not used`.
- Mode used: `full` or `lite`.
- Build summary.
- Verifier result or results.
- PR review gate result or results.
- Tracking issue or bead, when required by the repo.
- Post-merge `main` CI/deploy and production smoke Bead plus expected trigger, when required.

## Post-Merge Production Smoke Stage

Production smoke is repo-specific. Use the repo-local smoke skill or runbook instead of inventing a new script. For Birthday Club Hub, use `eclub-prod-smoke` after the `Test and Deploy` workflow succeeds on `main` and deploys the agent's code.

Before or while creating the draft PR:

- Create or claim a Beads task for monitoring `main` CI/deploy and running production smoke after this PR merges.
- Link it to the PR's tracking Bead when available, or reference the PR branch/URL in a Beads comment once the PR exists.
- Record the deploy workflow, target branch, smoke skill/runbook, and credential path needed for the autonomous run.
- Do not close this Bead at draft PR time unless the smoke has actually run.

After the PR is merged:

- Resolve the merge SHA on the target base branch and monitor hosted CI for that exact SHA on the base branch. For Birthday Club Hub, watch the GitHub Actions `Test and Deploy` push run on `main`.
- If `main` CI fails before deployment, record the failure evidence in the smoke Bead and stop for repair.
- If the workflow succeeds and deployed the agent's code to production, run the repo-defined production smoke against production and record pass/fail evidence in the smoke Bead.
- If the workflow succeeds but did not deploy the agent's code, record the no-deploy evidence in the smoke Bead and close it without running production smoke.
- Use only noninteractive credentials: GitHub Actions secrets, a 1Password service account, 1Password Connect, or an already-approved runner secret.
- Do not trigger desktop-biometric, Touch ID, or other interactive 1Password prompts for autonomous post-deploy smoke.
- If credentials or network access are unavailable without prompting, stop with `Authentication unavailable` or `Network unavailable`, leave the smoke Bead open, and record the blocker.
- Close the smoke Bead only after recording the actual CI/deploy/smoke outcome. A failed smoke is still a completed smoke run with production-incident evidence.

## Stop States

Stop and report clearly when:

- Required input cannot be resolved.
- A destructive reset would be unsafe.
- A gate returns `HUMAN DECISION`.
- A full-mode gate cannot be made to pass after a technically valid fix path is exhausted.
- Authentication, credentials, network, hosted CI, or PR creation is unavailable.
- Required post-merge `main` CI/deploy monitoring or production smoke cannot run autonomously because the only credential path would prompt a human.

## Report Format

Report:

```markdown
Agent-driven PR workflow complete.
Mode: full | lite
Spec: <path>
Architecture model: <path | not used>
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
Post-merge CI/deploy and smoke:
<not required | bead pending merge/deploy | main CI failed | no deploy for this code | PASS/FAIL evidence>
Residual risk:
- <risk or none>
```

If stopped early, replace the first line with:

```markdown
Agent-driven PR workflow stopped.
Reason: <specific stop state>
Next required action: <specific action>
```
