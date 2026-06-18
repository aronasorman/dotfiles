---
name: agent-driven-pr-workflow
description: Use when working from an approved spec, or when Beads-tracked PR work needs a spec created first, and the user asks for the full or lite agent-driven build-to-draft-PR process or autonomous post-merge main CI, deploy, and production-smoke follow-through for an agent-created PR.
---

# Agent-Driven PR Workflow

Orchestrate a feature-scoped approved spec through build, verification, hard review, optional second pass, draft PR creation, and required post-merge `main` CI/deploy monitoring plus production smoke follow-through.

Full and lite both use hard gates and iterate until they pass. Full mode adds
a second implementation pass; lite mode stops after one implementation pass.

This skill normally starts from an approved spec. If the target bead has no readable spec or design attachment, the controller first writes one with `feature-spec-writing`, saves it to a repo file, syncs it to the bead, and then runs `spec-review-gates` until PASS before implementation. The spec is for the feature or PR slice being built, not a full-project design unless the project bootstrap itself is the feature.

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

The controller must not do the normal build implementation manually.

Full-mode build work belongs to Claude Code in Opus mode. Invoke it with
streamed progress enabled:

```bash
claude -p "$(cat /private/tmp/<slug>-build-prompt.md)" \
  --model opus \
  --output-format stream-json \
  --include-partial-messages \
  --verbose
```

Use the streamed JSON form so long-running implementation work exposes tool,
hook, and partial-message progress instead of appearing hung. Extract the final
assistant text for the workflow log.

Lite-mode build work may still use `subagent-driven-development` unless the
user explicitly asks for the full-mode Claude Opus builder.

Fresh or external gates own:

- `spec-review-gates` review work.
- Claude Code Opus build work in full mode.
- `subagent-driven-development` build work in lite mode.
- `verifier` verification work.
- `pr-review-gates` opposite-family review work.

## Inputs

Resolve these before starting:

- Spec path, or the bead id whose design/spec attachment must be checked and populated.
- Architecture model packet path or Beads attachment, if the spec includes one.
- Repo/worktree path.
- Target base branch for reset, review, push, and draft PR.
- Mode: `full` or `lite`.
- Publication boundary: `internal/controlled` or `external/uncontrolled`. Treat
  any external forge, public PR, customer/vendor repo, fork, or repo Aron does
  not control as `external/uncontrolled` unless the user explicitly says
  internal workflow artifacts may be published there.
- Any repo instructions for Beads, issue tracking, PR body format, tests, or hosted CI.
- Any repo-local production smoke skill or runbook, target `main` CI/deploy workflow, and noninteractive credential path needed to run it autonomously.

If the repo path or base branch cannot be resolved safely, stop before making changes. If no spec path can be resolved from inputs or Beads, run the spec bootstrap before implementation.

## Publication Boundary

The spec, gate logs, workflow notes, one-off validation scripts, and similar
agent artifacts are internal working material. In `internal/controlled` repos
they may be committed only when the repo convention or user request makes them
part of the intended review surface.

In `external/uncontrolled` repos:

- Do not stage, commit, push, or publish the spec, internal design notes,
  gate-score artifacts, workflow logs, or generated one-off validation scripts.
- Keep specs in Beads, Obsidian, `/private/tmp`, or another local/internal
  location outside the target repo worktree. The builder may read them, but the
  PR branch must not carry them.
- Keep one-off validation scripts ephemeral. Prefer existing repo test
  harnesses; commit only durable tests that belong in the project.
- The pushed branch should contain only actual implementation code, durable
  tests, and files that are required by the target repo's normal contribution
  process.
- Scrub the PR body of spec paths, internal gate scores, verifier transcript
  paths, validation-script paths, and internal tracker links unless the user
  explicitly approves publishing those exact details.

## Spec Bootstrap

Before choosing full or lite implementation steps, ensure there is a readable feature-scoped spec for the build phase:

1. Inspect the tracking bead with `bd show <bead-id> --json` when a bead id is available.
2. Treat an existing `spec-id`, design attachment, or repo spec path as the spec only if it resolves to a readable file.
3. If no usable spec exists, invoke `feature-spec-writing`.
4. Save the spec to a repo-local file only when the publication boundary is
   `internal/controlled` and repo conventions allow it. Prefer
   `history/port-specs/<feature-slug>.md` unless repo conventions point
   somewhere more specific. For `external/uncontrolled` repos, save the spec in
   Beads, Obsidian, `/private/tmp`, or another local/internal location outside
   the target repo worktree.
5. Sync the file to the bead:

```bash
bd update <bead-id> --spec-id <spec-path> --design-file <spec-path> --json
bd comment <bead-id> "Spec materialized at <spec-path>; synced to bead design/spec-id."
```

6. Continue with `spec-review-gates` and iterate until PASS.
7. Confirm the final spec path is readable and describes the feature or PR slice at enough detail for the builder to work from it.

If Beads is unavailable, record the spec path in the repo artifact or PR body
only for `internal/controlled` repos. For `external/uncontrolled` repos, keep
the spec path local/internal and continue only when the user provided another
tracking source. Do not silently skip the spec materialization step.

## Build-Phase Spec Contract

No build phase starts without a readable spec path.

Before invoking Claude Code Opus or `subagent-driven-development`, the controller must:

- Pass the spec path to the builder.
- State that the spec is the authoritative feature-level contract.
- Include architecture model packet path when present.
- Include any spec review gate findings that materially shape implementation.
- State the publication boundary, and for `external/uncontrolled` repos tell
  builders and verifiers that specs, workflow notes, and one-off validation
  scripts must remain uncommitted.
- Stop if the only available requirements are chat history, bead title, issue summary, or inferred intent.

The spec should be as detailed as the feature requires. Do not expand it into a whole-project design unless the requested work is project bootstrap.

## Full Mode

Run this sequence continuously. Do not pause between stages unless blocked, a gate returns `HUMAN DECISION`, or the next destructive operation cannot be made safe.

1. Resolve or bootstrap the feature-scoped spec, run `spec-review-gates` on it, and confirm the build-phase spec contract. Iterate until PASS.
2. Build with Claude Code Opus using `--include-partial-messages --verbose`.
3. Run `verifier` as a fresh-context gate. Iterate until PASS or `HUMAN DECISION`.
4. Run `pr-review-gates` with writer recorded as Claude Opus so the hard reviewer is a Codex subagent. Iterate until PASS or an issue needs human judgment.
5. Run the second pass in the controller context.
   - Use the existing `second-pass` workflow if available.
   - Treat the V1 implementation as evidence, not a template.
   - Fold durable learnings into the spec.
   - Reset the code worktree to the target base branch.
   - Preserve the intended spec update if the spec lives inside the worktree.
6. Rerun `spec-review-gates` on the updated spec and reconfirm the build-phase spec contract. Iterate until PASS before any second build starts.
7. Build again with Claude Code Opus from the updated spec, using `--include-partial-messages --verbose`.
8. Run `verifier` again as a fresh-context gate. Iterate until PASS or `HUMAN DECISION`.
9. Rewrite the final commit history into a better developer narrative that is easy to review commit by commit.
10. Run `pr-review-gates` again with writer recorded as Claude Opus so the hard reviewer is a Codex subagent. Iterate until PASS or an issue needs human judgment.
11. Push the branch and create a draft PR.
12. Create or confirm the post-merge `main` CI/deploy and production smoke Bead when required.

## Lite Mode

Lite is a single implementation pass with hard gates and iteration. Iterate
inside this one pass until each gate returns `PASS` or reaches a stop state.

1. Resolve or bootstrap the feature-scoped spec, run `spec-review-gates` on it, and confirm the build-phase spec contract. Iterate until PASS.
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

For `pr-review-gates`, use the opposite family reviewer from the implementation writer whenever possible. In full mode, the implementation writer is Claude Opus, so the code reviewer must be a Codex subagent. Do not run Claude Opus as the hard reviewer for code it authored. Do not push or create the draft PR until the required review gates have passed for the selected mode.

## Commit Narrative Stage

Before the final `pr-review-gates` run and before pushing, rewrite the commit history so it is easy to review commit by commit.

The commit stack should:

- Tell a coherent developer narrative from foundation to behavior to verification.
- Keep mechanical setup, behavior changes, tests, and documentation/spec updates in reviewable commits.
- Avoid one large "misc fixes" commit when the changes can be split by concern.
- Avoid tiny noise commits that force reviewers to reconstruct intent across many diffs.
- Preserve authorship and avoid rewriting unrelated user commits.

For `external/uncontrolled` repos, override the normal documentation/spec
commit guidance: the final commit stack must exclude internal specs, workflow
notes, generated gate artifacts, and one-off validation scripts. Commit only the
actual code change, durable tests, and required contribution files.

If the branch has already been pushed or is shared, do not force-push rewritten history unless the user has explicitly approved that for the branch.

## Draft PR Stage

After the commit narrative stage and final gates pass:

- Check `git status`.
- Ensure only intended files are included.
- For `external/uncontrolled` repos, check `git diff --name-status <base>...HEAD`
  and remove any internal spec, gate artifact, workflow log, or one-off
  validation script before final gates, push, or PR creation.
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

For `external/uncontrolled` repos, replace the internal-heavy PR body with a
public-safe body containing only the change summary, durable tests/checks run,
and public-safe issue references. Do not mention internal spec paths, gate
scores, verifier transcript paths, validation-script paths, Beads links, or
private workflow details unless the user explicitly approves the exact text.

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
- A readable feature-scoped spec is not available by the time build would start.
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
Spec: <path> (<pre-existing | bootstrapped with feature-spec-writing>)
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
