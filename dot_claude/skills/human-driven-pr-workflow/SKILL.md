---
name: human-driven-pr-workflow
description: Use when the user wants to personally implement a PR-scoped change from an agent-reviewed spec, including command-guided, bootstrap, or BDD/TDD pairing.
---

# Human-Driven PR Workflow

Orchestrate a feature-scoped spec-reviewed PR workflow where agents own spec writing, review, verification, code review gates, commit narrative, push, and draft PR creation, while the build implementation phase is guided for the human developer.

This is the counterpart to `agent-driven-pr-workflow`: the review machinery stays agent-driven, but the code build is a walkthrough.

## Core Contract

The user is the build driver.

The controller is the navigator:

- Read local instructions and repo context.
- Resolve or bootstrap the spec.
- Run spec review gates and apply required spec edits.
- Stop for human spec review after spec review gates pass, and do not begin build until the human passes the spec or feedback has been incorporated and re-reviewed.
- During build, provide exact commands, one reversible step at a time.
- During build, write failing tests or behavior specs when the selected build mode allows it.
- Interpret pasted output, test failures, diffs, and screenshots.
- Receive feedback on the spec, justify decisions from evidence, and update the spec when feedback changes requirements or exposes a bad assumption.
- After build, run verifier and PR review gates, iterating back through the human-guided build loop when fixes are needed.
- Prepare the commit narrative, push the branch, and create a draft PR after required gates pass.

Default autonomy:

- The controller may run read-only inspection commands when useful.
- The controller may edit specs, tests, fixtures, docs, gate artifacts, PR text, and tracking comments as required by the workflow.
- The controller must not edit production implementation during the build phase. If the user wants the agent to patch production code, stop and switch to the appropriate non-human-driven workflow after explicit confirmation.
- In BDD/TDD build mode, the controller may edit tests, fixtures, docs, or specs that define desired behavior; the user writes the implementation that makes them pass.
- In command-only mode, the controller gives commands and waits for pasted output instead of running commands locally.
- Push and draft PR creation are part of this workflow after verifier and PR review gates pass, unless the user explicitly asks to keep those steps human-run.

Do not silently fall back to `agent-driven-pr-workflow`. If the user asks the agent to take over implementation, state that this changes the workflow and use the appropriate autonomous skill only after explicit confirmation.

## Build Modes

Default the build phase to `command-guided` unless the user explicitly asks for tests-first, BDD, TDD, pairing, or project bootstrap.

- `command-guided`: the controller gives exact commands and explains expected signals; the user edits code.
- `bdd-tdd`: the controller writes one failing behavior test at a time, verifies RED, then the user writes production code until GREEN.
- `bootstrap`: the controller guides project setup through commands, first smoke test, initial verification, and baseline commit.
- `hybrid`: combine command-guided steps with BDD/TDD slices when a feature has behavior worth pinning down.

## Inputs

Resolve before execution:

- Goal, feature, bug, or bootstrap target.
- Repo or project path.
- Desired build mode.
- Whether the controller may run read-only commands or should only provide commands.
- What file categories the controller may edit, if any.
- Existing spec, issue, bead, or tracking source, if any.
- Architecture model packet path or Beads attachment, if the spec includes one.
- Target base branch and whether a PR is expected.
- Local instructions: `AGENTS.md`, `CLAUDE.md`, `GEMINI.md`, repo docs, and project-specific runbooks.
- Any repo-local production smoke skill or runbook required after merge.

If the repo path, base branch, build mode, or implementation edit boundary cannot be resolved safely, stop and ask for the missing decision.

## Orientation And Safety

Start with safety and context:

1. Read applicable instruction files.
2. Check whether the current path is a git repo.
3. Check `git status --short` before suggesting edits.
4. Check for Beads or other repo-local tracking when present.
5. Identify package manager, test runner, build commands, and app entry points from repo files instead of guessing.
6. Summarize only the constraints that affect this work.

If there are unrelated user changes, preserve them. Do not suggest reset, checkout, clean, or force operations unless the user explicitly asks for that exact destructive action.

## Spec Bootstrap And Review

Before the build walkthrough:

1. Inspect the tracking bead with `bd show <bead-id> --json` when a bead id is available.
2. Treat an existing `spec-id`, design attachment, or repo spec path as the spec only if it resolves to a readable file.
3. If no usable spec exists, invoke `feature-spec-writing`.
4. Save the spec to a repo-local file. Prefer `history/port-specs/<feature-slug>.md` unless repo conventions point somewhere more specific.
5. Sync the file to the bead when Beads is available:

```bash
bd update <bead-id> --spec-id <spec-path> --design-file <spec-path> --json
bd comment <bead-id> "Spec materialized at <spec-path>; synced to bead design/spec-id."
```

6. Run `spec-review-gates` and iterate until `PASS`.
7. Confirm the final spec path is readable and describes the feature or PR slice at enough detail for the human builder to work from it.
8. If the spec includes an architecture model packet, carry that packet into verifier runs.

If Beads is unavailable, record the spec path in the repo artifact or PR body and continue only when the user provided another tracking source.

## Human Spec Review Gate

After `spec-review-gates` returns `PASS`, stop for human review before implementation.

Present:

- Spec path.
- Spec review gate result.
- Feature scope covered by the spec.
- Non-goals and assumptions that constrain implementation.
- Expected build mode.

Human outcomes:

- `PASS`: continue to the build-phase spec contract and work contract.
- `FAIL with feedback`: invoke `receiving-spec-feedback`, apply accepted spec edits, rerun `spec-review-gates`, then repeat this human review gate.
- `HUMAN DECISION`: stop until the decision is resolved.

Do not start command-guided, BDD/TDD, bootstrap, or hybrid build work before this gate passes.

## Build-Phase Spec Contract

No build phase starts without a readable spec path that has passed both `spec-review-gates` and the human spec review gate.

Before the build walkthrough, the controller must:

- Treat the spec as the authoritative feature-level contract.
- Keep the spec level of detail proportional to the feature or PR slice, not the whole project.
- Include architecture model packet path when present.
- Include any spec review gate findings or human review feedback that materially shape implementation.
- Stop if the only available requirements are chat history, bead title, issue summary, or inferred intent.

Do not expand the spec into a whole-project design unless the requested work is project bootstrap.

## Work Contract

After spec review and human spec review pass, and before build implementation, write a compact contract:

```markdown
Human-driven workflow ready.
Build mode: command-guided | bdd-tdd | bootstrap | hybrid
Spec: <path>
Architecture model: <path | not used>
Repo: <path>
Goal: <one sentence>
Non-goals:
- <items, if any>
Controller may edit: <spec/tests/docs/gate artifacts/PR text/tracking>
Human owns during build: <production implementation | all commands | other>
Success signals:
- <focused build checks>
- <final verifier/review gate expectations>
Tracking: <bead/issue/spec path | not present>
Human spec review: PASS
```

For a one-line fix, keep this contract short. For broad or ambiguous work, decompose before build.

## Spec Feedback During Build

During the walkthrough, the user may challenge the spec, ask why a decision was made, or provide new constraints.

For directed spec feedback after `spec-review-gates` has passed, invoke `receiving-spec-feedback` and follow its feedback ledger, edit, and review loop. If that skill is unavailable, use the same categories explicitly:

- `correction`: fix the spec and explain the prior incorrect assumption.
- `preference`: state the tradeoff and whether the preference changes the implementation contract.
- `scope change`: stop the build loop until the user accepts the scope change and the spec is updated.
- `re-think`: pause implementation, revise the design, and rerun required spec review gates.

When justifying a spec decision:

- Cite repo evidence, user requirements, constraints, or gate feedback.
- Acknowledge uncertainty instead of defending weak assumptions.
- Update the spec when the feedback changes behavior, data shape, architecture, risk, or success criteria.
- Rerun `spec-review-gates` after material spec changes before continuing build.
- If material feedback is accepted during build, repeat the human spec review gate before continuing implementation.

## Driver Cards

Use driver cards for human-run steps. Give one card at a time by default. Use a short batch only when the commands are atomic and low risk.

````markdown
Step <n>: <purpose>
cwd: <absolute path>
risk: read-only | local edit | install | network | destructive
command:
```bash
<exact command>
```
expected success:
<specific output, exit code, file, screenshot, or state>
paste back:
<the exact output or file snippet needed>
rollback:
<how to undo this step, or "not needed">
````

Rules:

- Never say "run the tests" without the exact command.
- Include the expected success signal before the user runs the command.
- Include a paste-back target so the next step has concrete evidence.
- Mark network, install, destructive, credentialed, push, deploy, and PR commands clearly.
- Split risky work into smaller reversible cards.
- If the command fails, diagnose from the output and issue a new card; do not stack speculative commands.

## BDD/TDD Loop

Use this loop during the build phase when the user asks for BDD, TDD, tests-first pairing, or "write failing tests and I will make them pass."

1. Select one behavior slice.
2. State the scenario in Given/When/Then or equivalent plain language.
3. Write one focused failing test or behavior spec.
4. Give the RED driver card and verify the failure is for the expected reason.
5. If the test passes immediately, rewrite it; it did not prove the missing behavior.
6. The user writes production code.
7. Give the GREEN driver card and interpret the result.
8. After GREEN, allow refactor-only cleanup while preserving the passing signal.
9. Repeat with the next behavior slice.

Test rules:

- Test behavior, not implementation details.
- Prefer existing test framework and local patterns.
- Avoid mocks unless the real dependency is slow, nondeterministic, paid, destructive, or unavailable.
- Keep each test narrow enough that the human can implement the next step without a large hidden design jump.
- Do not edit production code in this mode unless the user explicitly changes the boundary.

After the final GREEN for a slice, run only the quick confirmation needed to prove the guided build step works. Full verification belongs to the post-build gates.

## Bootstrap Loop

Use this loop during the build phase when the user wants to bootstrap a project themselves.

1. Identify target stack, package manager, runtime versions, and deployment expectations.
2. Prefer official scaffolding commands or existing organization templates.
3. Give preflight driver cards for tool versions and destination directory.
4. Give scaffold commands with risk and rollback.
5. Add or guide creation of the smallest smoke test that proves the project runs.
6. Verify install, format, lint, test, and run commands.
7. Guide the baseline commit only after the smoke signal passes.

Do not add large architecture, auth, database, CI, or deployment machinery during bootstrap unless the user explicitly includes it in scope.

## Post-Build Gates

After the human-guided build phase and quick confirmation:

1. Run `verifier` as a fresh-context gate. Include the spec, implementation summary, success criteria, and architecture model packet when present.
2. If verifier returns `FAIL` or `ITERATE`, route the finding back into the human-guided build loop with a focused driver card or BDD/TDD test. Rerun verifier after the fix.
3. Rewrite the final commit history into a coherent developer narrative before final review and push.
4. Run `pr-review-gates`. Use a fresh reviewer and make clear that the production implementation was human-authored under controller guidance.
5. If PR review gates return `FAIL` or `ITERATE`, route fixes back into the human-guided build loop unless the issue is only spec/docs/PR text.
6. Push the branch and create a draft PR after required gates pass.
7. Create or confirm the post-merge `main` CI/deploy and production smoke Bead when required by repo instructions, the spec, or the user.

Commit narrative should:

- Tell the story from spec/test foundation to behavior to verification.
- Keep mechanical setup, behavior, tests, and docs/spec updates reviewable.
- Avoid rewriting unrelated user commits.
- Avoid force-pushing shared branches unless explicitly approved.

The draft PR body should include:

- Spec path.
- Architecture model packet path or `not used`.
- Build mode used.
- Human-guided build summary.
- Assistant-authored artifacts.
- Verifier result or results.
- PR review gate result or results.
- Tracking issue or bead, when required.
- Post-merge `main` CI/deploy and production smoke Bead plus expected trigger, when required.

Do not mark the PR ready for review. Create a draft PR.

## Stop States

Stop and report the blocker when:

- Required repo path or mode cannot be resolved.
- A readable feature-scoped spec is not available by the time build would start.
- The human spec review gate has not passed.
- The worktree has conflicting user changes.
- A proposed command would be destructive and the user has not explicitly approved it.
- The RED test cannot be made to fail for the intended reason.
- Spec feedback changes material requirements and spec review has not rerun.
- A gate returns `HUMAN DECISION`.
- The user asks the controller to implement production code while still expecting a human-driven build.
- Required credentials, paid services, or network access are unavailable.
- Verification fails and the next step would be speculative.
- Push, PR creation, hosted CI, or required production smoke cannot run with available noninteractive credentials.

## Report Format

For checkpoints:

```markdown
Human-driven workflow checkpoint.
Build mode: <mode>
Spec: <path>
Current state: <short summary>
Evidence: <command/output/test signal>
Next driver card:
<driver card>
```

For completion:

```markdown
Human-driven workflow complete.
Build mode: <mode>
Spec: <path>
Architecture model: <path | not used>
Repo: <path>
Goal: <goal>
Human-owned implementation: <summary>
Assistant-authored artifacts: <tests/docs/specs/none>
Verification:
- <signals>
Verifier:
- <result summary>
PR review gates:
- <result summary>
Commit narrative:
<rewrite summary>
Draft PR:
<url or not created with reason>
Post-merge CI/deploy and smoke:
<not required | bead pending merge/deploy | main CI failed | no deploy for this code | PASS/FAIL evidence>
Residual risk:
- <risk or none>
```

If stopped early, replace the first line with:

```markdown
Human-driven workflow stopped.
Reason: <specific stop state>
Next required action: <specific user action or decision>
```
