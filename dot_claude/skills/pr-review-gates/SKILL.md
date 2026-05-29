---
name: pr-review-gates
description: Use when preparing to push code to a PR branch, open or update a pull request, push a branch that is about to become a PR, or run pre-PR review gates.
---

# PR Review Gates

This is the required pre-push workflow for code that will open or update a PR.
It is a skill workflow, not a git hook; use it before `git push`, PR creation,
PR updates, or "yeet" style publish requests.

## Required Order

1. Determine the repo/worktree, intended PR base branch, and which CLI authored
   the change.
   - Prefer the existing PR target branch if a PR exists.
   - Otherwise use the feature/workstream target branch the user named or the
     branch already established in the current task.
   - Treat the current agent as the writer unless the user or commit context
     clearly says otherwise.
   - If the base or writer cannot be discovered safely, ask before reviewing or
     pushing.
2. Run local quality gates for the changed code first.
   - Tests for changed behavior.
   - Build/typecheck/lint commands appropriate to the repo and blast radius.
   - If any required local gate fails, the review result is `ITERATE`; fix the
     failure or surface it before asking an agent to score the change.
3. Run the hard-gate agent review with the opposite reviewer family from the
   writer:
   - If Codex wrote the code, use Claude Opus.
   - If Claude wrote the code, use GPT-5.5 via Codex.
   - If another tool wrote the code, pick the reviewer least involved in the
     implementation and record that choice.
4. Loop on the hard-gate agent review until it passes.
5. After the hard gate passes, run CodeRabbit if `coderabbit` is installed.
6. Push/open/update the PR only after local gates pass, the hard gate passes,
   and CodeRabbit has no meaningful unresolved feedback or the user has decided
   how to handle that feedback.

## Hard-Gate Prompt

Write the prompt to `/private/tmp/<repo>-pr-review-gate-prompt.md` so the full
rubric and local gate evidence are reproducible.

Use this prompt shape:

```text
Run the PR review hard gate on the current <REPO> PR branch and working tree
against <BASE_BRANCH>. You are allowed to read the source code and diff as
needed. Review the effective diff that would be pushed, including uncommitted
working-tree changes.

Prerequisite build/test gate evidence already passed in this worktree after the
latest fixes:
- <COMMAND>: PASS, <SUMMARY>
- <COMMAND>: PASS, <SUMMARY>

Review only the diff from <BASE_BRANCH> plus local working-tree changes.

First determine the architecture mode for this change:
- Current-shape preservation: default. Use this for features, bugfixes,
  migration slices, mechanical ports, parity changes, and narrow
  implementations.
- Architecture refactor/rethink: use only when the task explicitly asks to
  change architecture, introduce a new pattern, move boundaries, or replace an
  existing design.

If the mode is current-shape preservation, review architecture by comparing the
diff to the app's existing shape. The implementation should fit the current
architecture unless the task explicitly authorizes a redesign.

Apply these six lenses and provide feedback for each lens:
1. Test completeness - new code has tests, all pass, edge cases covered
2. Correctness - bugs, data integrity, error handling, race conditions
3. Simplicity - least code that works, no over-engineering
4. Commit story - commits tell a narrative reviewable commit-by-commit
5. Excellence - would a human be proud to ship this?
6. Architecture - follows the current app shape for this kind of change.
   Required inspection:
   - Read the repo architecture instructions.
   - Identify at least 2 nearby existing files or patterns with the same role
     as the changed code.
   - List new exported types, interfaces, repository methods, app
     commands/queries, and handler dependencies.
   - Check whether the diff preserves existing dependency direction, package
     ownership, naming style, interface granularity, and use-case boundaries.
   For current-shape preservation tasks:
   - Score 5 when the change fits nearby patterns with no meaningful new
     architectural surface.
   - Score 4 when there are small naming or placement concerns but the shape is
     still clearly native to the app.
   - Score 3 when the change works but introduces questionable abstractions,
     page-shaped domain concepts, confusing exported methods, or broader
     interfaces than the task requires.
   - Score 2 when the change bends layer boundaries, invents a parallel
     pattern, or makes future changes harder without clear need.
   - Score 1 when it violates architecture rules or routes data access around
     the intended app/domain/infrastructure boundaries.
   For explicit architecture refactor/rethink tasks, score against the stated
   architectural intent and migration plan, not merely against the old shape.

Hard gate: all lenses must score 4+/5 AND total must be at least 27/30. If any
lens is below 4 or total is below 27, result is ITERATE.

Output exactly:
- Architecture mode: current-shape preservation or architecture refactor/rethink
- Nearby patterns inspected: at least 2 files or patterns
- New architectural surface: exported APIs, repositories, handlers, queries, or
  none
- Result: PASS or ITERATE
- Total: X/30
- Lens scores: each lens score plus concise rationale
- Significant findings to surface before push
- Concrete next actions
```

## Reviewer Commands

If Codex wrote the code, run Claude Opus:

```bash
claude -p "$(cat /private/tmp/<repo>-pr-review-gate-prompt.md)" --model opus --output-format stream-json --include-partial-messages --verbose
```

Use the streamed JSON form so long-running reviews show hook, tool, and partial
message progress instead of appearing hung. When reporting the gate result,
extract the final assistant text from the stream and summarize only the scored
review.

If Claude wrote the code, run GPT-5.5 via Codex:

```bash
codex exec -m gpt-5.5 -C <repo-path> --sandbox read-only "$(cat /private/tmp/<repo>-pr-review-gate-prompt.md)"
```

Do not use the same agent family that wrote the code for the hard-gate review
unless the opposite reviewer CLI is unavailable and the user explicitly approves
the fallback.

If the hard gate returns `ITERATE`, verify each finding against the code. Patch
concrete findings that are technically valid and reasonably scoped, then rerun
local quality gates and the hard gate. If a finding is architectural,
security-sensitive, ambiguous, or would materially change the implementation,
surface it to the user before rewriting the approach.

Do not proceed to CodeRabbit until the hard gate reports `PASS` with every lens
at least 4/5 and total at least 27/30.

## CodeRabbit Gate

After the hard gate passes, check whether CodeRabbit is available:

```bash
command -v coderabbit
```

If installed, run it against the same base:

```bash
coderabbit review --agent --base <BASE_BRANCH> -c <repo-guidance-file>
```

Use the repo guidance file the agents actually read, such as `CLAUDE.md`,
`AGENTS.md`, or a more specific project guidance file. When running from Codex,
run CodeRabbit outside the sandbox if sandboxing blocks auth, network, or its
local callback server.

If `coderabbit` is not installed, skip this gate and report that it was skipped
because the CLI was unavailable.

Meaningful CodeRabbit feedback means critical/major findings, security or data
safety risks, correctness issues, architecture/layering concerns, migration or
compatibility risks, or anything whose fix would change the design. Surface
meaningful feedback to the user for a relevance decision before patching.

Obvious small mechanical fixes can be handled directly. If any fix changes the
diff after the hard gate has passed, restart from local quality gates, rerun the
hard gate, then rerun CodeRabbit when available.

## Reporting

When done, report:

- Local quality gates run and their result.
- Writer CLI and hard-gate reviewer used.
- Hard-gate result, total score, and whether it passed.
- CodeRabbit status: run/skipped, issue count, and any meaningful findings.
- Whether the branch was pushed or why it was not.
