---
name: verifier
description: "Use when the user explicitly invokes /verifier or asks to independently verify a completed implementation with test runs, build/type/lint gates, focused smoke QA, and test-adequacy assessment before QA friction or hard review gates. Observational and verification-only: do not fix code, edit docs, update specs, create beads, create tests, or implement missing behavior."
---

# /verifier

Independently prove that the current implementation compiles, runs, works, and is adequately tested.

Do not fix code.
Do not edit docs.
Do not update the PER spec.
Do not create beads.
Do not create follow-up work items.
Do not create tests.
Do not implement missing behavior.
Do not run hard review gates.

## Execution Model

The verifier is a fresh-context gate, not work performed by the same agent
session that wrote the implementation.

When invoked:

1. Dispatch a new verifier subagent. In Codex, use `spawn_agent` with
   `fork_context=false`. Pass only the focus, repo/worktree path, relevant
   implementation summary, relevant spec or acceptance criteria, current git
   diff context, known command results, and these verifier instructions.
2. The verifier subagent runs the verification workflow and returns the
   Verification Packet. It must not edit files.
3. The implementation agent/session reviews the packet. If the result is
   `FAIL`, the implementation agent/session fixes the code in its original
   context and then dispatches a new fresh verifier subagent for the next
   iteration.
4. Repeat until the verifier returns `PASS` or `HUMAN DECISION`.

Freshness comes from isolated subagent context, not from an opposite-model
review. Do not require Claude-vs-Codex model switching for this gate.

If no subagent facility is available, stop and report that `/verifier` requires
a fresh verifier subagent. Do not self-verify in the implementation context.

## Usage

```text
/verifier <natural-language focus>

Examples:

/verifier
/verifier checkout retry
/verifier the feature we've been working on
/verifier login flow after the V1 build
/verifier run the required checks and smoke QA for this branch
```

The text after `/verifier` is a natural-language focus, not structured positional arguments.

Do not require exact commands, spec paths, routes, or test names when they can be derived from the current repo and workflow context.

## Purpose

Use this after a build pass and before QA friction or hard review gates.

The verifier answers:

- Does the implementation pass required local gates?
- Do relevant tests pass?
- Were obvious checks missed?
- Is test coverage adequate for the changed surface and risk?
- Does the feature basically work from the relevant user, developer, API, CLI, or docs entrypoint?
- If an architecture model packet is present, does the implementation preserve the modeled shape and constraints?
- What exact evidence supports PASS, FAIL, or HUMAN DECISION?

This is verification, not code review. Architecture fit, simplicity, summary truth, and hard-review rubric belong to hard review gates.

This is not QA friction. Verifier performs focused smoke QA to prove behavior works; `/qa-friction` performs a colder friction pass to find confusing or surprising use.

## Context Resolution

Derive the concrete verification task from available context.

Use this priority order:

1. Natural-language focus from the command line
2. Current conversation/task context
3. Implementation summary or patch notes, if present
4. PER spec, architecture model packet, or acceptance criteria for the current feature
5. Recent test results, QA notes, or failure logs
6. Git branch name
7. Current git diff
8. Changed routes, docs, APIs, CLI commands, tests, or UI surfaces
9. Repo-local verification or QA config
10. Local app/docs/API conventions

If the feature, changed surface, or verification target cannot be determined from available context, stop and report exactly what is missing.

## Success Criteria

If explicit success criteria are missing, infer them from the implementation just written.

Use:

1. Current git diff
2. Changed routes, commands, APIs, docs, tests, configs, and UI surfaces
3. New or changed user-visible behavior
4. New or changed developer-visible behavior
5. New or changed error paths, loading states, auth/session behavior, permissions, data writes, background jobs, retries, or external calls
6. Architecture model packet states, transitions, invariants, assumptions, and verifier obligations, if present
7. Existing nearby tests and conventions
8. Existing behavior implied by unchanged adjacent code

Turn those observations into concrete verification success criteria before running checks.

Examples:

- A new button or form implies verifying the happy path, disabled/loading state, validation failure, submission failure, and duplicate-submit behavior.
- A new API endpoint implies verifying documented request shape, success response, error response, auth/permission behavior, and persistence or side effects.
- A changed CLI command implies verifying help text, required flags, success output, bad-input output, exit status, and unintended mutation risk.
- A docs/onboarding change implies verifying command order, prerequisites, environment variables, and time-to-first-success blockers.

Do not treat inferred success criteria as product requirements. Mark them as verifier-inferred criteria in the report.

## Required Behavior

When invoked:

1. Identify the feature or workflow to verify.
2. Identify the current repo/worktree and relevant git state.
3. Read applicable repo instructions before running commands.
4. Infer success criteria from the spec, task context, diff, changed behavior, or implementation just written.
5. If an architecture model packet is present, derive model-conformance checks from its states, transitions, invariants, assumptions, and verifier obligations.
6. Identify required gates: build, compile, lint, typecheck, unit, integration, e2e, contract, coverage, smoke, API, CLI, or docs commands.
7. Run checks already reported when needed to confirm evidence.
8. Run checks that were missed but should have been run.
9. Perform focused smoke QA for the changed behavior when the target surface supports it.
10. Capture browser console/network evidence if available and useful.
11. Assess whether tests cover the changed behavior and risk.
12. Assess whether implementation shape matches any architecture model packet without requiring textual or syntactic similarity to Quint.
13. Record exact commands, pass/fail status, relevant output summary, and residual risk.
14. Produce a compact verification packet with PASS, FAIL, or HUMAN DECISION.

## Command Selection

Choose commands from the strongest available source:

1. User-provided commands
2. Repo instructions: `AGENTS.md`, `CLAUDE.md`, README, docs, QA config
3. Implementation summary or recent evidence
4. CI config
5. Package scripts, Makefile, Justfile, Taskfile, language tooling
6. Framework conventions
7. Focused commands derived from changed files

Prefer the smallest command set that gives real confidence, but do not skip broad gates when the change has broad blast radius.

If a command needs network, credentials, production access, destructive mutation, or external side effects, do not run it without explicit approval. Record it as blocked or requiring human decision.

## Result

Use `PASS` only when required gates passed, focused smoke QA supports the changed behavior when applicable, test coverage is adequate for the risk, and no blocking verification gaps remain.

Use `FAIL` when a required gate fails, the feature behavior fails, the implementation cannot run, required test coverage is missing for high-risk behavior, or evidence contradicts the implementation claim.

Use `HUMAN DECISION` when verification depends on unavailable credentials, environment, production access, destructive actions, ambiguous requirements, or policy/product judgment.

`FAIL` is an iteration gate, not a final verdict on the project. Return the
packet to the implementation agent/session, let it fix the implementation, and
rerun `/verifier` in a new fresh subagent. Do not let the implementation
agent/session downgrade, reinterpret, or override the verifier result.

## Output Format

Produce this report:

```markdown
# Verification Packet
## Focus
<natural-language focus supplied by user, or inferred current feature>
## Inferred Context
Feature:
<feature/spec/task/workflow inferred>
Source context:
<conversation / implementation notes / PER spec / branch / diff / repo instructions>
Worktree:
<path and git branch/ref>
Changed surface:
<files, routes, APIs, CLI commands, docs, or behavior verified>
Architecture model:
<packet path and model-conformance focus, or "not present">
Success criteria:
- <explicit criterion>
- <verifier-inferred criterion, marked as inferred>
## Verification Plan
Required gates:
- <gate>
Additional checks:
- <check verifier added>
Smoke QA:
- <focused QA path, or none>
## Commands Run
| Command | Purpose | Result | Notes |
|---|---|---|---|
| `<command>` | <why run> | pass / fail / blocked / skipped | <short evidence> |
## Smoke QA Evidence
- <route / command / docs entrypoint / API endpoint exercised>
- <observed result>
- <console/network/runtime evidence if available>
## Test Adequacy
Adequate:
yes | no | partial | not assessed
Changed behaviors covered:
- <behavior>
Gaps:
- <gap, or "No material gaps found.">
## Model Conformance
Present:
yes | no
States and transitions:
- <mapped / missing / not applicable>
Invariants:
- <covered / missing / not applicable>
Assumptions and abstractions:
- <respected / violated / not applicable>
## Findings
No verifier findings.
## Result
PASS | FAIL | HUMAN DECISION
## Residual Risk
- <risk>
## Next Gate
Ready for:
QA friction | hard review gates | builder follow-up | human decision
```

If there are findings, replace `No verifier findings.` with concise numbered findings.

## Report Rules

Keep the report concise and evidence-oriented.

Do not include an implementation plan.

Do not rewrite the PER spec.

Do not create follow-up beads.

Do not modify product code, docs, tests, or configuration.

Do not hide failed tests, weaken checks, or suppress errors to make work appear done.

If sufficient context exists, do not ask clarifying questions.

If context is genuinely insufficient, stop and report what is missing.
