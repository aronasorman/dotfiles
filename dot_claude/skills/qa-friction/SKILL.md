---
name: qa-friction
description: Use when the user explicitly invokes /qa-friction or asks for a cold-user, cold-developer, API, CLI, docs, or onboarding QA friction pass for a feature. Observational only: do not fix code, edit docs, update specs, create beads, create tests, or implement missing behavior.
---

# /qa-friction
Run a cold-user or cold-developer QA friction pass for the named feature or the current active feature.
This command is observational only.
Do not fix code.
Do not edit docs.
Do not update the PER spec.
Do not create beads.
Do not create tests.
Do not implement missing behavior.

## Execution Model

QA friction is a fresh-context gate, not work performed by the same agent
session that wrote the implementation.

When invoked:

1. Dispatch a new QA friction subagent. In Claude Code, use the Task tool with
   a fresh general-purpose subagent. Pass only the focus, repo/worktree path,
   relevant implementation summary, relevant spec or acceptance criteria,
   current git diff context, known command results, app/docs entrypoints, and
   these QA friction instructions.
2. The QA friction subagent runs the cold-user, cold-developer, API, CLI, docs,
   or onboarding pass and returns the QA Friction Report. It must not edit
   files.
3. The implementation agent/session reviews the report. If the gate result is
   `FAIL`, the implementation agent/session fixes the implementation or docs in
   its original context and then dispatches a new fresh QA friction subagent for
   the next iteration.
4. Repeat until QA friction returns `PASS` or `HUMAN DECISION`.

Freshness comes from isolated subagent context, not from an opposite-model
review. Do not require Claude-vs-Codex model switching for this gate.

If no subagent facility is available, stop and report that `/qa-friction`
requires a fresh QA friction subagent. Do not self-QA in the implementation
context.

## Usage

```text
/qa-friction <natural-language focus>

Examples:

/qa-friction our new login feature
/qa-friction the feature we've been working on
/qa-friction checkout retry
/qa-friction the new developer onboarding docs
/qa-friction our new login feature, especially slow requests
```

The text after /qa-friction is a natural-language focus, not structured positional arguments.

Do not require the user to provide a URL, spec path, route, test account, or exact goal if those can be derived from the current workflow context.

## Purpose

Use the feature as a cold user, cold developer, or external integrator would.

Find friction that normal code review may miss:

* confusing flow
* unclear copy
* missing loading states
* missing error states
* unexpected redirects
* bad or ambiguous docs
* missing setup steps
* broken commands
* slow requests
* failed requests
* duplicate requests
* console errors
* runtime warnings
* auth/session oddities
* workarounds required
* behavior inconsistent with the PER spec

This command should produce a compact markdown QA friction report.

## Context resolution

Derive the concrete QA task from the current active context.

Use this priority order:

1. Natural-language focus from the command line
2. Current conversation/task context
3. Current bead / active work item
4. PER spec for the current feature
5. Recent implementation notes
6. Git branch name
7. Current git diff
8. Changed routes, docs, APIs, CLI commands, or UI surfaces
9. Repo-local QA config
10. Local app/docs/API conventions

The focus string should guide scope selection.

Examples:

/qa-friction our new login feature

Prioritize:

* login route
* auth flow
* session handling
* redirects
* token/link behavior
* loading states
* error states
* duplicate submissions
* slow or failed auth requests

/qa-friction the feature we've been working on

Use:

* current bead
* current branch
* current PER spec
* changed files/routes/docs
* recent conversation context

/qa-friction the new developer onboarding docs

Prioritize:

* docs-only cold-start flow
* setup commands
* missing prerequisites
* ambiguous env vars
* wrong command ordering
* time-to-first-success blockers

/qa-friction checkout retry

Prioritize:

* checkout UX
* retry behavior
* duplicate submissions
* slow payment calls
* failed requests
* idempotency
* error states

Do not invent a generic flow if context is missing.

If the active feature, target surface, or starting point cannot be determined from available context, stop and report what context is missing.

## Required behavior

When invoked:

1. Identify the feature or workflow to QA.
2. Infer the relevant persona:
    * end user
    * admin
    * developer
    * external API integrator
    * CLI user
    * docs reader
3. Infer the starting point:
    * app route
    * local URL
    * docs page
    * API endpoint/docs
    * CLI command
    * onboarding flow
4. Infer success criteria from the PER spec, bead, current work, or changed behavior.
5. Attempt the task as the inferred persona.
6. Record every workaround.
7. Record confusing, slow, broken, ambiguous, or surprising behavior.
8. Capture dev-console/network evidence if available.
9. Produce a markdown QA friction report.
10. Do not fix anything.

## Cold-user rule

Do not inspect source code before attempting the user/developer flow unless the task is impossible without doing so.

Use visible product behavior, docs, API behavior, CLI output, browser behavior, logs, and network evidence first.

If source inspection becomes necessary after being blocked, clearly state why in the report.

## Target types

### Web app

Use browser automation if available.

Capture:

* route tested
* steps attempted
* screenshots or traces if available
* console errors
* runtime warnings
* failed requests
* slow requests
* duplicate requests
* confusing labels
* missing loading states
* missing empty states
* missing error states
* unexpected redirects
* dead ends
* auth/session problems
* behavior inconsistent with spec

### Docs / developer onboarding

Start from the documented entrypoint only.

Capture:

* missing prerequisites
* commands that fail
* unclear setup order
* missing environment variables
* ambiguous examples
* source inspection required
* bad error messages
* time-to-first-success blockers
* undocumented assumptions
* outdated instructions

### API

Use only documented API behavior unless explicitly instructed otherwise.

Capture:

* unclear auth instructions
* unclear request shape
* unclear response shape
* inconsistent status codes
* missing examples
* bad error messages
* slow requests
* failed requests
* duplicate writes
* spec mismatches

### CLI

Use the CLI as documented.

Capture:

* install/setup friction
* unclear help text
* missing flags
* commands requiring hidden state
* commands that mutate unexpectedly
* bad error messages
* failed commands
* slow commands
* confusing output

## Dev console / network instrumentation

If browser dev console, browser automation, Playwright traces, Chrome DevTools Protocol, or equivalent network instrumentation is available, use it to capture request timing and console/runtime errors during the QA friction pass.

Track:

* slow requests
* failed requests
* duplicate or repeated requests
* requests triggered by a single user action
* console errors
* runtime warnings that appear user-impacting
* navigation timing if available
* request waterfall or trace summary if available

Slow request thresholds should come from repo-local QA config if present.

If no threshold is configured, use these defaults:

* Flag user-facing API requests over 1000ms.
* Flag page navigations over 1500ms.
* Flag background requests over 3000ms if they affect visible state.
* Always flag failed requests.
* Always flag duplicate write requests from one user action.

Do not fail the QA pass if dev console or network instrumentation is unavailable.

If instrumentation is unavailable, state that clearly in the report:

Network/dev-console instrumentation was not available, so slow-request tracking is based only on visible behavior.

If the user specifically asks to track slow requests, prioritize network/dev-console instrumentation before subjective UX observations.

Examples:

/qa-friction our new login feature

Run normal cold-user QA and include slow-request tracking if available.

/qa-friction our new login feature, especially slow requests

Prioritize dev-console/network timing evidence and make slow requests a first-class section in the report.

## Optional repo-local QA config

If present, read repo-local QA configuration such as:

.claude/qa-friction.md
.claude/qa.md
docs/qa.md
docs/development.md

Use it for:

* local app base URL
* app start command
* seeded test users
* test credentials
* fixture setup
* browser tooling
* performance thresholds
* known local environment constraints

Do not require such a file. Use it only if available.

## Gate Result

Use `PASS` only when the target workflow can be completed, no material friction
remains, and available instrumentation does not show failed requests,
user-impacting console/runtime errors, duplicate writes, or spec-inconsistent
behavior.

Use `FAIL` when the workflow is blocked, requires a workaround, shows
medium/high/blocker friction, produces failed requests, duplicate writes,
user-impacting console/runtime errors, stale or misleading docs, or behavior
that contradicts the spec or acceptance criteria.

Use `HUMAN DECISION` when QA depends on unavailable credentials, external
access, destructive actions, ambiguous requirements, environment setup the
agent cannot perform, or product judgment.

`FAIL` is an iteration gate, not a final verdict on the project. Return the
report to the implementation agent/session, let it fix the implementation or
docs, and rerun `/qa-friction` in a new fresh subagent. Do not let the
implementation agent/session downgrade, reinterpret, or override the QA
friction gate result.

## Report format

Produce this report:

```markdown
# QA Friction Report
## Focus
<natural-language focus supplied by user>
## Inferred Context
Feature:
<feature/spec/bead/workflow inferred>
Source context:
<current bead / PER spec / git diff / branch / conversation / repo config>
Persona:
<end user / admin / developer / external API integrator / CLI user / docs reader>
Starting point:
<route / URL / docs path / API docs / CLI command / inferred entrypoint>
Success criteria:
<what counts as completing the task>
Result:
completed | completed with friction | blocked | not attempted
Gate result:
PASS | FAIL | HUMAN DECISION
## Summary
<3-6 sentences summarizing what was tested and what friction was found.>
## Instrumentation
Dev console / network capture:
available | unavailable | partially available
Method:
<Playwright trace / browser devtools / console logs / network logs / visible behavior only>
Thresholds:
- API requests: <N>ms
- page navigations: <N>ms
- background requests: <N>ms
Notes:
<any limitations in instrumentation>
## Friction Points
### F1: <short title>
Severity:
low | medium | high | blocker
Observed:
<What happened.>
Expected:
<What the user/developer likely expected.>
Evidence:
- Route / URL / command:
- Console / network / log details:
- Screenshot / trace if available:
Impact:
<Why this matters.>
Suggested fix category:
code | docs | test | product | performance | unknown
Suggested fix:
<Brief suggestion. Do not implement.>
### F2: <short title>
Severity:
low | medium | high | blocker
Observed:
...
Expected:
...
Evidence:
...
Impact:
...
Suggested fix category:
...
Suggested fix:
...
## Slow / Failed / Repeated Requests
| Request | Trigger | Duration | Status | Classification | Notes |
|---|---|---:|---:|---|---|
| <METHOD /path> | <user action> | <ms> | <status> | slow / failed / duplicate write / repeated | <notes> |
If no network instrumentation was available, say:
Network/dev-console instrumentation was not available, so slow-request tracking is based only on visible behavior.
## Console / Runtime Issues
- <console error or warning, if any>
- <runtime issue, if any>
If none were observed, say:
No console/runtime issues observed with available instrumentation.
## Workarounds Used
- <Workaround>
- <Workaround>
If none were required, say:
No workarounds required.
## Source Inspection
Did not inspect source before testing.
If source was inspected after being blocked:
<explain why source inspection was necessary>
## Potential Spec / Test Follow-ups
These are candidates only. Do not update the spec from this command.
- <Potential required test obligation>
- <Potential PER spec clarification>
- <Potential implementation issue>
- <Potential docs issue>
```

## Report rules

Keep the report concise and evidence-oriented.

Do not over-explain.

Do not include a V2 implementation plan.

Do not rewrite the PER spec.

Do not create follow-up beads.

Do not modify the repo unless required to run the app or capture QA evidence, and never modify product code.

## Interaction with /second-pass

This command produces QA evidence.

It does not fold durable learnings into the PER spec.

The intended workflow is:

V1 implementation
-> /qa-friction <feature focus>
-> QA friction report
-> /second-pass
-> PER spec changelog + main spec body updated with durable QA learnings
-> hard reset
-> V2 implementation from reviewed spec

If QA findings appear durable, list them only under Potential Spec / Test Follow-ups.

Let /second-pass decide what belongs in the spec.

## Final instruction

Run the QA pass using the best available context and tooling.

If sufficient context exists, do not ask clarifying questions.

If context is genuinely insufficient to identify the feature or starting point, stop and report exactly what is missing.
