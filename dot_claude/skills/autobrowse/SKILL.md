---
name: autobrowse
description: Use when building, testing, or improving a site-specific browser automation skill through repeated browse CLI attempts, trace review, and strategy updates.
---

# AutoBrowse

Use this to turn a fragile website task into a reliable site-specific skill. The outer agent owns the task definition, reads results, forms one hypothesis per iteration, and edits the final skill. A browser-worker subagent may run the attempts when the user explicitly asks for AutoBrowse, subagents, or parallel browser work.

This is adapted from Browserbase's AutoBrowse methodology, but it does not use an inner Anthropic API runner or any external agent loop. The running agent delegates to its own platform subagent when available; otherwise it runs the same browser attempts directly.

## Prerequisites

- `browse` CLI installed: `npm install -g browse`
- Local Chrome available for `browse open <url> --local`
- Optional Browserbase remote mode only when the user explicitly approves third-party browser infrastructure

## Workspace

Keep training artifacts under the current working directory:

```bash
mkdir -p ./autobrowse/tasks/<task> ./autobrowse/traces/<task> ./autobrowse/reports
```

Use these files:

- `./autobrowse/tasks/<task>/task.md` - URL, inputs, success criteria, output schema
- `./autobrowse/tasks/<task>/strategy.md` - current learned navigation strategy
- `./autobrowse/traces/<task>/run-NNN/` - copied command logs, snapshots, screenshots, and findings

Do not write traces or temporary task state inside the installed skill directory.

## Loop

1. Define or read `task.md`.
2. Run one browser attempt with `browse` in local mode unless the user approved remote mode.
3. Capture evidence: command log, snapshots, screenshots when useful, final JSON or failure reason.
4. Identify the exact failure turn.
5. Form one hypothesis: the single heuristic most likely to improve the next run.
6. Edit `strategy.md` or the target site skill with that one change.
7. Repeat until the task passes consistently or the known failure is clearly bounded.

Use subagents only when explicitly authorized. In Codex, use `spawn_agent` for a browser-worker and tell it to use `browse` CLI through shell commands, write no production files, and return a concise trace packet. In Claude, use its Task/Agent equivalent with the same instruction. For a single attempt where subagents are unavailable, run the attempt locally and keep the same trace discipline.

## Browse CLI Pattern

```bash
browse stop
browse open "<url>" --local --wait load --timeout 60000
browse wait timeout 2500
browse snapshot --compact
# interact only with refs/selectors visible in the latest snapshot
browse click <ref>
browse fill <selector-or-ref> "<value>"
browse type "<text>" --delay 25
browse get text body
browse screenshot --path ./autobrowse/traces/<task>/run-NNN/step.png
```

Rules:

- Do not put named `--session` flags in a finished site skill.
- Use refs from the latest snapshot only.
- Snapshot after every action that changes DOM state.
- Prefer `browse fill`; switch to click + `browse type` only when fill does not fire site events.
- If the page is an SPA, prove hydration before trusting visible controls.
- Stop rather than bypass CAPTCHA, Turnstile, login, or safety interstitials unless the user explicitly takes over or approves a different tool path.

## Trace Packet

Each attempt should return:

```json
{
  "task": "<task>",
  "environment": "local",
  "status": "pass|fail|blocked",
  "commands": ["browse open ...", "browse snapshot ..."],
  "observed_state": "what the page showed",
  "failure_turn": "exact command where progress stopped",
  "hypothesis": "one change for next iteration",
  "skill_update": "specific instruction to add or revise"
}
```

## Graduation

When the workflow is stable, write or update the target site skill directly. The finished skill should be self-contained and include:

- purpose and when to use
- exact `browse` commands
- site-specific gotchas
- failure recovery
- expected JSON output
- known bounded failures if the site blocks automation
