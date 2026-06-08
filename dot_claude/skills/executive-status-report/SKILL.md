---
name: executive-status-report
description: Use when writing, submitting, or updating a CTO/executive status report, PER report, readiness update, project handoff, or "why is this taking effort" summary for technical work.
---

# Executive Status Report

Write decision-ready status for leaders who need operational truth without implementation noise. Separate usable-now from blocked, and explain effort through risk, evidence gaps, and decisions.

## Use When

- The user asks for an executive summary, CTO update, PER report, readiness report, handoff, or status report.
- The work involves infra, release readiness, QA readiness, incidents, migration, reset/parity, or long-running debugging.
- The user asks why work took more effort than expected.

Do not use for code review, implementation plans, PR descriptions, or runbooks unless the user asks for an executive-facing version.

## Evidence First

Before writing:

1. Recheck current evidence from the relevant source of truth: repo artifacts, command logs, test output, tracker issues, deployment state, proof reports, or notes.
2. Distinguish confirmed facts from memory-derived or inferred facts.
3. Avoid secrets, raw payloads, request/response bodies, headers, tokens, or sensitive row contents.
4. If the report belongs in PER or another note system, write it there, not only in chat.
5. If a tracker issue is active, add a short pointer to the report.

## Required Shape

Use `templates/executive-status-report.md` unless the user asks for a different format.

Required sections: Executive Summary, Current Readiness, What Was Produced, What Was Verified, What I Was Actively Working On, What Can Start Being Used Now, What Remains, Why The Work Took More Effort Than Expected, Recommended Next Steps, Bottom Line.

## Writing Rules

- Lead with the verdict.
- Use operational categories: ready, usable, partial, blocked, excluded, unknown.
- Keep technical terms only when they identify an accountable system or risk.
- Convert implementation detail into executive meaning.
- Include useful numbers: pass counts, blocked counts, failed rows, verified targets, remaining targets.
- Say "not ready" plainly when full readiness is blocked.
- Describe active blockers as safety or evidence gaps when accurate.
- Avoid command lists, raw logs, stack traces, source code, internal agent narration, and sensitive data.
- Do not overstate readiness because a subset is green.

## Common Failure Modes

| Failure | Fix |
| --- | --- |
| Treats a clean deploy/reset as full readiness | Add separate readiness rows for mechanics, proof, QA, and remaining blockers. |
| Lists work chronologically | Group by outcome: produced, verified, usable now, remaining. |
| Explains effort as "debugging took long" | Name the specific complexity sources and why they mattered. |
| Hides the pause point | Add "What I Was Actively Working On" with the exact blocker. |

## PER / Project Note Behavior

For Forthbridge PER reports:

- Write under `/Users/aron/Desktop/notes/PER/PER.73 Forth Bridge/` unless project context says otherwise.
- Use filename format `YYYY-MM-DD <Workstream> Executive Report.md`.
- Include related Linear issue IDs and Beads IDs when known.
- After writing, read the note back from disk and report the path.
- If Beads is active, append a short note pointing to the report and push Beads state.
