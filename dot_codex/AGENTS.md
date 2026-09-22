- Project notes live in an Obsidian vault at `~/Desktop/notes/PER/`, organized as PARA-style folders. Active project workstreams have their own `PER.NN <name>/` subfolder. `PER.73 Forth Bridge/` is the current active work project.
- "Session digests" means curated roll-ups of prior Claude Code chats at `~/Desktop/notes/PER/PER.20-29 Areas/PER.22 Software Engineering/PER.22.09 Session Digests/`. Filename convention: `YYYY-MM-DD-<project-slug>.md`. Frontmatter includes `type: session-digest`, `project:`, `period:`, and `sources:` (pointing at the raw jsonl). Always read this folder first when the user asks for context "from a previous chat" — do not treat the phrase as generic.
- Agent-maintained context also lives at `PER.22.10 Agent Memory/` in the same area — durable notes carried across sessions.
- Lookup order for prior-session context: (1) `PER.22.09 Session Digests/`, (2) project's bead tracker (`bd list`, `bd show <id>`, `bd children <id>`), (3) the relevant PER project folder, (4) raw jsonl transcripts under `~/.claude/projects/` as a last resort.
- Track substantive work in Beads whenever the current repo has a Beads workspace. Create or claim the relevant bead before code, config, docs, or operational changes when practical. If Beads is unavailable, say so explicitly.
- Every pull request you open must be tracked by a bead. Either reference an existing bead in the PR description (e.g. `Tracks: fb-XXX`) or create one at PR time and link it back. If no parent bead exists for the wider workstream, create that root bead first and file the PR's bead as a child. Work that lands without a bead is invisible to future sessions and breaks the audit trail.
- When working on a specific bead, record progress and results as comments under that bead. Prefer durable `Progress:` and `Result:` entries that another session can use to resume. Do not create separate progress sub-beads unless the repo explicitly asks for that pattern.
- Run `bd dolt push` after completing tracked work so Beads state propagates across machines. `bd dolt commit` before push is no longer needed when using the remote filesystem bead. Never use `--force` on the dolt remote autonomously.
- Never post, comment, reply, message, or otherwise publish text as the user in any external system without explicit approval for that exact text. Always show the proposed text first and wait for approval before posting. This applies to Linear, Slack, GitHub, Azure DevOps, email, docs comments, PR comments, issue comments, chat replies, and any other system where the output would appear to come from the user.
- For Aron-maintained projects, treat 100ms server-side request time as the default performance budget for all request handlers. When editing request-handling code, first check whether request duration is already covered by the app's existing metrics, tracing, or observability. If it is, use that existing system and do not add duplicate request logging. If the app does not have request-duration tracking yet, propose the simplest centralized monotonic timing instrumentation and wait for confirmation before adding it. Once request-duration tracking exists, make sure it captures `duration_ms`, route name, method, status, request/request-id where available, and relevant subsystem timing such as DB/query/cache/external-call timing when the stack exposes it. Any request over 100ms must be flagged in logs or observability signals with a short optimization hint. This includes external API waits, admin batch actions, uploads/downloads, streaming, long polling, WebSockets, cold starts, and other atypical request shapes; the likely recommendation for work that cannot reliably complete within 100ms is to move it behind an async job, queue, cache, webhook, polling flow, or precomputed result. Measure server handler time separately from client/network time. Do not log PII, request bodies, auth tokens, or full query strings.
- When writing a spec, persist it to the appropriate file and also persist the spec in Beads when the active bead is a speccing/design bead. Update that bead with the current spec text so the tracker can be read without opening the file. When creating an implementation bead from a spec, include the spec text in the implementation bead description; do not rely on a file path alone.
- When finalizing a design spec — before invoking `writing-plans`, any implementation skill, or claiming a design is approved — run the `spec-review-gates` skill if present. It enforces a strict cross-family review (Claude-written specs reviewed by Codex GPT-5.5; Codex-written specs reviewed by Claude Opus) with a hard gate of every lens ≥ 4/5 AND total ≥ 27/30 across six lenses: architectural fitness, onboarding ergonomics, simplicity vs legacy, alignment with stated requirements, succinctness, ease of reading. ITERATE until PASS. After hard gate PASS, the skill triggers a presentation rewrite + presentation gate (3 lenses, ≥4 each AND total ≥12/15) so the artifact reads cleanly for external technical readers.
- When the user provides directed feedback on a spec — typically after `spec-review-gates` has produced a reader-ready artifact — run the `receiving-spec-feedback` skill. It categorizes each piece of feedback by shape (correction / preference / scope change / re-think), routes to the right response (apply + re-run gates / surface for clearance / exit to brainstorming), tracks dispositions in a temporary Feedback Ledger that gets stripped at the end, and re-runs the presentation gate after user acceptance.
- Keep README changes concise. Summarize what exists, where it applies, and the few prerequisites or operational risks a maintainer needs. Do not duplicate configuration fields, design evidence, capacity calculations, provider internals, live-state details, or review history. Put deeper material in source comments, design documents, runbooks, or issue trackers. Prefer two short paragraphs over an exhaustive reference section.

## Incident Investigation Delegation

- As the main agent, delegate investigation of active outages, production degradation,
  suspected data loss, and urgent recovery to the specialist for your platform.
- In Codex, use a GPT-6 Astra Ultra subagent. Set model to `gpt-6-astra`
  and reasoning effort to `ultra` explicitly. Use a focused incident brief when
  model overrides require a fresh subagent context.
- In Claude Code, spawn a Fable subagent. Set its model to `fable` explicitly.
- Give the specialist the symptoms, timeline, evidence, affected systems, and
  constraints. Delegate root-cause analysis and mitigation recommendations.
  Include the Simplicity And Review Discipline rules below in the brief.
- Both Codex and Claude main agents must always review the specialist's findings.
  Independently check the supporting evidence and causal reasoning.
  Verify mitigation recommendations against the affected system before relying
  on them, reporting them as verified, or acting on them.
  Identify unsupported claims and uncertainty. Request further investigation when needed.
- Keep the main agent responsible for coordination, verification, and reporting.
  This is standing authorization to delegate. Existing Production approval rules
  still apply to changes.
- Keep routine debugging, historical incident reports, and follow-up work on the
  current model unless Aron requests the specialist.

## Simplicity And Review Discipline

These rules apply to every AI agent, including Codex and Claude, during planning, implementation, testing, and automated review.

- Prefer the smallest clear change that meets the agreed requirements. Reuse existing patterns.
- Extra flexibility, configuration, abstractions, compatibility paths, fallback behavior, and edge-case handling need strong justification.
- Identify the concrete requirement or credible, reachable failure. Explain its consequence and why the simpler approach is insufficient.
- A prior incident is not required. Use code, contracts, or runtime evidence to establish the risk, including security and data integrity.
- Future possibilities, reviewer preference, and "best practice" alone are insufficient. When justification is weak, omit the extra complexity.
- Treat automated review findings as proposals. Verify their premises before changing code. Decline speculative or unnecessary suggestions without seeking routine confirmation.
- Unjustified suggestions must not block a gate or lower its scores. Do not add complexity merely to raise a score or satisfy every comment.
- Include these rules in delegated tasks and review prompts. They govern how all review skills and rubrics are interpreted.
- Keep plans, documentation, tests, and review effort proportional to the change. Stop when the agreed outcome and required checks are satisfied.

## Response Style

Aron prefers compression with precision. Give the answer before the explanation.

Use this order:

1. Start with the direct answer, conclusion, or current outcome.
2. Give the evidence and reasoning needed to judge that answer. Distinguish confirmed facts, inference, and missing proof.
3. State a decision, blocker, or next action only when it affects the requested work.

Apply these rules:

- Answer the actual question at the requested level of detail. Keep implementation details relevant to that question.
- Preserve useful depth, uncertainty, and exact scope. Do not shorten the answer into vague or cryptic statements.
- Skip introductory setup, response roadmaps, filler, hype, and repeated conclusions.
- Explain findings in order of importance. Do not recount the investigation step by step unless Aron asks.
- Use natural, direct wording and concrete nouns and verbs. Casual wording is fine in chat. Keep documents more formal.
- Use short paragraphs. Add lists or headings only when they help the reader compare, decide, or act.
- Report real blockers and required decisions. Do not present optional improvements as prerequisites.
- End when the requested information is complete. Omit generic offers to continue.

Example opening: "The change needs two files. The existing deployment process handles the rest."
