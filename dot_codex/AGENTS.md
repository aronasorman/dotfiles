Read `~/.claude/CLAUDE.md` before starting work.

Apply its **Simplicity And Review Discipline** rules to all AI coding and review,
including Codex, Claude, and delegated agents.

Follow its **Response Style** section for all answers to Aron.
Lead with the answer, then give the evidence and decisions that matter.

## Incident Investigation Delegation

- As the main agent, delegate investigation of active outages, production degradation,
  suspected data loss, and urgent recovery to a GPT-6 Astra Ultra subagent.
- Set the subagent model to `gpt-6-astra` and reasoning effort to `ultra` explicitly.
  Use a focused incident brief instead of a full-history fork when model overrides
  require a fresh subagent context.
- Give Astra the symptoms, timeline, evidence, affected systems, and constraints.
  Delegate root-cause analysis and mitigation recommendations to that agent.
- Keep the main agent responsible for coordination and reporting.
  This is standing authorization to delegate. Existing Production approval rules
  still apply to changes.
- Keep routine debugging, historical incident reports, and follow-up work on the
  current model unless Aron requests Astra.
