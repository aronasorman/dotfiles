---
name: spec-review-gates
description: Use when finalizing a spec or design document before transitioning to implementation (e.g., before invoking writing-plans), after brainstorming produces a written design, or whenever a hard gate is needed on a design artifact before treating it as approved.
---

# Spec Review Gates

This is the required workflow for finalizing a design spec before implementation.
It is a skill workflow that runs after a spec is written (typically by the
`brainstorming` skill or as the output of a design conversation) and before any
implementation plan is produced (typically by `writing-plans`).

The hard gate is a cross-family independent review with a numeric score. Until
the gate passes, the spec is treated as a draft.

## When to use

- After brainstorming has produced a written spec.
- Before invoking `writing-plans` or any implementation skill.
- When a design doc is being prepared for stakeholder review or hand-off.
- Whenever a "spec done" or "design approved" claim is about to be made.

## Required Order

1. Determine the spec file path and which CLI authored the spec.
   - The spec is typically at a path like
     `~/Desktop/notes/PER/<project>/<doc>.md` (Obsidian vault) or
     `docs/superpowers/specs/YYYY-MM-DD-<topic>-design.md` (in-repo).
   - Treat the current agent as the writer unless the user or commit context
     clearly says otherwise.
   - If the spec path or writer cannot be discovered safely, ask before
     reviewing.
2. **Detect prior iteration context.** Check whether a prior iteration's
   review output exists for this spec slug at
   `/private/tmp/<spec-slug>-spec-review-gate-iter-N.txt`. If yes, this is
   iteration N+1 and prior context MUST be included in the prompt (see Hard-
   Gate Prompt below). If no, this is iteration 1.
3. Run the hard-gate agent review with the strict cross-family reviewer:
   - If Claude wrote the spec, use Codex GPT-5.5 with reasoning effort
     **`high`** (NOT the default `xhigh`). `xhigh` is calibrated for
     production-code review and surfaces implementation-readiness findings
     during what should be a design review.
   - If Codex wrote the spec, use Claude Opus.
   - No fallback to same-family review unless the opposite CLI is provably
     unavailable AND the user explicitly approves the fallback in this
     session.
4. **Save the review output** to
   `/private/tmp/<spec-slug>-spec-review-gate-iter-<N>.txt` so the next
   iteration can include it as prior context.
5. Loop on the hard-gate agent review until it passes.
6. **After PASS, perform the presentation rewrite + presentation gate**
   (see "Presentation rewrite" and "Presentation gate" sections below).
   The spec is not reader-ready until the presentation gate also passes.
7. Report the final result with both gate outcomes.

## Hard-Gate Prompt

Write the prompt to
`/private/tmp/<spec-slug>-spec-review-gate-prompt.md` so the full rubric and
input pointers are reproducible. The prompt has two shapes:

### Iteration 1 prompt (no prior context)

```text
Run the spec review hard gate on the design spec at <SPEC_PATH>. You are
allowed to read the spec and any referenced inputs. Your job is to be
critical, not validating — the author has built the design through
brainstorming and may have rationalized choices that don't survive scrutiny.

Inputs to read:
- Primary: <SPEC_PATH>
- Context (if applicable): <BEAD_IDS>, <RELATED_DOCS>, <STATED_REQUIREMENTS>
- Project conventions: <CLAUDE_MD_OR_AGENTS_MD_PATH>

Apply these six lenses and score each 1-5. Be specific. Cite spec section
headings or decision-ledger row numbers when raising concerns.

1. Architectural fitness — minimum moving parts, clean component boundaries,
   sound trust/state boundaries, sensible failure modes, no premature
   abstraction for hypothetical futures.
2. Onboarding ergonomics — mental-model size, cold-start time, single source
   of truth on failure, intuitive verb/concept surface, strictly easier than
   the legacy process for the user.
3. Simplicity vs legacy AND vs prior iteration — (a) each net new component
   justified by an observed failure in the current system, a stated
   requirement, or a runtime constraint; (b) for iterations N+1, complexity
   added since iteration N must point to a finding the prior iteration
   explicitly flagged as Phase A required (not deferred, not hypothetical,
   not "what about"); (c) reviewer-suggested infrastructure that handles
   only hypothetical concerns is BLOAT and scores down; (d) perpetual carry
   cost reasonable, process-first-then-technology balance, 80/20 alternative
   considered, universal-tool assumptions surfaced (e.g., "this assumes
   every user has Claude / a specific CLI / an ADO account").
4. Alignment with stated requirements — every named requirement covered,
   gaps explicit, no aspirational scope creep beyond requirements.
5. Succinctness — no bloat, no duplicated content across sections, each
   section earns its place, length proportional to topic complexity, no
   padding / no filler / no soft asks.
6. Ease of reading — scannable structure, decision ledger or summary at top,
   consistent vocabulary, signposts and clear headings, diagrams where
   load-bearing, tables instead of prose for enumerations.

## Scope discipline (READ THIS BEFORE FINDING ANYTHING)

The spec is for a specific phase boundary (Phase A wrap, Phase B rebuild,
etc.). The reviewer MUST respect that boundary.

**Acceptable finding types:**
- Spec fails to satisfy a stated requirement (cite which)
- Spec has internal inconsistencies, contradictions, or drift
- Spec over-specifies work that's explicitly deferred to a later phase
- Spec is unclear, unscannable, or hard to read

**Unacceptable Phase A finding types** — list these in a separate "Phase B
candidates" output section, NOT as Phase A findings, and do NOT penalize
the spec for not handling them:
- Hypothetical scaling concerns absent observed contention or stated
  capacity targets
- Hypothetical race conditions absent observed user impact
- Hypothetical failure modes absent current incident evidence
- "What about edge case X" where X is not in the requirements brief

Phase B candidates are FYI for the author. They do not enter Phase A score.

Hard gate: all lenses must score 4+/5 AND total must be at least 27/30. If
any lens is below 4 or total is below 27, result is ITERATE.

Output exactly:
- Result: PASS or ITERATE
- Total: X/30
- Lens scores: each lens score plus 2-3 sentence rationale citing spec
  sections
- Significant findings to surface before approval (3-7, specific) —
  ACCEPTABLE FINDING TYPES ONLY
- Risks the spec missed (2-5 risks NOT in the spec's own risk table) —
  must be observed in the legacy system or required by the brief, not
  hypothetical
- Phase B candidates (0-N): hypothetical or out-of-scope concerns the
  reviewer thinks the author should consider AFTER Phase A ships
- Counter-proposals: 1 simpler alternative + 1 more-sophisticated
  alternative + recommendation (proceed / modify / pivot)
- Top 5 things to change, ranked, one sentence each, most-load-bearing
  first. Can return 0 if PASS or if all remaining concerns are Phase B
  candidates.
```

### Iteration N+1 prompt (with prior context)

```text
Run the spec review hard gate on the design spec at <SPEC_PATH>. This is
iteration <N+1>. The author has revised the spec to address findings from
iteration <N>.

## Iteration <N> verdict (prior)

Result: <PRIOR_RESULT>
Total: <PRIOR_TOTAL>/30

Lens scores from iteration <N>:
1. Architectural fitness: <N>/5
2. Onboarding ergonomics: <N>/5
3. Simplicity vs legacy: <N>/5
4. Alignment with stated requirements: <N>/5
5. Succinctness: <N>/5
6. Ease of reading: <N>/5

Top 5 things to change from iteration <N>:
1. <finding>
2. <finding>
3. <finding>
4. <finding>
5. <finding>

## Author's claim of what was fixed in this iteration

<AUTHOR_CHANGE_LOG>

## Your task

Read the current spec at <SPEC_PATH>. Then BEFORE adding any new findings,
classify each of iteration <N>'s top-5 findings as one of:
- RESOLVED — the author's change addresses the finding completely
- PARTIAL — the change addresses some but not all of the concern
- STILL OPEN — the finding remains
- NEW INTERPRETATION — the author's change is sound but raises a different
  concern; describe it
- DECLINED (author chose not to fix) — the prior finding was hypothetical
  or out-of-Phase-A scope; the author marked it as a Phase B candidate
  instead of building infrastructure for it. Credit the author with the
  scope-discipline call; do not re-raise the same finding.

Then re-score all six lenses with the current spec. The score should reflect
the current state, not a delta. If a lens improved because findings were
resolved, score it higher.

Apply the same six lenses, hard gate (≥4 per lens AND ≥27 total), and output
shape as iteration 1. Add one preamble section before the standard output:

```
Prior findings disposition:
  1. <finding>: RESOLVED / PARTIAL / STILL OPEN / NEW INTERPRETATION — <one line>
  2. ...
  3. ...
  4. ...
  5. ...
```

Be specific. Cite spec section headings or decision-ledger row numbers when
raising concerns. Default skepticism on NEW findings, but credit RESOLVED
findings — do not re-litigate fixes that landed.

Inputs to read:
- Primary: <SPEC_PATH>
- Context (if applicable): <BEAD_IDS>, <RELATED_DOCS>, <STATED_REQUIREMENTS>
- Project conventions: <CLAUDE_MD_OR_AGENTS_MD_PATH>
```

## Reviewer Commands

Strict cross-family rule. Same-family review is not permitted absent an
explicit user override. Reasoning effort is **`high`** for spec review, not
the default `xhigh`.

If Claude wrote the spec, run Codex GPT-5.5:

```bash
codex exec -m gpt-5.5 -c model_reasoning_effort=high \
  -C <project-path> --sandbox read-only \
  "$(cat /private/tmp/<spec-slug>-spec-review-gate-prompt.md)" \
  > /private/tmp/<spec-slug>-spec-review-gate-iter-<N>.txt 2>&1
```

If Codex wrote the spec, run Claude Opus:

```bash
set -o pipefail
claude -p "$(cat /private/tmp/<spec-slug>-spec-review-gate-prompt.md)" \
  --model opus \
  --output-format stream-json \
  --include-partial-messages \
  --verbose \
  2>&1 | tee /private/tmp/<spec-slug>-spec-review-gate-iter-<N>.jsonl
```

For Claude reviewer runs, the streamed `.jsonl` file is the progress log.
Extract the final assistant review text from the stream into the canonical
`/private/tmp/<spec-slug>-spec-review-gate-iter-<N>.txt` artifact before
classifying PASS/ITERATE:

```bash
jq -r 'select(.type == "result") | .result' \
  /private/tmp/<spec-slug>-spec-review-gate-iter-<N>.jsonl \
  > /private/tmp/<spec-slug>-spec-review-gate-iter-<N>.txt
```

If the opposite CLI is provably unavailable AND the user explicitly approves
the fallback in this session, you may use a fresh-context same-family
subagent and record the substitution in the report. Default behavior is to
fail loud and ask the user how to proceed.

If the hard gate returns ITERATE, classify each finding by action shape
BEFORE applying anything:

1. **Inconsistency fix / clarification / readability fix** — apply directly.
   Does not add complexity.
2. **Requirement gap fix** — apply directly with reference to the stated
   requirement.
3. **Complexity-increasing change** — STOP. Includes any of: new component,
   new verb, new annotation, new lock primitive, new infrastructure, new
   pipeline parameter, new permission scope, new external dependency, new
   audit store, new admin verb, new failure-mode handler.
   - Surface to the user before applying. Ask explicitly: "Reviewer's
     finding [N] says [<finding>]. Addressing it would add [<concrete new
     complexity>]. Is this an actual Phase A requirement, or should I
     mark it DECLINED as a Phase B candidate?"
   - Wait for the user's explicit response.
   - If confirmed as a requirement: apply with rationale documented in
     `AUTHOR_CHANGE_LOG` for the next iteration, citing the user's
     confirmation. The simplicity-lens deduction is offset by the
     requirement reference.
   - If declined or hypothetical: mark DECLINED in the prior-findings
     disposition for the next iteration's prompt. Do NOT apply.

**Auto mode does NOT override rule 3.** Complexity decisions are never
"routine"; the bloat cost is real and is reclaimed only by explicit
user-confirmed requirement. The reviewer's simplicity lens (Lens 3) will
score down for complexity added without observed pain — the author's
workflow gate prevents that complexity from being added unilaterally in
the first place.

After applying confirmed fixes, **document what changed** (an
`AUTHOR_CHANGE_LOG` for the next iteration's prompt). Re-run the hard
gate.

Do not invoke `writing-plans`, any implementation skill, or any "spec
approved" claim until the hard gate reports PASS with every lens at least
4/5 and total at least 27/30.

## Presentation rewrite (after hard gate PASS)

The hard gate validates the design. The presentation rewrite produces a
reader-ready artifact for an external technical audience (e.g., a senior
engineer outside the working group who needs to evaluate the design in 5
minutes).

The working spec accumulates a Decision Ledger and iteration scars over
the hard-gate loop; both are working-trail artifacts the author needed
during review but the reader does not. The bead/issue tracker captures
the same history durably, so the spec file can be cleaned up safely.

### Rewrite checklist

1. **Drop the Decision Ledger section.** Its history is preserved in the
   bead/issue comments. The spec content already reflects the locked
   decisions in the body sections.
2. **Add an Executive Summary** at the top (immediately after the
   frontmatter and title, before any other section). Target ≤ 250 words,
   5 paragraphs:
   - Problem and current pain (condensed from "Why this work")
   - Approach in one sentence (phasing + key tradeoff)
   - What Phase A delivers
   - What is deferred to later phases and why
   - Time, team, and top risks
3. **Rewrite working-trail language** to natural first-draft prose:
   - "We decided to X" → "X"
   - "This was added because Codex flagged Y" → just describe X
   - "Previously the spec said Z, now it says..." → just say the current
     thing
   - "Iter-2 added Y" → just describe Y
   The reader should not be able to tell this spec went through review
   iterations.
4. **Keep all substance**: requirements coverage table, architecture
   diagram, verb surface, implementation plan, risks, references.
5. **Preserve the file path**. Overwrite the working spec in place. The
   bead is the canonical record of how the spec got here.

The rewrite is a formatting + presentation pass, not a content pass.
**Do not add new components, decisions, or scope during the rewrite.**
The complexity-clearance gate from the hard-gate workflow still applies:
if a presentation finding looks like it requires new substance, surface
to the user instead of adding.

## Presentation gate

A smaller, faster cross-family review that validates the artifact for
external readers. Uses 3 lenses instead of 6, lower reasoning effort, and
a tighter threshold proportional to the reduced lens count.

### Reviewer command

Same cross-family rule as the hard gate. Use `model_reasoning_effort=low`
since formatting evaluation does not require deep reasoning.

If Claude wrote the spec:

```bash
codex exec -m gpt-5.5 -c model_reasoning_effort=low \
  -C <project-path> --sandbox read-only \
  "$(cat /private/tmp/<spec-slug>-presentation-gate-prompt.md)" \
  > /private/tmp/<spec-slug>-presentation-gate-iter-<N>.txt 2>&1
```

If Codex wrote the spec:

```bash
set -o pipefail
claude -p "$(cat /private/tmp/<spec-slug>-presentation-gate-prompt.md)" \
  --model haiku \
  --output-format stream-json \
  --include-partial-messages \
  --verbose \
  2>&1 | tee /private/tmp/<spec-slug>-presentation-gate-iter-<N>.jsonl
```

For Claude presentation-gate runs, the streamed `.jsonl` file is the progress
log. Extract the final assistant review text from the stream into the
canonical `/private/tmp/<spec-slug>-presentation-gate-iter-<N>.txt` artifact
before classifying PASS/ITERATE:

```bash
jq -r 'select(.type == "result") | .result' \
  /private/tmp/<spec-slug>-presentation-gate-iter-<N>.jsonl \
  > /private/tmp/<spec-slug>-presentation-gate-iter-<N>.txt
```

### Presentation gate prompt

```text
Run the presentation gate on the design spec at <SPEC_PATH>. This is a
smaller, formatting-focused gate that runs after the spec has already
PASSED the hard design-review gate (score <PASS_SCORE>/30).

The audience is an external technical reader — senior engineer outside
the working group who needs to evaluate the design in 5 minutes. Imagine
them reading the spec cold, no prior context.

Apply these 3 lenses and score each 1-5:

1. Executive Summary quality — Present at the top, ≤ 250 words, 5
   paragraphs covering problem/pain, approach, Phase A deliverables,
   deferred work, time/team/risks. Scannable in 60 seconds.
2. Absence of working-trail artifacts — No Decision Ledger section, no
   "iter-N" markers, no "previously...", no "this was added because", no
   review-driven phrasing. Reader cannot tell the spec went through
   review iterations.
3. Cold-reader comprehension — A senior engineer who has never seen this
   project before understands the design's intent, scope, and approach
   in 5 minutes from the executive summary plus headings.

Hard gate: each lens must score 4+/5 AND total must be at least 12/15.
If any lens is below 4 or total is below 12, result is ITERATE.

Output exactly:
- Result: PASS or ITERATE
- Total: X/15
- Lens scores: each lens score plus 1-2 sentence rationale
- Top 3 things to change (ranked, one sentence each) — formatting-level
  only; if a finding would require new substance, mark it OUT-OF-SCOPE
  rather than ITERATE.

Stay under 800 words.
```

### Loop

If presentation gate returns ITERATE, apply formatting fixes (rewording,
moving sections, tightening summary). **The complexity-clearance gate
still applies**: if a finding looks like it requires new substance, do
not apply autonomously — surface to the user. Most presentation findings
will be pure formatting and can be applied directly.

Re-run the presentation gate. Loop until PASS.

## Reporting

When done, report:

- Spec path under review and which CLI authored it.
- Writer CLI and reviewer used (including any approved fallback).
- **Hard gate**: iteration count, score trajectory (e.g., "iter 1: 20/30
  → iter 2: 24/30 → iter 3: 28/30 PASS"), final per-lens scores, and
  prior-iteration findings dispositions for the final iteration.
- **Presentation gate**: iteration count, score trajectory (e.g., "iter
  1: 11/15 → iter 2: 14/15 PASS"), final per-lens scores.
- Top significant findings carried into implementation (if any).
- Bead ID where the spec review activity was recorded (if Beads is in use).
- Whether the spec is now approved for the next step (writing-plans or
  similar) or what is still required.
