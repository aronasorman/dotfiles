---
name: receiving-spec-feedback
description: Use when the user provides feedback on a design spec — typically after `spec-review-gates` has passed and produced a reader-ready artifact. Categorizes feedback by shape (correction / preference / scope change / re-think), routes to the right response (apply + re-run gates / surface for clearance / exit to brainstorming), tracks dispositions in a Feedback Ledger, and runs a final presentation gate after user acceptance.
---

# Receiving Spec Feedback

This is the required workflow for handling user feedback on a design spec.
Use it after `spec-review-gates` has passed and the spec is in the user's
hands for final review, or whenever the user provides directed feedback on
a specific section or claim in a spec.

User feedback is structurally different from reviewer feedback. The AI's
design is authoritative for internal consistency and what the reviewer
scored. The user is authoritative for project context, intent, and scope.
**Both must be respected.** This skill prevents two failure modes:

- **Defensive rejection** — "but the gate already passed" — when the user
  has real context the AI missed.
- **Performative agreement** — "great catch!" → applies blindly — which
  recreates the bloat problem the spec-review-gates skill solved.

The discipline that prevents both: categorize the feedback shape first,
then route to the right response. Feedback is not monolithic.

## When to use

- User provides feedback on a spec after `spec-review-gates` passes.
- User pushes back on a finding the reviewer raised during a gate run.
- User points out a factual error or missing project context.
- User adds, removes, or modifies a stated requirement.
- User wants to re-think a section or the whole approach.

## Required Order

1. **Re-add a Feedback Ledger to the spec** at the top (or just after the
   Executive Summary). This is a working-trail artifact for the feedback
   phase, parallel to the Decision Ledger that `spec-review-gates` uses.
   It gets stripped at the end of the feedback phase, before the final
   presentation gate runs.

2. **For each piece of feedback, categorize by shape:**
   - **A. Correction** — fact, name, path, command, or technical detail
     is wrong.
   - **B. Preference** — phrasing, structure, or framing the user wants
     changed (no requirement change, no scope change).
   - **C. Scope change** — adding, removing, or modifying a stated
     requirement. The Requirements Coverage table changes.
   - **D. Re-think** — user wants to reconsider a section or the whole
     approach. This is a brainstorm exit, not a spec edit.

3. **Push back when warranted.** If the feedback contradicts a documented
   design choice that the AI thinks is load-bearing, surface to the user
   BEFORE applying. Cite the relevant spec section. Ask the user to
   confirm they want to override. **Don't perform agreement.** If the
   user confirms the override, apply with a note in the Feedback Ledger
   recording the override-with-rationale.

4. **Apply per category:**

   - **A. Correction** — verify the user's correction against referenced
     source if available (code, config, vault notes). Apply the fix.
     Re-run presentation gate (might affect readability). Hard gate
     re-run only if the correction is structural / load-bearing.
   - **B. Preference** — apply the change. Re-run presentation gate.
     No hard gate re-run.
   - **C. Scope change** — Requirements Coverage table changes. Apply
     the scope change. **Re-run BOTH gates** (hard gate lens 4 will
     shift; presentation gate's coverage shifts).
   - **D. Re-think** — STOP. Acknowledge to user: "this is a redesign;
     exiting to brainstorming." Invoke the `brainstorming` skill or
     continue the brainstorm conversation. When the redesign lands,
     re-run both gates from iter-1 (treat as new spec).

5. **Don't conflate categories.** If a single piece of feedback touches
   multiple shapes ("rename the API verb AND add a new requirement"),
   classify and handle each component separately. Apply preferences and
   corrections first; surface scope changes for explicit clearance.

6. **Document each piece in the Feedback Ledger** with:
   - Sequence number
   - Shape (A / B / C / D)
   - Verbatim or summarized feedback
   - Disposition (APPLIED / DECLINED-WITH-REASON / APPLIED-OVERRIDE /
     RE-THINK)
   - Affected sections / decisions
   - Gate re-runs triggered

7. **Save feedback-session logs** to
   `/private/tmp/<spec-slug>-feedback-iter-N.txt` (parallel to the
   `*-spec-review-gate-iter-N.txt` files from spec-review-gates).
   Persist disposition on the bead so future sessions can see the trail.

8. **Surface the updated spec to the user for acceptance.** After
   applying and re-running gates, ask: "feedback applied. Spec ready for
   re-acceptance — anything else?"

9. **Wait for user acceptance.** The skill remains in the
   feedback-receiving phase until the user explicitly signals acceptance
   ("looks good" / "ship it" / "approved" / "done" / "no more changes" /
   "accept" / "ok ship"). The user may iterate as many times as needed;
   each iteration cycles through steps 2-8.

10. **After user acceptance, run the final presentation gate.** Same
    prompt and tooling as `spec-review-gates`'s presentation gate. Same
    threshold (each lens ≥ 4 AND total ≥ 12/15). Same cross-family
    reviewer rule and `model_reasoning_effort=low`.

11. **Strip the Feedback Ledger from the spec** once the presentation
    gate PASSes. The Feedback Ledger history is preserved in the bead
    comments and the `/private/tmp/` log files. The spec returns to
    reader-ready state (Executive Summary at top, no working-trail
    artifacts in the body).

12. **Report final state.**

## Feedback Ledger format

Add this section to the spec at the top (just after Executive Summary).
Strip it when the final presentation gate passes.

```markdown
## Feedback Ledger

| # | Shape | Feedback | Disposition | Affected sections | Gate re-runs |
|---|---|---|---|---|---|
| 1 | A | "We use Azure Key Vault not 1Password for ADO PATs" | APPLIED — verified against `kubernetes/secrets/` | Artifact locations | Presentation only |
| 2 | C | "Add R7: PR-merge webhook destroys env" | APPLIED — Coverage table + verb table updated | Requirements coverage, Verb surface, Phase B preview → Phase A | Hard gate (iter 1: 26→27 PASS) + Presentation |
| 3 | B | "Rename `env_destroy` → `env_delete`" | DECLINED initially — destroy is canonical k8s vocabulary. User override on second pass → APPLIED-OVERRIDE | Verb surface, User experience, Authz table | Presentation |
| 4 | D | "Reconsider UAT approach — what if no UAT lane at all?" | RE-THINK — exited to brainstorming | UAT process section (full rewrite expected) | Both gates (full re-run from iter-1 when redesign lands) |
```

## Reviewer-relationship rule

User feedback supersedes reviewer findings when they conflict. The
reviewer is cross-family-independent; the user is the source of truth
for project context and intent. If the user says "this concern is
hypothetical, not real for us," the user wins — even if it changes a
score. When applying user feedback that contradicts a prior reviewer
score, the re-run gate will reflect the new state automatically (the
reviewer's prior finding becomes DECLINED in the iteration-context
prompt, per the spec-review-gates skill).

## Complexity-clearance gate (inherited from spec-review-gates)

The author-side complexity-clearance gate from `spec-review-gates` still
applies. Specifically: **Shape C feedback that introduces new components,
verbs, infrastructure, or other complexity** must be explicitly confirmed
by the user as a real requirement (not "what about" hypothetical) before
applying. This is automatic for Shape C since the user is the source of
the scope change — but the AI should mirror the requirement back ("you
want R7 (PR-merge webhook → env destroy) added as a Phase A requirement,
not a Phase B candidate. Confirm?") to prevent ambiguous Shape C from
becoming the same accretion failure mode the gate was built to prevent.

## Reporting

When done, report:

- Spec path under review.
- Number of feedback items received, by shape (A / B / C / D counts).
- Disposition breakdown (applied / declined / applied-override /
  re-thought).
- Gate re-runs triggered: hard gate (count, score trajectory),
  presentation gate (count, score trajectory).
- Final presentation gate score.
- Bead ID where the feedback session was recorded.
- Spec is now reader-ready and feedback-accepted; the Feedback Ledger
  has been stripped.
