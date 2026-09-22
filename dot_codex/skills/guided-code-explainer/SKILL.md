---
name: guided-code-explainer
description: Use only when Aron explicitly asks for a guided code explainer, a guided source walkthrough, or a PR walkthrough in the explainer. Shows complete source files, evidence, change markers, and inline questions answered by GPT-5.6 Terra through Pi. Never use it automatically during incident work.
disable-model-invocation: true
user-invocable: true
---

# Guided code explainer (prototype)

Build a local walkthrough that lets Aron inspect and challenge a source-backed explanation. The page shows complete files. Your notes sit under the lines they explain. Aron can select lines and ask questions. GPT-5.6 Terra answers in the background through Pi, using the same worktree.

Prototype home: `~/src/guided-code-explainer-prototype`. Below, `gce` means `node ~/src/guided-code-explainer-prototype/bin/gce.ts`.

## Consistent presentation

Reuse the existing renderer and assets. Keep the sidebar, navigation, full-file views, reading controls, and inline questions when creating or refreshing a guide. Update its content data without rebuilding its CSS, JavaScript, or layout per invocation.

For bug walkthroughs, follow the procedure below: `walkthrough.json` is the input to `gce render`. For a requested PR walkthrough, follow [the local PR page workflow](../review-other-authors-prs/references/local-review-page.md). Reuse its `assets/review-data.json`, renderer, and question interface. That page workflow also applies to Aron's own PRs; it grants no review or publishing authority. `gce render` only renders bug walkthroughs, so do not run it over a custom PR page.

When showing an actual patch, derive change markers from the recorded Git comparison. Show additions with green `+` markers and removals with red `-` markers. A replacement has a removed line and an added line. Keep unchanged context neutral, and keep reading highlights distinct from change markers. Preserve the original text and line coordinates on each side; removed lines belong to the before view or exact diff. Label a proposed replacement as proposed until it exists in the compared source.

Before delivery, compare markers and counts with Git, then check the sidebar, navigation, source selection, and narrow-screen layout. Preserve existing discussions and their revision context when refreshing.

## Boundaries

- Run only on an explicit request. An urgent investigation continues without this skill.
- Do not apply the repair, commit, push, or publish anything.
- Create one worktree per explainer with `gce new`. Reuse it for every question. Do not freeze it or fork it per question.
- Never create the worktree from the coordination hub `~/src/forthbridge`. Resolve the leaf repository that owns the code.
- Answers always use GPT-5.6 Terra through Pi's `openai-codex` provider. Do not change the model or switch to paid API access.

## Procedure

1. **Read the signal.** For a Forth Bridge Slack link, run this from `~/src/forthbridge`:
   `direnv exec . swamp model method run slack fetch_thread --input 'permalink=<URL>'`
   Then read the bundle with `swamp data get slack <resource-name> --json`.
2. **Collect verbatim evidence.** Copy exact artifacts, not paraphrases. Examples: the error message and stack trace, log lines with timestamps, the request or payload, a database row, a metric value, the Slack message with its permalink. Record the command or query that produced each one. Use logs, ClickHouse, or traces as far as the explanation needs.
3. **Choose the source state.** Find the owning repository and the commit that matches the evidence, such as the deployed image tag or release commit. If you cannot determine it, use the current main commit and record that as a known gap.
4. **Create the explainer.**
   `gce new --id <short-slug> --repo <repo path> --commit <sha> --title "<title>"`
   Add `--include-uncommitted` only when the explanation depends on local changes.
   The command prints the explainer directory and worktree path.
5. **Trace and prove in the worktree.** Read complete functions, callers, and guards. Follow the request or data entrypoint to the failure mechanism.
   - Find each assignment, default, guard, transformation, and caller that changes the outcome.
   - Separate the primary failure from downstream effects.
   - Check that any hypothetical input passes every guard before the point you describe.
   - Where you can, prove a step with a concrete check and keep its verbatim output as evidence. Examples: run the extracted expression with the real input, `git log -L` for when a line changed, `git grep` for other writers or callers.
6. **Write `walkthrough.json`** in the explainer directory, using the schema below and the voice rules. Add the ticket id and its link when the report names one. Also write `context.md` there with the symptom, evidence, starting source state, causal claims, and known gaps.
7. **Render** with `gce render --id <slug>`. Fix every reported problem and render again.
8. **Start or reuse the server** with `gce up`. It prints the URL, and it restarts a server that is running older prototype code. If a reply fails with a Pi login error, tell Aron to run `npx pi` in the prototype directory and use `/login` with ChatGPT Plus/Pro (Codex).
9. **Hand off.** Give Aron the URL and a three-sentence summary. Do not paste the walkthrough into chat.

## How the page reads

The page follows one reading order: overview, steps, cause, fix.

1. Overview: the ticket link, the title, the source state, and the issue as one concrete failing case, then the rate. The evidence follows, verbatim. Data tables render as tables. Queries sit behind "Show the query".
2. Steps: the complete files, along the causal path. Each step opens above its focused lines with the call that enters it and the values that lead to the failure. The first step repeats the concrete example the walkthrough follows.
3. Likely cause: the numbered chain on its own page after the last step. Each link opens its evidence in place and shows the exact source lines.
4. Fix: last, after the cause. Never put the fix or the cause before the proof.

## Voice

Write the page as a debugging note to a colleague, not as a report. The Simplified Technical English rules for specs do not apply to this page.

- Open with one concrete failing case: when, which record, and what happened. Then give the rate.
- Say each fact once. The cause chain is the diagnosis, and the reader meets it after the steps. Steps and notes add only what the code shows.
- Vary sentence length. Join two facts in one sentence when one causes the other.
- Put identifiers in backticks. The page sets them in code type.
- Keep evidence IDs, "Label: value" lines, colon headlines, and lists of three out of the text. Name evidence by what it is, such as "the 00:26 sign-off".
- Put a non-breaking space (U+00A0) between a number and its unit, such as 0.9 s.
- Write data evidence as pipe-delimited rows with a header row. Put the query that produced it in `command`.
- The cause and fix headlines appear in the step list. Each is one sentence of 70 characters or fewer that says what, such as "Overlapping signs fail, and the failure is swallowed." Never a count or a status.

Before:

> Sign-off runs all six note types at once. Each note is signed right after it is saved, so SignWandNote calls for the same encounter overlap. Overlapping signs fail with NoteText NULL or a deadlock.

After:

> On Sep 15 at 00:26 UTC, one sign-off ran four SignWandNote calls within 0.9 s. The DISCHARGE sign (document 41880417) failed with `NoteText NULL`, and the endpoint still returned 201.

## Evidence labels

- `observed`: seen directly in logs, traces, stored data, history, search results, or source.
- `reconstructed`: derived from observed evidence, or run outside the application. An extracted expression run in Node is reconstructed.
- `hypothetical`: a constructed example. It must satisfy the preceding guards.

A possible code path is not proof that production executed it. Fix status is `proposed`, `applied`, or `runtime-verified`. Its before-and-after basis is `prediction`, `test`, or `runtime`. Claim `runtime-verified` only with runtime evidence.

## walkthrough.json schema

```json
{
  "title": "Short title",
  "ticket": { "id": "NL-4077", "url": "https://linear.app/forthbridge-ai/issue/NL-4077" },
  "issue": "One concrete failing case, then the rate. Separate paragraphs with a blank line.",
  "sourceNote": "Optional: how this source state differs from what ran.",
  "cause": { "status": "confirmed | probable | hypothesis | unknown", "headline": "One short sentence: what causes it.", "note": "Optional: what the claims leave unexplained." },
  "evidence": [
    {
      "id": "E1",
      "title": "Stack trace from the 10:42 alert",
      "status": "observed",
      "kind": "stack",
      "source": "Slack #prod-alerts, ClickHouse query, command, or file",
      "at": "2026-09-17 10:42 UTC",
      "link": "https://union-studio.slack.com/archives/...",
      "artifact": "Verbatim text: error, log lines, payload, command output, or diff.",
      "command": "Optional: the query or command that produced the artifact.",
      "note": "Optional: why it matters."
    }
  ],
  "claims": [
    { "id": "C1", "text": "One link in the causal chain, in order. Falsifiable.", "evidence": ["E1"], "code": [{ "step": "S2", "lines": [20, 20] }] }
  ],
  "steps": [
    {
      "id": "S1",
      "title": "Entrypoint",
      "file": "src/path/in/worktree.ts",
      "focus": [10, 24],
      "body": "Plain text. Separate paragraphs with a blank line.",
      "entry": {
        "call": "TenantsService.findByIdentifier(identifier, field)",
        "values": [{ "expr": "identifier", "value": "'acme'", "status": "observed" }],
        "note": "Optional. On the first step, name the concrete example the walkthrough follows.",
        "evidence": ["E1"]
      },
      "claims": ["C1"],
      "annotations": [
        { "lines": [12, 12], "kind": "guard", "text": "Rejects requests without the header before next().", "evidence": ["E1"] }
      ],
      "args": [
        { "expr": "tenantId", "value": "'acme'", "origin": "x-tenant-id header, line 12", "status": "hypothetical", "line": 20 }
      ],
      "stepInto": [{ "line": 20, "label": "TenantsService.findByIdentifier", "target": "S2" }]
    }
  ],
  "fix": {
    "status": "proposed",
    "headline": "One short sentence: what the fix changes.",
    "summary": "What changes, then what it leaves out.",
    "addresses": ["C1"],
    "changes": [{ "file": "src/path.ts", "lines": [20, 20], "replacement": "exact replacement text for those lines" }],
    "sameInput": { "input": "The input from the evidence", "before": "Current behavior", "after": "Behavior after the change", "basis": "prediction" },
    "verification": "Required when basis is test or runtime: commands run and their output"
  }
}
```

- `kind` for evidence is one of `log`, `stack`, `trace`, `request`, `response`, `data`, `message`, `metric`, `command`, `history`, `search`, `source`, or `other`.
- Annotation `kind` is one of `entrypoint`, `assignment`, `default`, `guard`, `transform`, `caller`, `call`, `failure`, `effect`, or `note`.
- Every claim cites at least one evidence item or code line. Prefer both.
- Line numbers are 1-based and refer to the worktree file. `gce render` rejects missing artifacts, paths outside the worktree, out-of-range lines, and unknown step, claim, or evidence IDs.
- Order steps along the causal path. Put contributing assignments and guards before the call they affect.
- Every step needs `entry`: the call that enters it and the values that lead to the failure. Carry the same failing case through all the steps, so the reader follows one flow. Mark a value you did not see directly as reconstructed or hypothetical.
- Write facts about the code and the evidence. Do not explain the page, the labels, or how to read them.
- No disclaimers about what was not done, such as "The application was not started." The page does not show the fix status or basis. It shows what was run only when something was run.
- Put what the chain leaves unexplained in `cause.note`, and what the fix leaves out in `fix.summary`, as sentences.
- The page marks only `reconstructed` and `hypothetical` items. Unmarked items are observed.
- `fix` is optional. `replacement` may span several lines. Empty text deletes the lines.

See `~/src/guided-code-explainer-prototype/examples/tenant-lookup-fixture.walkthrough.json` for a complete example.
