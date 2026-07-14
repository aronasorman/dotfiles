---
name: fix-explainer
description: Use only when the user explicitly invokes $fix-explainer after saying an already-evidenced problem or proposed fix is still hard to understand.
disable-model-invocation: true
user-invocable: true
---

# Fix Explainer

Create one temporary static annotated-source page. Do not diagnose, implement, or write in the target repository.

## Evidence gate

Require diagnosis, repository root, and source paths/ranges. Put external claims (Slack, logs) in `verification`, observed source in evidence cards, and prefix reasoning `Inference:`. If anything is missing, name the gaps and create nothing. Require logs for log-dependent claims. Do not investigate.

## Workflow

1. Create an external session directory and snapshot repository state:

   ```bash
   SKILL_DIR="${CODEX_HOME:-$HOME/.codex}/skills/fix-explainer"
   ARTIFACT_DIR=$(mktemp -d "${TMPDIR:-/tmp}/fix-explainer.XXXXXX")
   python3 "$SKILL_DIR/scripts/snapshot_repo.py" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/repo-before.json"
   ```

2. Read `references/explainer-schema.json` and `evaluations/fixtures/example-manifest.json`. Write `$ARTIFACT_DIR/manifest.json` from verified prose. Hash source bytes:

   ```bash
   python3 -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest())' "$REPO_ROOT/$REL_PATH"
   ```

3. Keep order: problem -> context -> evidence -> proposed change -> verification -> takeaway. For a bug or bug fix, start the first `problem.symptom` paragraph with the user-visible failure (the exact error when supplied), then order the main causal `context.flow` and evidence backward from that symptom through mechanism to root cause. `problem.symptom`, `problem.expected`, and `problem.diagnosis` each contain at least two paragraph strings. Render exactly three cards; paragraph one states the fact and later paragraphs explain its meaning and transition. Place labeled supporting evidence after the chain. Use 1-5 evidence blocks of ideally 8-24 lines; never exceed 24. Keep each combined Situation-Mechanism-Implication-Gotcha callout at 90 words or fewer. Use `proposed_fix: null` when none exists. Otherwise show only supplied candidate code and keep the rendered `Proposed — not applied` label. Verification status is exactly `verified`, `not_run`, or `unsupported`.

4. Render with the bundled validator:

   ```bash
   python3 "$SKILL_DIR/scripts/render_explainer.py" --manifest "$ARTIFACT_DIR/manifest.json" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/index.html"
   ```

   Validation errors do not permit weaker evidence.

5. Open it directly in the system browser by default with `open "$ARTIFACT_DIR/index.html"`; no server is needed. Only when the user explicitly requests the in-app browser, serve `$ARTIFACT_DIR` on temporary `127.0.0.1`; never use `file://`. Keep the server and `$ARTIFACT_DIR` alive while the tab is a user-facing deliverable. If that tab is blank or cannot attach, use the direct system-browser path. If both paths fail, return the active local path.

6. Prove the repository stayed unchanged. The snapshot covers HEAD/ref, status, tracked and index binary-diff hashes, and untracked/ignored path, mode, and content hashes:

   ```bash
   python3 "$SKILL_DIR/scripts/snapshot_repo.py" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/repo-after.json"
   cmp -s "$ARTIFACT_DIR/repo-before.json" "$ARTIFACT_DIR/repo-after.json"
   ```

   Clean up only after the user finishes viewing or the delivery session ends. Then stop the server, if any, and remove `$ARTIFACT_DIR`.

## Quick reference

Input is established evidence. Output is `$ARTIFACT_DIR/index.html`. The manifest is temporary; the repository receives no tour, comments, fix, or documentation.

## Common mistakes

- Freehand HTML without verified relative paths, exact ranges, hashes, or source excerpts.
- Calling every statement “confirmed” instead of separating attributed claims, observed code, inference, and proposal.
- Creating a walkthrough or beginning triage when evidence is insufficient.
- Activating for ordinary chat explanations without explicit `$fix-explainer` invocation.
