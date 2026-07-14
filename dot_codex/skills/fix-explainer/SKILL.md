---
name: fix-explainer
description: Use only when the user explicitly invokes $fix-explainer after saying an already-evidenced problem or proposed fix is still hard to understand.
disable-model-invocation: true
user-invocable: true
---

# Fix Explainer

Create one temporary, static annotated-source page. This is read-only: do not diagnose, debug, implement, or write inside the target repository.

## Evidence gate

Proceed only when supplied evidence establishes the diagnosis and provides the repository root plus real source paths and ranges. Attribute external claims (for example, Slack or logs) in `verification`; reserve evidence cards for observed source; label reasoning as `Inference:`. If source anchors, a confirmed diagnosis, or evidence supporting it are missing, stop, name what is missing, and create nothing. Logs are required only for claims that depend on them. Do not investigate to fill gaps.

## Workflow

1. Create a session directory outside the repository and snapshot its exact state:

   ```bash
   SKILL_DIR="${CODEX_HOME:-$HOME/.codex}/skills/fix-explainer"
   ARTIFACT_DIR=$(mktemp -d "${TMPDIR:-/tmp}/fix-explainer.XXXXXX")
   python3 "$SKILL_DIR/scripts/snapshot_repo.py" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/repo-before.json"
   ```

2. Read `references/explainer-schema.json` and the fixture in `evaluations/fixtures/`. Write `$ARTIFACT_DIR/manifest.json`; never copy unverified prose into it. Calculate every source hash from exact bytes:

   ```bash
   python3 -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest())' "$REPO_ROOT/$REL_PATH"
   ```

3. Arrange the narrative as problem -> file role and flow -> observed source in causal order -> proposed change -> verification -> takeaway. Use 1-5 evidence blocks. For each block, target 8-24 source lines; never exceed 24. Keep its combined Situation-Mechanism-Implication-Gotcha callout at 90 words or fewer. Use `proposed_fix: null` when none exists. Otherwise show only supplied candidate code and keep the rendered `Proposed — not applied` label. Verification status is exactly `verified`, `not_run`, or `unsupported`.

4. Render with the bundled validator:

   ```bash
   python3 "$SKILL_DIR/scripts/render_explainer.py" --manifest "$ARTIFACT_DIR/manifest.json" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/index.html"
   ```

   A validation error is a stop, not permission to weaken evidence.

5. The output is static HTML. Open it directly in the system browser by default with `open "$ARTIFACT_DIR/index.html"`; no server is needed. Only when the user explicitly requests the in-app browser, serve `$ARTIFACT_DIR` on temporary `127.0.0.1`; never use `file://`. Keep the server and `$ARTIFACT_DIR` alive while the tab is a user-facing deliverable. If that tab is blank or cannot attach, use the direct system-browser path. If both paths fail, return the active local path.

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
