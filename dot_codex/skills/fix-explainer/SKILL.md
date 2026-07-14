---
name: fix-explainer
description: Use only when the user explicitly invokes $fix-explainer after saying an already-evidenced problem or proposed fix is still hard to understand.
disable-model-invocation: true
user-invocable: true
---

# Fix Explainer

Create one temporary, static annotated-source page. This is read-only: do not diagnose, debug, implement, or write inside the target repository.

## Evidence gate

Proceed only when supplied evidence establishes the diagnosis and provides the repository root plus real source paths and ranges. Attribute external claims (for example, Slack or logs) in `verification`; reserve evidence cards for observed source; label any reasoning as `Inference:`. If logs, source anchors, or a confirmed diagnosis are missing, stop, name what is missing, and create nothing. Do not investigate to fill gaps.

## Workflow

1. Create a session directory outside the repository and snapshot its Git state:

   ```bash
   ARTIFACT_DIR=$(mktemp -d "${TMPDIR:-/tmp}/fix-explainer.XXXXXX")
   git -C "$REPO_ROOT" status --porcelain=v1 --untracked-files=all > "$ARTIFACT_DIR/repo-before.txt"
   ```

2. Read `references/explainer-schema.json` and the fixture in `evaluations/fixtures/`. Write `$ARTIFACT_DIR/manifest.json`; never copy unverified prose into it. Calculate every source hash from exact bytes:

   ```bash
   python3 -c 'import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],"rb").read()).hexdigest())' "$REPO_ROOT/$REL_PATH"
   ```

3. Arrange the narrative as problem -> file role and flow -> observed source in causal order -> proposed change -> verification -> takeaway. Use 1-5 evidence blocks; target 8-24 source lines and never exceed 24. Keep each block's combined Situation-Mechanism-Implication-Gotcha callout at 90 words or fewer. Use `proposed_fix: null` when none exists. Otherwise show only supplied candidate code and keep the rendered `Proposed — not applied` label. Verification status is exactly `verified`, `not_run`, or `unsupported`.

4. Render with the bundled validator:

   ```bash
   SKILL_DIR="${CODEX_HOME:-$HOME/.codex}/skills/fix-explainer"
   python3 "$SKILL_DIR/scripts/render_explainer.py" --manifest "$ARTIFACT_DIR/manifest.json" --repo-root "$REPO_ROOT" --output "$ARTIFACT_DIR/index.html"
   ```

   A validation error is a stop, not permission to weaken evidence.

5. In Codex Desktop, serve the artifact on a temporary `127.0.0.1` HTTP port and open that URL with the in-app browser; never navigate that browser to `file://`. If in-app control fails, use the system browser with `open "$URL"`. If the loopback server cannot start, open the local HTML directly in the system browser. If neither browser works, return the still-active local path. After a page loads, stop the server.

6. Prove the repository stayed unchanged:

   ```bash
   git -C "$REPO_ROOT" status --porcelain=v1 --untracked-files=all > "$ARTIFACT_DIR/repo-after.txt"
   diff -u "$ARTIFACT_DIR/repo-before.txt" "$ARTIFACT_DIR/repo-after.txt"
   ```

   After the diff succeeds and the page has loaded, remove `$ARTIFACT_DIR`. Keep it only when returning the active local path fallback.

## Quick reference

Input is established evidence. Output is `$ARTIFACT_DIR/index.html`. The manifest is temporary; the repository receives no tour, comments, fix, or documentation.

## Common mistakes

- Freehand HTML without verified relative paths, exact ranges, hashes, or source excerpts.
- Calling every statement “confirmed” instead of separating attributed claims, observed code, inference, and proposal.
- Creating a walkthrough or beginning triage when evidence is insufficient.
- Activating for ordinary chat explanations without explicit `$fix-explainer` invocation.
