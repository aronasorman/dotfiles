---
name: forgejo-review
description: >
  Use when working with internal Forgejo review PRs: "open for review",
  "review PR #X", "address Forgejo comments", "approve rv-NNNN",
  "close rv-NNNN", or pasted Forgejo review links.
---

# Forgejo Review

Use this workflow for annotated review PRs on an internal Forgejo forge. It
keeps review-only metadata in Forgejo branches and strips that metadata before
anything is pushed to the real upstream.

## Required Configuration

Set or infer these values before opening or updating a review PR:

- `FORGEJO_URL`: Forgejo base URL, for example `http://host:3300`.
- `FORGEJO_ORG`: Forgejo organization or owner.
- `FORGEJO_REPO`: Forgejo repository name.
- `FORGEJO_BASE`: target base branch.
- `FORGEJO_HEAD`: review branch, usually `review/<feature-name>`.
- `FORGEJO_REVIEWER`: human reviewer username.
- `FORGEJO_TOKEN`: API token with repository and PR permissions.
- `FORGEJO_REMOTE`: git remote name for Forgejo, default `forge`.

Use the active agent's bundled scripts:

Claude:
```bash
export FORGEJO_REVIEW_SKILL_DIR="$HOME/.claude/skills/forgejo-review"
```

Codex:
```bash
export FORGEJO_REVIEW_SKILL_DIR="$HOME/.codex/skills/forgejo-review"
```

Allocate review IDs:
```bash
bash "$FORGEJO_REVIEW_SKILL_DIR/scripts/allocate-rv-ids.sh" 1
```

Strip review metadata:
```bash
bash "$FORGEJO_REVIEW_SKILL_DIR/scripts/strip-review.sh" "$FORGEJO_BASE"
```

## Review IDs

Every review PR gets `rv-NNNN`, zero-padded and globally unique for the
configured counter. Allocate IDs before opening PRs, one ID per review PR.

For multiple PRs:

```bash
bash "$FORGEJO_REVIEW_SKILL_DIR/scripts/allocate-rv-ids.sh" 2
```

The allocator defaults to:

- host: `${FORGEJO_REVIEW_HOST:-host.gorgon-ratio.ts.net}`
- counter: `${FORGEJO_RV_COUNTER:-/home/aron/forgejo/rv-counter.txt}`
- lock: `${FORGEJO_RV_LOCK:-/home/aron/forgejo/rv-counter.lock}`

Override those environment variables when a different Forgejo installation uses
a different counter host or path.

## PR Types

- `[agent]`: authored by an agent or automation.
- `[external]`: authored by a human or third party.

Title format:

```text
[rv-NNNN] [agent|external] <conventional commit title>
```

## Opening A Review PR

1. Identify the source repo and real upstream target branch.
2. Ensure a Forgejo remote exists:
   ```bash
   git remote get-url "${FORGEJO_REMOTE:-forge}" >/dev/null 2>&1 ||
     git remote add "${FORGEJO_REMOTE:-forge}" "$FORGEJO_URL/$FORGEJO_ORG/$FORGEJO_REPO.git"
   ```
3. Create or update `FORGEJO_HEAD`, usually `review/<feature-name>`.
4. Split the reviewed change into narrative commits: one logical concept per
   commit. Individual commits do not need to compile, but the final head must
   pass the relevant quality gates.
5. Add a `----- REVIEW` annotation section to every commit message.
6. Push the review branch:
   ```bash
   git push "${FORGEJO_REMOTE:-forge}" "$FORGEJO_HEAD"
   ```
7. If the Forgejo base ref is stale, update it from the trusted upstream base:
   ```bash
   git push "${FORGEJO_REMOTE:-forge}" "origin/$FORGEJO_BASE:$FORGEJO_BASE"
   ```
8. Open the PR with the API, then request review from and assign to the human
   reviewer.

For jj-managed repos, keep the same review branch naming but use jj bookmarks
and remotes:

```bash
jj git remote list | rg "^${FORGEJO_REMOTE:-forge}\\b" >/dev/null ||
  jj git remote add "${FORGEJO_REMOTE:-forge}" "$FORGEJO_URL/$FORGEJO_ORG/$FORGEJO_REPO.git"
jj bookmark create "$FORGEJO_HEAD" -r @
jj git push --remote "${FORGEJO_REMOTE:-forge}" --bookmark "$FORGEJO_HEAD"
```

The bundled `strip-review.sh` is a git history rewrite tool. Use it in normal
git worktrees. For jj worktrees, strip or rewrite commit descriptions with jj's
native history editing flow instead.

Use `jq` to avoid hand-escaped JSON:

```bash
TITLE="[rv-NNNN] [agent] feat: concise title"
BODY="$(cat <<'EOF'
Tracks: <ticket-or-work-item>

<short summary>

Verification:
- <command>: PASS, <summary>
EOF
)"

payload=$(jq -n \
  --arg base "$FORGEJO_BASE" \
  --arg head "$FORGEJO_HEAD" \
  --arg title "$TITLE" \
  --arg body "$BODY" \
  '{base:$base, head:$head, title:$title, body:$body}')

curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
  -H "Content-Type: application/json" \
  -X POST \
  -d "$payload" \
  "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls"
```

Then request and assign review:

```bash
PR=<forgejo-pr-number>

curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
  -H "Content-Type: application/json" \
  -X POST -d "$(jq -n --arg reviewer "$FORGEJO_REVIEWER" '{reviewers:[$reviewer]}')" \
  "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls/$PR/requested_reviewers"

curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
  -H "Content-Type: application/json" \
  -X PATCH -d "$(jq -n --arg reviewer "$FORGEJO_REVIEWER" '{assignees:[$reviewer]}')" \
  "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/issues/$PR"
```

## Commit Message Format

```text
<type>: short subject under 72 chars

Body text explaining what and why.

-----
REVIEW

LEGEND: Clean | Flag | Context | Risk | Needs your eyes | Mechanical

CONTEXT
- file.ts: what it does, who calls it, what tests cover it
- other-file.ts: same

Mechanical: lines 48-95 copied verbatim from old export()
Clean: error types match the upstream contract
Needs your eyes: should this retry failure be surfaced to callers?
Flag: offset pagination can drift if rows are inserted mid-export
```

Every commit must include the legend, a context block for touched files, and at
least one specific flag annotation. Everything below `-----` is stripped before
pushing clean history upstream.

## Addressing Review Comments

1. Fetch review comments:
   ```bash
   curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
     "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls/$PR/reviews"
   curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
     "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls/$PR/reviews/<review-id>/comments"
   ```
2. For `[agent]` PRs, group comments by original commit and create annotated
   `fixup! <original subject>` commits. Do not use `git commit --fixup=SHA`,
   because it drops the body where review annotations live.
3. For `[external]` PRs, do not push code, post comments, or interact with the
   upstream PR unless explicitly asked.

## Approval Workflow

When the human reviewer approves an `[agent]` review PR:

1. Autosquash fixups:
   ```bash
   GIT_SEQUENCE_EDITOR=true git rebase -i --autosquash "$FORGEJO_BASE"
   ```
2. Strip review metadata:
   ```bash
   bash "$FORGEJO_REVIEW_SKILL_DIR/scripts/strip-review.sh" "$FORGEJO_BASE"
   ```
3. Verify no review annotations remain:
   ```bash
   git log --format="%s%n%b" "$FORGEJO_BASE..HEAD" | grep -c "^REVIEW$" || true
   ```
   The count must be `0`.
4. Push the clean branch to the real upstream only after confirming the target:
   ```bash
   git push origin HEAD:<upstream-branch> --force-with-lease
   ```
5. Mark the review PR done and close it:
   ```bash
   current_title=$(curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
     "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls/$PR" | jq -r .title)

   curl -sS -H "Authorization: token $FORGEJO_TOKEN" \
     -H "Content-Type: application/json" \
     -X PATCH \
     -d "$(jq -n --arg title "[DONE] $current_title" \
       '{title:$title, state:"closed"}')" \
     "$FORGEJO_URL/api/v1/repos/$FORGEJO_ORG/$FORGEJO_REPO/pulls/$PR"
   ```

For `[external]` PRs, keep upstream hands-off. If asked to close the review
copy, close only the Forgejo review PR unless explicitly asked for upstream
approval or merge.

## Quality Checks Before Annotation

Before writing review annotations, read the repo guidance and compare the
change against nearby code. Flag architecture, naming, file placement,
data-safety, migration, rollout, and operational risks directly in the commit
where the concern appears.

## Availability Check

Check Forgejo directly before falling back:

```bash
curl -s -o /dev/null -w "%{http_code}" "$FORGEJO_URL/api/v1/version"
```

Any HTTP response means Forgejo is reachable. Fall back to a non-Forgejo review
artifact only for timeout or connection-refused failures.
