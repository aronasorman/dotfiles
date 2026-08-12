---
name: manual-review
description: Use when Aron asks to manually review a pull request, branch, worktree, or code Codex generated.
---

# Manual Review

Use one named tuicr tab for one review cycle. A cycle starts when the review is
opened and ends when Aron explicitly accepts it and the tab closes. Comments,
code fixes, and requests to review the updated diff are iterations of the same
cycle; reuse the existing tab for all of them.

Choose the longest session name that appears as a directory component in the
canonical repository path. This makes repositories under
`/Users/aron/src/forthbridge` use the `forthbridge` session. If no session name
matches the path, use the current session or the first listed session.

## Open the review

Choose the tuicr target:

- Pull request: `pr <PR-URL>`
- Uncommitted generated code: `-w`
- Committed branch or worktree: `-r <base>..HEAD`

Name PR tabs `Review: <repo> #<number>`. Name local reviews
`Review: <repo> <branch>`.

Run:

```bash
<skill-directory>/scripts/open-review.sh \
  /absolute/repository/path \
  "Review: <repo> <target>" \
  <tuicr-target-arguments>
```

The launcher prints the selected session name and stable tab ID. Keep both for
the entire review cycle.

The launcher creates a fresh tab only when no live tab has the same canonical
repository path and review title. Otherwise it returns the matching tab's
existing stable ID. This recovers the current cycle without spawning another
tab when controller context was lost.

After launch, use `tuicr review list --repo /absolute/repository/path` to find
the exact active review and read its existing comments.

Before telling Aron the review is ready, seed that tuicr session once. This
workflow explicitly permits these agent comments despite tuicr's general rule
for user-led reviews. Apply the `annotate-diffs` selection rules, but transport
the output with `tuicr review add`:

- Add 3-8 comments: one review note for the net change, important flow, and
  watch item, then tight line or file notes for intent, invariants, risk, and
  why important blocks exist.
- Use `--username "Codex"`, prefix each body with `[Codex focus]`, and default
  to `--type note`. Reserve `suggestion` or `issue` for a real unresolved concern.
- Keep them local. Do not restate syntax, add generic praise, or publish them.

If `[Codex focus]` is already present, do not seed again. Recovery, changed
`HEAD`, fixes, and `:e` remain the same cycle. After a material revision, add
only a net-new focus note for an important area the existing notes do not cover.

Tell Aron the tab name. When he says his comments are ready, read them with
`tuicr review comments --repo <path> --session <slug>`.

After changing the code, keep the same review open and tell Aron to enter `:e`
in tuicr to reload the diff. Read the next comments from the same tuicr session.
Repeat this fix, reload, and comment loop until explicit acceptance. Do not
launch another tab merely because `HEAD` or the working tree changed, Aron left
comments, or Aron asks to review the revision.

In tuicr, focus the Comments panel with `Shift-Tab` or `Tab`, move with `j`/`k`,
and press `Enter` to jump to a comment.

## Finish the review

When Aron explicitly accepts the review, the cycle ends. Close its stable tab
ID:

```bash
zellij --session <session> action close-tab-by-id <tab-id>
```

This terminates tuicr and removes the tab. Do not close whichever tab happens
to be focused.
