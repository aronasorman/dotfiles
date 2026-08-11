---
name: manual-review
description: Use when Aron asks to manually review a pull request, branch, worktree, or code Codex generated.
---

# Manual Review

Open every requested review in a fresh, named tuicr tab in the existing Zellij
session.

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
the review loop.

The launcher always creates a new tab. Do not look for, reuse, or focus an
existing review.

After launch, use `tuicr review list --repo /absolute/repository/path` to find
the active review. Tell Aron the tab name. When he says his comments are ready,
read them with `tuicr review comments --repo <path> --session <slug>`.
After changing the code, tell Aron to enter `:e` in tuicr to reload the diff.

In tuicr, focus the Comments panel with `Shift-Tab` or `Tab`, move with `j`/`k`,
and press `Enter` to jump to a comment.

## Finish the review

When Aron explicitly accepts the review, close its stable tab ID:

```bash
zellij --session <session> action close-tab-by-id <tab-id>
```

This terminates tuicr and removes the tab. Do not close whichever tab happens
to be focused.
