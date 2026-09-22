---
name: review-other-authors-prs
description: Help Aron understand and review a pull request written by someone else through a concise summary, file explanations, and an optional local commit walkthrough with saved AI questions. Use for non-Aron PR reviews, not as a publishing gate for Aron's changes.
---

# Review other authors' PRs

Help Aron understand the change before judging it. Follow the requested stage. A request for a short summary does not require a full review page.

## Open with purpose and scope

Read the current PR description and diff. Record the head and base SHAs. Check callers when they determine the affected environments or workflows.

Use the `write-like-aron` skill. Start with a short paragraph about purpose and affected scope. Add a few short bullets for the main changes.

- Explain behavior before implementation details.
- Use concrete nouns, active verbs, and short sentences.
- Base the summary on code. Flag material differences from the description.
- Distinguish expected improvements from measured results.
- Include scope boundaries only when they matter to the review.

## Explain each file in two points

Follow the behavior through callers and helpers. For each changed file, explain only:

1. What does this file do?
2. What changed from before?

In chat, follow this with a short, exact source snippet and a source link. Use a small diff when comparison helps.

In a review page, show the complete file by default. Highlight the whole important block, including unchanged lines needed to understand it. Place the two-point explanation beside that block. Keep added-line markers distinct from the reading highlights. Do not add separate “why it matters” or “what to check” sections.

## Preserve the commit narrative

For a larger PR or a requested review page, use the local guided-code-explainer infrastructure. Read [the working example](references/local-review-page.md) when building or updating that page. Use this PR workflow rather than the explainer's incident-specific structure.

- Open with the agreed summary and bullets. Use one tab for each actual commit, in the PR's order.
- Keep each original commit title and SHA. Do not invent a cleaner history.
- Compare each commit with its first parent. Show source and line numbers from that commit.
- Show each file once per commit. Section links jump to complete highlighted blocks within the full file.
- Keep parent-relative diffs behind disclosure controls. Full files stay open.
- Label earlier behavior that later commits replace. Do not describe an intermediate defect as a current-head finding.
- For merges, distinguish incoming base-branch changes, conflict resolutions, and the final PR diff. A merge's changed-file list is not the PR's changed-file list.
- Flag confusing sequencing when the actual history does not support a clear narrative.

Create one explainer worktree from the owning repository and reuse it. Read historical git objects without switching that checkout. For ForthBridge, use the nested application repository, never the coordination hub.

## Keep questions attached to the source

Provide questions about the whole PR, one commit, a section, a file diff, or selected source lines. Support line-number selection and Shift-click ranges.

Use GPT-5.6 Terra through Pi's existing `openai-codex` provider. Keep the same worktree for every answer. Save discussions locally with their head, base, selected commit, parent, and source or diff coordinates.

- Follow-ups retain the original discussion context when Aron opens another tab.
- Derive excerpts and diffs from the saved revisions. Validate them against git. Do not trust client-supplied source text.
- Treat the PR description as claims to check. Keep historical behavior separate from the final PR.
- Answers start with the conclusion and cite the commit, file, and lines. Keep them short.
- Show progress, clear failures, and retry controls. Preserve drafts during navigation and polling. Poll only while an answer is active.
- Ctrl+Enter or Command+Enter sends a new question or follow-up. Plain Enter inserts a newline. Use the same submit path as the send button.

## Review boundaries and verification

Apply the shared Simplicity And Review Discipline policy. Findings need a concrete requirement or a credible, reachable failure. Verify automated suggestions. Preference and speculative edge cases do not justify extra work or block a review.

This workflow creates local review artifacts. It does not authorize code changes, deployment, PR comments, approval, or merging. Follow Aron's explicit scope for those actions.

Before presenting a page, verify its stored source, line ranges, and parent-relative diffs against the recorded revisions. Check commit navigation, whole-block highlights, full-file access, and narrow-screen layout. When adding or changing questions, verify a real answer, saved discussion, and follow-up with its original context. Mocked tests do not prove the real model path.

Refresh the PR head before continuing a later review. If it moved, identify the snapshot on the existing page and refresh deliberately. Do not silently relabel old source as current.
