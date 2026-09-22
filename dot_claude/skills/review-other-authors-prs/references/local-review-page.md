# Local PR review page

Use the existing working sample when creating another commit review page. Inspect the files before adapting them. Do not copy the sample's PR metadata or saved discussions into a new review.

## Accepted opening: be-k8s PR 48

Use this as the prose shape, not as claims about another PR.

Speeds up branch deployments and stops failed migrations from passing as successful. Normal staging and production pipelines stay unchanged.

- **Reuse Docker builds.** Cache intermediate build stages in the registry so unchanged steps can be skipped. Remove redundant image pulls and an extra dependency install.
- **Run migrations together.** Launch all eight domain migration jobs in parallel, including clinical, instead of waiting for each one separately.
- **Catch migration failures.** Check whether each job completed or failed. Fail the deployment if any job fails or exceeds 20 minutes.
- **Batch deployment work.** Edit manifest files together and apply them in one command. This removes repeated command startup overhead.

## Existing implementation

- Prototype: `/Users/aron/src/guided-code-explainer-prototype`
- Sample: `/Users/aron/.local/share/guided-code-explainer/explainers/be-k8s-pr48-commits`
- Sample worktree: `/Users/aron/.local/share/guided-code-explainer/worktrees/be-k8s-pr48-commits`
- Workflow evidence: the sample's `context.md`

The sample contains:

| File | Purpose |
| --- | --- |
| `assets/review-data.json` | PR snapshot, actual commits and parents, full source, diffs, notes, and focus ranges. |
| `render-review.mjs` | Produces the complete-file reader and checks each highlighted range against stored source. |
| `assets/review.js` | Commit navigation, section focus, theme, and text size. |
| `assets/review.css` | Reader, highlights, and questions panel styling. |
| `assets/review-questions.js` | Context selection, saved discussions, follow-ups, progress, and retry. |
| `index.html` | Generated page served by the existing explainer server. |

Use the installed `guided-code-explainer` skill for worktree creation and server startup. The PR page uses custom HTML and assets. `gce render` produces the incident walkthrough, so use the sample's renderer for this page. Adapt its fixed titles and identifiers to the new PR.

Below, `gce` means `node /Users/aron/src/guided-code-explainer-prototype/bin/gce.ts`.

```text
gce new --id <slug> --repo <owning-repository> --commit <head-sha> --title "<title>"
node <explainer-directory>/render-review.mjs
gce up
```

Use the server URL returned by `gce up`; the sample's recorded port is not a permanent configuration. Do not overwrite another explainer or restart an active answer merely to refresh the page.

## Consistent layout and diff markers

Reuse the reader's sidebar, commit navigation, full-file views, controls, and question interface. Populate `assets/review-data.json`; keep the layout and assets consistent between PRs. Preserve discussions and their saved revisions when refreshing an existing page.

The verified marker example is `/Users/aron/.local/share/guided-code-explainer/explainers/be-k8s-pr72`. Inspect its `render-review.mjs`, `assets/review.css`, and `verification.json` for the existing treatment. The sample contains PR-specific titles, anchors, and counts; derive those from the new review's data. It is an artifact renderer, not a `gce` PR command.

- Derive markers from the exact Git patch. Show added lines with green `+` and removed lines with red `-`. A replacement shows both sides.
- Compare each commit with its first parent. For the final PR diff, use the recorded merge base and PR head. Name the comparison on the page.
- Keep source-reading highlights separate. An unchanged line can be important without being an addition.
- In full after-source, mark only actual additions. Show removed lines in the before-source or exact diff, with their original coordinates.
- Check the displayed patch, counts, and source markers against Git. Also check sidebar navigation, source selection, and narrow-screen layout.

## Build historical evidence

Fetch current PR metadata, including head, base, commits, and changed files. Fetch the required git objects into the owning repository. Preserve existing local changes.

For each selected commit, record all parents and read:

```text
git show <commit>:<path>
git diff --no-ext-diff --no-color --unified=3 <first-parent> <commit> -- <path>
git show --format= --cc <merge>
```

The last command helps inspect merge resolutions. A first-parent merge diff also contains incoming base changes. Use the other parent and final PR diff to attribute them correctly.

Store full before/after source and exact patches separately from prose. Line numbers refer to the named revision. Use the parent source for deleted files. Make highlight ranges complete logical blocks, not clipped excerpts. Verify stored source against git before rendering.

## Existing question API

The server serves custom `assets/*` files and saves discussions under the explainer directory. Reuse these routes under `/api/explainers/<id>/`:

| Route | Use |
| --- | --- |
| `GET state` | Agent, active answer, and saved discussions. |
| `POST discussions` | Start a question with its review context. |
| `POST discussions/<discussion-id>/messages` | Follow up using the saved context. |
| `GET jobs/<job-id>` | Running status and progress. |
| `POST jobs/<job-id>/retry` | Retry a failed or interrupted answer. |

New question bodies use `question` plus one of these contexts:

```json
{"review":{"scope":"pr"}}
{"review":{"scope":"commit","commit":"<full-sha>"}}
{"file":"<path>","review":{"scope":"file","commit":"<full-sha>","view":"diff"}}
{"file":"<path>","startLine":10,"endLine":25,"review":{"scope":"lines","commit":"<full-sha>","view":"source","side":"after"}}
```

A section question uses that section's full source range. Follow-up bodies contain only `question`; the server retains the original coordinates. Never rebuild follow-up context from the open tab.

Inspect `src/review.ts` and `src/agent/review-tools.ts` in the prototype for current validation and historical-source access. `read_revision` and `diff_revisions` read allowlisted git revisions in the same worktree without changing its checkout. `src/server.ts`, `src/store.ts`, and `src/jobs.ts` own routes, persistence, and background answers. Reuse this implementation rather than creating another backend.
