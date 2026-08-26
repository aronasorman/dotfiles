# AGENTS.md

## Project Guidance

Read `CLAUDE.md` in this directory before starting work. It contains the current project-specific workflow, documentation, swamp, and prior-session context instructions.

This folder is a Forthbridge coordination hub with nested application git
repositories. Follow the repository boundary and worktree policy in
`CLAUDE.md` before using any Codex, Claude, or git worktree flow.

## Local Aliases And Shell

- `fb` means this repo: `/Users/aron/src/forthbridge`.
- For any 1Password CLI operation (`op read`, `op item get`, etc.) in this
  repo, run the command through `direnv exec . ...` so `.envrc` supplies
  `OP_SERVICE_ACCOUNT_TOKEN` and avoids biometric prompts.
- Kubernetes/Swamp fan-out workflows can exceed macOS' default open-file
  limit. User shell startup files should keep the soft descriptor limit at
  `8192` for both fish and zsh.

## Swamp Kubernetes Debugging

- `@john/k8s` is installed for Kubernetes operational workflows and model
  types.
- Test pod model: `cluster-pods-test` (`@john/pod`,
  `context=AKS-TEST`, `namespace=inservice-ai`).
- Dev pod model: `cluster-pods-dev` (`@john/pod`,
  `context=aks-dev`, `namespace=inservice-ai`).
- Stage pod model: `cluster-pods-stage` (`@john/pod`,
  `context=aks-stage`, `namespace=inservice-ai`).
- Prod pod model: `cluster-pods-prod` (`@john/pod`,
  `context=aks-prod`, `namespace=inservice-ai`).
- Local workflow wrappers:
  - `pod-health-check-stage` targets `cluster-pods-stage`.
  - `pod-health-check-prod` targets `cluster-pods-prod`.
- The upstream `@john/pod-health-check` workflow expects a model named
  `cluster-pods`; use the local environment-specific wrappers instead.
- Known caveat: the pod-health workflow pattern uses `data.findBySpec`, which
  can include historical pod resources. In active prod rollouts, the inspect
  phase may fail on pods that disappeared after discovery.

## Azure DevOps

- Organization: `https://dev.azure.com/inserviceai`
- Project: `Inservice`

## Command History

- Atuin is the canonical shell-command history. Codex shell commands are
  captured by the user-level Atuin hook, and interactive fish/zsh commands are
  captured by shell initialization. Do not duplicate them in a Markdown log.
- `COMMANDS.md` is a historical, read-only archive. Do not append to, recreate,
  or use it as the current logging system.
- Atuin records command provenance, not operational proof. In the current Codex
  hook, failure exit status is not reliable and output/result summaries are not
  stored. Never use an Atuin entry alone as evidence that a command succeeded.
- Put intent, decisions, result summaries, acceptance evidence, and remaining
  work in Beads comments, PER notes, runbooks, or handoff notes as appropriate.
- Never place literal credentials in commands or annotations. Use environment,
  1Password, or vault references; Atuin's secrets filter remains enabled as a
  secondary safeguard.

## Bug and Incident Problem/Fix Explanations

When explaining a bug, incident, failure, regression, or proposed repair, use
the local `explaining-bugs-and-incidents` skill when available and present the
answer in this order:

1. **Short version** — State the user-visible symptom and whether the issue is
   ongoing, historical, latent, mitigated, fixed, or unverified.
2. **Symptom** — Contrast expected and observed behavior. Name the trigger,
   affected scope, and exact error when known.
3. **Problem** — Give a numbered causal chain from trigger or context, through
   the failing mechanism and any masking or propagation, to downstream impact.
   Separate the primary defect from secondary errors.
4. **Evidence** — Put direct links beside the claims they prove. Prefer the
   exact failing run or log, immutable source lines, governing ticket, and
   relevant Slack thread. Label inference, correlation, and missing proof.
5. **Proposed fix** — Separate the root repair from safety hardening. Explain
   what changes, why it prevents recurrence, and what existing behavior stays
   unchanged.
6. **Acceptance** — State what proves the repair in the real runtime.
   Distinguish historical failures, a still-present code defect, a workaround,
   an implemented fix, and successful live acceptance.

Use a compact outcome table when it materially clarifies allowed success,
no-op, and failure states. A canceled run proves neither failure nor recovery;
a successful manual bypass does not validate the broken automatic path; ticket
status is not runtime proof; and a downstream error is not the root cause when
an earlier failure explains it. Link to exact evidence, not a system homepage.

## Linear Project IDs

- **SRE & Observability**: `ba3bd0fc-c7a5-44ab-a105-aef891cc7628`

## Branch Environment Setup — Known Issues

### auth_config URL mismatch for patient apps

Branch environments use `forthbridge-patient-app-{env}.inservice.ai` as the cloudflared hostname, but the auth_config table in tenant-info (cloned from stage) only has `ftb-patient-app-{env}.inservice.ai`. The supertokens service matches the `X-ORIGIN` header against `auth_config.url_pattern` using Postgres regex. Without a matching entry, login returns "Configuration for {url} not found".

**Fix:** After cloning tenant-info, insert the missing auth_config row:

```sql
INSERT INTO auth_config (url_pattern, tenant_id, config)
SELECT 'https://forthbridge-patient-app-{ENV}.inservice.ai', tenant_id, config
FROM auth_config
WHERE url_pattern = 'https://ftb-patient-app-{ENV}.inservice.ai' AND deleted_on IS NULL
LIMIT 1;
```

### branch-db-clone pipeline — ftb database race condition

The CloneFtbOs job runs parallel `kubectl exec` commands that write `query_N.sql` files to `/root/` on the postgresdb pod. The parallel writes clobber each other, causing most databases to silently fail the "db exists" check. Only the first database (clinical) actually gets cloned. The job reports success because it doesn't exit on these errors.

**Workaround:** Clone ftb databases manually one at a time:

```bash
# From the postgresdb pod, for each db in (master_data, configuration, personnel, process_automation, scheduling):
PGPASSWORD='...' pg_dump -h {stage-host} -p 5432 -U {user} -F c -Z 9 -O -x -d {db} -f /var/lib/postgresql/data/{db}.dump
psql -U test -d postgres -c 'DROP DATABASE IF EXISTS "{db}" WITH (FORCE);'
pg_restore -d postgres -U test -C -O -x -Fc -j 4 /var/lib/postgresql/data/{db}.dump
rm /var/lib/postgresql/data/{db}.dump
```

Write dump files to the PVC (`/var/lib/postgresql/data/`) not `~/` to avoid ephemeral storage eviction.

### branch-db-clone pipeline — ActivePieces timeout

The CloneActivePieces job was missing `timeoutInMinutes: 360` (fixed in PR 11439). The ActivePieces `file` table is ~32GB on stage. Also needs `-Z 9` compression to avoid ephemeral storage eviction during dump.

## Notes

Obsidian notes for this project live at `/Users/aron/Desktop/notes/PER/PER.73 Forth Bridge/`. Read and write to this folder as necessary for incident reports, operations manuals, architecture notes, and other project documentation.

Anything that requires the user to read and review (design docs, smoke test plans, architecture proposals, incident reports, etc.) should be written as an Obsidian note in that folder, not inline in the conversation.

## Opus Review Gate

Use the local `pr-review-gates` skill whenever preparing to push code to a PR
branch, open or update a PR, or push a branch that is about to become a PR. It
runs the full loop: local quality gates, Opus saute gate until pass, then
CodeRabbit.

When running CodeRabbit from Codex, run the CodeRabbit CLI outside the sandbox.
It needs access to its local auth token, network, and auth callback server; a
sandboxed run can report false `not_authenticated` or callback failures.

Before pushing code to a branch that will open or update a PR, run the local
quality gates for the changed code first (tests, builds, linters/typechecks as
appropriate for the repo). The review gate does not score failing code: if the
required local gates fail, treat the result as **ITERATE** and fix or surface
the failure before asking for a scored review.

After local gates pass, run the Opus review gate from the repo or worktree that
owns the change. Do not rely on the bare `claude -p 'review gate'` shortcut; it
can produce an unscored review. Pass the hard-gate rubric and the already-run
local gate evidence explicitly:

```bash
claude -p $'Run the Forthbridge saute review gate on the current branch diff against <BASE_BRANCH>.\n\nPrerequisite build/test gate evidence already passed in this worktree:\n- <COMMAND>: PASS\n- <COMMAND>: PASS\n\nReview only the diff from <BASE_BRANCH>...HEAD. Apply these six lenses and provide feedback for each lens:\n1. Test completeness — new code has tests, all pass, edge cases covered\n2. Correctness — bugs, data integrity, error handling, race conditions\n3. Simplicity — least code that works, no over-engineering\n4. Commit story — commits tell a narrative reviewable commit-by-commit\n5. Excellence — would a human be proud to ship this?\n6. Architecture — follows repo conventions including file placement, layer boundaries, naming patterns, and how existing code is organized\n\nHard gate: all lenses must score 4+/5 AND total must be at least 27/30. If any lens is below 4 or total is below 27, result is ITERATE.\n\nOutput exactly:\n- Result: PASS or ITERATE\n- Total: X/30\n- Lens scores: each lens score plus concise rationale\n- Significant findings to surface before push\n- Concrete next actions\n' --model opus
```

Use this hard gate for the review:

- Test completeness — new code has tests, all pass, edge cases covered
- Correctness — bugs, data integrity, error handling, race conditions
- Simplicity — least code that works, no over-engineering
- Commit story — commits tell a narrative reviewable commit-by-commit
- Excellence — would a human be proud to ship this?
- Architecture — follows repo conventions: file placement, layer boundaries,
  naming patterns, and how existing code is organized

Each lens is scored 1-5. The gate passes only if every lens scores at least
4/5 **and** the total score is at least 27/30. If Opus reports significant
findings, failing scores, or uncertainty about correctness/architecture, surface
those findings to the user for a decision before pushing or rewriting the
approach. Obvious small fixes can be handled directly, then rerun the local
quality gates and Opus review gate.

## ASD-STE100 Writing

Use ASD-STE100 Simplified Technical English for specifications and commit
messages that Aron will read. This rule includes OpenSpec artifacts, design
documents, architecture proposals, and implementation specifications.

- Use active voice and simple verb forms.
- Put one idea or instruction in each sentence.
- Limit descriptive sentences to 25 words.
- Limit procedural instructions to 20 words.
- Use one consistent technical term for each concept.
- Use vertical lists for complex conditions or procedures.
- Do not use semicolons.
- Prefer common approved words when practical.
- Treat required technical nouns as declared technical names.
- Preserve exact identifiers, commands, formulas, limits, and normative meaning.
- Conventional Commit prefixes and exact technical names remain valid.

## External PR And Review Text Defaults

For PR descriptions, review responses, and other user-visible external text,
default to one short three-sentence paragraph written as an executive summary.
Omit validation checklists, command logs, gate scores, and internal Beads IDs
unless Aron explicitly asks for them. Keep validation evidence and Beads
tracking in Beads comments, PER notes, and handoff notes instead of
public-facing text.

<!-- BEGIN BEADS INTEGRATION v:1 profile:minimal hash:ca08a54f -->
## Beads Issue Tracker

This project uses **bd (beads)** for issue tracking. Run `bd prime` to see full workflow context and commands.

### Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --claim  # Claim work
bd close <id>         # Complete work
```

### Rules

- Use `bd` for ALL task tracking — do NOT use TodoWrite, TaskCreate, or markdown TODO lists
- Run `bd prime` for detailed command reference and session close protocol
- Use `bd remember` for persistent knowledge — do NOT use MEMORY.md files

## Session Completion

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd dolt push
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds
<!-- END BEADS INTEGRATION -->
