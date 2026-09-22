# AGENTS.md

## Project Guidance

This file contains shared Forthbridge guidance for Codex, Claude, and delegated agents.
Resolve its relative paths against `/Users/aron/src/forthbridge`.
Apply the user-level rules in `~/.codex/AGENTS.md`, including incident delegation and independent verification.
Keep platform-specific incident routing in that user file.

Read the relevant section of [Operations - Agent Reference](</Users/aron/Desktop/notes/PER/PER.73 Forth Bridge/Operations - Agent Reference.md>)
before using Swamp inventory, troubleshooting branch environments, locating detailed project notes, or recovering project context.
This local note preserves operational details and historical procedures. Verify live state before relying on them.

## Repository Boundary And Worktrees

`/Users/aron/src/forthbridge` (`fb`) is a coordination hub with nested application repositories.
Keep the hub on `main`. Use it for cross-repository context, operations, tracking, and documentation.
Gather shared context from this file, Beads, Swamp, Obsidian, and Atuin when command provenance matters.

Resolve the actual owning repository before implementation, such as `backend`, `forthbridge-os`, or `terraform`.
Run changes, local checks, review gates, commits, pushes, and PR creation from that repository or its worktree.
Do not start the Codex or Claude app-native worktree flow from the hub. It selects the wrong Git root.
For isolated work, use `using-git-worktrees` inside the owning repository and reuse its `.worktrees/` convention when present.
Open the app session on that leaf worktree. Read its instructions and this hub file.
Leaf-specific build and test commands take precedence for the leaf repository.

Start delegated Claude sessions with both options:

```bash
claude --add-dir /Users/aron/src/forthbridge \
  --append-system-prompt-file /Users/aron/src/forthbridge/AGENTS.md
```

The directory option supplies shared `.claude/skills` and `.claude/commands`, including Swamp and OpenSpec.
The prompt-file option supplies hub guidance even from external worktrees. Instruction visibility alone does not provide skills.
The local Operations - Agent Reference note preserves the detailed worktree procedure.

## Claude Opus Implementation Delegation

Keep planning, architecture, scope, and consequential decisions with Codex.
For Codex sessions, an OpenSpec change dominated by code changes and tests is
the strongest signal to delegate execution to Claude Opus.
Once the change is ready for implementation, invoke Claude Code with
`--model opus` by default. This standing instruction authorizes delegation
without a separate confirmation for each task.

- Start Claude in the owning repository or its worktree, following this file.
  Include `--add-dir /Users/aron/src/forthbridge` to load shared skills.
  Include `--append-system-prompt-file /Users/aron/src/forthbridge/AGENTS.md`
  so Claude receives the hub policy from any owning repository or worktree.
- Pass the exact OpenSpec change, accepted scope, constraints, acceptance
  criteria, and relevant Beads issue.
- Delegate implementation and the complete test/fix loop. Claude runs the
  applicable checks and fixes routine failures, resuming as needed.
- Bring scope changes and consequential design decisions back to Codex.
  Leave routine implementation choices and test repairs with Claude.
- Keep detailed logs outside the Codex conversation. Request a concise report
  with changed files, validation evidence, remaining risks, and decisions needed.
- Codex examines the relevant diff and evidence before reporting completion.
  Avoid repeating the full implementation or test/fix loop in Codex.
- If Claude is unavailable, report the blocker instead of silently switching
  implementation to Codex. Existing review, publishing, and Production
  authorization rules still apply.

## Local Aliases And Shell

- `fb` means this repo: `/Users/aron/src/forthbridge`.
- For any 1Password CLI operation (`op read`, `op item get`, etc.) in this
  repo, run the command through `direnv exec . ...` so `.envrc` supplies
  `OP_SERVICE_ACCOUNT_TOKEN` and avoids biometric prompts.
- Kubernetes/Swamp fan-out workflows can exceed macOS' default open-file
  limit. User shell startup files should keep the soft descriptor limit at
  `8192` for both fish and zsh.

## Granola Meeting Links

When Aron provides a Granola meeting link, use the `granola` Swamp model.
For `notes.granola.ai/t/<share-id>` links, first resolve the HTTP redirect to
the supported Granola meeting URL. Pass that destination to the model.
Run `direnv exec . swamp model method run granola fetch_meeting --input 'link=<URL>'`
from this folder. Use `fetch_transcript` when exact wording is needed. Read the
returned resource with `swamp data get granola <resource-name> --json` and reuse
that data within the task. Both `notes.granola.ai/d/<UUID>` and
`notes.granola.ai/meetings/<UUID>` links are supported. This is an on-demand
read. OAuth refresh uses the existing `op-aronbot` vault.

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

## Kubernetes Context Discipline

Always pass the Kubernetes context explicitly on every `kubectl` command that
touches a cluster, for example `kubectl --context aks-stage ...` or
`kubectl --context aks-prod ...`. Do not run `kubectl config use-context` as
part of Forthbridge work, and do not rely on the ambient current context. This
keeps multiple Codex or Claude threads from racing over shared kubeconfig state
when they are investigating or deploying to different clusters.

## Test Code

Continue writing and running tests to validate changes.
Commit test code when it tests application code.
For infrastructure, configuration, and operational changes, keep generated test
code local and outside tracked repository paths. Do not commit those tests.
Record validation results in the normal task or deployment evidence.

## Source Artifact Comments

Do not add rollout approvals, process warnings, operational gate reminders, or
review-status notes as comments in manifests or source files unless they are
machine-enforced, required by repo convention, or explain non-obvious runtime
behavior. Put process guidance in README files, runbooks, PR text, Beads
comments, or PER notes instead.

## Bug and Incident Problem/Fix Explanations

Use `explaining-bugs-and-incidents` when explaining bugs, incidents, failures, regressions, or repairs.
Read the matching section in the local Operations - Agent Reference note for the complete explanation and evidence rules.
Present the short version, symptom, numbered causal chain, linked evidence, proposed fix, and runtime acceptance in that order.
Separate confirmed facts from inference, primary defects from secondary errors, root repairs from hardening, and implementation from verified recovery.
A canceled run or successful manual bypass does not prove recovery of the automatic path. Ticket status is not runtime proof.

## Linear Project IDs

- **SRE & Observability**: `ba3bd0fc-c7a5-44ab-a105-aef891cc7628`

## Notes

Obsidian notes for this project live at `/Users/aron/Desktop/notes/PER/PER.73 Forth Bridge/`. Read and write to this folder as necessary for incident reports, operations manuals, architecture notes, and other project documentation.

Anything that requires the user to read and review (design docs, smoke test plans, architecture proposals, incident reports, etc.) should be written as an Obsidian note in that folder, not inline in the conversation.

## Opus Review Gate

Apply **Simplicity And Review Discipline** from `~/.codex/AGENTS.md` to this gate and
all reviewer feedback. Include that policy in every review prompt. Verify
findings before escalation. Unjustified suggestions cannot block or lower scores.

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

The exact invocation is preserved under **Opus Review Invocation** in the local
Operations - Agent Reference note linked above. Pass the rubric and local gate evidence explicitly.

Use this hard gate for the review:

- Test completeness — required behavior and credible failure cases are tested, applicable checks pass
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

## CodeRabbit Pre-PR Review

Before pushing code to a branch that will open or update a PR, run CodeRabbit
against the local diff from the repo or worktree that owns the change. Treat it
as a shift-left version of PR review: it is useful for catching actionable
review findings early, but it is not a replacement for local builds, tests, or
human judgment.

Use the intended PR target branch as the base and pass the local project
guidance as review context, for example:

```bash
coderabbit review --agent --base <target-branch> -c /Users/aron/src/forthbridge/AGENTS.md
```

If CodeRabbit raises significant feedback (critical/major findings, broad
architecture concerns, data-safety risks, or changes that would meaningfully
alter the implementation), surface it to the user for a decision before
rewriting the approach or pushing the PR branch. Obvious small fixes can be
handled directly, then rerun CodeRabbit before pushing.

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

## Prior-Session Context

For previous-chat context, read Session Digests first, then Beads and the relevant Obsidian project notes.
Follow the user-level lookup order in `~/.codex/AGENTS.md`.
The local Operations - Agent Reference note preserves Beads sync details, workspace memory paths, and raw transcript locations.
Use raw transcripts only when the curated sources do not answer the question.

## Beads Workflow

Read **Beads Workflow** in the local Operations - Agent Reference note before tracking coding or investigation work.
Reuse durable workstream roots. Name them by outcome, and put progress and results in root comments.
Use child beads only for questions that need human input. Mirror answers on those children and close them when resolved.
Keep PR URLs, branches, and status on the root bead. Omit internal Beads IDs from public PR descriptions unless Aron requests them.
Implementation beads must contain the spec text and link the canonical PER note. Keep them aligned or explain which is canonical.

## Swamp Building Blocks

Keep `.swamp.yaml` set to `tool: codex` so Swamp maintains its section in `AGENTS.md`.

Reuse the kit in `~/src/workflows`. Search loaded model types and community extensions before building or calling service CLIs directly.
Read **Companion swamp kit** in the local Operations - Agent Reference note for available types, instances, methods, and caveats.
Add reusable types in `~/src/workflows/extensions/models/` and create project instances with `swamp model create`.
Keep the shell descriptor limit at `8192`. If it is `256`, restart the shell or run `ulimit -n 8192`.

<!-- BEGIN swamp managed section - DO NOT EDIT -->
# Project

This repository is managed with [swamp](https://github.com/systeminit/swamp).

## Rules

1. **Search before you build.** When automating AWS, APIs, or any external service: (a) search local types with `swamp model type search <query>`, (b) search community extensions with `swamp extension search <query>`, (c) if a community extension exists, install it with `swamp extension pull <package>` instead of building from scratch, (d) only create a custom extension model in `extensions/models/` if nothing exists. Read `.agents/skills/swamp-extension-model/SKILL.md` for guidance. The `command/shell` model is ONLY for ad-hoc one-off shell commands, NEVER for wrapping CLI tools or building integrations.
2. **Extend, don't be clever.** When a model covers the domain but lacks the method you need, extend it with `export const extension` — don't bypass it with shell scripts, CLI tools, or multi-step hacks. One method, one purpose. Use `swamp model type describe <type> --json` to check available methods.
3. **Use the data model.** Once data exists in a model (via `lookup`, `start`, `sync`, etc.), reference it with CEL expressions. Don't re-fetch data that's already available.
4. **CEL expressions everywhere.** Wire models together with CEL expressions. Always prefer `data.latest("<name>", "<dataName>").attributes.<field>` over the deprecated `model.<name>.resource.<spec>.<instance>.attributes.<field>` pattern.
5. **Verify before destructive operations.** Always `swamp model get <name> --json` and verify resource IDs before running delete/stop/destroy methods.
6. **Prefer fan-out methods over loops.** When operating on multiple targets, use a single method that handles all targets internally (factory pattern) rather than looping N separate `swamp model method run` calls against the same model. Multiple parallel calls against the same model contend on the per-model lock, causing timeouts. A single fan-out method acquires the lock once and produces all outputs in one execution. Check `swamp model type describe` for methods that accept filters or produce multiple outputs.
7. **Extension npm deps are bundled, not lockfile-tracked.** Swamp's bundler inlines all npm packages (except zod) into extension bundles at bundle time. `deno.lock` and `package.json` do NOT cover extension model dependencies — this is by design. Always pin explicit versions in `npm:` import specifiers (e.g., `npm:lodash-es@4.17.21`).
8. **Reports for reusable data pipelines.** When the task involves building a repeatable pipeline to transform, aggregate, or analyze model output (security reports, cost analysis, compliance checks, summaries), create a report extension. Read `.agents/skills/swamp-report/SKILL.md` for guidance.

## Skills

**IMPORTANT:** Skills are detailed guides stored in `.agents/skills/`. When a task
matches a skill area below, read the corresponding `SKILL.md` file for guidance.

- `.agents/skills/swamp-model/SKILL.md` - Work with swamp models (creating, editing, validating)
- `.agents/skills/swamp-workflow/SKILL.md` - Work with workflows (creating, editing, running)
- `.agents/skills/swamp-vault/SKILL.md` - Manage secrets and credentials
- `.agents/skills/swamp-data/SKILL.md` - Manage model data lifecycle
- `.agents/skills/swamp-report/SKILL.md` - Create and run reports for models and workflows
- `.agents/skills/swamp-repo/SKILL.md` - Repository management
- `.agents/skills/swamp-extension-model/SKILL.md` - Create custom TypeScript models
- `.agents/skills/swamp-extension-driver/SKILL.md` - Create custom execution drivers
- `.agents/skills/swamp-extension-datastore/SKILL.md` - Create custom datastore backends
- `.agents/skills/swamp-extension-vault/SKILL.md` - Create custom vault providers
- `.agents/skills/swamp-issue/SKILL.md` - Submit bug reports and feature requests
- `.agents/skills/swamp-troubleshooting/SKILL.md` - Debug and diagnose swamp issues

## Getting Started

**IMPORTANT:** At the start of every conversation, run
`swamp model search --json`. If no models are returned (empty result), you MUST
immediately read `.agents/skills/swamp-getting-started/SKILL.md` and follow its
instructions. This walks new users through an interactive onboarding tutorial.

If models already exist, start by reading `.agents/skills/swamp-model/SKILL.md`
to work with swamp models.

## Commands

Use `swamp --help` to see available commands.
<!-- END swamp managed section -->

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
