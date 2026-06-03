---
name: sync-chezmoi
description: Use when syncing live filesystem dotfiles, skills, or config into chezmoi, pulling upstream dotfile changes, resolving drift, committing, and pushing.
---

# Sync Chezmoi

Copy live filesystem changes into the chezmoi source tree, pull upstream changes, merge intelligently, apply only intended targets, then commit and push.

Use this for personal dotfile and skill-tree syncs where live files under `$HOME` are the starting point and `~/.local/share/chezmoi` is the durable source of truth.

## Core Rules

- Preserve unrelated live filesystem drift.
- Preserve unrelated dirty source-tree changes.
- Prefer native chezmoi commands over hand-built path transforms.
- Never run broad `chezmoi apply` or `chezmoi apply --force` unless the user explicitly asks and unrelated drift has been ruled out.
- Never use `git reset --hard`, `git clean`, checkout, or force-push without explicit user approval.
- Do not flatten secrets into plaintext source files. If a target needs encryption or 1Password indirection, stop and choose the secret-handling path explicitly.
- For personal skills, keep live Codex, live Claude, chezmoi `dot_codex`, and chezmoi `dot_claude` copies in lockstep when the skill is intended for both agents.

## Inputs

Resolve before mutating anything:

- Live target paths to sync.
- Whether each target is already managed by chezmoi.
- Whether each target is a plain file, directory, symlink, template, encrypted file, or generated artifact.
- Chezmoi source directory, usually `/Users/aron/.local/share/chezmoi`.
- Remote branch and push target.
- Intended commit message.
- Whether upstream changes should be applied back to the live filesystem after merge.

If target paths are ambiguous, stop and ask for exact paths.

## Preflight

Run read-only checks first:

```bash
chezmoi source-path
chezmoi git -- status --short
chezmoi git -- branch --show-current
chezmoi git -- remote -v
```

For each live target:

```bash
chezmoi status <target>
chezmoi source-path <target>
chezmoi diff <target>
```

If `chezmoi source-path <target>` says `not managed`, use the unmanaged-target path below.

Classify existing dirty source-tree entries:

- `intended`: part of this sync.
- `upstream/local`: changes that must be merged with this sync.
- `unrelated`: do not touch, stage, stash, apply, commit, or overwrite.

Stop if unrelated dirty changes cannot be separated from intended paths.

## Capture Live Files Into Chezmoi

For already managed plain files or directories:

```bash
chezmoi re-add <target>
```

For a new unmanaged target:

```bash
chezmoi add <target>
```

Use flags only when the target requires them:

```bash
chezmoi add --template <target>
chezmoi add --encrypt <target>
chezmoi add --exact --recursive <directory>
```

Rules:

- Do not `re-add` templates blindly; `re-add` will not overwrite templates, and the source template usually needs manual editing.
- Do not add credential files unencrypted.
- For symlinks, confirm whether the source should track the symlink or the symlink target before using `--follow`.
- After capture, inspect the source diff before pulling:

```bash
chezmoi git -- diff -- <source-paths>
chezmoi git -- status --short
```

## Pull Upstream Without Losing Local Capture

If the source tree is clean except intended captured paths, stash only those paths before pulling:

```bash
chezmoi git -- stash push --include-untracked -m "sync-chezmoi pre-pull <slug>" -- <source-paths>
chezmoi git -- pull --rebase
chezmoi git -- stash pop
```

If there are unrelated dirty paths, do not stash everything. Either narrow the pathspec to intended paths or stop.

If the pull or stash pop conflicts:

```bash
chezmoi git -- status --short
chezmoi git -- diff --cc
```

Resolve conflicts by reading the local live target, local source change, and upstream source change. Do not choose `--ours` or `--theirs` just to clear conflicts.

When the conflict is between a live target and an updated source target, prefer chezmoi's three-way merge:

```bash
chezmoi merge <target>
```

Then inspect:

```bash
chezmoi diff <target>
chezmoi git -- diff -- <source-paths>
```

## Apply Targeted Upstream Changes

After merging source state, apply only the intended live targets that need upstream changes:

```bash
chezmoi apply --dry-run --verbose <targets>
chezmoi diff <targets>
chezmoi apply <targets>
chezmoi diff <targets>
```

Expected final state for intended targets is an empty `chezmoi diff <targets>` unless the user intentionally wants live files to remain different from source.

Do not apply all targets to clear a small diff. A broad apply can overwrite unrelated machine-local state.

## Skill Tree Sync Pattern

For a skill intended to exist in both Codex and Claude:

1. Update the live skill files:

```bash
~/.codex/skills/<skill>/SKILL.md
~/.claude/skills/<skill>/SKILL.md
```

2. Mirror into chezmoi source paths:

```bash
~/.local/share/chezmoi/dot_codex/skills/<skill>/SKILL.md
~/.local/share/chezmoi/dot_claude/skills/<skill>/SKILL.md
```

3. If Codex has UI metadata, also sync:

```bash
~/.codex/skills/<skill>/agents/openai.yaml
~/.local/share/chezmoi/dot_codex/skills/<skill>/agents/openai.yaml
```

4. Verify lockstep:

```bash
cmp -s ~/.codex/skills/<skill>/SKILL.md ~/.claude/skills/<skill>/SKILL.md
cmp -s ~/.codex/skills/<skill>/SKILL.md ~/.local/share/chezmoi/dot_codex/skills/<skill>/SKILL.md
cmp -s ~/.codex/skills/<skill>/SKILL.md ~/.local/share/chezmoi/dot_claude/skills/<skill>/SKILL.md
```

Use `chezmoi add` or direct source edits only after confirming whether the skill directory is already managed. Do not use a broad `chezmoi apply` to propagate one skill.

## Commit And Push

Before committing:

```bash
chezmoi git -- status --short
chezmoi git -- diff --stat
chezmoi git -- diff -- <source-paths>
```

Stage only intended source paths:

```bash
chezmoi git -- add <source-paths>
chezmoi git -- status --short
chezmoi git -- commit -m "<message>"
chezmoi git -- pull --rebase
chezmoi git -- push
```

If the final pull rebases onto new upstream commits, rerun targeted validation and amend only if needed. Do not force-push unless the user explicitly approves.

## Verification

Minimum checks:

- `chezmoi git -- status --short` shows only expected post-commit state before push, then clean after push.
- `chezmoi diff <targets>` is empty or the intentional remaining live/source difference is documented.
- For mirrored skills, `cmp -s` confirms every intended mirror pair.
- For templates, `chezmoi execute-template` or a targeted `chezmoi apply --dry-run --verbose <target>` succeeds.
- No unrelated source-tree paths were staged.

## Stop States

Stop and report when:

- Target path is ambiguous.
- Secret handling is unclear.
- A target is a template and the correct template merge is not obvious.
- Unrelated source or live drift overlaps the intended paths.
- `chezmoi pull`, stash pop, or rebase creates conflicts that cannot be resolved from local evidence.
- Applying upstream changes would overwrite unrelated live machine state.
- Push is rejected and a non-force resolution is not clear.

## Report Format

```markdown
Chezmoi sync complete.
Targets:
- <live target>
Source paths:
- <source path>
Upstream:
<pull/rebase summary>
Merge:
<conflict or no-conflict summary>
Apply:
<targeted apply summary or not needed>
Commit:
<hash and message>
Push:
<remote/branch result>
Residual risk:
- <risk or none>
```

If stopped early:

```markdown
Chezmoi sync stopped.
Reason: <specific stop state>
Next required action: <specific action>
Dirty state:
<relevant status lines>
```
