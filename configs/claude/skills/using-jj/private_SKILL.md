---
name: using-jj
description: Use when user asks to "group commits", "split commits", or "organize changes" with Jujutsu - analyzes working copy, proposes logical commit structure, and executes splits after approval
---

# Using Jujutsu (jj) for Commit Organization

## Overview

Automates the workflow of grouping changes in jj's working copy into logical commits. Analyzes changes, proposes grouping, waits for approval, then executes using `jj split`.

## When to Use

**Trigger phrases:**
- "Group my commits"
- "Split these changes"
- "Organize my working copy"
- "Break this up into commits"

**When NOT to use:**
- Single logical change that doesn't need splitting
- Changes already committed and organized

## Workflow

### 1. Analyze Changes

```bash
jj status           # See what changed
jj diff --stat      # Understand scope
```

### 2. Group Logically

Group by:
- **Feature/Purpose**: Same feature together
- **Layer**: domain, infrastructure, application, config
- **Type**: docs, code, tests, generated files
- **Module**: Separate subprojects

**Principles:**
- Keep related changes together
- Separate concerns
- Isolate generated/vendor files
- Keep config separate unless directly related

### 3. Propose Plan

Present structured proposal:

```
Proposed commit structure:

1. **feat(module): add feature X**
   - src/feature.go
   - src/feature_test.go

2. **docs: update README**
   - README.md
   - docs/guide.md

3. **chore: regenerate files**
   - vendor/...

Proceed? (yes/no)
```

Wait for explicit approval before executing.

### 4. Execute Splits

After approval, execute in order:

```bash
# Split out first group
jj split -m "feat(module): add feature X" src/feature.go src/feature_test.go

# Split out second group
jj split -m "docs: update README" README.md docs/guide.md

# Describe remaining
jj describe -m "chore: regenerate files"
```

### 5. Clean Up

```bash
jj log -n 5                    # Check structure
jj abandon <id>                # Remove any empty commits
jj workspace update-stale      # If working copy stale
```

## Key jj Commands

| Command | Purpose |
|---------|---------|
| `jj status` | Show working copy changes |
| `jj diff --stat` | Show change statistics |
| `jj split [files...] -m "msg"` | Split files into new commit |
| `jj describe -m "msg"` | Set/change commit message |
| `jj log -n N` | Show recent commits |
| `jj abandon <id>` | Remove commit from history |

## Common Mistakes

**❌ Don't split without analyzing:** Always run `jj status` and `jj diff --stat` first

**❌ Don't execute without approval:** Propose plan and wait for user confirmation

**❌ Don't forget cleanup:** Check for empty commits after splitting

**✅ Do group related changes:** Keep feature + tests + docs together

**✅ Do isolate generated files:** vendor/, *_templ.go in separate commits

**✅ Do use semantic commit messages:** feat/fix/docs/chore prefixes
