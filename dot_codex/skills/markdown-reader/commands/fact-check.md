---
name: fact-check
description: Verify a generated document against actual code and git history
---

Load the markdown-reader skill and fact-check the document named by `$@`. If no argument is given, use the most recently modified file in `~/.agent/diagrams/`.

## Claim extraction

Read the target document - the authored Markdown when one exists, otherwise the rendered page. Extract verifiable claims about file paths, function/type/module names, behavior, architecture, data flow, APIs, commands, dependencies, tests, performance/security assertions, and git history. Skip subjective design opinions.

## Verification

For each claim, inspect the actual source or git history. Re-read referenced files. For diff reviews, compare before/after with `git show` or the relevant range. For plan docs, verify referenced files/functions/types exist and behave as described.

Classify claims as verified, corrected, unsupported, or unverifiable. Preserve the document's structure.

## Reporting

Correct factual errors in the Markdown in place and add a verification summary listing what was checked and what changed. Re-render to a new output file with `scripts/render.py` - do not edit a generated page by hand, and do not `--force` over the previous render unless the user asked to replace that exact file. Report both paths.

If only a rendered page exists, report the corrections in chat rather than editing the HTML.
