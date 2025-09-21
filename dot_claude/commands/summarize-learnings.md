Produce a markdown file `/tmp/summary.md` that contains ONLY the items below, in this exact order.
Absolute hard-limit: 50 total non-blank lines.

1. `## Task` - one concise sentence describing the core problem solved.
2. `## Results` - key numbers, metrics, or conclusions.
   • Use a single markdown table when tabular data fits.
3. `## Commands` – working code/CLI snippets only.
   • Shell fenced block ```bash … ``` (one per command).
   • Strip or redact any tokens, ARNs, URLs, or PII.
4. `## Notes` – critical caveats or follow-ups (max 3 bullet points).

Formatting & content rules:
• No additional explanations, narrative, or section headers.
• Elide any line that is not strictly essential to replicate the outcome.
• Remove intermediate / failed attempts.
• For multi-line SQL or YAML, keep it compact and inline if <4 lines; otherwise include as a fenced block.
• Prefer column alignment in tables; wrap long strings.
• Do NOT exceed 50 lines in the final file (including table rows and code blocks).

When done, copy to clipboard:
```bash
cat /tmp/summary.md | pbcopy
```

Respond with the markdown file content only—no extra prose.
