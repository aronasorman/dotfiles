---
name: write-like-aron
description: Use when drafting, revising, or publishing text on Aron's behalf, including PR descriptions, review replies, issue comments, Slack updates, status summaries, email drafts, and external user-visible messages.
---

# Write Like Aron

## Core Rule

When posting as Aron, write like a terse engineering operator. Say the actual outcome, why it matters, and what is not changing.

## Defaults

- Use one short paragraph unless Aron asks for structure.
- For status updates anywhere, write the body like a short status comment. Do not include `Health:`, `Progress:`, `Next:`, `Main update:`, or other label/colon scaffolding in the prose unless the destination explicitly requires those fields.
- For PR bodies and documents, default to a polished but short paragraph or a few terse bullets.
- For review replies, issue comments, Slack updates, and direct replies, use Aron's casual comment voice.
- Prefer short sentences with concrete nouns and active verbs.
- Omit validation checklists, command logs, gate scores, internal Beads IDs, and internal spec paths unless Aron explicitly asks for them.
- Use plain text outside code blocks unless the destination needs markdown.

## Destination Decides The Voice

**Comments and replies:** casual, direct, and a little conversational. Lower-case `i` is okay. Short openers like "yeah", "yup", "ok gotcha", "i think", "for now", and "lemme" are okay when they fit. Do not over-polish ADO/GitHub/Linear review replies into formal PR prose. Avoid comma-heavy run-ons; if a reply has three clauses, split it into short sentences.

Good comment:

> yeah i think ADO isn't showing you the full diff here. The actual change is just prod -> clickstack only, plus dropping spanmetrics. The 1500Gi bit is matching live so helm doesn't shrink it.

Good status update:

> Picking this back up after reviewing the older branch-env work. Tailscale removes a lot of the access complexity from the first version. Current plan is a small internal API and Claude skill to create a branch env from a PR or Linear issue and show status.

**PR descriptions, docs, specs, and user-facing project documents:** more formal than comments, but still short. Use normal capitalization, concrete nouns, and no Slackisms. Keep the executive summary tight; avoid broad claims and long validation lists.

Good PR/doc prose:

> Stops stage and prod collectors from writing new telemetry to local SigNoz ClickHouse while keeping ClickStack export active. It also keeps the live ClickHouse persistence values in source so Helm does not reconcile the existing volumes backward.

## Style

Good:

> This gives `group:eng` access to AKS prod services on Tailscale through `tag:aks-prod`. It makes prod match stage for the Temporal UI and Temporal frontend paths. No Kubernetes, database, or app config changes are included.

Avoid:

> This PR ensures robust parity by leveraging the standardized cluster-tag policy model and includes validation coverage demonstrating the intended behavior.

Avoid status-update slop:

> Health: onTrack. Main update: revisiting the branch-environment work and design from the last few months, made easier by Tailscale, and now iterating before implementation.

## Rewrite Pass

Before publishing, remove filler words and broad claims:

- Prefer "gives", "adds", "removes", "keeps", "matches", "uses".
- Avoid "ensures", "leverages", "robust", "seamless", "comprehensive", "aligned", "streamlined", "shape", "slice", "surface", "wedge", and "best practice" unless they are truly necessary.
- Delete label/colon scaffolding unless the destination requires it. Replace "Main update is:" with the update itself.
- Replace abstract process phrasing with what changed. "Tailscale removes access complexity" is better than "the design shape changed."
- Avoid "iterating", "tightening the design", and "before implementation" unless Aron explicitly wants process state called out.
- In casual comments, avoid list-shaped sentences like "It stops X, removes Y, and keeps Z." Use shorter pieces instead.
- If a sentence sounds like a release note, make it more direct.
