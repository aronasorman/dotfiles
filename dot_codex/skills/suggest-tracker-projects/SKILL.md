---
name: suggest-tracker-projects
description: Use when generating top-ranked techno-activism tracker or watchdog project ideas from current news sources, especially Seattle Times and ABS-CBN, or when preparing recurring civic-tech tracker recommendations across Seattle, U.S., and Philippines issues.
---

# Suggest Tracker Projects

Generate exactly 10 timely tracker/watchdog product ideas grounded in current news. Do not repeat ideas already emitted in earlier runs.

## Prior Suggestion Registry

Use the Turso/libSQL database as the durable globally accessible dedupe ledger. Do not use Beads for this skill.

- Database URL: `libsql://project-ideas-tracker-aronasorman.aws-us-west-2.turso.io`
- Token source: `op://LocalAgents/Project Ideas Tracker Turso/credential`
- Service-account env file on this Mac: `/Users/aron/.config/local-agents/eclub-prod-smoke.env`
- The service account currently sees the `LocalAgents` vault; do not use the `AronBot` vault for this secret.
- Never put the Turso token in the skill, prompts, logs, or shell arguments.
- If the Turso ledger or 1Password service-account read fails, stop and report the blocker instead of generating suggestions without dedupe.

Before generating ideas, load prior suggestions with the bundled helper:

```bash
node /Users/aron/.codex/skills/suggest-tracker-projects/scripts/turso-ledger.mjs list
```

Treat an idea as already suggested if its title, slug, core tracked entity, or core accountability question is substantially the same as a prior Turso row. Do not merely rename the same concept.

After deciding the final 10 and before sending them to the user, record each emitted idea:

```bash
node /Users/aron/.codex/skills/suggest-tracker-projects/scripts/turso-ledger.mjs record < ideas.json
```

Each recorded JSON object must include `slug`, `title`, `bucket`, `canonical_question`, `pitch`, `hook`, `tracks`, `data_spine`, `why_now`, `sources`, and `suggested_at`.

## Source Policy

- Use Seattle Times for Seattle and Pacific Northwest signals.
- Use ABS-CBN for Philippines national signals.
- Mention NYT as excluded for now when U.S. national context would normally be useful: local DNS currently resolves `www.nytimes.com` to `0.0.0.0` through Tailscale Quad100, so do not attempt NYT unless the user asks to retest it.
- If Browse is available, prefer:

```bash
browse open https://www.seattletimes.com/ --local --headed
browse get text body
browse stop --force
browse open https://news.abs-cbn.com/ --local --headed
browse get text body
browse stop --force
```

- Treat `waitForMainLoadState(load) timed out` as non-fatal if `browse get text body` returns page content.
- Headless Browse and simple HTTP may be blocked by CloudFront or Akamai; do not conclude that the site is unavailable until headed Browse has been tried.

## Idea Standard

Recommend maintained tracker products, not generic investigations.

Favor ideas with:
- repeated public data updates,
- accountable institutions or vendors,
- geographic or entity-level comparison,
- a public-facing map, database, alert feed, or scorecard,
- a clear affected public,
- a reason the issue is timely now.

Downrank ideas that are just article recaps, pure explainers, one-off scandals with no data exhaust, or advocacy pages without a measurable tracking loop.

## Output Format

Return exactly 10 ranked recommendations. Do not use a table.

Use readable mini-briefs with blank lines between ideas:

```markdown
1. Title
Bucket: Seattle local, U.S. national, Philippines national, or cross-border

One-sentence pitch.

Hook: One sentence tying the idea to the source scan.
Tracks: What the product continuously tracks.
Data spine: Likely data sources, records, feeds, or repeatable collection methods.
Why now: One sentence on urgency/newsworthiness.
```

End with:
- `NYT note`: one sentence saying NYT was not run because of the current local DNS issue.
- `Top pick`: one title and the reason it should be built first.
- `Dedupe`: count of prior suggestions checked and count of new suggestions recorded in Turso.
