---
name: explaining-bugs-and-incidents
description: Use when the user asks for a bug explanation, incident or indicent explanation, failure analysis, outage summary, regression explanation, symptom explanation, or root-cause explanation, including requests to explain again or provide links for verification.
---

# Explaining Bugs and Incidents

## Goal

Make the failure understandable and independently checkable. Lead with what happened, then trace why it happened.

## Response contract

1. **Short version:** State the user-visible symptom and whether it is ongoing, historical, latent, or unverified.
2. **Symptom:** Contrast expected and observed behavior. Name the trigger and affected scope.
3. **Problem:** Give a numbered causal chain: trigger/context -> failing mechanism -> masking or propagation -> downstream impact. Separate the primary defect from secondary errors.
4. **Evidence:** Put direct links beside the claims they prove. Prefer the failing run or log, immutable source lines, and the governing ticket or discussion. Note when access requires authentication.
5. **Current status:** Distinguish confirmed failures, workarounds, implemented fixes, and acceptance proof.

Use established evidence. Label inference and correlation. If root cause is not proved, call it the leading hypothesis and state what evidence is missing.

For a bug, emphasize expected versus actual behavior and reproduction conditions. For an incident, add the timeline, impact, mitigation, and recovery state when known.

## Evidence rules

- A canceled run proves neither failure nor recovery.
- A successful manual bypass does not validate the broken automatic path.
- A ticket status is not runtime proof.
- A downstream error is not the root cause when an earlier failure explains it.
- Links should resolve to the exact run, log, source range, ticket, or thread—not a homepage.

## Boundary

Use this skill for ordinary chat explanations. If the user explicitly invokes `$fix-explainer`, use that artifact-producing skill instead. Do not mutate systems while explaining unless the user separately asks for a change.
