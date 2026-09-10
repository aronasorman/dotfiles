---
name: deploy-evidence
description: Use when preparing or executing a deployment in any environment, including infrastructure applies, service configuration changes, migrations, and rollouts, where commands, results, and verification need to be tracked.
---

# Deploy Evidence

Make each meaningful deployment step visible as it happens, in any environment. Show the command, its result, and what that result proves.
Keep the existing deployment procedure simple.

## Scope and state

- State the target environment and resources before execution. Use explicit target arguments where the tool supports them.
- Preserve the approved scope and deployment method. This skill adds reporting, not permission or a new deployment framework.
- A preparation request permits preparation, not application. Label preparation, dry runs, actual mutations, and smoke tests separately.
- Reuse explicit authorization already present in the conversation. Do not request the same approval again unless the scope materially changes.
- Use the existing backup and rollback procedure. Show its evidence paths and identify the source revision or artifact hash when relevant.
- Do not add gates, permanent infrastructure, scripts, or unrelated repairs just to satisfy this skill.

## Commands and output along the way

For each meaningful operation, show the exact executed command in a fenced code block and then its observed output.
Use commentary during execution, not only a final transcript.
Group related read-only checks when their results remain clear. Report mutations separately.

Show short output verbatim. For long output, give a concise result summary, the exit status, and a link to retained evidence when available.
Explain expected special exit codes, such as a diff exit code of 1.
Exclude credentials, tokens, connection strings, patient data, and other sensitive values from displayed commands and saved output.
Prefer secure references over literal secrets. Label necessary redactions.

Do not present an illustrative command as executed. For a scripted batch, show the actual command pattern and summarize its per-target results.
Keep shell setup and bookkeeping out of the progress narrative unless they explain a blocker.

Example after a successful apply:

```sh
kubectl --context aks-prod -n payments apply -f /tmp/approved-gateway.yaml
```

```text
configmap/gateway configured
```

If an operation fails, show the failure before dependent work. Read live state before retrying an ambiguous mutation.
Use an authorized rollback when appropriate. Surface a required decision if recovery changes the approved scope.

## Smoke results

Capture a relevant baseline before mutation. Afterward, verify actual runtime readiness and the changed behavior.
An apply or rollout success alone does not prove a working service.
For HTTP routing, verify the responding service or expected content as well as its status.

Present before/after results with these distinctions:

- Previously healthy behavior that remains healthy.
- The new behavior and its acceptance result.
- Approved intentional changes.
- Pre-existing failures, new regressions, and checks that remain blocked or unverified.

If an expected value changes, verify the intended target independently before correcting the check. Record why it changed.
Never weaken a check merely to make the result pass.
Match claims to coverage. A routing smoke test does not prove an entire business workflow.

## Close the run

Lead with what was applied, where, and the smoke outcome.
State remaining failures and whether rollback occurred.
Link the backup and detailed results in the existing project run record.
Prefer compact result tables over raw logs.
Use the repository's tracking and evidence conventions without creating a duplicate command-history system.
