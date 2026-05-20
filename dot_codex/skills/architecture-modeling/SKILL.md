---
name: architecture-modeling
description: Use when brainstorming has produced a design direction but state, transitions, retries, rollback, cutover, concurrency, authorization, or invariant questions remain before writing the full implementation spec.
---

# Architecture Modeling

Use this after brainstorming and before writing the full implementation spec.

This skill is currently Quint-only. Do not substitute another modeling language unless this skill is revised.

## Purpose

Create a Quint-backed architecture model packet that helps the human designer understand system constraints before the implementation spec is finalized.

The agent bootstraps the model. The human owns the architectural meaning.

## When To Use

Use when the proposed design includes:

- State machines or lifecycle transitions.
- Retry, idempotency, rollback, or partial failure.
- Migration cutovers, fallback routing, or staged promotion.
- Authorization or trust-boundary transitions.
- Concurrent actors or race-prone workflows.
- Invariants that should survive implementation details.

Do not use for simple CRUD, cosmetic changes, copy edits, or implementation-only refactors with no meaningful state model.

## Workflow

1. Read the brainstorming output, repo instructions, relevant Beads, specs, and nearby code.
2. Decide whether Quint is useful. If not, record `Architecture modeling: not applicable` with the reason.
3. Create an architecture model packet:
   - Quint model skeleton.
   - Actors and state vocabulary.
   - Transitions/actions.
   - Candidate invariants.
   - Assumptions and abstractions ledger.
   - Known blanks for the human to complete.
   - Suggested ways the model should inform implementation and verification.
4. Leave meaningful design blanks unresolved. Do not pretend the agent can infer correctness from prose.
5. Ask the human to complete or revise the model definitions, invariants, and intended use.
6. Run Quint checks when the model is executable.
7. Summarize traces, counterexamples, and design tensions.
8. Bundle the final model packet with the implementation spec, preferably in the same Bead or linked from the Bead.
9. Ensure the implementation spec maps each accepted invariant to a code, test, runtime guard, or explicit non-goal obligation.

## Packet Location

Follow repo instructions for planning artifacts. If the repo uses Beads, attach the packet to the relevant Bead or link the file from a Beads comment.

For a repo-local Quint bootstrap, prefer a durable checked-in directory such as `quint/` or `specs/quint/` when the repo intends to keep models as implementation contracts. Otherwise, keep exploratory model packets with the spec or planning document.

## Output Packet

```markdown
# Architecture Model Packet

Modeling result: applicable | not applicable

Quint model:
<path or embedded code>

Human-owned blanks:
- <blank>

Actors:
- <actor>

State vocabulary:
- <state>

Actions / transitions:
- <action>

Candidate invariants:
- <invariant>

Assumptions and abstractions:
- <assumption>

Counterexample notes:
- <trace or none>

Spec obligations:
- <invariant -> implementation/test/guard/non-goal>

Verifier obligations:
- <what verifier must check against the model>
```

## Verifier Contract

When a spec includes an architecture model packet, verifier must treat it as an optional formal contract.

Verifier checks:

- Modeled states map to real code, database fields, configuration, queues, routes, or documented operational states.
- Modeled transitions have corresponding implementation paths.
- Forbidden transitions have guards, tests, or explicit impossibility arguments.
- Accepted invariants are covered by tests, runtime checks, database constraints, or documented operational procedures.
- Assumptions and abstractions are not accidentally treated as production guarantees.
- Deviations from the model are called out as `PASS`, `FAIL`, or `HUMAN DECISION` depending on severity.

The verifier must not require the implementation to mirror Quint syntax. It checks shape and constraints, not textual structure.

## Common Mistakes

- Treating the agent-generated Quint file as finished architecture. It is a scaffold until the human completes the model and approves the abstractions.
- Modeling every implementation detail. Model the state and transition constraints that matter to the decision.
- Letting an unverified model become decorative. Run Quint once it is executable, or mark exactly why it remains a thinking artifact.
- Forgetting to map invariants into the implementation spec. Every accepted invariant needs a concrete implementation, test, guard, operational procedure, or explicit non-goal.
