# TerraLingua Rebuild Plan (`rkt/`)

Ground-up rebuild of the TerraLingua simulation core in **Racket**, with a DSL
designed for the simulation. This file is the decision log: it exists so the
project can be resumed cold. The normative contract for behavior is
[`SPEC.md`](SPEC.md).

## Why rebuild

A review of the Python codebase (~12.7k lines) found: localized but real bugs
(respawned agents ignoring the configured genome/motivation; toroidal vision but
non-toroidal interactions; rewards computed but never used), one 1,951-line
god-file (`env.py`), three drifting representations of the action schema
(prompt JSON, step handlers, param validators), a custom serialization layer
underneath pickle, and copy-pasted analysis plumbing. The architecture was
sound, but the decision was made to leave Python entirely. There is **no
legacy-compatibility requirement**: log/checkpoint formats may break.

## Why Racket

The SDK requirement is near zero — the entire LLM transport is two REST
endpoints (OpenAI Responses, Anthropic Messages). Language choice was therefore
driven by domain modeling, and the goal is a **DSL for the simulation**: Racket
is built for language-oriented programming. The decisive win: the Python bugs
were largely *drift between representations*; here, one macro generates the
action handler, the prompt JSON schema, and the param validator from a single
definition. Drift becomes unrepresentable.

Also for free: the PRG state serializes to a plain vector
(`pseudo-random-generator->vector`) for exact checkpoint/resume, and state as
prefab data round-trips through `write`/`read`.

## Design decisions (baked in from the review)

1. **State is data.** The world is pure prefab structs — no ports, no
   procedures, no PRG inside state. `step` is a pure function;
   `(values World (Listof Event))` out.
2. **Action registry via macros.** `define-action` generates handler + prompt
   schema + validator (Phase 1.5). Until then, a plain registry.
3. **Artifact single-location invariant by construction.** Location is a field
   on the artifact (`at pos` | `held tag`), not two parallel maps. Python's
   duplicate-cleanup pass is unrepresentable.
4. **O(1) indices.** `pos->agent` and `name->tag` maintained on every mutation.
5. **Observations are pure functions of world state.** Checkpoints store state
   only; resume rebuilds observations. Python's checkpoint-repair path does not
   exist.
6. **Toroidal consistency.** Vision and interaction both wrap (semantic change,
   SPEC §2).
7. **Rewards deleted.** They were computed and never consumed. Info strings
   remain.
8. **JSONL event log is the source of truth.** Versioned typed events; all
   other outputs derived.
9. **Owned thin transport.** ~300 lines, two endpoints, no SDK dependency.
10. **Determinism.** One seeded PRG threaded everywhere; agents act in
    sorted-tag order (semantic change, SPEC §2).

## v1 cut list (enforced)

Human agent, live rendering/video, PettingZoo API, `obs_style: grid`,
numerical-agent remnants, reward machinery, `reset_agent` event.

## Architecture

```
rkt/
  PLAN.md  SPEC.md  info.rkt
  main.rkt             CLI entry (Phase 3: runner; Phase 4: #lang surface)
  world/    state.rkt food.rkt artifacts.rkt actions.rkt step.rkt obs.rkt
  genome/   main.rkt   (ocean5 / rpg6 / notraits behind one interface)
  agent/    prompt.rkt parse.rkt memory.rkt
  llm/      transport.rkt retry.rkt
  runner/   loop.rkt checkpoint.rkt spawn.rkt
  eventlog/ writer.rkt
  tests/               rackunit; run with `raco test rkt/`
```

## Phasing

| Phase | Deliverable | Gate |
|---|---|---|
| 0 | `PLAN.md` + `SPEC.md` + scaffold + smoke test | `raco test rkt/` green |
| 1 | World core + genomes (plain Racket, no macros) | Property tests green |
| 1.5 | DSL v0: `define-action` / `define-event`; actions re-expressed | Generated schema == hand-checked |
| 2 | Agent + LLM transport vs mock server | Parser corpus + retry tests green |
| 3 | Runner + checkpoint/resume + eventlog | Ctrl+C–resume test passes |
| 4 | `#lang terralingua` surface; 8 paper configs ported; live smoke run | Cost-capped run passes |

The Python tree stays untouched as the executable reference until Phase 4's gate
passes; then archive it. The future analysis pipeline (re-imagined separately)
reads only the JSONL event log.

## Risks

- **DSL over-design** → library first; macros earn syntax only after semantics
  stabilize (Phase 1 before Phase 1.5).
- **Tokenizer fidelity** → internal-memory cap uses a documented chars-heuristic
  (no tiktoken in Racket), SPEC §5.
- **SIGTERM handling** → Ctrl+C is clean via `exn:break`; SIGTERM needs FFI or a
  shell-trap wrapper (Phase 3).
- **Prompt/message-structure drift** → prompts ported verbatim; parser test
  corpus written first (Phase 2).
