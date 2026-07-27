# TerraLingua Functional Spec (v1)

Normative contract for the Racket rebuild. Semantics are extracted from the
Python implementation (`core/`); **deliberate changes** are marked ⚠️CHANGE and
collected in §10. Where this spec and the Python code disagree, this spec wins.

## §1 World model

- **Grid**: `grid-size`² cells, toroidal. `wrap(x) = x mod grid-size`. All
  coordinates wrap; "out of bounds" does not exist. (Python had a dead "X"
  branch — removed.)
- **Agent**: tag (symbol, stable, e.g. `being0`), display name (string, unique
  across living *and* dead agents), position, energy, time-left, color
  (`#f` | string), inventory (set of artifact names), trajectory (append-only).
  Live trajectories are stored newest-first for O(1) extension and exposed in
  chronological order through `agent-trajectory-chronological`; archived
  trajectories are chronological. Spawn is a list of child tags.
- **Food**: hash pos → value. Per step (§2 S7): each tile decays by
  `food-decay-amount` with probability `food-decay-rate`; expired tiles
  (value ≤ 0) are removed; then `Poisson(food-spawn-rate)` new tiles spawn at
  `max-food-value`. Spawn placement follows a density field: uniform, or a
  mixture of Gaussians over `food-zones` centers (σ = 2.0, toroidal distance),
  zeroed on cells occupied by agents, existing food, or artifacts.
  `static-food?` mode respawns only at previously-emptied cells.
- **Artifact**: text only (v1). Payload ≤ 500 tokens, enforced at create/modify
  time. Fields: name (unique, auto-suffixed `_1` on collision), payload,
  location (`at pos` | `held tag` — exactly one, by construction), creator,
  lifespan, remaining-time (`+inf.0` = immortal), creation-time, version,
  version-creation-time, past-versions, users (tag → set of steps), deletion-time.
- **Genomes**: pluggable trait sets (`ocean5` | `rpg6` | `notraits`) behind one
  interface: `random-genome`, `mutate-genome`, `genome->string` (for prompts),
  `genome->jsexpr` / `jsexpr->genome`. `ocean5`: 8 float traits in [-1, 1]
  (fertility in [0.5, 1.0]), Gaussian mutation σ=0.3 rate 0.5, clamped.
  `rpg6`: six integer stats in [3, 18], rolled 3d6, mutation ±1 clamped.
- **Rule #1 — state is data**: world/agent/artifact/genome are prefab structs.
  No ports, procedures, or the PRG in state. The PRG is threaded by the caller
  and checkpointed via `pseudo-random-generator->vector` (§7).

## §2 Step pipeline

`step : World (Hash Tag → Action) PRG → (values World (Listof Event) (Hash Tag → Info))`

An `Action` is `#s(action name params message)`: name string, params
(hash string → datum), message string. `Info` is a list of (label . string)
pairs returned to the acting agent (fed to its next prompt).

Agents act in **sorted-tag order** (⚠️CHANGE: Python used insertion order).
`step` derives that roster from the living-agent hash. Missing action entries
default to `move`/`stay`; entries for non-living tags are rejected before any
transition.
For each acting agent, in order:

1. **S1 Validate**: action must be in the agent's available-actions with
   exactly the required param keys; else coerce to `move`/`stay` and note in
   info (no penalty — ⚠️CHANGE, rewards deleted).
2. **S2 Execute** the action handler (§3). Handlers return updated world,
   events, and an info string for the agent.
3. **S3 Message**: non-empty `message` is recorded as the agent's broadcast for
   this step and appended to the step's chat log.
4. **S4 Move & eat**: apply the move delta (wrap); if the target cell holds
   another agent, stay (no penalty — ⚠️CHANGE). If the destination has food,
   energy += value and the tile is consumed. Then apply passive effects of
   artifacts at the agent's cell and in its inventory (record users, emit
   `artifact-passive-interaction`), unless `inert-artifacts?`.

After all agents:

5. **S5 Artifact expiry**: remaining-time -= 1 for all artifacts; those ≤ 0
   expire (emit `artifact-removed`, record deletion-time).
   ⚠️CHANGE: an artifact created during this very step does not expire this
   step (Python's `lifespan+1` offset hack removed).
6. **S6 Drain**: every agent loses 1 energy and 1 time.
7. **S7 Deaths**: energy ≤ 0 or time-left ≤ 0 → agent dies: inventory drops at
   death cell, cell freed, `agent-died` event (reason: `hunger` | `old-age`),
   and food is left per `dead-agent-food`: `single` (death cell gets
   `max(max-food-value, remaining-energy)`), `area` (3×3 around death cell gets
   `max-food-value`), `none`.
8. **S8 Food**: decay + respawn per §1 (skipped when `not food-mechanism?`).

Observations and available-actions are **pure functions of world state** (§4),
computed on demand outside `step`. `step-count` increments at the end.

## §3 Action catalog

`move` — params: `direction` ∈ {up, down, left, right, stay}. One cell, wrap.
Always available.

`give` — params: `target` (being name), `amount` (int ≥ 1). Requires
`food-mechanism?` and target within vision radius (**wraps** — ⚠️CHANGE).
Transfers `min(amount, giver energy)`.

`take` — as `give`, but transfers `min(amount, target energy)` from target.

`reproduce` — params: `name`, `energy` (additional gift, int ≥ 0). Requires
`reproduction-allowed?` and parent energy ≥ `reproduction-cost`. Parent pays
`reproduction-cost` up front; on success (energy still ≥ 0 and a free 8-neighbor
cell exists): child tag `parent_N` (N = next per-parent index), name deduplicated
with `_1` suffix, child gets `init-energy` + gift from parent. Failure reasons:
`no-energy` | `no-space`.

`create_artifact` — params: `name`, `type` (= `text`), `payload`,
`lifespan` (int > 0, or -1 = immortal). Requires `artifact-creation?` and
energy ≥ `artifact-creation-cost`. Payload ≤ 500 tokens. Name dedup `_1`.
Created at the creator's cell.

`pickup_artifact` / `drop_artifact` — params: `name`. Requires
`use-inventory?`, artifact at agent's cell / in inventory.

`give_artifact` — params: `artifact_name`, `target_agent`. Requires
`use-inventory?`, artifact in giver inventory, target within vision (wraps).

`set_color` — params: `color`. Requires `use-colors?`.

`modify_artifact_<name>` / `destroy_artifact_<name>` — artifact-defined
actions, available when the artifact is at the agent's cell or in inventory.
Modify takes `payload` (≤ 500 tokens) and optional `lifespan`; bumps version,
pushes past version. Destroy sets remaining-time to 0.

**Availability rules** (driving the prompt schema): `give`/`take` only when a
being is within vision and `food-mechanism?`; `create_artifact` when enabled
and affordable; pickup/drop/give-artifact per inventory contents and colocated
artifacts; `reproduce` when allowed and affordable; artifact-defined actions
when colocated or held.

## §4 Observation contract (v1: list style only)

Egocentric, vision-radius square, **toroidal**. Maps relative `(dx, dy)` → list
of cell contents: food value (as string), being display names (with color when
`use-colors?`: `Name(color)`), artifacts as `A(text): name`. Only non-empty
cells appear. Plus: `message` (map name → last broadcast from co-visible
beings), `energy`, `time`, `inventory` (list of `A(text): name`),
`vision_radius`.

## §5 Agent contract

- **Prompt**: system prompt rendered from template (ported verbatim from
  `core/agents/prompt_templates.py`): identity, observation format, energy/time
  rules, action rules, communication, internal memory, artifacts, inventory,
  genome traits, exogenous motivation (`base` | `creative` | `none`).
- **User turn**: history (last `max-history` steps: obs/info/action/message),
  current observation, additional info from the environment, available actions
  as JSON schema, reply-format instructions.
- **Response**: JSON `{action, message, params, internal_memory?}`.
- **Tolerant parse pipeline** (test corpus first, Phase 2): strip `</think>`
  prefix → code-fence JSON → first `{...}` span → repair unquoted keys and
  trailing commas → case-fold keys → validate action ∈ available and param keys
  exact. On failure: append error message and reprompt; ≤ 5 attempts; then
  **fallback** `move`/`stay` with empty message.
- **Internal memory**: returned verbatim next step; budget
  `internal-memory-size` tokens. ⚠️CHANGE: approximated as `4 × size` chars
  (no tiktoken in Racket), tail-truncated.
- **Decision state** (history, internal memory, prompt) lives in the agent
  layer, NOT in world state.

## §6 Invariants (property tests)

- **I1**: ≤ 1 agent per cell.
- **I2**: each artifact in exactly one location (map cell XOR one inventory).
- **I3**: energy changes only via: −1/step drain, +food value, ±transfers,
  −action costs.
- **I4**: population = initial + reproduced + respawned − died.
- **I5**: tags unique; display names unique across living and dead.
- **I6**: `pos->agent` and `name->tag` indices exactly mirror agent state.

## §7 Checkpoint format (v2)

One file, atomically written (tmp + rename). Plain `write`-able datum:
`#s(checkpoint version step prg-state world agents config)` where `prg-state`
is `pseudo-random-generator->vector` output; `world` is §1 state; `agents` is
decision state (§5); `config` is §8 params. `version` = 2; the loader rejects
unsupported versions. Version 2 stores live trajectories newest-first. The
loader accepts version 1 and migrates its chronological live trajectories.
Resume restores PRG via `vector->pseudo-random-generator` and rebuilds
observations from state.

Checkpoint configuration is authoritative on resume. Only enumerated explicit
runner overrides are applied; `max-steps-override` replaces `max-ts` and is
persisted in the next checkpoint. Agent LLM calls are bounded by
`max-parallel-workers` and are prepared from one immutable pre-step world.
Ctrl+C cancels outstanding workers and checkpoints the last fully committed
world, decision-state, event-log, and PRG boundary.

The CLI rejects `--config` together with `--checkpoint`; resumed runs use the
checkpoint configuration plus explicit CLI overrides. `--seed` accepts exact
integers from 0 through 2³¹−1 and rejects other input before run artifacts are
created.

## §8 Config surface

Mirrors `run_experiment.sh`: `exp-name exp-description max-ts |
provider model openai-base-url | agents-name-prefix name-seed
exogenous-motivation genome max-history internal-memory-size
use-internal-memory use-inventory use-colors | grid-size init-agents
min-agents init-agent-energy init-food food-zones food-mechanism
agent-lifespan vision-radius dead-agent-food artifact-creation
artifact-creation-cost inert-artifacts reproduction-allowed reproduction-cost |
max-parallel-workers ckpt-interval`. Phase 4: expressed as `#lang terralingua`
programs; the 8
`paper_experiment_scripts/*.sh` conditions are the port targets.

## §9 Event schema (v1)

JSONL, one JSON object per line, every record: `{v: 1, ts: int, type: string,
...}`. Types: `run-started`, `env-reset`, `agent-added`, `agent-died`,
`agent-reproduced`, `agent-moved`, `gift-energy`, `take-energy`, `artifact-added`,
`artifact-removed`, `artifact-interaction`, `artifact-passive-interaction`,
`artifact-pickup`, `artifact-drop`, `give-artifact`, `set-color`, `end-run`.
(`reset_agent` / `set_state_ckpt` from Python are dropped.) Per-agent traces
(one JSONL per tag) and a retry ledger (`{v, logged-at, layer, attempt,
outcome, ...}`) are derived outputs written alongside.

## §10 Semantic changes vs Python (all deliberate)

| # | Python | Racket v1 |
|---|---|---|
| 1 | Vision wraps; give/take/give-artifact don't | All interaction radii wrap |
| 2 | Rewards computed, never consumed | Deleted |
| 3 | −0.5 energy penalty for moving into an occupied cell | No penalty (stay) |
| 4 | −1 param penalty for unknown action/params | Coerce to stay, info note |
| 5 | Artifact expires on creation step (lifespan+1 hack) | Expiry starts next step |
| 6 | Agents act in insertion order | Sorted-tag order (determinism) |
| 7 | Internal memory capped via tiktoken | `4 × size` chars heuristic |
| 8 | Dead "X" out-of-bounds obs branch | Removed (grid is toroidal) |
| 9 | Respawned agents ignore configured genome/motivation (bug) | Use configured values |
| 10 | `reset_agent`, `set_state_ckpt` events | Dropped |
| 11 | Reproduction spawn cell and death "area" food bounded by grid edges | Both wrap (toroidal consistency) |
| 12 | 500-token artifact payload cap via tiktoken | 2000-char cap (4 chars ≈ 1 token) |
