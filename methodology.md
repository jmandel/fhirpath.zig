# Methodology

This project is spec‑driven. Official tests are treated as pointers to required behavior, not as the full definition.

## Always (before any change)
- Read the design docs: `zig/design.md`, `zig/principles.md`, `zig/tests/artisinal/README.md`.
- Skim the relevant spec section(s) in `zig/spec/` for the feature you’re touching.
- Check the current artisinal tests for the feature to avoid duplicating or contradicting expectations.

## Official tests and source data
- Converted official tests live at:
  - `zig/tests/r5/tests-fhir-r5.json`
  - (original XML) `zig/tests/r5/tests-fhir-r5.xml`
  - example inputs `zig/tests/r5/input/`
- These are produced by `zig/scripts/prepare_tests_json.py` after `zig/scripts/fetch_spec.sh`.
- `zig/build/tmp-spec/` is a scratch checkout used by the fetch script; don’t treat it as the authoritative test location.

## The work loop

Choose one mode per session:

### Mode selection (randomized, state‑aware)
Start by running the chooser script:

```
python zig/scripts/choose_mode.py
```

The script uses a summary “state” to bias the choice toward where effort is most needed, while **always keeping at least a 15% chance** for each mode. The state is a summary of:
- how many artisinal files exist (written),
- how many have been reviewed,
- how many have been implemented.

If there are open blockers in `blockers.yaml`, the chooser will also include a **TACKLE_BLOCKER** mode and select a specific blocker to work on.

By default the script derives this state from `tests/artisinal/*.json` using `meta.status` if present:
- `drafted` → written only
- `reviewed` → written + reviewed
- `implemented` → written + reviewed + implemented

You can also pass an explicit state file:

```
python zig/scripts/choose_mode.py --state tests/artisinal/state.json
```

State format:

```
{
  "artisinal_total": 12,
  "reviewed_total": 5,
  "implemented_total": 3,
  "params": {
    "explore_scale": 10.0,
    "floor": 0.15,
    "seed": 12345
  }
}
```

Parameters:
- `explore_scale`: how quickly EXPLORE probability drops as the number of artisinal files grows.
- `floor`: minimum probability for each mode (default 0.15).
- `seed`: optional RNG seed for reproducibility.

### EXPLORE
Goal: expand coverage.
- Pick a capability seen in official tests that has no artisinal file yet.
- Read the spec section(s) that define the correct behavior.
- Write a new artisinal test file with:
  - A short “what the spec says” note in your own words.
  - A TODO checklist for follow‑up work.
  - A set of tests that cover both normal and edge cases.
  - Any explicit assumptions or open questions.

### DEVELOP
Goal: implement to pass existing artisinal tests.
- Run the Zig harness and identify failing tests.
- Implement the smallest correct change to make the tests pass.
- Add/adjust tests only if they clarify the spec or correct a wrong assumption.

### CONFIRM
Goal: verify correctness of existing tests.
- Review an artisinal file for internal consistency.
- Cross‑check each expectation against the spec text.
- Tighten or correct tests when they diverge from the spec.

### TACKLE_BLOCKER
Goal: resolve a high‑level blocker.
- Blockers live in `blockers.yaml` (YAML) and are managed via `wiggum/scripts/blockers.py` (do not hand‑edit).
- Run `python wiggum/scripts/blockers.py list` to see open blockers.
- Work the selected blocker to completion or move it forward (e.g., from `open` to `in_progress`).
- Update the blocker via `python wiggum/scripts/blockers.py update <slug> ...` or `resolve <slug>`.

## Artisinal file structure & TODOs
Each artisinal file should combine:
- Spec distillation (plain‑language summary of expected behavior).
- A TODO checklist (future work and open questions).
- Tests that must pass.

Always include a TODO entry for the current pass/fail rate. If the pass rate is not 100%, add or update a TODO item that explicitly says:
- “Improve passing rate (X/Y passing)” with the current counts.

## Blockers file format
`blockers.yaml` is simple YAML (single‑line values only), with this shape:

```
version: 1
blockers:
  - slug: parse-function-calls
    status: open
    severity: high
    title: Parse function calls in expressions
    description: One‑line description of the blocker.
    created: 2026-01-27
    updated: 2026-01-27
    tags: [parser, tests]
```

Use `wiggum/scripts/blockers.py` for all updates; the scripts keep the format deterministic for tooling.

## Why this loop converges
- Official tests seed coverage.
- Artisinal tests document the spec in our own words and expand edge‑case coverage.
- Implementation follows the artisinal tests, not the other way around.
- Repeating EXPLORE → DEVELOP → CONFIRM gradually builds a complete, robust spec‑faithful engine.

## Testing posture
- Prefer Zig‑native tests first for clearer errors and easier debugging.
- Once logic is correct in Zig, wire it through the ABI boundary and add integration tests.

## Rework policy
- If a mistake blocks the current task or invalidates its assumptions, do the minimal rework needed to unblock, then return to the original task.
- If it isn’t blocking, record it as a rework note and keep going; address it in a later DEVELOP or CONFIRM session.
- If a redesign affects public ABI or core semantics, treat it as its own session: update design docs first, then tests, then code.

## Autonomy and completion signaling
- Each session is fully autonomous: pick a mode via the chooser, execute the step, and stop.
- At the end of a session, output exactly one of:
  - `STEP COMPLETE`
  - `STEP FAILED`
  - `PROJECT FINISHED`

## Git discipline (required)
- Any session that edits or creates files must end with a git commit, regardless of success or failure.
- Commit messages must be meaningful and specific to the change.
- External drivers can decide whether to keep or revert the commit based on the final step status.
