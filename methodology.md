# Methodology

This project is spec‑driven. Official tests are treated as pointers to required behavior, not as the full definition.

## For each session (required)
1. Read the root MDs: `design.md`, `principles.md`, `tests/artisinal/README.md`, and this file.
2. Run the chooser: `python scripts/choose_mode.py`.
3. Perform exactly one mode (see “The work loop”).
4. Run relevant tests for your change (at least the focused Zig harness for affected artisinal files).
5. If you discover urgent external blockers, add them via `wiggum/scripts/blockers.py` with bug‑report quality detail (clear steps, rationale, pointers).
6. Commit any changes with a subject + detailed body (see Git discipline).
7. End output with:
   - `ONE-LINE LOG: <high-signal summary, <=140 chars>`
   - then exactly one of: `STEP COMPLETE`, `STEP FAILED`, or `PROJECT FINISHED`.

Notes:
- Do not edit files under `wiggum/` unless explicitly asked; use scripts there instead.
- Skim relevant spec sections in `spec/` and existing artisinal tests before changing behavior.

## Official tests and source data
- Converted official tests live at:
  - `tests/r5/tests-fhir-r5.json`
  - example inputs `tests/r5/input/`
  - (original XML, used to generate tests-fhir-r5.json) `tests/r5/tests-fhir-r5.xml`
- These are produced by `scripts/prepare_tests_json.py` after `scripts/fetch_spec.sh`.
- `build/tmp-spec/` is a scratch checkout used by the fetch script; don’t treat it as the authoritative test location.

## The work loop

Choose one mode per session:

### Mode selection (randomized, state‑aware)
Start by running the chooser script:

```
python scripts/choose_mode.py
```

The script uses a summary “state” to bias the choice toward where effort is most needed, while **always keeping at least a 15% chance** for each mode. The state is a summary of:
- how many artisinal files exist (written),
- how many have been reviewed,
- how many have been implemented.

If there are open blockers, the chooser will also include a **TACKLE_BLOCKER** mode and select a specific blocker to work on.

By default the script derives this state from `tests/artisinal/*.json` using `meta.status` if present:
- `drafted` → written only
- `reviewed` → written + reviewed
- `implemented` → written + reviewed + implemented

The chooser derives state from the repository. If any internal state file is used by tooling, it should live under `wiggum/`.

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
- Blockers are managed via `wiggum/scripts/blockers.py` (do not hand‑edit).
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
Blockers are simple YAML (single‑line values only), with this shape:

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
- Commit messages must be meaningful and specific to the change, and should include a short body with details.
- Use a subject + body format (subject <= 72 chars), for example:
  - Subject: concise what/why
  - Body: 2–6 bullet lines covering key changes, tests (or why tests were skipped), and any learnings/surprises
- Include at least one line capturing insights, unexpected findings, or rationale (not just a diff summary).
- Do not embed literal `\n` sequences in commit messages; use multiple `-m` flags or a here‑doc so the body has real line breaks.
- External drivers can decide whether to keep or revert the commit based on the final step status.
