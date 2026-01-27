# Methodology

This project is spec‑driven. Official tests are treated as pointers to required behavior, not as the full definition.

## For each session (required)
1. Read the root MDs: `design.md`, `principles.md`, `tests/artisinal/README.md`, and this file.
2. Run the chooser: `python scripts/choose_mode.py`.
3. Perform exactly one mode (see “The work loop”).
4. Run relevant tests for your change:
   - `zig build harness` - all artisinal tests
   - `zig build harness -- -f <pattern>` - filter by name
   - `zig build harness -- tests/r5/tests-fhir-r5.json` - official R5 tests
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
- Refresh the XML + input JSONs from upstream with `scripts/fetch_tests.sh`.
- Regenerate JSON tests from XML with `scripts/prepare_tests_json.py` (e.g., `--fhir-version r5`).
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
- Sampling ideas (pick one):
  - Skim `tests/r5/tests-fhir-r5.json` and choose a function/operator that is missing in artisinal files.
  - Run the official test runner and pick a failing case to reverse‑engineer into artisinal tests.
  - Read a spec section that has little or no artisinal coverage and draft tests from it.
- Read the spec section(s) that define the correct behavior.
- Write a new artisinal test file with:
  - A short “what the spec says” note in your own words.
  - A TODO checklist for follow‑up work.
  - A set of tests that cover both normal and edge cases.
  - Any explicit assumptions or open questions.

### DEVELOP
Goal: implement to pass existing artisinal tests.
- Run the harness to identify failing tests:
  ```bash
  zig build harness          # summary of all artisinal
  zig build harness -- -n 5  # show first 5 failures
  zig build harness -- -f string  # focus on string-related tests
  ```
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
- **Comprehensive spec documentation** (see below)
- A TODO checklist using checkbox format (see below)
- Tests that must pass

### Spec documentation in artisinal files (critical)
The `_spec_summary` field is not just a brief note—it should be a **thorough, standalone explanation** of how this part of the spec works. Include:

1. **Core behavior**: What the function/operator does in plain language.
2. **Type handling**: What input types are accepted, how type coercion works.
3. **Empty collection semantics**: What happens with empty inputs (this is often surprising!).
4. **Edge cases**: Boundary conditions, special values, error conditions.
5. **Surprising behaviors**: Anything that might trip up an implementer:
   - Does empty string `''` behave differently from empty collection `{}`?
   - Are results ordered or unordered?
   - Does the function eliminate duplicates?
   - What happens with negative indices, out-of-bounds, etc.?
6. **Relationships to other functions**: e.g., "union uses `=` equality, not `~` equivalence"
7. **Direct spec quotes**: Include line numbers or section references when useful.

The goal is that someone implementing this feature can read the `_spec_summary` and understand the full behavior **without needing to read the spec themselves**. Future sessions will rely on this documentation.

### TODO checklist format
Use checkbox format in the `_todo` array so progress is trackable:
```json
"_todo": [
  "[ ] Improve passing rate (5/20 passing)",
  "[ ] Add tests for negative index edge case",
  "[x] Verify empty collection behavior against spec",
  "[ ] Open question: does this apply to DateTime?"
]
```
- `[ ]` = not done
- `[x]` = done
- Update checkboxes as work progresses across sessions

### Expected value format (critical)
Expected values must use explicit `type` and `value` fields:

```json
"expect": [
  {"type": "string", "value": "hello"},
  {"type": "integer", "value": 42},
  {"type": "date", "value": "2024-01-15"},
  {"type": "Quantity", "value": {"value": 10, "unit": "mg"}}
]
```

**Do NOT use FHIRPath literal prefixes** in expected values:
- ❌ `"@2024-01-15"` → ✅ `{"type": "date", "value": "2024-01-15"}`
- ❌ `"@T10:30:00"` → ✅ `{"type": "time", "value": "10:30:00"}`
- ❌ `"10 'mg'"` → ✅ `{"type": "Quantity", "value": {"value": 10, "unit": "mg"}}`

See `tests/artisinal/README.md` for the full type table.

### Unordered comparison (for set-like results)
Some FHIRPath operations return collections where order is not guaranteed. Use `"unordered": true` to compare as multisets:

```json
{
  "name": "union returns both collections",
  "expr": "a.union(b)",
  "input": {"a": [1, 2], "b": [3]},
  "unordered": true,
  "expect": [{"type": "integer", "value": 3}, {"type": "integer", "value": 1}, {"type": "integer", "value": 2}]
}
```

With `unordered: true`, the harness checks that:
- Same number of items in expected and actual
- Each expected item matches exactly one actual item (multiset comparison)
- Order doesn't matter

### Meta fields
- `meta.status` informs `scripts/choose_mode.py`:
  - `drafted` = written only
  - `reviewed` = written + reviewed
  - `implemented` = written + reviewed + implemented

Always include a TODO entry for the current pass/fail rate. If the pass rate is not 100%, add or update a TODO item that explicitly says:
- `"[ ] Improve passing rate (X/Y passing)"` with the current counts.
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

## CROSS_CHECK mode

Compare our artisinal tests against fhirpath.js reference implementation and adjudicate differences.

**Goal**: Ensure our expected values match the reference implementation, or document why they differ.

**Setup**: Run `cd vendor/fhirpath.js && bun install` once to install fhirpath.js.

**Steps**:
1. The mode chooser will give you a sample of unadjudicated differences (typically 3)
2. For each difference:
   a. Check the FHIRPath spec to determine correct behavior
   b. Add an `_adjudicated` annotation to the test case with your findings
   c. If our test is wrong, fix the expected value AND record the old value in the annotation
   d. If fhirpath.js is wrong, document why in the annotation

**Running manually**:
```bash
python scripts/cross_check.py              # all unadjudicated diffs
python scripts/cross_check.py -n 20        # limit to 20 tests
python scripts/cross_check.py math         # filter by filename
python scripts/cross_check.py --sample 3   # random sample of 3 diffs
```

**Adjudication annotation format** (add to the test case):
```json
{
  "name": "round: negative decimal rounds away from zero",
  "expr": "(-3.5).round()",
  "expect": [{"type": "integer", "value": -4}],
  "_adjudicated": {
    "fhirpath_js": -3,
    "our_old_value": null,
    "verdict": "ours_correct",
    "reason": "Spec says values <= -0.5 round away from zero. fhirpath.js rounds toward zero.",
    "spec_ref": "§Math functions, round()"
  }
}
```

**Adjudication fields**:
- `fhirpath_js`: What fhirpath.js returned
- `our_old_value`: Our previous expected value if we changed it (null if unchanged)
- `verdict`: One of the values below
- `reason`: Explanation of the decision
- `spec_ref`: Optional reference to spec section

**Verdict values**:
- `ours_correct` - our expected value is correct per spec, fhirpath.js is wrong
- `theirs_correct` - fhirpath.js is correct, we fixed our test (record old value in `our_old_value`)
- `both_valid` - both interpretations are valid (spec ambiguity)
- `js_limitation` - difference due to JS limitations (e.g., Number type can't distinguish int/decimal)

**Note**: The cross_check.py script automatically skips tests with `_adjudicated` annotations.
