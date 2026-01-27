# Methodology

This project is spec‑driven. Official tests are treated as pointers to required behavior, not as the full definition.

## For each session (required)
1. Root MDs (`methodology.md`, `design.md`, `principles.md`) are provided at session start.
2. Run the chooser: `python scripts/choose_mode.py`.
3. Perform exactly one mode (see “The work loop”).
4. Run relevant tests for your change:
   - `zig build harness` - all artisinal tests
   - `zig build harness -- -F <pattern>` - filter by filename
   - `zig build harness -- -t <pattern>` - filter by test name
   - `zig build harness -- tests/r5/tests-fhir-r5.json` - official R5 tests
5. If you discover urgent bugs, add them via `wiggum/scripts/bug_backlog.py` with bug‑report quality detail (clear steps, rationale, pointers).
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

If there are open bugs, the chooser will also include a **FIX_BUG** mode and select a specific bug to work on.

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
Goal: make artisinal tests pass.

**Finding a seed issue**:
```bash
# Random failing test
zig build harness 2>&1 | grep '^\[FAIL\]' | shuf -n 1

# Random skipped test
jq -r '.cases[]? | select(.skip) | "\(.name): \(.skip)"' tests/artisinal/*.json | shuf -n 1
```
Pick one and expand from there.

**Working the issue**:
1. Understand the seed failure - read the test, trace through the code
2. **Fix it first**: Implement the minimal fix for JUST that one test
3. Run harness to confirm the fix works
4. **Then expand**: Check if the fix also passes related tests (same function, similar patterns)
5. Remove `skip` from any tests that now pass

**Scope discipline**:
- Do NOT try to understand the full system before fixing one test
- Do NOT fix test expectations unless you're certain they're wrong
- Do NOT implement multiple unrelated functions in one session
- If you find yourself reading more than ~5 files to understand one failure, stop and form a hypothesis - then make a focused change to test it

**Dependency chains**: Some features depend on more basic features. If your sampled failure requires unimplemented dependencies:
1. Identify what's missing (e.g., "toQuantity needs quantity literal parsing")
2. Check if that dependency has tests - search for related failures/skips
3. Pivot to implementing the dependency instead
4. Return STEP COMPLETE - you made progress on a real blocker

**Discovered bugs**: While working, you may discover bugs unrelated to your sampled issue:
- **Blocking bug** (prevents progress on current issue) or **engine bug** (underlying problem affecting correctness): Fix it instead, return STEP COMPLETE. You made real progress.
- **Incidental bug** (matters but doesn't block you): File a backlog issue via `wiggum/scripts/bug_backlog.py add`, then continue on your original issue.

**Commands**:
```bash
zig build harness                    # summary of all artisinal
zig build harness -- -n 5            # show first 5 failures
zig build harness -- -F string       # filter by filename (string-*.json)
zig build harness -- -t divide       # filter by test name
grep -r '"skip":' tests/artisinal/   # find skipped tests
```

**Discipline**:
- Implement the smallest correct change to make tests pass
- Add/adjust tests only if they clarify the spec or correct a wrong assumption
- Update `_todo` checkboxes and pass rates in affected files
- **Incremental progress is success**: If you leave something better than you found it, consider the step complete and wrap up. Don't chase perfection—ship the improvement.

### CONFIRM
Goal: verify correctness of existing tests.
- Review an artisinal file for internal consistency.
- Cross‑check each expectation against the spec text.
- Tighten or correct tests when they diverge from the spec.

### FIX_BUG
Goal: resolve a bug from the backlog.
- Bugs are managed via `wiggum/scripts/bug_backlog.py` (do not hand‑edit).
- Run `python wiggum/scripts/bug_backlog.py list` to see open bugs.
- Work the selected bug to completion or move it forward (e.g., from `open` to `in_progress`).
- Update the bug via `python wiggum/scripts/bug_backlog.py update <slug> ...` or `resolve <slug>`.

## Artisinal file structure & TODOs
Each artisinal file should combine:
- **Comprehensive spec documentation** (see below)
- A TODO checklist using checkbox format (see below)
- Tests that must pass

## Test case format (unified)

The harness supports both artisinal and official test formats. It auto-detects based on the array key (`cases` = artisinal, `tests` = official).

### Test case fields

| Field | Type | Description |
|-------|------|-------------|
| `name` | string | Test name (required) |
| `expr` or `expression` | string | FHIRPath expression to evaluate (required) |
| `input` | object | Inline JSON input document |
| `inputfile` | string | Path to input file (alternative to `input`) |
| `expect` or `outputs` | array | Expected output items |
| `env` | object | Environment variables (keys without `%` prefix) |
| `skip` | string | Skip test with this reason |
| `expect_error` | boolean | If true, expect parse or eval error |
| `unordered` | boolean | Compare results as unordered set |
| `mode` | string | Parsing mode (for FHIR-specific tests) |

**Notes:**
- Use either `input` (inline) or `inputfile` (file reference), not both
- `env` keys should be bare names (e.g., `"root"` not `"%root"`)
- `skip` should explain why the test is skipped (e.g., parser limitation)
- Tests with `predicate: true` are filtered out during JSON generation (not supported)

### Expecting errors

Use `expect_error: true` when the expression should fail to parse or evaluate:

```json
{
  "name": "type error on comparison",
  "expr": "1 > 'hello'",
  "input": {},
  "expect_error": true
}
```

The test passes if parse or eval throws an error. It fails if the expression succeeds.

### Skipping tests

Use `skip` with a reason string for tests that can't run yet:

```json
{
  "name": "parenthesized expression",
  "expr": "(1 + 2) * 3",
  "input": {},
  "skip": "Parser: parenthesized expressions not supported",
  "expect": [{"type": "integer", "value": "9"}]
}
```

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
## Bug backlog file format
Bugs are simple YAML (single‑line values only), with this shape:

```
version: 1
bugs:
  - slug: parse-function-calls
    status: open
    severity: high
    title: Parse function calls in expressions
    description: One‑line description of the bug.
    created: 2026-01-27
    updated: 2026-01-27
    tags: [parser, tests]
```

Use `wiggum/scripts/bug_backlog.py` for all updates; the scripts keep the format deterministic for tooling.

## Upstream issues (spec bugs, test discrepancies)

When we discover issues that should be reported to FHIRPath maintainers, track them using `wiggum/scripts/upstream.py`. This is for issues in **the specification or official tests**, not our artisinal tests.

### When to file an upstream issue

File an upstream issue when you find:
- **Spec bugs**: Errors, ambiguities, or contradictions in the FHIRPath specification
- **Spec/test disagreements**: Official tests that contradict the spec language (NOT artisinal vs official)
- **Test errors**: Incorrect expected values in official test suites
- **Missing tests**: Important edge cases the official tests don't cover
- **Clarification requests**: Spec wording that multiple implementations interpret differently

**Do NOT file upstream for**:
- Differences between our artisinal tests and official tests (our tests might be wrong)
- Implementation bugs in fhirpath.zig
- Internal bugs (use `bug_backlog.py` instead)

### Target: fhirpath-editors

Currently we file upstream issues to the FHIRPath editors. The `upstream.py` CLI tracks the target explicitly so we can generalize later if needed (e.g., different targets for spec vs test issues).

Upstream repositories:
- **FHIRPath spec**: https://github.com/HL7/FHIRPath (file spec issues here)
- **FHIR test cases**: https://github.com/FHIR/fhir-test-cases (file test issues here)
- **FHIR JIRA**: https://jira.hl7.org/projects/FHIR (formal HL7 tracker)

### Workflow

1. **Draft**: Add issue with `status=draft` while investigating
   ```bash
   python wiggum/scripts/upstream.py add \
       --title "union() ordering not specified" \
       --category spec-ambiguity \
       --description "Spec does not define whether union() preserves order" \
       --spec_section "5.4.1"
   ```

2. **Ready**: Mark as ready when investigation is complete
   ```bash
   python wiggum/scripts/upstream.py ready union-ordering-not-specified
   ```

3. **Report**: Generate the issue report for filing
   ```bash
   python wiggum/scripts/upstream.py report union-ordering-not-specified
   ```
   Copy the output and file at the appropriate upstream repository.

4. **Track**: Update with the upstream issue URL
   ```bash
   python wiggum/scripts/upstream.py reported union-ordering-not-specified \
       --url "https://github.com/HL7/FHIRPath/issues/123"
   ```

### CLI commands

```bash
# List pending issues (excludes reported/fixed)
python wiggum/scripts/upstream.py list

# List all issues including reported
python wiggum/scripts/upstream.py list --all

# Show issue details
python wiggum/scripts/upstream.py show <slug>

# Add a spec ambiguity
python wiggum/scripts/upstream.py add --title "..." --category spec-ambiguity --spec_section "..."

# Add a test error
python wiggum/scripts/upstream.py add --title "..." --category test-error \
    --test_file "tests-fhir-r5.xml" --test_name "testFoo" \
    --expected "true" --actual "false"

# Generate report for filing
python wiggum/scripts/upstream.py report <slug>

# Mark as reported
python wiggum/scripts/upstream.py reported <slug> --url "..."
```

### Issue categories

- `spec-ambiguity`: Spec language is unclear or can be interpreted multiple ways
- `spec-error`: Spec contains an error or contradiction
- `test-error`: Official test has wrong expected value
- `test-missing`: Important case not covered by official tests
- `clarification`: Request for clarification on intended behavior

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

Compare our artisinal tests against fhirpath.js reference implementation and adjudicate differences through careful spec analysis.

**Goal**: Ensure our expected values are correct per the FHIRPath specification, using fhirpath.js as a reference point for discovering potential issues.

**Setup**: Run `cd vendor/fhirpath.js && bun install` once to install fhirpath.js.

**Steps**:
1. The mode chooser will give you a sample of unadjudicated differences (typically 3)
2. For each difference, **carefully review the FHIRPath spec**:
   a. Find the relevant section in `spec/index.md` (search for function name, operator, or concept)
   b. Read the full specification text, including any notes, examples, and edge cases
   c. Check if there are related official tests in `tests/r5/tests-fhir-r5.json`
   d. Determine the correct behavior according to the spec
3. Add an `_adjudicated` annotation documenting your findings
4. If our test was wrong, fix the expected value AND record the old value

**Key principle**: The spec is the source of truth, not fhirpath.js. fhirpath.js may have bugs or different interpretations. Your job is to determine what the spec actually says.

**Running manually**:
```bash
python scripts/cross_check.py              # all unadjudicated diffs
python scripts/cross_check.py -n 100       # limit to 100 tests
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
    "fhirpath_js": [{"type": "number", "value": -3}],
    "our_old_value": null,
    "verdict": "ours_correct",
    "reason": "Spec §5.7.2 says 'a decimal value less than or equal to -0.5 and greater than -1.0 will round to -1', implying rounding away from zero. -3.5 should round to -4, not -3.",
    "spec_ref": "§5.7.2 round()"
  }
}
```

**Adjudication fields**:
- `fhirpath_js`: What fhirpath.js returned (copy exactly from cross_check output)
- `our_old_value`: Our previous expected value if we changed it (null if unchanged)
- `verdict`: One of the values below
- `reason`: Detailed explanation citing the spec
- `spec_ref`: Section reference (e.g., "§5.7.2 round()" or "§6.3 Type Functions")

**Verdict values**:
- `ours_correct` - our expected value is correct per spec, fhirpath.js is wrong or has a bug
- `theirs_correct` - fhirpath.js is correct per spec, we fixed our test (record old value)
- `both_valid` - spec is ambiguous, both interpretations are defensible (explain why)
- `js_limitation` - difference due to JS limitations (e.g., Number type can't distinguish int/decimal)

**Spec review checklist**:
- [ ] Found the relevant spec section
- [ ] Read the full function/operator description
- [ ] Checked for edge cases mentioned in the spec
- [ ] Looked for related examples in the spec
- [ ] Checked official tests for similar cases
- [ ] Documented reasoning with spec citations

**Note**: The cross_check.py script automatically skips tests with `_adjudicated` annotations.
