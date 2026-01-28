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
Goal: expand coverage by discovering gaps organically.

**Finding gaps** - sample randomly and let understanding emerge:
- Skim a random section of the FHIRPath spec (`spec/index.md`) and note functions/operators without artisinal coverage.
- Browse `tests/r5/tests-fhir-r5.json` randomly and find test patterns we haven't captured.
- Run `zig build harness -- -m models/r5/model.bin -q tests/r5/tests-fhir-r5.json` to see overall R5 pass rate.
- Compare what exists in `tests/artisinal/` against what the spec describes.

**Mining R5 failures for work items** - the official tests surface real gaps:
```bash
# See failure breakdown (parse errors, eval errors, mismatches)
zig build harness -- -m models/r5/model.bin tests/r5/tests-fhir-r5.json -q 2>&1 | grep -E "(TOTAL|Pass rate)"

# Sample some mismatches to understand patterns
zig build harness -- -m models/r5/model.bin tests/r5/tests-fhir-r5.json -n 30 2>&1 | grep -B2 "reason: mismatch" | grep "expr:"

# Find which functions appear most in failures
zig build harness -- -m models/r5/model.bin tests/r5/tests-fhir-r5.json -n 300 2>&1 | grep -B2 "reason: mismatch" | grep "expr:" | grep -oE '\.[a-zA-Z]+\(' | sort | uniq -c | sort -rn | head -15

# Check tests expecting errors that we don't throw
zig build harness -- -m models/r5/model.bin tests/r5/tests-fhir-r5.json -n 100 2>&1 | grep -B2 "expected error but succeeded"
```
Pick a cluster (e.g., "15 failures on `.is()` type checking") and create artisinal tests for that feature.

**Writing new tests**:
- Read the spec section(s) that define the correct behavior.
- Write a new artisinal test file with:
  - A short "what the spec says" note in your own words.
  - A TODO checklist for follow‑up work.
  - A set of tests covering normal and edge cases.
  - Any explicit assumptions or open questions.

### DEVELOP
Goal: make artisinal tests pass.

**Finding a seed issue**:
```bash
# Random failing test
zig build harness 2>&1 | grep '^\[FAIL\]' | shuf -n 1
```
Pick one and expand from there.

**Working the issue**:
1. Understand the seed failure - read the test, trace through the code
2. **Fix it first**: Implement the minimal fix for JUST that one test
3. Run harness to confirm the fix works
4. **Then expand**: Check if the fix also passes related tests (same function, similar patterns)
5. Update any `comment` fields that reference now-fixed limitations

**Scope discipline**:
- Do NOT try to understand the full system before fixing one test
- Do NOT fix test expectations unless you're certain they're wrong
- Do NOT implement multiple unrelated functions in one session
- If you find yourself reading more than ~5 files to understand one failure, stop and form a hypothesis - then make a focused change to test it

**Dependency chains**: Some features depend on more basic features. If your sampled failure requires unimplemented dependencies:
1. Identify what's missing (e.g., "toQuantity needs quantity literal parsing")
2. Check if that dependency has tests - search for related failures or comments
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

**Using reference engines**: You can use `python scripts/eval_engines.py` to check what other implementations return, but don't over-trust them—they can be wrong too. The spec is the authority. Use engine results as one data point, not the answer.

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
| `comment` | string | Free-form note about the test (not used by harness) |
| `expect_error` | boolean | If true, expect parse or eval error |
| `unordered` | boolean | Compare results as unordered set |
| `mode` | string | Parsing mode (for FHIR-specific tests) |

**Notes:**
- Use either `input` (inline) or `inputfile` (file reference), not both
- `env` keys should be bare names (e.g., `"root"` not `"%root"`)
- `comment` is for human-readable notes; update or remove comments when they become stale
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

### Test comments

Use `comment` to add context about a test. The harness ignores this field—it's purely for documentation:

```json
{
  "name": "parenthesized expression",
  "expr": "(1 + 2) * 3",
  "input": {},
  "comment": "Parser: parenthesized expressions not yet supported",
  "expect": [{"type": "integer", "value": "9"}]
}
```

**Maintaining comments**: When you implement a feature or fix a bug, check if any `comment` fields reference that work. Update or remove comments that are no longer accurate.

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

Compare our artisinal tests against multiple reference FHIRPath implementations and adjudicate differences through holistic analysis.

**Goal**: Ensure our expected values are correct by comparing against multiple engines (fhirpath.js, Firely .NET, HAPI FHIR), the FHIRPath spec, and official R5 tests.

**Reference engines**:
- `fhirpath.js` - JavaScript implementation (fast, good coverage)
- `Firely .NET` - C#/.NET implementation (requires FHIR resources, not plain JSON)
- `HAPI FHIR` - Java implementation (slow startup ~22s, but authoritative)

**Setup**:
```bash
cd vendor/fhirpath.js && bun install          # JavaScript engine
cd vendor/fhirpath.net/FhirPathRunner && dotnet build  # .NET engine  
cd vendor/fhirpath.hapi && mvn package        # Java engine
```

**Running the adjudication sampler**:
```bash
python scripts/adjudicate.py                  # Sample 5 disagreements (default)
python scripts/adjudicate.py --sample 3       # Sample 3 disagreements
python scripts/adjudicate.py --skip-slow      # Skip HAPI for faster iteration
python scripts/adjudicate.py comparison       # Filter by filename pattern
```

The sampler:
1. Runs fast engines (js, net) on all unadjudicated tests (~3s for 1500 tests)
2. Finds disagreements where at least one engine returns a different non-error result
3. Samples N disagreements randomly
4. Runs all engines (including slow HAPI) only on the sampled items
5. Outputs a detailed report showing all engine results

**Adjudication process**:

For each sampled disagreement:

1. **Review all engine results** - Note which engines agree/disagree and who errors
2. **Check the FHIRPath spec** (`spec/index.md`) - Find the relevant section, read carefully
3. **Check official R5 tests** - Search `tests/r5/tests-fhir-r5.json` for similar cases
4. **Form a judgment** - Determine correct behavior based on evidence
5. **Document your findings** - Add `_adjudicated` annotation with reasoning

**Key principles**:
- The spec is the primary authority, but it can be ambiguous or incomplete
- Engine consensus is informative but not definitive (all engines can be wrong)
- Official tests are strong evidence but may themselves have bugs
- When uncertain, document the ambiguity honestly rather than forcing a verdict
- Firely .NET errors on non-FHIR JSON are expected (not a real disagreement)

**Adjudication annotation format**:
```json
{
  "name": "round: negative decimal rounds away from zero",
  "expr": "(-3.5).round()",
  "expect": [{"type": "integer", "value": -4}],
  "_adjudicated": {
    "engines": {
      "fhirpath_js": [{"type": "number", "value": -3}],
      "firely_net": [{"type": "integer", "value": -4}],
      "hapi_fhir": [{"type": "integer", "value": -4}]
    },
    "our_old_value": null,
    "verdict": "ours_correct",
    "reason": "Spec §5.7.2 defines rounding behavior. Two of three engines agree with us. fhirpath.js appears to use JavaScript's default rounding which rounds toward zero.",
    "spec_ref": "§5.7.2 round()",
    "official_test": null
  }
}
```

**Adjudication fields**:
- `engines`: Results from each engine (copy from adjudicate.py output)
- `our_old_value`: Our previous expected value if we changed it (null if unchanged)
- `verdict`: One of the values below
- `reason`: Detailed explanation with evidence from spec, engines, and tests
- `spec_ref`: Section reference (e.g., "§5.7.2 round()")
- `official_test`: Name of relevant official test if found (null if none)

**Verdict values**:
- `ours_correct` - our expected value is correct based on spec/evidence
- `theirs_correct` - we were wrong, fixed our test (record old value in `our_old_value`)
- `spec_ambiguous` - spec doesn't clearly define behavior, document interpretations
- `engines_disagree` - engines disagree, no clear answer, we made a reasonable choice
- `engine_limitation` - difference due to engine limitations (e.g., JS Number type, .NET requires FHIR resources)

**Evidence strength (strongest to weakest)**:
1. Explicit spec text with examples
2. Official R5 test with matching case
3. Multiple engines agreeing
4. Spec text requiring interpretation
5. Single engine behavior

**When you can't determine the answer**:
It's okay to adjudicate with `spec_ambiguous` or `engines_disagree`. Document:
- What the spec says (or doesn't say)
- How each engine behaves
- Why we chose our current expected value
- What would be needed to resolve the ambiguity (e.g., "needs HL7 clarification")

**Note**: The adjudicate.py script automatically skips tests with `_adjudicated` annotations.
