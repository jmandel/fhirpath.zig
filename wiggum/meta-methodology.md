# Meta‑Methodology

This document explains how to design **methodology.md** and the **prompt** for an autonomous loop so that LLMs can apply the same system to new projects with minimal drift.

## Core goals
- **Single source of truth** for rules (methodology).
- **Minimal prompt** that points to methodology.
- **Deterministic tools** for state updates (scripts instead of hand‑editing).
- **Early failure detection** (tests each session, fast feedback).
- **Auditability** (one‑line logs + structured git summaries).

## What belongs in the prompt
Keep the prompt short and operational:
- “Read the root MDs, especially methodology.md.”
- “Run choose_mode and execute exactly one mode.”
- “End with required output lines (ONE‑LINE LOG + status).”

Avoid duplicating policies in the prompt. The prompt should **point** to methodology, not replace it.

## What belongs in methodology
Methodology is the **policy and procedure** source:
- **Per‑session checklist** (read docs, choose mode, run tests, commit, end with status).
- **Modes** and their goals (EXPLORE/DEVELOP/CONFIRM/FIX_BUG).
- **How to select modes** (randomized chooser, floors, inputs).
- **Testing posture** (what to run, how to report skips).
- **Git discipline** (subject+body, test reporting, learnings).
- **Artifact formats** (artisinal tests, bug backlog schema).
- **Rework policy** (blocking vs non‑blocking).
- **Completion signaling** (STEP COMPLETE / FAILED / PROJECT FINISHED).

If a rule is important for compliance, it should be in methodology. The prompt should merely remind the model to follow it.

## DRY principles
- **Prompt = thin wrapper**, methodology = full policy.
- **Scripts for edits** (bug backlog, state) to keep format deterministic.
- **Logs drive prompts**, not the other way around (one‑line logs feed future prompts).

## Tooling patterns
- **choose_mode script**: randomized selection with floors; outputs selected mode (+ bug slug if applicable).
- **bug_backlog script**: add/update/resolve bugs by slug; never hand‑edit the YAML.
- **logging**: one‑line logs and git logs per iteration to enable replay and forensic debugging.

<example kind="prompt:minimal">
Read the root MDs, especially methodology.md. Follow it exactly for one session.
Run choose_mode, perform the selected mode, and end with the required output lines specified in methodology.md.
</example>

<example kind="prompt:overstuffed:bad">
Read all docs, run choose_mode, do EXPLORE/DEVELOP/CONFIRM/FIX_BUG, remember to run tests, commit with
subject+body, add bugs via script, update YAML, include one-line log, output STEP COMPLETE, do not forget...
</example>
<note>Why bad: duplicates policy, drifts from methodology, and becomes stale.</note>

<example kind="methodology:skeleton">
# Methodology

## For each session (required)
1) Read root MDs
2) Run choose_mode
3) Execute one mode
4) Run relevant tests
5) Record bugs if urgent
6) Commit with subject+body
7) Output ONE-LINE LOG + STEP status

## The work loop
- Mode selection…
- EXPLORE…
- DEVELOP…
- CONFIRM…
- FIX_BUG…

## Testing posture
## Rework policy
## Git discipline
## Artifact formats
</example>

## Concrete examples (copy‑ready)

<example kind="one-line:good">
ONE-LINE LOG: Added artisinal select() spec/tests with 5 cases; harness fails on function-call parse.
</example>

<example kind="one-line:bad">
ONE-LINE LOG: did stuff
</example>

<example kind="commit:good">
Add artisinal select spec tests

- Draft select() spec summary and 5 cases (flattening, empty, $index)
- Tests: zig build harness -- tests/artisinal/select.json (fails: function-call parse)
- Learnings: spec implies projection flattens; empty projection yields no element
</example>

<example kind="commit:bad">
Add select tests
</example>

<example kind="tests:run">
- Tests: zig build harness -- tests/artisinal/where.json (fails: indexer parse)
</example>

<example kind="tests:skipped">
- Tests: skipped (reason: only doc edit; no code changes)
</example>

<example kind="bug:command">
python wiggum/scripts/bug_backlog.py add --title "Parser fails on function calls" \
  --severity high \
  --description "Expressions like name.empty() fail to parse; blocks many tests. Repro: zig build harness -- tests/artisinal/existence.json. See spec: index.md#functions."
</example>

<example kind="bug:yaml">
- slug: parser-fails-on-function-calls
  status: open
  severity: high
  title: Parser fails on function calls
  description: Expressions like name.empty() fail to parse; blocks many tests...
</example>

<example kind="flow:fix_bug">
1. python scripts/choose_mode.py → Mode: FIX_BUG + slug
2. python wiggum/scripts/bug_backlog.py show <slug>
3. Work the fix (code + tests)
4. python wiggum/scripts/bug_backlog.py update <slug> --status in_progress (or resolve)
5. Commit with test info + one-line log
</example>

<example kind="mode:explore">
- Identify missing artisinal file for where().
- Write tests/artisinal/where.json with spec summary + TODOs + edge cases.
</example>

<example kind="mode:develop">
- Run harness for a specific artisinal file.
- Implement minimal parser/eval changes to make those tests pass.
</example>

<example kind="mode:confirm">
- Re-read existing artisinal file vs spec section.
- Tighten tests or notes if they diverge from spec.
</example>

<example kind="rework:blocking">
You start DEVELOP but discover the parser can't handle any function calls (blocks most tests).
Action: add a bug to the backlog, do the minimal parser fix needed, then return to the original task.
</example>

## Common failure modes (and how to avoid them)
- **No status line** → always end with `STEP COMPLETE` / `STEP FAILED` / `PROJECT FINISHED`.
- **No tests run** → include a reason in commit body and one‑line log.
- **One‑line commit** → use subject + body with bullet list.
- **Hand‑editing bug backlog** → always use `wiggum/scripts/bug_backlog.py`.

## Porting to a new project
1. Copy the wiggum driver and scripts.
2. Adapt methodology to the project’s artifacts, tests, and build tools.
3. Keep the prompt minimal; push policy into methodology.
4. Ensure all state updates go through scripts.
5. Run a few sessions, then refine the methodology based on observed failures.
