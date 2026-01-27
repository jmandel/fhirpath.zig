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
- **Modes** and their goals (EXPLORE/DEVELOP/CONFIRM/TACKLE_BLOCKER).
- **How to select modes** (randomized chooser, floors, inputs).
- **Testing posture** (what to run, how to report skips).
- **Git discipline** (subject+body, test reporting).
- **Artifact formats** (artisinal tests, blockers schema).
- **Rework policy** (blocking vs non‑blocking).
- **Completion signaling** (STEP COMPLETE / FAILED / PROJECT FINISHED).

If a rule is important for compliance, it should be in methodology. The prompt should merely remind the model to follow it.

## DRY principles
- **Prompt = thin wrapper**, methodology = full policy.
- **Scripts for edits** (blockers, state) to keep format deterministic.
- **Logs drive prompts**, not the other way around (one‑line logs feed future prompts).

## Tooling patterns
- **choose_mode script**: randomized selection with floors; outputs selected mode (+ blocker slug if applicable).
- **blockers script**: add/update/resolve blockers by slug; never hand‑edit the YAML.
- **logging**: one‑line logs and git logs per iteration to enable replay and forensic debugging.

## Example minimal prompt template
```
Read the root MDs, especially methodology.md. Follow it exactly for one session.
Run choose_mode, perform the selected mode, and end with the required output lines specified in methodology.md.
```

## Example methodology skeleton
```
# Methodology

## For each session (required)
1) Read root MDs
2) Run choose_mode
3) Execute one mode
4) Run relevant tests
5) Record blockers if urgent
6) Commit with subject+body
7) Output ONE-LINE LOG + STEP status

## The work loop
- Mode selection…
- EXPLORE…
- DEVELOP…
- CONFIRM…
- TACKLE_BLOCKER…

## Testing posture
## Rework policy
## Git discipline
## Artifact formats
```

## Porting to a new project
1. Copy the wiggum driver and scripts.
2. Adapt methodology to the project’s artifacts, tests, and build tools.
3. Keep the prompt minimal; push policy into methodology.
4. Ensure all state updates go through scripts.
5. Run a few sessions, then refine the methodology based on observed failures.
