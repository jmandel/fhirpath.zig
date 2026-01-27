#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$ROOT/driver.log"

PROMPT=$'Read all mds in repo root and follow the methodology by running choose_mode, then proceed with this step autonomously making progress as through your task if you cna, then return STEP COMPLETE or STEP FAILED or PROJECT FINISHED.'

while true; do
  cd "$ROOT"
  BEFORE=$(git rev-parse HEAD)

  OUT="$(mktemp)"
  codex --dangerously-bypass-approvals-and-sandbox <<<"$PROMPT" | tee "$OUT"

  STATUS=$(grep -Eo 'STEP COMPLETE|STEP FAILED|PROJECT FINISHED' "$OUT" | tail -n 1 || true)
  AFTER=$(git rev-parse HEAD)
  MSG=$(git log -1 --pretty=%s)

  echo "$(date -Is) status=$STATUS before=$BEFORE after=$AFTER msg=$MSG" >> "$LOG"

  if [[ "$STATUS" == "PROJECT FINISHED" ]]; then
    break
  fi

  if [[ "$STATUS" != "STEP COMPLETE" ]]; then
    git reset --hard "$BEFORE"
  fi
done
