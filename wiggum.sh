#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$ROOT/driver.log"
LOG_DIR="$ROOT/logs"

PROMPT=$'Read all mds in repo root and follow the methodology by running choose_mode, then proceed with this step autonomously making progress as long as you can, then return STEP COMPLETE or STEP FAILED or PROJECT FINISHED.'

next_log_id() {
  mkdir -p "$LOG_DIR"
  local max=0
  local base
  for base in "$LOG_DIR"/[0-9][0-9][0-9][0-9]-output.log; do
    [[ -e "$base" ]] || continue
    local name
    name="$(basename "$base")"
    local num="${name%%-*}"
    if [[ "$num" =~ ^[0-9]{4}$ ]]; then
      if ((10#$num > max)); then
        max=$((10#$num))
      fi
    fi
  done
  printf "%04d" $((max + 1))
}

while true; do
  cd "$ROOT"
  ID="$(next_log_id)"
  OUT_LOG="$LOG_DIR/${ID}-output.log"
  GIT_LOG="$LOG_DIR/${ID}-git.log"

  BEFORE=$(git rev-parse HEAD)

  codex exec --dangerously-bypass-approvals-and-sandbox -C "$ROOT" - <<<"$PROMPT" | tee "$OUT_LOG"
  chmod 444 "$OUT_LOG"

  STATUS=$(grep -Eo 'STEP COMPLETE|STEP FAILED|PROJECT FINISHED' "$OUT_LOG" | tail -n 1 || true)
  AFTER=$(git rev-parse HEAD)
  MSG=$(git log -1 --pretty=%s)

  echo "$(date -Is) status=$STATUS before=$BEFORE after=$AFTER msg=$MSG" >> "$LOG"
  git log -1 --decorate --stat > "$GIT_LOG"
  chmod 444 "$GIT_LOG"

  if [[ "$STATUS" == "PROJECT FINISHED" ]]; then
    break
  fi

  if [[ "$STATUS" != "STEP COMPLETE" ]]; then
    git reset --hard "$BEFORE"
  fi
done
