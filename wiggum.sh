#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$ROOT/driver.log"
LOG_DIR="$ROOT/logs"
ONE_LINE_LOG="$LOG_DIR/one-liners.log"

recent_logs() {
  if [[ -f "$ONE_LINE_LOG" ]]; then
    tail -n 1000 "$ONE_LINE_LOG"
  else
    printf "<none>\n"
  fi
}

build_prompt() {
  local history
  history="$(recent_logs)"
  cat <<EOF
Read all mds in repo root and follow the methodology by running choose_mode, then proceed with this step autonomously making progress as long as you can.

Before the final status line, output exactly one line:
ONE-LINE LOG: <high-signal summary of what changed or learned, <=140 chars>

Then output exactly one of:
STEP COMPLETE
STEP FAILED
PROJECT FINISHED

Context:
- One-line logs accumulate across sessions for future prompts.
- Full logs are in ./logs and are very verbose; use rg/grep if you need to inspect history.

RECENT ONE-LINE LOGS (most recent last, up to 1000):
$history
EOF
}

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

  PROMPT="$(build_prompt)"
  codex exec --dangerously-bypass-approvals-and-sandbox -C "$ROOT" - <<<"$PROMPT" | tee "$OUT_LOG"
  chmod 444 "$OUT_LOG"

  STATUS=$(grep -Eo 'STEP COMPLETE|STEP FAILED|PROJECT FINISHED' "$OUT_LOG" | tail -n 1 || true)
  ONE_LINE_RAW=$(grep -E '^ONE-LINE LOG:' "$OUT_LOG" | tail -n 1 || true)
  ONE_LINE="${ONE_LINE_RAW#ONE-LINE LOG: }"
  AFTER=$(git rev-parse HEAD)
  MSG=$(git log -1 --pretty=%s)

  if [[ -z "$ONE_LINE_RAW" ]]; then
    ONE_LINE="(missing one-line log)"
  fi

  TS="$(date -Is)"
  echo "$TS id=$ID status=$STATUS before=$BEFORE after=$AFTER msg=$MSG | $ONE_LINE" >> "$LOG"
  echo "$TS id=$ID status=$STATUS commit=${AFTER:0:7} msg=$MSG | $ONE_LINE" >> "$ONE_LINE_LOG"

  {
    echo "ONE-LINE LOG: $ONE_LINE"
    echo "STATUS: $STATUS"
    echo "BEFORE: $BEFORE"
    echo "AFTER: $AFTER"
    echo "MESSAGE: $MSG"
    echo ""
    git log -1 --decorate --stat --patch
  } > "$GIT_LOG"
  chmod 444 "$GIT_LOG"

  if [[ "$STATUS" == "PROJECT FINISHED" ]]; then
    break
  fi

  if [[ "$STATUS" != "STEP COMPLETE" ]]; then
    git reset --hard "$BEFORE"
  fi
done
