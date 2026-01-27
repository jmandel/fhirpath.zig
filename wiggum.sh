#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LOG="$ROOT/driver.log"
LOG_DIR="$ROOT/logs"
ONE_LINE_LOG="$LOG_DIR/one-liners.log"
CONFIG_DIR="$ROOT/wiggum.config"
HEAD_FILE="$CONFIG_DIR/prompt-head.txt"
TAIL_FILE="$CONFIG_DIR/prompt-tail.txt"

recent_logs() {
  if [[ -f "$ONE_LINE_LOG" ]]; then
    tail -n 1000 "$ONE_LINE_LOG"
  else
    printf "<none>\n"
  fi
}

build_prompt() {
  local history
  local head
  local tail
  history="$(recent_logs)"
  if [[ -f "$HEAD_FILE" ]]; then
    head="$(cat "$HEAD_FILE")"
  else
    head=$'Read all mds in repo root and follow the methodology by running choose_mode, then proceed with this step autonomously making progress as long as you can.\n\nBefore the final status line, output exactly one line:\nONE-LINE LOG: <high-signal summary of what changed or learned, <=140 chars>\n\nThen output exactly one of:\nSTEP COMPLETE\nSTEP FAILED\nPROJECT FINISHED\n\nContext:\n- One-line logs accumulate across sessions for future prompts.\n- Full logs are in ./logs and are very verbose; use rg/grep if you need to inspect history.\n\nRECENT ONE-LINE LOGS (most recent last, up to 1000):'
  fi

  if [[ -f "$TAIL_FILE" ]]; then
    tail="$(cat "$TAIL_FILE")"
  else
    tail=""
  fi

  printf "%s\n%s\n%s" "$head" "$history" "$tail"
}

next_log_id() {
  mkdir -p "$LOG_DIR"
  local max=0
  local base
  for base in "$LOG_DIR"/[0-9][0-9][0-9][0-9]-*; do
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
  set +e
  codex exec --dangerously-bypass-approvals-and-sandbox -C "$ROOT" - <<<"$PROMPT" | tee "$OUT_LOG"
  CODEX_EXIT=${PIPESTATUS[0]}
  set -e
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
  echo "$TS id=$ID status=$STATUS codex_exit=$CODEX_EXIT before=$BEFORE after=$AFTER msg=$MSG | $ONE_LINE" >> "$LOG"
  echo "$TS id=$ID status=$STATUS codex_exit=$CODEX_EXIT commit=${AFTER:0:7} msg=$MSG | $ONE_LINE" >> "$ONE_LINE_LOG"

  {
    echo "ONE-LINE LOG: $ONE_LINE"
    echo "STATUS: $STATUS"
    echo "CODEX_EXIT: $CODEX_EXIT"
    echo "BEFORE: $BEFORE"
    echo "AFTER: $AFTER"
    echo "MESSAGE: $MSG"
    echo ""
    echo "GIT ONE-LINE:"
    git log -1 --oneline
    echo ""
    echo "GIT DETAILS:"
    git log -1 --decorate --stat
  } > "$GIT_LOG"
  chmod 444 "$GIT_LOG"

  if [[ "$STATUS" == "PROJECT FINISHED" ]]; then
    break
  fi

  if [[ "$STATUS" != "STEP COMPLETE" ]]; then
    git reset --hard "$BEFORE"
  fi
done
