#!/usr/bin/env bash
set -uo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# Prevent multiple instances
LOCKFILE="$ROOT/.wiggum.lock"
if [ -f "$LOCKFILE" ]; then
  OTHER_PID=$(cat "$LOCKFILE")
  if kill -0 "$OTHER_PID" 2>/dev/null; then
    echo "Another loop is running (PID $OTHER_PID). Exiting."
    exit 1
  fi
fi
echo $$ > "$LOCKFILE"

# Log errors to a file
ERROR_LOG="$ROOT/wiggum-errors.log"

# Clean up lock on exit or error
cleanup() { rm -f "$LOCKFILE"; }
trap cleanup EXIT
# Log errors but don't exit - let the loop handle failures via STATUS
trap 'echo "$(date -Is) ERR trap at line $LINENO: $BASH_COMMAND (exit $?)" >> "$ERROR_LOG"' ERR
LOG="$ROOT/driver.log"
LOG_DIR="$ROOT/logs"
ONE_LINE_LOG="$LOG_DIR/one-liners.log"
CONFIG_DIR="$ROOT/wiggum"
HEAD_FILE="$CONFIG_DIR/prompt-head.txt"
TAIL_FILE="$CONFIG_DIR/prompt-tail.txt"

# LLM backend: "codex" (default), "claude", or "shelley"
LLM_BACKEND="${LLM_BACKEND:-codex}"

# Claude settings (when LLM_BACKEND=claude)
CLAUDE_MODEL="${CLAUDE_MODEL:-opus}"

# Shelley settings (when LLM_BACKEND=shelley)
SHELLEY_PROMPT="${SHELLEY_PROMPT:-$CONFIG_DIR/shelley-prompt.ts}"
SHELLEY_SERVER="${SHELLEY_SERVER:-http://localhost:9999}"
SHELLEY_MODEL="${SHELLEY_MODEL:-}"
SHELLEY_USER="${SHELLEY_USER:-wiggum}"

run_llm() {
  local cwd="$1"
  # Prompt comes via stdin
  case "$LLM_BACKEND" in
    claude)
      cd "$cwd"
      claude -p --dangerously-skip-permissions --model "$CLAUDE_MODEL" \
        --output-format stream-json --include-partial-messages
      ;;
    codex)
      codex exec --dangerously-bypass-approvals-and-sandbox -C "$cwd" -
      ;;
    shelley)
      local args=("$SHELLEY_PROMPT" -server "$SHELLEY_SERVER" -cwd "$cwd" -user "$SHELLEY_USER" -v)
      if [[ -n "$SHELLEY_MODEL" ]]; then
        args+=(-model "$SHELLEY_MODEL")
      fi
      "${args[@]}"
      ;;
    *)
      echo "Unknown LLM_BACKEND: $LLM_BACKEND" >&2
      exit 1
      ;;
  esac
}

recent_logs() {
  if [[ -f "$ONE_LINE_LOG" ]]; then
    tail -n 1000 "$ONE_LINE_LOG"
  else
    printf "<none>\n"
  fi
}

root_md_files() {
  # Include root MD files with filename + contents
  local files=("$ROOT/methodology.md" "$ROOT/design.md" "$ROOT/principles.md")
  for f in "${files[@]}"; do
    if [[ -f "$f" ]]; then
      local name="${f#$ROOT/}"
      printf "=== %s ===\n" "$name"
      cat "$f"
      printf "\n\n"
    fi
  done
}

build_prompt() {
  local history
  local head
  local tail
  local mds
  mds="$(root_md_files)"
  history="$(recent_logs)"
  if [[ -f "$HEAD_FILE" ]]; then
    head="$(cat "$HEAD_FILE")"
  else
    head=$'Follow the methodology by running choose_mode, then proceed with this step autonomously making progress as long as you can.\n\nBefore the final status line, output exactly one line:\nONE-LINE LOG: <high-signal summary of what changed or learned, <=140 chars>\n\nThen output exactly one of:\nSTEP COMPLETE\nSTEP FAILED\nPROJECT FINISHED\n\nContext:\n- One-line logs accumulate across sessions for future prompts.\n- Full logs are in ./logs and are very verbose; use rg/grep if you need to inspect history.\n\nRECENT ONE-LINE LOGS (most recent last, up to 1000):'
  fi

  if [[ -f "$TAIL_FILE" ]]; then
    tail="$(cat "$TAIL_FILE")"
  else
    tail=""
  fi

  printf "%s\n\n%s\n%s\n%s" "$mds" "$head" "$history" "$tail"
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
  echo "$(date -Is) Starting step" >> "$ERROR_LOG"
  
  ID="$(next_log_id)"
  OUT_LOG="$LOG_DIR/${ID}-output.log"
  GIT_LOG="$LOG_DIR/${ID}-git.log"
  echo "$(date -Is) Step ID=$ID" >> "$ERROR_LOG"

  BEFORE=$(git rev-parse HEAD)

  PROMPT="$(build_prompt)"
  set +o pipefail
  run_llm "$ROOT" <<<"$PROMPT" 2>&1 | tee "$OUT_LOG"
  LLM_EXIT=${PIPESTATUS[0]}
  set -o pipefail
  echo "$(date -Is) LLM finished, exit=$LLM_EXIT" >> "$ERROR_LOG"
  chmod 444 "$OUT_LOG" || true

  # If LLM exited non-zero, treat as failure regardless of output
  if [[ "$LLM_EXIT" -ne 0 ]]; then
    STATUS="LLM_ERROR"
  else
    STATUS=$(grep -Eo 'STEP COMPLETE|STEP FAILED|PROJECT FINISHED' "$OUT_LOG" | tail -n 1 || true)
    if [[ -z "$STATUS" ]]; then
      STATUS="UNKNOWN"
    fi
  fi
  ONE_LINE_RAW=$(grep -E '^ONE-LINE LOG:' "$OUT_LOG" | tail -n 1 || true)
  ONE_LINE="${ONE_LINE_RAW#ONE-LINE LOG: }"
  AFTER=$(git rev-parse HEAD)
  MSG=$(git log -1 --pretty=%s)

  if [[ -z "$ONE_LINE_RAW" ]]; then
    ONE_LINE="(missing one-line log)"
  fi

  TS="$(date -Is)"
  echo "$TS id=$ID status=$STATUS llm_exit=$LLM_EXIT before=$BEFORE after=$AFTER msg=$MSG | $ONE_LINE" >> "$LOG"
  echo "$TS id=$ID status=$STATUS llm_exit=$LLM_EXIT commit=${AFTER:0:7} msg=$MSG | $ONE_LINE" >> "$ONE_LINE_LOG"

  {
    echo "ONE-LINE LOG: $ONE_LINE"
    echo "STATUS: $STATUS"
    echo "LLM_EXIT: $LLM_EXIT"
    echo "BEFORE: $BEFORE"
    echo "AFTER: $AFTER"
    echo "MESSAGE: $MSG"
    echo ""
    echo "GIT ONE-LINE:"
    git log -1 --no-color --oneline
    echo ""
    echo "GIT MESSAGE:"
    git log -1 --no-color --pretty=%B | python - <<'PY'
import sys
text = sys.stdin.read()
sys.stdout.write(text.replace("\\\\n", "\n"))
PY
    echo ""
    echo "GIT STATS:"
    git log -1 --no-color --stat --pretty=format:
  } > "$GIT_LOG"
  chmod 444 "$GIT_LOG"

  if [[ "$STATUS" == "PROJECT FINISHED" ]]; then
    break
  fi

  if [[ "$STATUS" != "STEP COMPLETE" ]]; then
    echo "$(date -Is) Resetting to $BEFORE (status=$STATUS)" >> "$ERROR_LOG"
    # Preserve upstream-issues.yaml across resets (tracking upstream bugs is valuable even on failed steps)
    UPSTREAM_FILE="$ROOT/wiggum/upstream-issues.yaml"
    UPSTREAM_BACKUP=""
    if [[ -f "$UPSTREAM_FILE" ]]; then
      UPSTREAM_BACKUP=$(cat "$UPSTREAM_FILE")
    fi
    git reset --hard "$BEFORE"
    if [[ -n "$UPSTREAM_BACKUP" ]]; then
      echo "$UPSTREAM_BACKUP" > "$UPSTREAM_FILE"
    fi
  fi
  
  echo "$(date -Is) Step $ID complete, looping" >> "$ERROR_LOG"
done
