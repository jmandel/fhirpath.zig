#!/usr/bin/env bash
# Watch wiggum loop logs - just tail the latest output log
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1

# Kill tail on exit
TAIL_PID=
cleanup() {
  [[ -n "$TAIL_PID" ]] && kill $TAIL_PID 2>/dev/null
  exit 0
}
trap cleanup INT TERM EXIT

latest() {
  ls -t logs/*-output.log 2>/dev/null | head -1
}

echo "Watching wiggum logs (Ctrl-C to stop)..."
while true; do
  f=$(latest)
  if [[ -z "$f" ]]; then
    echo "Waiting for log files..."
    sleep 2
    continue
  fi
  
  echo "==> $f <=="
  tail -f "$f" &
  TAIL_PID=$!
  
  while [[ "$(latest)" == "$f" ]]; do
    sleep 2
    kill -0 $TAIL_PID 2>/dev/null || break
  done
  
  kill $TAIL_PID 2>/dev/null
  wait $TAIL_PID 2>/dev/null
done
