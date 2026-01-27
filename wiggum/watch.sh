#!/usr/bin/env bash
# Watch wiggum loop logs, including new files as they appear
cd "$(dirname "${BASH_SOURCE[0]}")/.." || exit 1

while true; do
  # Snapshot current files
  mapfile -t files < <(ls -t logs/*-output.log logs/*-git.log driver.log wiggum-errors.log 2>/dev/null)
  
  if [[ ${#files[@]} -eq 0 ]]; then
    echo "Waiting for log files..."
    sleep 2
    continue
  fi
  
  # Tail current files, kill when new file appears
  tail -F "${files[@]}" 2>/dev/null &
  TAIL_PID=$!
  
  # Check every 3s for new files
  while kill -0 $TAIL_PID 2>/dev/null; do
    sleep 3
    mapfile -t new_files < <(ls -t logs/*-output.log logs/*-git.log driver.log wiggum-errors.log 2>/dev/null)
    if [[ "${files[*]}" != "${new_files[*]}" ]]; then
      kill $TAIL_PID 2>/dev/null
      wait $TAIL_PID 2>/dev/null
      echo -e "\n=== New log file detected ==="
      break
    fi
  done
done
