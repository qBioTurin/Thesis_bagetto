#!/usr/bin/env sh
set -eu

APP_CMD="Rscript --vanilla inst/Shiny/app.R"

cleanup_shared_results() {
  if [ -d /shared_results ]; then
    # Remove all children while preserving the mount point.
    find /shared_results -mindepth 1 -maxdepth 1 -exec rm -rf -- {} + 2>/dev/null || true
  fi
}

forward_term_and_wait() {
  if [ -n "${child_pid:-}" ]; then
    kill -TERM "$child_pid" 2>/dev/null || true
    wait "$child_pid" 2>/dev/null || true
  fi
}

on_term() {
  cleanup_shared_results
  forward_term_and_wait
  exit 0
}

trap on_term TERM INT
trap cleanup_shared_results EXIT

sh -c "$APP_CMD" &
child_pid=$!
wait "$child_pid"
exit_code=$?

cleanup_shared_results
exit "$exit_code"
