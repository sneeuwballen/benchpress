#!/usr/bin/env bash
# Fake a benchpress run over ~5 minutes via NATS.
# Sends incremental progress reports to benchpress.progress.solve.<uuid>
# and a final done message to benchpress.progress.done.<uuid>.
#
# Usage: ./fake-nats-run.sh [nats-server-addr] [total-tasks]
#   default nats server addr: localhost:4222
#   default total tasks: 200

set -euo pipefail

NATS_SERVER="${1:-localhost:4222}"
TOTAL="${2:-200}"

command -v nats >/dev/null 2>&1 || { echo >&2 "need nats CLI"; exit 1; }

UUID="$(uuidgen 2>/dev/null || echo "fake-run-$$-$(date +%s)")"
START_TS="$(date +%s.%N)"
SOLVE_SUBJECT="benchpress.progress.solve.${UUID}"
DONE_SUBJECT="benchpress.progress.done.${UUID}"

dir="$(dirname "$0")"
exec python3 "${dir}/fake-nats-run.py" "${NATS_SERVER}" "${TOTAL}" "${UUID}" "${START_TS}" "${SOLVE_SUBJECT}" "${DONE_SUBJECT}"
