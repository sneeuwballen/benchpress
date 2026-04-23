#!/bin/sh
# Smoke-tests for benchpressctl.
# Requires BENCHPRESS_SERVER and BENCHPRESS_API_KEY to be set.
# The server must have prover "always-true" and dir "/tmp/bench".
set -e

die() { echo "FAIL: $*" >&2; exit 1; }

echo "  [ctl] submit job and capture job_id"
JOB_ID=$(benchpressctl run \
    --prover always-true \
    --path /tmp/bench \
    2>/dev/null)
[ -n "$JOB_ID" ] || die "job_id is empty"
echo "    job_id=$JOB_ID"

echo "  [ctl] query status (queued or running)"
benchpressctl status "$JOB_ID" 2>/dev/null | grep -qE 'status:|completed' \
    || die "unexpected status output"

echo "  [ctl] submit and wait for completion"
RESULT=$(benchpressctl run \
    --prover always-true \
    --path /tmp/bench \
    --wait \
    2>/dev/null)
# wait exits 0 on success; output should be result file or "completed"
echo "    result: $RESULT"

echo "  [ctl] submit slow job then cancel via benchpressctl"
SLOW_ID=$(benchpressctl run \
    --prover slow \
    --path /tmp/bench \
    2>/dev/null)
[ -n "$SLOW_ID" ] || die "slow job_id is empty"
benchpressctl cancel "$SLOW_ID" 2>/dev/null

echo "  [ctl] status of cancelled job"
# Retry: worker may take a moment to act on the interrupt flag.
STATUS_OUT=""
for _i in $(seq 1 20); do
    STATUS_OUT=$(benchpressctl status "$SLOW_ID" 2>&1 || true)
    echo "$STATUS_OUT" | grep -qi 'cancel' && break
    sleep 0.5
done
echo "    $STATUS_OUT"
echo "$STATUS_OUT" | grep -qi 'cancel' \
    || die "expected cancelled status, got: $STATUS_OUT"

echo "  [ctl] invalid api-key is rejected"
RESULT=$(BENCHPRESS_API_KEY=notavalidkey \
    benchpressctl status "$JOB_ID" 2>&1 || true)
echo "$RESULT" | grep -qi 'error\|unauthenticated\|401' \
    || die "expected auth error, got: $RESULT"

echo "  [ctl] all smoke tests passed"
