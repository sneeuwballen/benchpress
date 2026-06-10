#!/usr/bin/env python3
"""Fake a benchpress run over ~5 minutes via NATS.

Sends incremental progress reports to benchpress.progress.solve.<uuid>
and a final done message to benchpress.progress.done.<uuid>.

Usage: ./fake-nats-run.py <nats-server> <total-tasks> <uuid> <start-ts> <solve-subject> <done-subject>
"""

import json
import random
import subprocess
import sys
import time


def main() -> None:
    nats_server = sys.argv[1]
    total = int(sys.argv[2])
    uuid = sys.argv[3]
    start_ts = float(sys.argv[4])
    solve_subject = sys.argv[5]
    done_subject = sys.argv[6]

    provers = ["vampire", "z3", "cvc5", "e-prover"]
    files = ["bench_%d.smt2" % i for i in range(1, 11)]

    print("Fake run starting — UUID: %s" % uuid, flush=True)
    print("Publishing to NATS at %s" % nats_server, flush=True)
    print("Total tasks: %d, sending updates every ~1.5s for ~%ds" % (total, total * 15 // 10), flush=True)
    print("", flush=True)

    for done in range(1, total + 1):
        elapsed = time.time() - start_ts
        active = []
        if total > 0:
            active.append({
                "prover": provers[done % len(provers)],
                "file": files[done % len(files)],
                "runningTime": round(elapsed / 3, 1),
            })

        report = {
            "uuid": uuid,
            "startTs": start_ts,
            "totalTasks": total,
            "doneTasks": done,
            "active": active,
            "finished": False,
            "stats": "sat:%d unsat:%d timeout:%d"
            % (done // 3, done // 5, done // 10),
        }

        payload = json.dumps(report)
        subprocess.run(
            ["nats", "pub", "--server=" + nats_server, solve_subject, payload],
            capture_output=True,
            timeout=10,
        )

        sys.stderr.write(
            "\r[%s] %d/%d (%d%%)"
            % (time.strftime("%H:%M:%S"), done, total, done * 100 // total)
        )
        sys.stderr.flush()
        time.sleep(random.uniform(0.8, 2.0))

    # final done report
    report = {
        "uuid": uuid,
        "startTs": start_ts,
        "totalTasks": total,
        "doneTasks": total,
        "active": [],
        "finished": True,
        "stats": "sat:%d unsat:%d timeout:%d"
        % (total // 3, total // 5, total // 10),
    }
    payload = json.dumps(report)
    subprocess.run(
        ["nats", "pub", "--server=" + nats_server, solve_subject, payload],
        capture_output=True,
        timeout=10,
    )
    subprocess.run(
        ["nats", "pub", "--server=" + nats_server, done_subject, payload],
        capture_output=True,
        timeout=10,
    )

    sys.stderr.write("\nAll done!\n")


if __name__ == "__main__":
    main()
