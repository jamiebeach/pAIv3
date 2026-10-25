#!/usr/bin/env python3
"""A synthetic perception service (bead v3-byw.2): no camera, invented ids.

Plays a scene to one agent the way pAI-Eyes will: read the lease, then send
observations under its epoch.

  python tools/synthetic-eyes.py URL KEY_FILE SOURCE_ID [scene]

scene: enter-stay-leave (default), enter, leave, lose, recover.
Prints each response. Only the standard library is used.
"""
import json
import sys
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone


def call(url, key, path, body=None):
    data = None if body is None else json.dumps(body).encode()
    req = urllib.request.Request(url + path, data=data, method="GET" if body is None else "POST",
                                 headers={"Authorization": "Bearer " + key, "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=10) as r:
            return json.loads(r.read())
    except urllib.error.HTTPError as e:
        return {"http": e.code, "body": e.read().decode(errors="replace")}


def stamp(seconds=0):
    return (datetime.now(timezone.utc) + timedelta(seconds=seconds)).strftime("%Y-%m-%dT%H:%M:%SZ")


def main():
    if len(sys.argv) < 4:
        print(__doc__)
        return 2
    url, key_file, source = sys.argv[1].rstrip("/"), sys.argv[2], sys.argv[3]
    scene = sys.argv[4] if len(sys.argv) > 4 else "enter-stay-leave"
    key = open(key_file, encoding="utf-8").read().strip()
    lease = call(url, key, "/v1/perception/lease")
    print("lease", lease)
    if scene in ("lose", "recover"):
        print(call(url, key, "/v1/perception/state", {"state": "lost" if scene == "lose" else "live", "reason": "synthetic " + scene}))
        return 0
    if lease.get("state") != "live":
        print("source is not live; the operator starts it from the PWA (POST /api/perception/start)")
        return 1
    epoch, seq = lease["epoch"], int(time.time())
    kinds = {"enter-stay-leave": ["person.entered", "person.present", "person.present", "person.left"],
             "enter": ["person.entered"], "leave": ["person.left"]}[scene]
    for i, kind in enumerate(kinds):
        o = {"source_id": source, "epoch": epoch, "sequence": seq + i, "kind": kind, "track_id": "synthetic-1",
             "recognition": "unknown", "observed_at": stamp(), "expires_at": stamp(30), "detector": "synthetic-v1"}
        print(kind, call(url, key, "/v1/perception/observations", {"observations": [o]}))
        time.sleep(1)
    return 0


if __name__ == "__main__":
    sys.exit(main())
