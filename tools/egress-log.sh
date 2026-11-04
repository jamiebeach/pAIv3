#!/bin/sh
# What the egress proxy saw (bead v3-r2j.1): requests by agent and host, and
# every deny or would-deny with its reason.
#
#   tools/egress-log.sh            summary of the whole log
#   tools/egress-log.sh tail [n]   the last n lines (default 20)
set -eu
export MSYS_NO_PATHCONV=1
case "${1:-summary}" in
  tail) docker exec pai-egress tail -n "${2:-20}" /data/egress-log.jsonl ;;
  *) docker exec pai-egress cat /data/egress-log.jsonl | python -c '
import collections, json, sys
seen, refused = collections.Counter(), collections.Counter()
for line in sys.stdin:
    e = json.loads(line)
    if "host" not in e:
        continue
    seen[(e.get("client"), e["host"], e.get("decision"))] += 1
    if e.get("decision") in ("deny", "would-deny"):
        refused[(e.get("client"), e["host"], e.get("reason"))] += 1
print("Requests by agent, host and decision:")
for (c, h, d), n in sorted(seen.items(), key=lambda kv: -kv[1]):
    print(f"  {n:6d}  {c:10s} {d:11s} {h}")
print("Denied or would be denied:" if refused else "Nothing denied or would-be-denied.")
for (c, h, r), n in sorted(refused.items(), key=lambda kv: -kv[1]):
    print(f"  {n:6d}  {c:10s} {h}  ({r})")
' ;;
esac
