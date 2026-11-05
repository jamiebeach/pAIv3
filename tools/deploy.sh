#!/bin/sh
# Staged deploys (bead v3-r2j.7): Angel is the production agent, so a change
# reaches it only after Specter (dev) and Aria have run the same commit for a while with
# no new model failures.
#
#   tools/deploy.sh                    Specter, then Aria, each checked healthy
#   tools/deploy.sh angel              Angel, once both have run this commit for
#                                      SOAK minutes (default 30) with no new failures
#   tools/deploy.sh angel --now "why"  Angel at once, for an urgent fix; the
#                                      reason is recorded
#
# Each stage records the commit and time in private/deploy-state.jsonl.
set -eu
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
state=private/deploy-state.jsonl
soak_minutes=${SOAK:-30}
commit=$(git rev-parse --short=10 HEAD)
dirty=$(git status --porcelain --untracked-files=no -- src web tests tools | head -1)

record() { # agent, note
  printf '{"agent":"%s","commit":"%s","dirty":%s,"at":%d,"note":"%s"}\n' \
    "$1" "$commit" "$([ -n "$dirty" ] && echo true || echo false)" "$(date +%s)" "$2" >> "$state"
}

# New model failures on AGENT since unix time SINCE.
new_failures() (
  env_file=$1; since=$2
  # Each agent's env in its own subshell, with dev-agent.sh's port default.
  unset PAI_DEV_PORT
  . "./$env_file"
  docker exec "$PAI_DEV_NAME" curl -s "http://127.0.0.1:${PAI_DEV_PORT:-8090}/v1/dev/peek?what=failures&limit=20" |
    python -c "
import json,sys,datetime
since=$since
n=0
for r in json.load(sys.stdin).get('items',[]):
    at=r.get('at') or ''
    try: t=datetime.datetime.fromisoformat(at.replace('Z','+00:00')).timestamp()
    except ValueError: continue
    if t>=since: n+=1
print(n)"
)

stage() { # env file, agent name
  echo "== $2"
  PAI_DEV_ENV=private/$1.env tools/dev-agent.sh sync
  record "$2" "deployed"
  echo "   healthy; watching 60 s for model failures"
  t0=$(date +%s); sleep 60
  n=$(new_failures "private/$1.env" "$t0")
  if [ "$n" != "0" ]; then
    echo "   STOP: $n new model failure(s) on $2 since the deploy. Look with: tools/dev-agent.sh peek failures"
    exit 1
  fi
}

case "${1:-}" in
  "")
    stage dev-agent specter
    stage aria-dev aria
    echo "Specter and Aria are on $commit. Angel follows with: tools/deploy.sh angel (after ${soak_minutes} min)"
    ;;
  angel)
    if [ "${2:-}" = "--now" ]; then
      reason=${3:?give the reason the soak is skipped}
      echo "Skipping the soak: $reason"
      record angel-soak-skipped "$reason"
    else
      for a in specter aria; do
        since=$(grep "\"agent\":\"$a\",\"commit\":\"$commit\"" "$state" 2>/dev/null | tail -1 | sed 's/.*"at":\([0-9]*\).*/\1/')
        if [ -z "$since" ]; then echo "STOP: $a is not on $commit yet. Run tools/deploy.sh first."; exit 1; fi
        waited=$(( ($(date +%s) - since) / 60 ))
        if [ "$waited" -lt "$soak_minutes" ]; then
          echo "STOP: $a has run $commit for $waited min; the soak is $soak_minutes. Try again later, or use --now \"reason\"."
          exit 1
        fi
        env=$([ "$a" = specter ] && echo dev-agent || echo aria-dev)
        n=$(new_failures "private/$env.env" "$since")
        if [ "$n" != "0" ]; then echo "STOP: $n model failure(s) on $a since it took $commit."; exit 1; fi
      done
    fi
    stage angel-dev angel
    echo "Angel is on $commit."
    ;;
  *) sed -n '2,12p' "$0"; exit 2 ;;
esac
