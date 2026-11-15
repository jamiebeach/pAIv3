#!/bin/sh
# Staged deploys (bead v3-r2j.7; dev, test, prod by the operator, 2026-11-12).
# A change reaches each stage only after the one before has run the same commit
# for a while with no new model failures. Which agent runs each stage is in
# private/stages.env (DEV_ENV, TEST_ENV, PROD_ENV: each agent's env file name).
#
#   tools/deploy.sh                    dev, checked healthy
#   tools/deploy.sh test               test, once dev has run this commit
#                                      for SOAK minutes (default 30) with no new failures
#   tools/deploy.sh prod               prod, once test has, likewise
#   tools/deploy.sh test|prod --now "why"   at once, for an urgent fix; the
#                                      reason is recorded
#
# Each stage records the commit and time in private/deploy-state.jsonl.
set -eu
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
state=private/deploy-state.jsonl
soak_minutes=${SOAK:-30}
commit=$(git rev-parse --short=10 HEAD)
. ./private/stages.env
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

stage() { # env file, stage name
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

# soaked STAGE ENV: stop unless STAGE has run this commit SOAK minutes with no new failures.
soaked() {
  since=$(grep "\"agent\":\"$1\",\"commit\":\"$commit\"" "$state" 2>/dev/null | tail -1 | sed 's/.*"at":\([0-9]*\).*/\1/')
  if [ -z "$since" ]; then echo "STOP: $1 is not on $commit yet."; exit 1; fi
  waited=$(( ($(date +%s) - since) / 60 ))
  if [ "$waited" -lt "$soak_minutes" ]; then
    echo "STOP: $1 has run $commit for $waited min; the soak is $soak_minutes. Try again later, or use --now \"reason\"."
    exit 1
  fi
  n=$(new_failures "private/$2.env" "$since")
  if [ "$n" != "0" ]; then echo "STOP: $n model failure(s) on $1 since it took $commit."; exit 1; fi
}

skip_soak() {
  reason=${1:?give the reason the soak is skipped}
  echo "Skipping the soak: $reason"
  record "$2-soak-skipped" "$reason"
}

case "${1:-}" in
  ""|dev)
    stage "$DEV_ENV" dev
    echo "dev is on $commit. test follows with: tools/deploy.sh test (after ${soak_minutes} min)"
    ;;
  test)
    if [ "${2:-}" = "--now" ]; then skip_soak "${3:-}" test; else soaked dev "$DEV_ENV"; fi
    stage "$TEST_ENV" test
    echo "test is on $commit. prod follows with: tools/deploy.sh prod (after ${soak_minutes} min)"
    ;;
  prod)
    if [ "${2:-}" = "--now" ]; then skip_soak "${3:-}" prod; else soaked test "$TEST_ENV"; fi
    stage "$PROD_ENV" prod
    echo "prod is on $commit."
    ;;
  *) sed -n '2,14p' "$0"; exit 2 ;;
esac
