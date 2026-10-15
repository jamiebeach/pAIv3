#!/bin/sh
# Send the dev agent a message from the coding agent (bead v3-clp.16).
#   tools/message-agent.sh <bead-id|-> <text...>
# Each coding agent has its own token (default private/coding-agent.token;
# set CODING_AGENT_TOKEN_FILE for another). The agent holds only each
# token's SHA-256 and the name it belongs to (setting coding_agent.tokens),
# and records the message under that name. It becomes a stimulus for the
# agent, which answers on the bead.
set -eu
cd "$(dirname "$0")/.."
bead=$1; shift
token=$(cat "${CODING_AGENT_TOKEN_FILE:-private/coding-agent.token}")
body=$(python -c 'import json,sys; b=sys.argv[1]; d={"text": " ".join(sys.argv[2:])}
if b != "-": d["bead"]=b
print(json.dumps(d))' "$bead" "$@")
curl -s -X POST "${AGENT_URL:-http://localhost:8090}/v1/coding-agent/message" \
  -H "Authorization: Bearer $token" -H "Content-Type: application/json" --data-binary "$body"
echo
