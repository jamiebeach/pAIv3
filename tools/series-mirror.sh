#!/bin/sh
# Copy the series repository's committed text into each agent's workspace, so
# the crew can read it without a GitHub credential (operator, 2026-11-09).
#   tools/series-mirror.sh [repo-dir]     default: ../AI_Entity_Architecture_Series
# Read-only for the agents: what they write still comes back through the forum.
# Run it after each commit to the series repo.
set -e
repo=${1:-$(dirname "$0")/../../AI_Entity_Architecture_Series}
commit=$(git -C "$repo" rev-parse --short HEAD)
for c in pai-specter pai-aria-dry pai-angel-dry; do
  if ! docker inspect -f '{{.State.Running}}' "$c" 2>/dev/null | grep -q true; then
    echo "$c: not running, skipped"; continue
  fi
  # Committed files only, never the working tree.
  git -C "$repo" archive --format=tar HEAD \
    | MSYS_NO_PATHCONV=1 docker exec -i "$c" sh -c '
        d=/home/pai/workspace/series-repo
        [ -d "$d" ] && chmod -R u+w "$d" && rm -rf "$d"
        mkdir -p "$d" && tar xf - -C "$d"
        printf "Mirror of the series repository at commit %s. Read-only: send changes through the series forum thread.\n" "'"$commit"'" > "$d/MIRROR.txt"
        chmod -R a-w "$d"'
  echo "$c: series-repo at $commit"
done
