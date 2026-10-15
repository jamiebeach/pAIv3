#!/bin/sh
# Event-type census of a COPY of an agent's events store (checklist B3).
# The copy's directory is mounted read-only; the store is opened read-only.
# Usage: tools/census.sh <path-to-events.sqlite3-copy> <agent-id> <output-dir>
set -eu
export MSYS_NO_PATHCONV=1
copy=$(cd "$(dirname "$1")" && (pwd -W 2>/dev/null || pwd))/$(basename "$1")
agent=$2
out=$(mkdir -p "$3" && cd "$3" && (pwd -W 2>/dev/null || pwd))
cd "$(dirname "$0")/.."
docker run --rm -v pai-v3-cache:/home/v3/.cache --memory 1g \
  -v "$(pwd -W 2>/dev/null || pwd):/v3:ro" \
  -v "$(dirname "$copy"):/data:ro" -v "$out:/out" pai-v3-dev:local \
  sbcl --noinform --non-interactive --load /opt/quicklisp/setup.lisp \
    --eval '(push #p"/v3/" asdf:*central-registry*)' \
    --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "pai-v3-maintenance" :silent t))' \
    --eval "(let ((r (pai-v3.maintenance:census \"/data/$(basename "$copy")\" :agent-id \"$agent\")))
              (with-open-file (s \"/out/census-$agent.json\" :direction :output :if-exists :supersede)
                (write-string (pai-v3.kernel:json-encode r) s))
              (with-open-file (s \"/out/census-$agent.md\" :direction :output :if-exists :supersede)
                (write-string (pai-v3.maintenance:census-markdown r) s))
              (format t \"census: ~d events, ~d types~%\" (pai-v3.kernel:obj-get r \"events\")
                      (length (pai-v3.kernel:obj-get r \"types\"))))"
