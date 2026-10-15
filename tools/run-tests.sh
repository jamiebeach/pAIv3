#!/bin/sh
# Run each test suite in a fresh SBCL process inside the v3 toolchain image.
# Usage: tools/run-tests.sh [suite-file ...]   (default: every tests/*-test.lisp)
set -u
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
suites="${*:-$(ls tests/*-test.lisp)}"
status=0
for suite in $suites; do
  docker run --rm -v pai-v3-cache:/home/v3/.cache --memory 1g --cpus 2 -v "$(pwd -W 2>/dev/null || pwd):/v3" pai-v3-dev:local \
    sbcl --dynamic-space-size 900 --noinform --non-interactive \
      --load /opt/quicklisp/setup.lisp \
      --eval '(push #p"/v3/" asdf:*central-registry*)' \
      --eval '(let ((*compile-verbose* nil) (*load-verbose* nil)) (handler-bind ((warning (function muffle-warning))) (ql:quickload "pai-v3" :silent t)))' \
      --load tests/harness.lisp \
      --load "$suite" || status=1
done
exit $status
