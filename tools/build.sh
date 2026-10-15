#!/bin/sh
# Build the standalone pai executable for Linux x64 into dist/linux-x64/
# (spec section 17, NFR PORT-1). The executable carries the whole image; the
# cipher library ships beside it and is found there at start. Windows and
# macOS builds use the same steps natively (bead v3-fu8.6).
set -eu
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
mkdir -p dist/linux-x64
docker run --rm -v pai-v3-cache:/home/v3/.cache -v "$(pwd -W 2>/dev/null || pwd):/v3" pai-v3-dev:local \
  sh -c 'sbcl --dynamic-space-size 1024 --noinform --non-interactive \
           --load /opt/quicklisp/setup.lisp \
           --eval "(push #p\"/v3/\" asdf:*central-registry*)" \
           --eval "(handler-bind ((warning (function muffle-warning))) (ql:quickload \"pai-v3\" :silent t))" \
           --eval "(dolist (lib (cffi:list-foreign-libraries)) (cffi:close-foreign-library lib))" \
           --eval "(sb-ext:save-lisp-and-die \"/v3/dist/linux-x64/pai\" :executable t :save-runtime-options t :toplevel (function pai-v3.cli:main))" \
         && cp /opt/pai/lib/libsqlite3mc.so /v3/dist/linux-x64/'
ls -l dist/linux-x64
