#!/bin/sh
# Compile the system from scratch; fail on any full WARNING (style warnings
# and notes are reported but allowed).
set -u
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
docker run --rm -v pai-v3-cache:/home/v3/.cache --memory 1g -v "$(pwd -W 2>/dev/null || pwd):/v3" pai-v3-dev:local \
  sbcl --noinform --non-interactive --load /opt/quicklisp/setup.lisp \
    --eval '(push #p"/v3/" asdf:*central-registry*)' \
    --eval '(ql:quickload (list "cffi" "shasht" "ironclad" "babel" "bordeaux-threads" "dexador" "hunchentoot") :silent t)' \
    --eval '(let ((failed nil))
              (handler-bind ((style-warning (function muffle-warning))
                             (warning (lambda (w) (setf failed t) (format t "~&WARNING: ~a~%" w) (muffle-warning w))))
                (asdf:compile-system "pai-v3" :force t) (asdf:load-system "pai-v3") (asdf:compile-system "pai-v3-maintenance" :force t))
              (format t "~&check-load: ~:[ok~;FAILED~]~%" failed)
              (sb-ext:exit :code (if failed 1 0)))'
