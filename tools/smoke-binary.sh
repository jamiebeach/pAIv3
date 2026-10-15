#!/bin/sh
# Smoke-test the built executable in a bare Debian container: no SBCL, no
# Quicklisp, no source tree. Creates an agent, lists its unlock methods,
# runs it and checks the API answers, then restarts it to prove the store
# reopens.
set -eu
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
docker run --rm -v "$(pwd -W 2>/dev/null || pwd)/dist/linux-x64:/opt/pai:ro" \
  debian:bookworm-slim@sha256:7b140f374b289a7c2befc338f42ebe6441b7ea838a042bbd5acbfca6ec875818 bash -c '
    set -e
    apt-get update -qq >/dev/null && apt-get install -y -qq ca-certificates >/dev/null
    unset PAI_SQLITE_LIBRARY
    printf "a long enough smoke passphrase\n" | /opt/pai/pai init /data/beta beta --passphrase --no-keychain
    /opt/pai/pai unlock-methods /data/beta
    for round in 1 2; do
      printf "a long enough smoke passphrase\n" | /opt/pai/pai run /data/beta &
      pid=$!
      sleep 4
      exec 3<>/dev/tcp/127.0.0.1/8080
      printf "GET /v1/me HTTP/1.0\r\nHost: localhost\r\n\r\n" >&3
      head -1 <&3
      exec 3>&-
      kill -INT $pid; wait $pid || true
    done
    printf "a long enough smoke passphrase
" | /opt/pai/pai settings /data/beta | head -8
    ls -l /data/beta'
