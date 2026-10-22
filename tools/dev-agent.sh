#!/bin/sh
# The dev agent runs from source in the toolchain image, like local
# development: no build per change. Its container mounts nothing; the source
# is copied into its volume, and the running agent loads what changed.
#
#   tools/dev-agent.sh create          make the container (run once)
#   tools/dev-agent.sh sync            copy the source in and reload it live (restarts instead
#                                        when kernel, trust or package files changed)
#   tools/dev-agent.sh sync --restart  copy the source in and restart
#   tools/dev-agent.sh threads          print what each thread is doing (when it hangs)
#   tools/dev-agent.sh peek <what> [n]  read the running agent: dialogue,
#                                        calls (raw model responses), thoughts or board
#   tools/dev-agent.sh pai <command>   run the pai CLI from source while the
#                                        agent is stopped (reads stdin)
#   tools/dev-agent.sh rebuild <name>...  stop the agent, rebuild those
#                                        projections from its ledger, start it
#
# Live reload needs the agent's dev.live_reload setting. Some changes still
# need --restart: kernel code, new tables or event schemas, changed structs, new systems,
# and anything installed once at start (such as hooks). A projection added
# after the agent has history starts as needs-rebuild and is never replayed
# silently: rebuild it once after deploying it.
#
# Which container, volume and agent folder: the file PAI_DEV_ENV names
# (default private/dev-agent.env) when it exists (PAI_DEV_NAME, PAI_DEV_VOLUME,
# PAI_DEV_DIR, PAI_DEV_PORT, and PAI_DEV_ARGS for extra docker options), else
# these defaults.
set -eu
export MSYS_NO_PATHCONV=1
cd "$(dirname "$0")/.."
# A hash of the files a live reload cannot safely replace.
risky_hash='find src/kernel src/trust pai-v3.asd src/*/package.lisp -type f | sort | xargs sha256sum | sed "s/ \*/  /" | sha256sum | cut -c1-16'

env_file=${PAI_DEV_ENV:-private/dev-agent.env}
[ -f "$env_file" ] && . "./$env_file"
name=${PAI_DEV_NAME:-pai-dev}
image=pai-v3-dev:local
volume=${PAI_DEV_VOLUME:-pai-dev-home}
agent_dir=${PAI_DEV_DIR:-/home/pai/agent}
port=${PAI_DEV_PORT:-8090}
extra=${PAI_DEV_ARGS:-}
load='(push #p"/home/pai/v3/" asdf:*central-registry*)'

copy_source() {
  # Through a pipe, not a mount. File times are kept, so ASDF recompiles
  # only what changed.
  tar cf - pai-v3.asd pai-v3-maintenance.asd src web \
    | docker run --rm -i -v "$volume:/home/pai" --entrypoint sh "$image" \
        -c 'mkdir -p /home/pai/v3 && rm -rf /home/pai/v3/src /home/pai/v3/web && tar xf - -C /home/pai/v3'
}

wait_up() {
  for i in $(seq 1 90); do
    curl -s -m 2 http://localhost:$port/v1/auth/state >/dev/null && return 0
    sleep 2
  done
  echo "$name did not answer within three minutes."; return 1
}

case "${1:-}" in
  create)
    copy_source
    docker rm -f "$name" >/dev/null 2>&1 || true
    docker create --name "$name" --restart unless-stopped -p 127.0.0.1:$port:$port $extra \
      -e HOME=/home/pai -w /home/pai -v "$volume:/home/pai" "$image" \
      sbcl --dynamic-space-size 2048 --noinform --non-interactive \
        --load /opt/quicklisp/setup.lisp --eval "$load" \
        --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "pai-v3" :silent t))' \
        --eval '(pai-v3.cli::cmd-run (list "'"$agent_dir"'"))' >/dev/null
    echo "Created $name. Start it with: docker start $name"
    ;;
  sync)
    # Kernel, trust and package changes cannot be loaded into a running image
    # safely (a live reload of kernel code once wedged an agent's database
    # access), so they always restart it.
    before=$(docker exec "$name" sh -c "cd /home/pai/v3 && $risky_hash" 2>/dev/null || echo none)
    copy_source
    after=$(sh -c "$risky_hash")
    # Live reload wedged all three agents on 2026-10-22 (threads deadlocked
    # waiting on locks after the reload), so it now happens only on request.
    if [ "${2:-}" != "--live" ] || [ "$before" != "$after" ]; then
      [ "${2:-}" != "--live" ] || echo "Kernel, trust or package files changed: restarting $name instead of reloading live."
      docker restart "$name" >/dev/null
      wait_up && echo "Restarted $name."
    else
      docker exec "$name" curl -s -X POST http://127.0.0.1:$port/v1/dev/reload
      echo
    fi
    # A projection left needs-rebuild serves empty answers: fail loudly.
    health=$(docker exec "$name" curl -s http://127.0.0.1:$port/v1/dev/health)
    case "$health" in
      *'"ok":true'*) ;;
      *) echo "NOT READY: $health"; echo "Fix with: tools/dev-agent.sh rebuild <name>..."; exit 1 ;;
    esac
    ;;
  threads)
    # What every thread is doing, for an agent that has stopped answering.
    docker kill -s USR1 "$name" >/dev/null && sleep 2 && docker logs --since 10s "$name" 2>&1 | grep -v '^;'
    ;;
  peek)
    # Read recent state of the running agent: dialogue, calls or thoughts.
    docker exec "$name" curl -s "http://127.0.0.1:$port/v1/dev/peek?what=${2:-dialogue}&limit=${3:-6}"
    echo
    ;;
  pai)
    shift
    docker run --rm -i -e HOME=/home/pai -w /home/pai -v "$volume:/home/pai" $extra "$image" \
      sbcl --dynamic-space-size 1024 --noinform --non-interactive \
        --load /opt/quicklisp/setup.lisp --eval "$load" \
        --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "pai-v3" :silent t))' \
        --eval "(sb-ext:exit :code (pai-v3.cli::run-command (uiop:split-string \"$*\")))" 2>&1 | grep -v '^;'
    ;;
  rebuild)
    shift
    [ $# -gt 0 ] || { echo "Name the projections to rebuild."; exit 2; }
    copy_source
    docker stop "$name" >/dev/null
    docker run --rm -e HOME=/home/pai -e REBUILD="$*" -w /home/pai -v "$volume:/home/pai" $extra "$image" \
      sbcl --dynamic-space-size 2048 --noinform --non-interactive \
        --load /opt/quicklisp/setup.lisp --eval "$load" \
        --eval '(handler-bind ((warning (function muffle-warning))) (ql:quickload "pai-v3-maintenance" :silent t))' \
        --eval '(let ((h (pai-v3.host::open-agent "'"$agent_dir/"'")))
                  (unwind-protect
                       (dolist (n (uiop:split-string (uiop:getenv "REBUILD")))
                         (format t "~a: ~a~%" n
                                 (pai-v3.maintenance::rebuild-projection (pai-v3.host::agent-handle-kernel h) n)))
                    (pai-v3.host::close-agent h)))' 2>&1 | grep -v '^;'
    docker start "$name" >/dev/null
    wait_up && echo "Started $name again."
    ;;
  *)
    sed -n '2,26p' "$0"; exit 2 ;;
esac
