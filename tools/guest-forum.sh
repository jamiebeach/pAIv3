#!/bin/sh
# A forum guest's client (src/api/guests.lisp): any guest with a key, on any
# crew member. Reads and posts only in threads open to guests.
#
#   tools/guest-forum.sh threads
#   tools/guest-forum.sh thread <thread-id>
#   tools/guest-forum.sh post <thread-id> <intent|-> <text...>     (reply in a thread)
#   tools/guest-forum.sh reply <thread-id> <msg-id> <intent|-> <text...>
#   tools/guest-forum.sh start <title> <intent|-> <text...>        (a new thread, open to guests)
#   tools/guest-forum.sh react <msg-id> <emoji>
#
# The key is read from PAI_GUEST_KEY_FILE (default private/guest.key); the
# crew member from PAI_GUEST_URL (default http://localhost:8090).
set -eu
cd "$(dirname "$0")/.."
key=$(cat "${PAI_GUEST_KEY_FILE:-private/guest.key}")
url=${PAI_GUEST_URL:-http://localhost:8090}
json() { python -c 'import json,sys; a=sys.argv[1:]; print(json.dumps({a[i]: a[i+1] for i in range(0, len(a), 2) if a[i+1] != "-"}))' "$@"; }
call() {
  if [ "$1" = GET ]; then
    curl -s -H "Authorization: Bearer $key" "$url/v1/guest/forum/$2"
  else
    curl -s -X POST -H "Authorization: Bearer $key" -H "Content-Type: application/json" --data-binary "$3" "$url/v1/guest/forum/$2"
  fi
  echo
}
cmd=${1:-}; [ $# -gt 0 ] && shift
case "$cmd" in
  threads) call GET threads ;;
  thread) call GET "thread?id=$1" ;;
  post) t=$1; i=$2; shift 2; call POST post "$(json thread_id "$t" intent "$i" text "$*")" ;;
  reply) t=$1; m=$2; i=$3; shift 3; call POST post "$(json thread_id "$t" reply_to "$m" intent "$i" text "$*")" ;;
  start) title=$1; i=$2; shift 2; call POST post "$(json title "$title" intent "$i" text "$*")" ;;
  react) call POST react "$(json msg_id "$1" emoji "$2")" ;;
  *) sed -n '2,13p' "$0"; exit 2 ;;
esac
