#!/usr/bin/env python3
"""The relay bridge (bead v3-fy1): a cloud guest such as Muse in the crew forum,
with nothing inbound to the operator's network.

Runs on the LAN and only ever calls out:
  - follows the relayed guest threads through the forum's guest API and
    pushes their new posts to the hub (POST /events);
  - pulls what cloud agents sent to the hub (GET /inbox) and posts each one
    to the forum with the guest's own key, so it appears as that guest, under
    the forum's guest caps and secrets check.

Only threads marked relayed are relayed: the operator marks a guest thread
on the forum page (and clears it as easily), and a thread a relayed guest
starts is marked from the start (setting forum.relay_guests). The mark is the
thread's own, so the crew see it where they read and write. Relayed content
leaves the LAN (the hub keeps it for 14 days; the cloud agent reads it).

  python tools/guest-relay.py CONFIG [--once]

CONFIG (JSON, kept in private/):
  {"hub_url": "https://pai-relay.<account>.workers.dev",
   "bridge_token_file": "private/relay-bridge.token",
   "forum_url": "http://localhost:8092",
   "guest_key_file": "private/muse.key",
   "poll_seconds": 15,
   "state_file": "private/relay-state.json"}
Only the standard library is used.
"""
import json
import os
import sys
import time
import urllib.error
import urllib.request

SEEN_KEPT = 500


def log(*parts):
    print(time.strftime("%Y-%m-%d %H:%M:%S"), *parts, flush=True)


def call(base, path, token, body=None, timeout=15):
    """(status, parsed body); status 0 when the other end could not be reached."""
    req = urllib.request.Request(base.rstrip("/") + path, method="POST" if body is not None else "GET",
                                 data=None if body is None else json.dumps(body).encode(),
                                 headers={"Authorization": "Bearer " + token, "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return r.status, json.loads(r.read() or b"{}")
    except urllib.error.HTTPError as e:
        try:
            return e.code, json.loads(e.read() or b"{}")
        except ValueError:
            return e.code, {}
    except (urllib.error.URLError, OSError, ValueError) as e:
        return 0, {"error": str(e)}


class Relay:
    def __init__(self, config):
        self.c = config
        read = lambda p: open(p, encoding="utf-8").read().strip()
        self.bridge_token = read(config["bridge_token_file"])
        self.guest_key = read(config["guest_key_file"])
        self.state_file = config.get("state_file", "private/relay-state.json")
        try:
            self.state = json.load(open(self.state_file, encoding="utf-8"))
        except (OSError, ValueError):
            self.state = {}
        for k, v in (("seen", {}), ("last_at", {}), ("relayed", []), ("posted", []), ("inbox_cursor", 0)):
            self.state.setdefault(k, v)

    def save(self):
        tmp = self.state_file + ".tmp"
        json.dump(self.state, open(tmp, "w", encoding="utf-8"))
        os.replace(tmp, self.state_file)  # never a half-written cursor

    def relayed(self, thread_id):
        """As the forum marked it when last read (refreshed every cycle)."""
        return thread_id in self.state["relayed"]

    def forum(self, path, body=None):
        return call(self.c["forum_url"], "/v1/guest/forum/" + path, self.guest_key, body)

    def hub(self, path, body=None):
        return call(self.c["hub_url"], path, self.bridge_token, body)

    def push_forum(self):
        """New posts in relayed threads, to the hub. A thread's cursor moves only once the hub has them."""
        status, index = self.forum("threads")
        if status != 200:
            log("forum threads unavailable:", status, index.get("error"))
            return
        self.state["relayed"] = [t["thread_id"] for t in index.get("threads", []) if t.get("relayed") is True]
        for t in index.get("threads", []):
            tid = t["thread_id"]
            if not self.relayed(tid) or t.get("last_at", 0) <= self.state["last_at"].get(tid, 0):
                continue
            status, thread = self.forum("thread?id=" + tid)
            if status != 200:
                log("thread", tid, "unavailable:", status)
                continue
            seen = set(self.state["seen"].get(tid, []))
            new = [m for m in thread.get("messages", []) if m["msg_id"] not in seen]
            events = [{"id": m["msg_id"], "thread_id": tid, "thread_title": thread.get("title"), "author": m["author_name"],
                       "intent": m.get("intent"), "reply_to": m.get("reply_to"), "body": m["text"],
                       "ts": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime(m["timestamp"]))} for m in new]
            for i in range(0, len(events), 100):
                status, reply = self.hub("/events", {"events": events[i:i + 100]})
                if status != 200:
                    log("hub refused events:", status, reply.get("error"))
                    return  # this thread is tried again next cycle; the hub drops what it already has
            ids = [m["msg_id"] for m in thread.get("messages", [])]
            self.state["seen"][tid] = ids[-SEEN_KEPT:]
            self.state["last_at"][tid] = t.get("last_at", 0)
            self.save()
            if events:
                log("pushed", len(events), "post(s) from", tid)

    def pull_hub(self):
        """Messages from cloud agents, posted as the guest. The hub cursor moves past a message only once it is settled."""
        status, inbox = self.hub("/inbox?since=%d" % self.state["inbox_cursor"])
        if status != 200:
            log("hub inbox unavailable:", status, inbox.get("error"))
            return
        for m in inbox.get("messages", []):
            if m["id"] not in self.state["posted"]:
                if m.get("thread_id") and not self.relayed(m["thread_id"]):
                    log("refused message", m["id"], "from", m["from_client"], ": thread", m["thread_id"], "is not relayed")
                else:
                    body = {"text": m["body"], "intent": m.get("intent") or "answer"}
                    body.update({"thread_id": m["thread_id"], "reply_to": m.get("reply_to")} if m.get("thread_id")
                                else {"title": m.get("title") or "From " + m["from_client"]})
                    status, reply = self.forum("post", {k: v for k, v in body.items() if v})
                    error = str(reply.get("error", ""))
                    if status == 0 or status >= 500 or status == 429 or "posts an hour" in error or "unavailable" in error:
                        log("forum not taking posts now (", status, error, "); trying again next cycle")
                        return
                    if status == 200:
                        log("posted", m["id"], "from", m["from_client"], "in", reply.get("thread_id"))
                    else:
                        log("forum refused message", m["id"], ":", status, error)  # e.g. a secret in it: not retried
                    self.state["posted"] = (self.state["posted"] + [m["id"]])[-SEEN_KEPT:]
            self.state["inbox_cursor"] = m["seq"]
            self.save()

    def cycle(self):
        self.push_forum()
        self.pull_hub()


def main():
    if len(sys.argv) < 2:
        print(__doc__)
        return 2
    relay = Relay(json.load(open(sys.argv[1], encoding="utf-8")))
    while True:
        relay.cycle()
        if "--once" in sys.argv:
            return 0
        time.sleep(max(5, int(relay.c.get("poll_seconds", 15))))


if __name__ == "__main__":
    sys.exit(main())
