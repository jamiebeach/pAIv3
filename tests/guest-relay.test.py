"""The relay bridge end to end (bead v3-fy1), with the real hub code served
locally and a fake forum speaking the guest API. Run: python tests/guest-relay.test.py"""
import json
import os
import subprocess
import sys
import tempfile
import threading
import time
import importlib.util
import urllib.request
import uuid
from datetime import datetime, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
spec = importlib.util.spec_from_file_location("relay", os.path.join(ROOT, "tools", "guest-relay.py"))
relay_mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(relay_mod)

checks = 0


def ok(cond, what):
    global checks
    assert cond, what
    checks += 1


# A fake forum: two guest threads, one relayed; posts as "Muse".
FORUM = {"threads": {"t-relayed": {"title": "SYNTHETIC relayed", "messages": []},
                     "t-other": {"title": "SYNTHETIC not relayed", "messages": []}}, "refuse_next": None}


def add_post(tid, author, text, ts):
    FORUM["threads"][tid]["messages"].append({"msg_id": str(uuid.uuid4()), "author_name": author, "text": text,
                                              "intent": "fyi", "reply_to": None, "timestamp": ts})


class Forum(BaseHTTPRequestHandler):
    def log_message(self, *a):
        pass

    def reply(self, status, body):
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(data)

    def do_GET(self):
        if self.headers.get("Authorization") != "Bearer muse-guest-key":
            return self.reply(401, {"error": "a guest key is needed"})
        if self.path == "/v1/guest/forum/threads":
            return self.reply(200, {"threads": [{"thread_id": k, "title": t["title"], "posts": len(t["messages"]),
                                                 "last_at": max([m["timestamp"] for m in t["messages"]] or [0])}
                                                for k, t in FORUM["threads"].items()]})
        tid = self.path.split("id=")[-1]
        t = FORUM["threads"].get(tid)
        return self.reply(200, {"thread_id": tid, "title": t["title"], "messages": t["messages"]}) if t else self.reply(404, {})

    def do_POST(self):
        body = json.loads(self.rfile.read(int(self.headers["Content-Length"])))
        if FORUM["refuse_next"]:
            status, error = FORUM["refuse_next"]
            FORUM["refuse_next"] = None
            return self.reply(status, {"error": error})
        tid = body.get("thread_id")
        if not tid:
            tid = "t-new-" + str(uuid.uuid4())[:8]
            FORUM["threads"][tid] = {"title": body["title"], "messages": []}
        add_post(tid, "Muse", body["text"], int(time.time()))
        self.reply(200, {"thread_id": tid, "msg_id": FORUM["threads"][tid]["messages"][-1]["msg_id"]})


forum = ThreadingHTTPServer(("127.0.0.1", 0), Forum)
threading.Thread(target=forum.serve_forever, daemon=True).start()
hub_port = 18787 + os.getpid() % 1000
hub = subprocess.Popen(["node", os.path.join(ROOT, "services", "relay-hub", "local.mjs"), "serve", str(hub_port),
                        "bridge:bridge:bridge-token", "muse:agent:muse-token"], stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
hub.stdout.readline()
HUB = "http://127.0.0.1:%d" % hub_port
tmp = tempfile.mkdtemp()
open(os.path.join(tmp, "bridge.token"), "w").write("bridge-token")
open(os.path.join(tmp, "muse.key"), "w").write("muse-guest-key")
config = {"hub_url": HUB, "bridge_token_file": os.path.join(tmp, "bridge.token"), "guest_key_file": os.path.join(tmp, "muse.key"),
          "forum_url": "http://127.0.0.1:%d" % forum.server_address[1], "threads": ["t-relayed"],
          "state_file": os.path.join(tmp, "state.json")}


def muse(path, body=None):
    return relay_mod.call(HUB, path, "muse-token", body)


def say(**over):
    m = {"id": str(uuid.uuid4()), "from": "muse", "body": "SYNTHETIC from Muse", "nonce": str(uuid.uuid4()),
         "ts": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")}
    m.update(over)
    return muse("/messages", m)


try:
    add_post("t-relayed", "Angel", "SYNTHETIC hello Muse", 1000)
    add_post("t-other", "Aria", "SYNTHETIC private to the crew and guests here", 1001)
    relay = relay_mod.Relay(config)
    relay.cycle()
    events = muse("/events?since=0")[1]["events"]
    ok([e["body"] for e in events] == ["SYNTHETIC hello Muse"], "only the relayed thread reaches the hub")
    relay.cycle()
    ok(len(muse("/events?since=0")[1]["events"]) == 1, "nothing is pushed twice")

    # Muse replies; the bridge posts it in the forum as the guest.
    say(thread_id="t-relayed")
    relay.cycle()
    posted = FORUM["threads"]["t-relayed"]["messages"]
    ok(posted[-1]["author_name"] == "Muse" and posted[-1]["text"] == "SYNTHETIC from Muse", "Muse's message is in the thread, as Muse")
    relay.cycle()
    ok(len(FORUM["threads"]["t-relayed"]["messages"]) == 2, "and only once")

    # A thread that is not relayed is refused; a new thread Muse starts is relayed from then on.
    say(thread_id="t-other")
    say(title="SYNTHETIC Muse's own thread")
    relay.cycle()
    ok(len(FORUM["threads"]["t-other"]["messages"]) == 1, "a message for a thread that is not relayed is not posted")
    new = [k for k in FORUM["threads"] if k.startswith("t-new-")]
    ok(len(new) == 1 and new[0] in relay.state["started"], "a thread Muse starts is created and relayed")

    # The forum is busy: the message waits, and goes when it can.
    FORUM["refuse_next"] = (400, "at most 12 posts an hour")
    say(thread_id="t-relayed", body="SYNTHETIC later")
    relay.cycle()
    ok(FORUM["threads"]["t-relayed"]["messages"][-1]["text"] != "SYNTHETIC later", "held while the forum is not taking posts")
    relay = relay_mod.Relay(config)  # and a restart loses nothing
    relay.cycle()
    ok(FORUM["threads"]["t-relayed"]["messages"][-1]["text"] == "SYNTHETIC later", "posted once the forum takes it, after a restart")

    # A permanent refusal (a secret in it) is logged and not retried.
    FORUM["refuse_next"] = (400, "the post seems to contain a credential or secret; nothing was posted")
    say(thread_id="t-relayed", body="SYNTHETIC key sk-FAKE")
    relay.cycle()
    relay.cycle()
    ok(all("sk-FAKE" not in m["text"] for m in FORUM["threads"]["t-relayed"]["messages"]), "a refused post is dropped, not retried")

    # Muse's own posts come back to it as events too, so it can see the thread as it stands.
    relay.cycle()
    authors = [e["author"] for e in muse("/events?since=0")[1]["events"]]
    ok(authors.count("Muse") >= 1 and authors[0] == "Angel", "the thread as it stands, in order")
    print("guest-relay: %d checks, 0 failures" % checks)
finally:
    hub.terminate()
    forum.shutdown()
