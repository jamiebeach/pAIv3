"""A narrow bridge from an agent to a Beadwork ledger on this machine.

The agent's container mounts nothing, so it cannot run bw itself. This serves
one route, POST /run, that takes {"action": ..., ...} and runs bw with a
fixed argument list: no shell, and only reading plus comment and create.
ping names a bead for the coding agent: it is appended to the inbox file,
which the coding agent watches. patch saves a proposed diff for a bead under
private/patches/, notes it on the bead and pings; the coding agent reviews
and applies it, the agent never does. Writes are rate-limited per day.
Nothing can close, delete, reopen or sync. Requests need the shared token as
a bearer token. Comments are signed with the agent's name, and beads it
creates get the label from-agent so they are easy to review.

Usage: python tools/beads-bridge.py [config.json]   (default settings in private/beads-bridge.json:
{"repo": "Z:/dev/paiV3", "bw": "Z:/beadwork/bw.exe", "author": "gamma",
 "port": 8765, "token_file": "private/beads-bridge.token"})
"""
import datetime, hmac, json, os, re, secrets, subprocess, sys, threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CONFIG = json.load(open(sys.argv[1] if len(sys.argv) > 1 else os.path.join(HERE, "private", "beads-bridge.json")))
TOKEN_FILE = os.path.join(HERE, CONFIG.get("token_file", "private/beads-bridge.token"))
if not os.path.exists(TOKEN_FILE):
    with open(TOKEN_FILE, "w") as f:
        f.write(secrets.token_urlsafe(32))
TOKEN = open(TOKEN_FILE).read().strip()

INBOX = os.path.join(HERE, CONFIG.get("inbox", "private/beads-inbox.jsonl"))
PATCHES = os.path.join(HERE, CONFIG.get("patches", "private/patches"))
MAX_PATCH = 60000
LIMITS = {"create": 10, "comment": 40, "ping": 20, "patch": 10, **CONFIG.get("daily_limits", {})}
USED = {}
USED_LOCK = threading.Lock()

ID = re.compile(r"^[a-z0-9]+-[a-z0-9.]+$")
TYPES = {"task", "bug", "feature", "chore", "epic"}
MAX_TEXT = 8000


class Refused(Exception):
    pass


def text(args, key, required=True, limit=MAX_TEXT):
    value = args.get(key)
    if value is None or value == "":
        if required:
            raise Refused(f"{key} is required")
        return None
    if not isinstance(value, str) or len(value) > limit:
        raise Refused(f"{key} must be text of at most {limit} characters")
    # bw would read a leading dash as a flag.
    return " " + value if value.startswith("-") else value


def bead_id(args, key="id", required=True):
    value = text(args, key, required, 40)
    if value is not None and not ID.match(value):
        raise Refused(f"{value!r} is not a bead id")
    return value


def command(args):
    """The bw arguments for one request, or Refused."""
    action = args.get("action")
    if action == "ready":
        return ["ready"]
    if action == "list":
        argv = ["list"]
        if args.get("all"):
            argv.append("--all")
        grep = text(args, "grep", False, 200)
        if grep:
            argv += ["--grep", grep]
        return argv
    if action == "show":
        return ["show", bead_id(args)]
    if action == "history":
        return ["history", bead_id(args), "--limit", "20"]
    if action == "comment":
        return ["comment", bead_id(args), text(args, "text"), "--author", CONFIG["author"]]
    if action == "create":
        argv = ["create", text(args, "title", True, 200), "--silent"]
        description = text(args, "description", False)
        if description:
            argv += ["-d", description]
        parent = bead_id(args, "parent", False)
        if parent:
            argv += ["--parent", parent]
        kind = args.get("type")
        if kind:
            if kind not in TYPES:
                raise Refused(f"type must be one of {sorted(TYPES)}")
            argv += ["-t", kind]
        priority = args.get("priority")
        if priority is not None:
            if not isinstance(priority, int) or not 0 <= priority <= 4:
                raise Refused("priority must be 0 to 4")
            argv += ["-p", str(priority)]
        return argv
    if action in ("ping", "patch"):
        if action == "patch":
            text(args, "diff", True, MAX_PATCH)
        return ["show", bead_id(args), "--json"]
    raise Refused("action must be ready, list, show, history, comment, create, ping or patch")


def spend(action):
    """Count one write today, or Refused when today's allowance is gone."""
    if action not in LIMITS:
        return
    today = datetime.date.today().isoformat()
    with USED_LOCK:
        key = (today, action)
        if USED.get(key, 0) >= LIMITS[action]:
            raise Refused(f"today's allowance of {LIMITS[action]} {action} actions is used up")
        USED[key] = USED.get(key, 0) + 1


def bw(argv):
    result = subprocess.run([CONFIG["bw"], "-C", CONFIG["repo"], *argv],
                            capture_output=True, text=True, encoding="utf-8", timeout=60)
    return result.returncode, (result.stdout + result.stderr).strip()


def ping(args, title):
    entry = {"at": datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds"),
             "from": CONFIG["author"], "bead": args["id"], "title": title,
             "note": text(args, "text", False, 1000) or ""}
    with USED_LOCK, open(INBOX, "a", encoding="utf-8") as f:
        print(json.dumps(entry), file=f)


def save_patch(args):
    """Write the proposed diff, note it on the bead, and return its file name."""
    os.makedirs(PATCHES, exist_ok=True)
    stamp = datetime.datetime.now(datetime.timezone.utc).strftime("%Y%m%dT%H%M%SZ")
    name = f"{args['id']}-{stamp}.diff"
    with open(os.path.join(PATCHES, name), "w", encoding="utf-8", newline="\n") as f:
        f.write(args["diff"])
    lines = args["diff"].count("\n") + 1
    why = text(args, "text", False, 2000)
    bw(["comment", args["id"], f"Patch proposed: patches/{name} ({lines} lines), for review."
        + (f" {why}" if why else ""), "--author", CONFIG["author"]])
    return name


def run(args):
    argv = command(args)
    spend(args.get("action"))
    code, out = bw(argv)
    if code == 0 and args.get("action") == "patch":
        name = save_patch(args)
        bead = json.loads(out)
        bead = bead[0] if isinstance(bead, list) else bead
        ping(dict(args, text=f"patch {name}: " + (args.get("text") or "")[:900]), bead.get("title", ""))
        return 0, f"Patch saved as {name}, noted on {args['id']}, and the coding agent pinged."
    if code == 0 and args.get("action") == "ping":
        bead = json.loads(out)
        bead = bead[0] if isinstance(bead, list) else bead
        ping(args, bead.get("title", ""))
        return 0, f"The coding agent has been pinged about {args['id']}."
    if code == 0 and argv[0] == "create":
        new_id = out.splitlines()[-1].strip()
        bw(["label", new_id, "+from-agent"])
        out = f"Created {new_id}."
    return code, out


class Handler(BaseHTTPRequestHandler):
    def reply(self, status, body):
        data = json.dumps(body).encode()
        self.send_response(status)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_POST(self):
        given = self.headers.get("Authorization", "")
        if not hmac.compare_digest(given.encode(), f"Bearer {TOKEN}".encode()):
            return self.reply(401, {"error": "not authorised"})
        if self.path != "/run":
            return self.reply(404, {"error": "no such route"})
        try:
            length = min(int(self.headers.get("Content-Length", 0)), 80000)
            args = json.loads(self.rfile.read(length) or b"{}")
            code, out = run(args)
            self.reply(200, {"ok": code == 0, "output": out})
        except Refused as e:
            self.reply(400, {"error": str(e)})
        except Exception as e:  # noqa: BLE001
            self.reply(500, {"error": f"{type(e).__name__}: {e}"})

    def log_message(self, fmt, *a):
        sys.stderr.write("beads-bridge: " + (fmt % a) + "\n")


if __name__ == "__main__":
    address = CONFIG.get("address", "127.0.0.1")
    port = CONFIG.get("port", 8765)
    print(f"beads-bridge on {address}:{port} for {CONFIG['repo']}", flush=True)
    ThreadingHTTPServer((address, port), Handler).serve_forever()
