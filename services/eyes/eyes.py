#!/usr/bin/env python3
"""pAI-Eyes: a perception service for the crew (bead v3-byw; docs/system1-sensory-perception.md).

Eyes is a perception service, not a mind. A camera sends it frames under a
short-lived lease; it finds people with a local detector, follows them with a
simple tracker, and sends each subscribed agent compact observations
(entered, still present, left). Frames never leave Eyes and are never
stored: only the latest one per source is held, in memory, until the
detector takes it.

Eyes owns each source's on/off state and epoch. An agent asks it to start or
stop a source (on the operator's behalf); Eyes moves the epoch on and tells
every subscriber, so a stop from any agent's page holds for all, and frames
or results from before it are dropped.

Configuration: a JSON file (EYES_CONFIG, default /data/eyes.json):

  {"agent_keys_sha256": ["<sha256 of each agent's key to Eyes>"],
   "cors_origins": ["https://<the PWA origins a phone uses>"],
   "sources": [{"source_id": "phone", "name": "Jamie's phone",
                "subscribers": [{"url": "http://host.docker.internal:8090",
                                 "key": "<the source key that agent issued>"}]}]}

Endpoints (agents authenticate with their key; a camera with its lease):
  GET  /health
  GET  /sources                      status of every source
  POST /sources/{id}/start|stop      move the epoch on and tell every subscriber
  POST /stop-all
  POST /sources/{id}/lease           a capture lease for a camera (10 minutes)
  POST /frames?source={id}           one JPEG frame, Authorization: Bearer <lease>
"""
import hashlib
import json
import os
import secrets
import threading
import time
import urllib.error
import urllib.request
from datetime import datetime, timedelta, timezone
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer

import numpy as np

import detector

CONFIG_PATH = os.environ.get("EYES_CONFIG", "/data/eyes.json")
PORT = int(os.environ.get("EYES_PORT", "8470"))
LEASE_SECONDS = 600
MAX_FRAME_BYTES = 2 * 1024 * 1024
FRAMES_GONE_SECONDS = 15      # no frames this long while live: the source is lost
PRESENT_EVERY = 5             # seconds between "still present" for a track
OBSERVATION_TTL = 15          # how long an observation holds (agents cap it too)
SNAPSHOT_FRESH = 10            # an agent may look at the last analysed frame this long after it
LEFT_AFTER = 8                # seconds unseen before a track has left (a moving phone misses often)
REJOIN_WITHIN = 10            # a person seen again this soon after leaving is the same arrival
ENTER_HITS = 2                # detections before a track counts as an arrival

LOCK = threading.Lock()


def now():
    return time.monotonic()


def stamp(seconds=0):
    return (datetime.now(timezone.utc) + timedelta(seconds=seconds)).strftime("%Y-%m-%dT%H:%M:%SZ")


def log(*parts):
    print(time.strftime("%H:%M:%S"), *parts, flush=True)


class Source:
    def __init__(self, config):
        self.id = config["source_id"]
        self.name = config.get("name", self.id)
        self.subscribers = config.get("subscribers", [])
        # Epochs start from the clock, so a restarted Eyes never goes behind one an agent holds.
        self.state, self.epoch, self.reason = "disabled", int(time.time()), "not started"
        self.sequence = 0
        self.leases = {}          # token -> (epoch, expires monotonic)
        self.frame = None         # (epoch, jpeg bytes, received monotonic)
        self.last_frame_at = None
        self.tracks = []
        self.recent = None        # a track that just left, held for REJOIN_WITHIN
        self.last_seen = None     # (epoch, jpeg, monotonic, utc stamp): the last analysed frame, for an agent's look
        self.next_track = 0
        self.wake = threading.Event()

    def status(self):
        return {"source_id": self.id, "name": self.name, "state": self.state, "epoch": self.epoch,
                "reason": self.reason, "tracks": len([t for t in self.tracks if t["announced"]]),
                "subscribers": len(self.subscribers),
                "last_frame_seconds_ago": None if self.last_frame_at is None else round(now() - self.last_frame_at, 1)}


SOURCES = {}
CONFIG = {}


def load_config():
    with open(CONFIG_PATH, encoding="utf-8") as f:
        config = json.load(f)
    CONFIG.clear()
    CONFIG.update(config)
    for c in config.get("sources", []):
        if c["source_id"] not in SOURCES:
            SOURCES[c["source_id"]] = Source(c)
        else:
            SOURCES[c["source_id"]].subscribers = c.get("subscribers", [])


def agent_authorized(header):
    if not header or not header.startswith("Bearer "):
        return False
    digest = hashlib.sha256(header[7:].encode()).hexdigest()
    return any(secrets.compare_digest(digest, h) for h in CONFIG.get("agent_keys_sha256", []))


# Telling agents

def post_json(url, key, path, body):
    req = urllib.request.Request(url.rstrip("/") + path, data=json.dumps(body).encode(), method="POST",
                                 headers={"Authorization": "Bearer " + key, "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=5) as r:
            return r.status, r.read()
    except urllib.error.HTTPError as e:
        return e.code, e.read()
    except Exception as e:  # an agent that is down misses this; it reads state again on its next lease
        return 0, str(e).encode()


def broadcast_state(source):
    body = {"state": source.state, "epoch": source.epoch, "reason": source.reason}
    for sub in source.subscribers:
        status, _ = post_json(sub["url"], sub["key"], "/v1/perception/state", body)
        if status != 200:
            log("state to", sub["url"], "failed:", status)


def send_observations(source, epoch, observations):
    for sub in source.subscribers:
        status, reply = post_json(sub["url"], sub["key"], "/v1/perception/observations", {"observations": observations})
        if status != 200:
            log("observations to", sub["url"], "failed:", status, reply[:200])


def set_state(source, state, reason):
    with LOCK:
        if state == "live" and source.state == "live":
            return source.status()
        source.epoch += 1
        source.state, source.reason = state, reason
        source.sequence = 0
        source.leases.clear()
        source.frame = None
        source.tracks = []
        source.recent = None
        source.last_seen = None
        source.last_frame_at = None  # loss counts only once frames have arrived
    log(source.id, "->", state, "epoch", source.epoch, "(" + reason + ")")
    threading.Thread(target=broadcast_state, args=(source,), daemon=True).start()
    return source.status()


# Watching

def observation(source, epoch, kind, track):
    source.sequence += 1
    return {"source_id": source.id, "epoch": epoch, "sequence": source.sequence, "kind": kind,
            "track_id": "%d:%d" % (epoch, track["id"]), "recognition": "not-enabled",
            "score": round(float(track["score"]), 3), "detector": detector.MODEL_ID,
            "observed_at": stamp(), "expires_at": stamp(OBSERVATION_TTL)}


def update_tracks(source, epoch, boxes, at):
    """Match detections to tracks; return the observations to send."""
    out = []
    unmatched = list(range(len(boxes)))
    for track in source.tracks:
        best, best_iou = None, 0.15
        for i in unmatched:
            iou = detector.iou(track["box"], boxes[i][:4])
            if iou > best_iou:
                best, best_iou = i, iou
        if best is not None:
            unmatched.remove(best)
            track.update(box=boxes[best][:4], score=boxes[best][4], seen=at, hits=track["hits"] + 1)
    # Someone seen again soon after leaving is the same arrival, not a new one.
    for i in list(unmatched):
        if source.tracks or not getattr(source, "recent", None):
            break
        if at - source.recent["left_at"] <= REJOIN_WITHIN:
            track = source.recent
            track.update(box=boxes[i][:4], score=boxes[i][4], seen=at, hits=ENTER_HITS)
            source.tracks.append(track)
            source.recent = None
            unmatched.remove(i)
    for i in unmatched:
        source.next_track += 1
        source.tracks.append({"id": source.next_track, "box": boxes[i][:4], "score": boxes[i][4],
                              "seen": at, "hits": 1, "announced": False, "said": 0})
    for track in list(source.tracks):
        if at - track["seen"] > LEFT_AFTER:
            source.tracks.remove(track)
            if track["announced"]:
                # Held back briefly: they may only have stepped out of frame.
                source.recent = dict(track, left_at=at)
        elif not track["announced"] and track["hits"] >= ENTER_HITS:
            track["announced"], track["said"] = True, at
            out.append(observation(source, epoch, "person.entered", track))
        elif track["announced"] and track["seen"] == at and at - track["said"] >= PRESENT_EVERY:
            track["said"] = at
            out.append(observation(source, epoch, "person.present", track))
    recent = getattr(source, "recent", None)
    if recent and at - recent["left_at"] > REJOIN_WITHIN:
        out.append(observation(source, epoch, "person.left", recent))
        source.recent = None
    return out


def watch(source):
    """One worker per source: take the latest frame, detect, track, tell."""
    while True:
        source.wake.wait(timeout=1.0)
        source.wake.clear()
        with LOCK:
            frame, source.frame = source.frame, None
            epoch, live = source.epoch, source.state == "live"
            gone = live and source.last_frame_at is not None and now() - source.last_frame_at > FRAMES_GONE_SECONDS
        if gone:
            set_state(source, "lost", "no frames for %d seconds" % FRAMES_GONE_SECONDS)
            continue
        if not live:
            continue
        at = now()
        boxes = []
        if frame is not None:
            if frame[0] != epoch:
                continue  # sent before a stop or restart: dropped
            try:
                boxes = detector.people(frame[1])
            except Exception as e:
                log(source.id, "detector failed:", e)
                continue
        with LOCK:
            if source.epoch != epoch or source.state != "live":
                continue  # stopped while detecting: the result is dropped
            if frame is not None:
                source.last_seen = (epoch, frame[1], at, stamp())
            out = update_tracks(source, epoch, boxes, at) if frame is not None or source.tracks or source.recent else []
        if out:
            send_observations(source, epoch, out)


# HTTP

class Handler(BaseHTTPRequestHandler):
    server_version = "pAI-Eyes"

    def log_message(self, *args):
        pass

    def cors(self):
        origin = self.headers.get("Origin")
        if origin and origin in CONFIG.get("cors_origins", []):
            self.send_header("Access-Control-Allow-Origin", origin)
            self.send_header("Access-Control-Allow-Headers", "Authorization, Content-Type")
            self.send_header("Access-Control-Allow-Methods", "POST, OPTIONS")
            self.send_header("Vary", "Origin")

    def reply(self, status, body):
        data = json.dumps(body).encode()
        self.send_response(status)
        self.cors()
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(data)))
        self.end_headers()
        self.wfile.write(data)

    def do_OPTIONS(self):
        self.send_response(204)
        self.cors()
        self.end_headers()

    def do_GET(self):
        if self.path == "/health":
            return self.reply(200, {"ok": True, "detector": detector.MODEL_ID, "provider": detector.provider()})
        if self.path == "/sources":
            if not agent_authorized(self.headers.get("Authorization")):
                return self.reply(401, {"error": "an agent key is needed"})
            return self.reply(200, {"sources": [s.status() for s in SOURCES.values()]})
        self.reply(404, {"error": "no such route"})

    def do_POST(self):
        path, _, query = self.path.partition("?")
        if path == "/frames":
            return self.frame(dict(p.split("=", 1) for p in query.split("&") if "=" in p))
        if not agent_authorized(self.headers.get("Authorization")):
            return self.reply(401, {"error": "an agent key is needed"})
        length = int(self.headers.get("Content-Length") or 0)
        if length:
            self.rfile.read(min(length, 65536))
        if path == "/stop-all":
            return self.reply(200, {"stopped": [set_state(s, "disabled", "stopped by the operator")["source_id"] for s in SOURCES.values()]})
        parts = path.strip("/").split("/")
        if len(parts) == 3 and parts[0] == "sources" and parts[1] in SOURCES:
            source = SOURCES[parts[1]]
            if parts[2] == "start":
                return self.reply(200, set_state(source, "live", "started by the operator"))
            if parts[2] == "stop":
                return self.reply(200, set_state(source, "disabled", "stopped by the operator"))
            if parts[2] == "snapshot":
                # An agent's deliberate look: the last analysed frame, if fresh. Kept nowhere here.
                with LOCK:
                    seen = source.last_seen
                    if source.state != "live" or seen is None or seen[0] != source.epoch or now() - seen[2] > SNAPSHOT_FRESH:
                        return self.reply(404, {"error": "nothing fresh to look at: the source is off or has sent no frame in the last %d seconds" % SNAPSHOT_FRESH})
                    import base64
                    return self.reply(200, {"source_id": source.id, "epoch": seen[0], "seen_at": seen[3],
                                            "image": "data:image/jpeg;base64," + base64.b64encode(seen[1]).decode()})
            if parts[2] == "lease":
                with LOCK:
                    if source.state not in ("live", "lost"):
                        return self.reply(409, {"error": "start the source first"})
                    token = secrets.token_urlsafe(24)
                    source.leases[token] = (source.epoch, now() + LEASE_SECONDS)
                    return self.reply(200, {"source_id": source.id, "epoch": source.epoch, "token": token,
                                            "expires_in_seconds": LEASE_SECONDS, "frames_per_second": 1})
        self.reply(404, {"error": "no such source or route"})

    def frame(self, query):
        source = SOURCES.get(query.get("source", ""))
        auth = self.headers.get("Authorization") or ""
        length = int(self.headers.get("Content-Length") or 0)
        if source is None:
            return self.reply(404, {"error": "no such source"})
        if length <= 0 or length > MAX_FRAME_BYTES:
            return self.reply(413, {"error": "a frame is one JPEG of at most 2 MB"})
        data = self.rfile.read(length)
        with LOCK:
            lease = source.leases.get(auth[7:]) if auth.startswith("Bearer ") else None
            if lease is None or lease[1] < now():
                return self.reply(401, {"error": "no valid capture lease; ask for a new one"})
            if lease[0] != source.epoch or source.state == "disabled":
                return self.reply(409, {"error": "the source was stopped or restarted", "state": source.state})
            source.frame = (source.epoch, data, now())
            source.last_frame_at = now()
            recovered = source.state == "lost"
        if recovered:
            with LOCK:
                source.state, source.reason = "live", "frames resumed"
            threading.Thread(target=broadcast_state, args=(source,), daemon=True).start()
        source.wake.set()
        self.reply(200, {"ok": True, "epoch": source.epoch})


def main():
    load_config()
    detector.load()
    for source in SOURCES.values():
        threading.Thread(target=watch, args=(source,), daemon=True).start()
        # Every source starts off: tell the subscribers so nothing old reads as current.
        threading.Thread(target=broadcast_state, args=(source,), daemon=True).start()
    log("pAI-Eyes on port", PORT, "with", len(SOURCES), "source(s); detector", detector.MODEL_ID, "on", detector.provider())
    ThreadingHTTPServer(("0.0.0.0", PORT), Handler).serve_forever()


if __name__ == "__main__":
    main()
