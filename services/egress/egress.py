"""The egress proxy: the one way out for the agents' containers (bead v3-r2j.1).

Default-allow, with deny rules: no IP-literal hosts; nothing that resolves to
a private, loopback, link-local, multicast, reserved or cloud-metadata
address; no host on the abuse.ch URLhaus or ThreatFox malware lists; no
address in a Spamhaus DROP range. The lists are fetched at start and daily.
Run it with Quad9 (9.9.9.9) as its resolver and known-malware names do not
resolve at all.

Every request is one JSON line in the log: who asked (the name in the proxy
URL, http://specter@host:3128), method, host, port, decision and reason,
bytes each way, seconds. In dry-run mode a request the rules would deny is
logged as would-deny and let through, so the real traffic can be learned
before anything is enforced.

An instance checks its own coverage with `curl http://egress.check/`: through
the proxy it answers with the caller's name and recent log lines; without the
proxy the name does not exist, so a bypass fails instead of passing quietly.

    python egress.py config.json
"""
import asyncio
import base64
import ipaddress
import json
import os
import socket
import sys
import time
import urllib.parse
import urllib.request

# Host lists come from the config ("host_lists": [{"name", "url",
# "auth_key_file"}]): abuse.ch's bulk downloads need a free account key since
# 2025, sent as the Auth-Key header. Without them, Quad9 still refuses the
# known-malware names those lists carry.
DROP_URLS = ["https://www.spamhaus.org/drop/drop_v4.json", "https://www.spamhaus.org/drop/drop.txt"]
METADATA = {"169.254.169.254", "fd00:ec2::254", "100.100.100.200"}
LOG_MOST = 50 * 1024 * 1024
HEAD_MOST = 64 * 1024

config = {}
denied_hosts = {}            # host -> list name
drop_nets = []               # ip_network


def log(entry):
    path = os.path.join(config["data_dir"], "egress-log.jsonl")
    try:
        if os.path.exists(path) and os.path.getsize(path) > LOG_MOST:
            os.replace(path, path + ".1")
        with open(path, "a", encoding="utf-8") as f:
            f.write(json.dumps(entry) + "\n")
    except OSError as e:
        print("log failed:", e, file=sys.stderr)


CHECK_HOST = "egress.check"


def recent_for(client, most):
    """The last MOST log entries for CLIENT, newest last."""
    path = os.path.join(config["data_dir"], "egress-log.jsonl")
    try:
        with open(path, encoding="utf-8") as f:
            lines = f.readlines()[-2000:]
    except OSError:
        return []
    mine = [json.loads(l) for l in lines if f'"client": "{client}"' in l]
    return [{k: e.get(k) for k in ("ts", "method", "host", "decision", "reason")} for e in mine[-most:]]


def fetch(url, key=None, body=None):
    headers = {"User-Agent": "pai-egress/1 (blocklist refresh)"}
    if key:
        headers["Auth-Key"] = key
    req = urllib.request.Request(url, headers=headers, data=body.encode() if body else None)
    with urllib.request.urlopen(req, timeout=60) as r:
        return r.read().decode("utf-8", "replace")


# abuse.ch's bulk host files are gone (410); its APIs answer with a free
# Auth-Key. Each kind turns one API answer into (hosts, ip addresses).
def urlhaus_api(text):
    hosts = set()
    for u in json.loads(text).get("urls") or []:
        h = urllib.parse.urlparse(u.get("url") or "").hostname
        if h:
            hosts.add(h.lower())
    return hosts, set()


def threatfox_api(text):
    hosts, ips = set(), set()
    for ioc in json.loads(text).get("data") or []:
        kind, value = ioc.get("ioc_type"), ioc.get("ioc") or ""
        if kind == "domain":
            hosts.add(value.lower().rstrip("."))
        elif kind == "url":
            h = urllib.parse.urlparse(value).hostname
            if h:
                hosts.add(h.lower())
        elif kind == "ip:port":
            ips.add(value.rsplit(":", 1)[0].strip("[]"))
    return hosts, ips


API_KINDS = {"urlhaus_api": urlhaus_api, "threatfox_api": threatfox_api}


def load_lists():
    """Fetch the lists, keeping the last good copy on disk when a fetch fails."""
    global denied_hosts, drop_nets
    hosts, nets, counts = {}, [], {}
    lists_dir = os.path.join(config["data_dir"], "lists")
    os.makedirs(lists_dir, exist_ok=True)

    def cached(name, url_list, key=None, body=None):
        path = os.path.join(lists_dir, name)
        for url in url_list:
            try:
                text = fetch(url, key, body)
                with open(path, "w", encoding="utf-8") as f:
                    f.write(text)
                return text
            except Exception as e:  # keep going: an old list beats none
                print(f"list {name} from {url} failed: {e}", file=sys.stderr)
        if os.path.exists(path):
            with open(path, encoding="utf-8") as f:
                return f.read()
        return ""

    for spec in config.get("host_lists", []):
        name, key = spec["name"], None
        if spec.get("auth_key_file"):
            try:
                with open(spec["auth_key_file"], encoding="utf-8") as f:
                    key = f.read().strip()
            except OSError as e:
                print(f"list {name}: no key ({e})", file=sys.stderr)
        n = 0
        text = cached(name, [spec["url"]], key, spec.get("body"))
        if spec.get("kind") in API_KINDS:
            try:
                found, ips = API_KINDS[spec["kind"]](text) if text else (set(), set())
            except ValueError as e:
                print(f"list {name}: unreadable answer ({e})", file=sys.stderr)
                found, ips = set(), set()
            for h in found:
                hosts[h] = name
            for ip in ips:
                try:
                    nets.append(ipaddress.ip_network(ip))
                except ValueError:
                    pass
            n = len(found) + len(ips)
        else:
            for line in text.splitlines():
                parts = line.split("#", 1)[0].split()
                if len(parts) >= 2:
                    hosts[parts[1].lower().rstrip(".")] = name
                    n += 1
        counts[name] = n
    listed_ips = len(nets)
    text = cached("spamhaus-drop", DROP_URLS)
    for line in text.splitlines():
        cidr = None
        line = line.strip()
        if line.startswith("{"):
            try:
                cidr = json.loads(line).get("cidr")
            except ValueError:
                pass
        elif line and not line.startswith(";"):
            cidr = line.split(";", 1)[0].strip()
        if cidr:
            try:
                nets.append(ipaddress.ip_network(cidr, strict=False))
            except ValueError:
                pass
    counts["spamhaus-drop"] = len(nets) - listed_ips
    denied_hosts, drop_nets = hosts, nets
    log({"ts": time.time(), "event": "lists", "counts": counts})
    print("lists loaded:", counts, flush=True)


async def refresh_lists():
    while True:
        await asyncio.sleep(24 * 3600)
        await asyncio.get_running_loop().run_in_executor(None, load_lists)


def is_ip(host):
    try:
        ipaddress.ip_address(host.strip("[]"))
        return True
    except ValueError:
        return False


def bad_address(addr):
    ip = ipaddress.ip_address(addr)
    if str(ip) in METADATA:
        return "metadata address"
    if ip.is_private or ip.is_loopback or ip.is_link_local or ip.is_multicast or ip.is_reserved or ip.is_unspecified:
        return "private or reserved address"
    if ip.version == 4 and ip in ipaddress.ip_network("100.64.0.0/10"):
        return "carrier-grade NAT address"
    for net in drop_nets:
        if ip.version == net.version and ip in net:
            return "spamhaus-drop"
    return None


def listed(host):
    """The list naming HOST or a domain above it, or None."""
    labels = host.split(".")
    for i in range(len(labels) - 1):
        name = denied_hosts.get(".".join(labels[i:]))
        if name:
            return name
    return None


async def judge(host, port):
    """(reason, addresses) -- reason None when allowed."""
    host = host.lower().rstrip(".")
    if is_ip(host):
        return "IP-literal host", []
    name = listed(host)
    if name:
        return name, []
    try:
        infos = await asyncio.get_running_loop().getaddrinfo(host, port, type=socket.SOCK_STREAM)
    except OSError:
        return "does not resolve (Quad9 refuses known-malware names)", []
    addrs = sorted({i[4][0] for i in infos})
    for a in addrs:
        why = bad_address(a)
        if why:
            return why, addrs
    return None, addrs


def client_name(headers):
    auth = headers.get("proxy-authorization", "")
    if auth.lower().startswith("basic "):
        try:
            user = base64.b64decode(auth[6:].strip()).decode("utf-8", "replace").split(":", 1)[0]
            return user[:40] or "unknown"
        except ValueError:
            pass
    return "unknown"


async def read_head(reader):
    data = await reader.readuntil(b"\r\n\r\n")
    if len(data) > HEAD_MOST:
        raise ValueError("head too large")
    lines = data.decode("latin-1").split("\r\n")
    method, target, version = lines[0].split(" ", 2)
    headers, raw = {}, []
    for line in lines[1:]:
        if not line:
            continue
        k, _, v = line.partition(":")
        headers[k.strip().lower()] = v.strip()
        raw.append((k.strip(), v.strip()))
    return method.upper(), target, version, headers, raw


async def pipe(src, dst, counter, key):
    try:
        while True:
            chunk = await src.read(65536)
            if not chunk:
                break
            counter[key] += len(chunk)
            dst.write(chunk)
            await dst.drain()
    except (ConnectionError, asyncio.IncompleteReadError):
        pass
    finally:
        try:
            dst.close()
        except Exception:
            pass


async def handle(reader, writer):
    started = time.time()
    entry = {"ts": started, "client": "unknown"}
    try:
        method, target, version, headers, raw = await read_head(reader)
        entry["client"] = client_name(headers)
        entry["method"] = method
        if method == "CONNECT":
            host, _, port = target.rpartition(":")
            host, port = host.strip("[]"), int(port or 443)
            path = None
        else:
            if not target.lower().startswith("http://"):
                writer.write(b"HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\n\r\n")
                return
            rest = target[7:]
            hostport, slash, tail = rest.partition("/")
            host, _, p = hostport.rpartition(":") if ":" in hostport and not hostport.endswith("]") else (hostport, "", "")
            host, port = host.strip("[]"), int(p) if p else 80
            path = "/" + tail if slash else "/"
        entry.update({"host": host.lower(), "port": port})
        if method != "CONNECT" and host.lower() == CHECK_HOST:
            # An instance asking whether its traffic comes through here. Only a
            # request through the proxy can reach this name at all, so a bypass
            # shows as a failure, never as a false green.
            body = json.dumps({"through_proxy": True, "client": entry["client"], "mode": config.get("mode"),
                               "your_recent": recent_for(entry["client"], 10)}, indent=1).encode()
            writer.write(b"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: "
                         + str(len(body)).encode() + b"\r\nConnection: close\r\n\r\n" + body)
            entry["decision"] = "check"
            return
        reason, addrs = await judge(host, port)
        enforce = config.get("mode") == "enforce"
        if reason:
            entry["reason"] = reason
            entry["decision"] = "deny" if enforce else "would-deny"
            if enforce:
                body = f"Refused by the egress proxy: {host} ({reason}).\n".encode()
                writer.write(b"HTTP/1.1 403 Forbidden\r\nContent-Type: text/plain\r\nContent-Length: "
                             + str(len(body)).encode() + b"\r\nConnection: close\r\n\r\n" + body)
                return
        else:
            entry["decision"] = "allow"
        # IPv4 first (the container may have no IPv6 route), each address in turn,
        # always one the rules checked, never a fresh lookup.
        up_r = up_w = None
        for addr in sorted(addrs, key=lambda a: ":" in a) or [host]:
            try:
                up_r, up_w = await asyncio.wait_for(asyncio.open_connection(addr, port), 15)
                break
            except (OSError, asyncio.TimeoutError) as e:
                entry["error"] = f"connect failed: {e}"
        if up_w is None:
            writer.write(b"HTTP/1.1 502 Bad Gateway\r\nContent-Length: 0\r\nConnection: close\r\n\r\n")
            return
        entry.pop("error", None)
        counter = {"up": 0, "down": 0}
        if method == "CONNECT":
            writer.write(b"HTTP/1.1 200 Connection Established\r\n\r\n")
            await writer.drain()
        else:
            # Forward the request with a relative path; drop the proxy's own headers.
            keep = [(k, v) for k, v in raw if k.lower() not in ("proxy-authorization", "proxy-connection")]
            head = f"{method} {path} {version}\r\n" + "".join(f"{k}: {v}\r\n" for k, v in keep) + "\r\n"
            up_w.write(head.encode("latin-1"))
            await up_w.drain()
        await asyncio.gather(pipe(reader, up_w, counter, "up"), pipe(up_r, writer, counter, "down"))
        entry.update(counter)
    except (ValueError, asyncio.IncompleteReadError, asyncio.LimitOverrunError, ConnectionError) as e:
        entry["error"] = str(e)[:200]
    finally:
        entry["seconds"] = round(time.time() - started, 2)
        if "host" in entry or "error" in entry:
            log(entry)
        try:
            writer.close()
        except Exception:
            pass


async def main():
    global config
    with open(sys.argv[1], encoding="utf-8") as f:
        config = json.load(f)
    config.setdefault("data_dir", "/data")
    config.setdefault("mode", "dry-run")
    os.makedirs(config["data_dir"], exist_ok=True)
    await asyncio.get_running_loop().run_in_executor(None, load_lists)
    server = await asyncio.start_server(handle, config.get("address", "0.0.0.0"), config.get("port", 3128))
    print(f"egress proxy on {config.get('port', 3128)}, mode {config['mode']}", flush=True)
    asyncio.create_task(refresh_lists())
    async with server:
        await server.serve_forever()


if __name__ == "__main__":
    asyncio.run(main())
