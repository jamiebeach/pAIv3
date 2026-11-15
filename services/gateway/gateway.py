"""The gateway for agents on the internal network (bead v3-r2j.1).

An agent moved to the Docker network pai-internal has no route out. This
container sits on that network and on the ordinary bridge, answers to the
name host.docker.internal on the internal network, and forwards each
configured port: an agent's own port to the agent (so the host's published
port, Tailscale and the other agents still reach it), and every other port to
the real host (the other agents, the bridges, LM Studio, the voice and eyes
services, and the egress proxy, which is the only way to the internet).

    python gateway.py config.json
    config: {"routes": {"8090": "pai-agent:8090", "8765": "host-gateway:8765", ...}}
"""
import asyncio
import json
import sys


async def pipe(src, dst):
    try:
        while True:
            chunk = await src.read(65536)
            if not chunk:
                break
            dst.write(chunk)
            await dst.drain()
    except (ConnectionError, asyncio.IncompleteReadError):
        pass
    finally:
        try:
            dst.close()
        except Exception:
            pass


def forwarder(target):
    host, _, port = target.rpartition(":")

    async def handle(reader, writer):
        try:
            up_r, up_w = await asyncio.wait_for(asyncio.open_connection(host, int(port)), 10)
        except (OSError, asyncio.TimeoutError) as e:
            print(f"gateway: {target} unreachable: {e}", file=sys.stderr, flush=True)
            writer.close()
            return
        await asyncio.gather(pipe(reader, up_w), pipe(up_r, writer))
    return handle


async def main():
    with open(sys.argv[1], encoding="utf-8") as f:
        routes = json.load(f)["routes"]
    servers = [await asyncio.start_server(forwarder(target), "0.0.0.0", int(port)) for port, target in routes.items()]
    print("gateway routes:", routes, flush=True)
    await asyncio.gather(*(s.serve_forever() for s in servers))


if __name__ == "__main__":
    asyncio.run(main())
