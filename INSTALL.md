# Installing pAI

pAI is a personal project, not a product. It runs, and three agents have run
on it every day since October 2026. But setting it up still takes a technical
person, and it goes most smoothly with a coding agent (such as Claude Code)
working beside you in this repository. This page lists the steps. Where your
machine differs, the coding agent can read the source and adapt them.

> **Status.** These steps follow how the existing agents are set up. They have
> not yet been run end to end on a clean machine. If a step fails, please open
> an issue with what you ran and what it printed.

## What you need

- **Docker.** Each agent is one container. Linux, macOS and Windows (with
  WSL2 or Git Bash) all work.
- **A model provider key.** System-2 thoughts go to a chat model through any
  OpenAI-compatible endpoint. OpenRouter is the simplest; a local server
  (LM Studio, Ollama) works too.
- **A name for the agent's address, if you want it on your phone.** Sign-in
  uses passkeys, and passkeys need a name, not a bare IP. On one machine,
  `localhost` is enough. For a phone, Tailscale with `tailscale serve` gives
  you an HTTPS name on your own tailnet.
- **About 2 GB of memory per agent.**

## 1. Build the image

```sh
git clone https://github.com/jamiebeach/pAIv3.git
cd pAIv3
docker build -f docker/dev.Dockerfile -t pai-v3-dev:local .
```

The image holds SBCL, Quicklisp (pinned), and SQLite3 Multiple Ciphers, which
encrypts every store the agent keeps.

## 2. Create the agent

Pick a short id (lower case, no spaces), here `nova`. `tools/dev-agent.sh`
manages one agent's container and volume, named in an env file. Make
`private/nova.env` (the `private/` folder is git-ignored):

```sh
PAI_DEV_NAME=pai-nova
PAI_DEV_VOLUME=pai-nova-home
PAI_DEV_DIR=/home/pai/nova
PAI_DEV_PORT=8090
```

Make the container (it copies the source into the volume and does not start
yet), then create the agent's encrypted storage with `pai init`. The storage
key is kept in a key file in the agent's volume:

```sh
export PAI_DEV_ENV=private/nova.env
tools/dev-agent.sh create
tools/dev-agent.sh pai init /home/pai/nova nova --no-keychain --keyfile /home/pai/nova.key
```

`dev-agent.sh pai` runs the `pai` command against the agent's volume. It
needs the agent stopped, so stop it first whenever you use it later
(`docker stop pai-nova`).

`pai init` prints a **storage recovery code** once. Write it down and keep it away
from this machine: without it, a lost key file means a lost agent.

Edit `/home/pai/nova/config.json` in the volume so the agent listens inside
its container, and pick its port:

```json
{"agent_id": "nova", "address": "0.0.0.0", "port": 8090, "created_at": "..."}
```

To reach it from a phone over Tailscale, add the name it is served at:

```json
"sites": [{"rp_id": "your-machine.your-tailnet.ts.net",
           "origin": "https://your-machine.your-tailnet.ts.net:8443"}]
```

and on the host: `tailscale serve --bg --https 8443 http://127.0.0.1:8090`.

## 3. Store the model key, and get a pairing code

With the agent still stopped, store your provider key as a secret bound to its
host (secrets are read from standard input, never from arguments), and get a
one-use pairing code for your first device. The code is good for ten minutes.

```sh
tools/dev-agent.sh pai secret-set /home/pai/nova openrouter openrouter.ai
tools/dev-agent.sh pai pair /home/pai/nova
```

## 4. Start it and sign in

```sh
docker start pai-nova
docker logs -f pai-nova        # "nova is running on 0.0.0.0:8090."
```

Open `http://localhost:8090` (or your Tailscale address), choose **Use a
recovery code**, enter the pairing code, and create a passkey. That device is
now the operator's. You'll be offered recovery codes for sign-in; keep those
offline too.

## 5. Give it a model

In the app under **Settings → Models**, add a profile to
`llm.profiles` and point `llm.routes` at it:

```json
[{"name": "main", "base_url": "https://openrouter.ai/api/v1",
  "model": "<a chat model id>", "secret": "openrouter"}]
```

```json
{"default": [{"profile": "main"}]}
```

Restart the agent (`docker restart pai-nova`) and say hello.

## 6. Give it a persona

An agent starts without a persona. Write one as a JSON file with
`persona_id`, `identity` and `voice` (optionally `instructions` and `tone`),
copy it into the volume, and set it with `pai persona-set <dir> <file>`. Or ask
the agent to help you write its own, which is how ours started.

## Optional

- **Mail and calendar** (read-only Google access): `google.*` settings and a
  Google OAuth client of your own.
- **Backups**: `pai backup-init <dir> <backup-dir>` sets up encrypted daily
  backups, with their own recovery code.
- **More agents**: repeat steps 2 to 6 with another id, volume and port. They
  meet on the crew forum.
- **The avatar**: a separate plugin (`pai-avatar`), installed into an agent's
  `plugins/` folder.

## Updating

Pull, then copy the new source in and restart:

```sh
git pull
PAI_DEV_ENV=private/nova.env tools/dev-agent.sh sync --restart
```

If the agent's health check says a projection needs a rebuild after an update,
`tools/dev-agent.sh rebuild <name>` replays it from the ledger.

## Where to go next

- [README.md](README.md): how an agent works, with diagrams.
- [AGENTS.md](AGENTS.md): the rules every change follows, if you or your
  coding agent want to work on the substrate.
