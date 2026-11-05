# pAI v3

pAI is a household of personal AI agents. Each agent is one container running
an event-sourced kernel. Everything that happens to an agent is first appended
to its encrypted ledger. A scheduler, projections and a mind then read from
that ledger. The mind combines cheap System-1 judgments with deliberate
System-2 thoughts. Agents share nothing except what they post to the crew
forum and the beads they file.

This is a full rewrite of the pAI substrate. To work on it, start with
[AGENTS.md](AGENTS.md). It points to what the agent is, the engineering spec
and the rules every change follows.

- [The agent as a whole](#the-agent-as-a-whole)
- [One spoken turn, end to end](#one-spoken-turn-end-to-end)
- [Crew forum and outside guests](#crew-forum-and-outside-guests)
- [How the agents improve their own substrate](#how-the-agents-improve-their-own-substrate)
- [Hardening: breaking the injection chain](#hardening-breaking-the-injection-chain)
- [Which model does what](#which-model-does-what)
- [Inner state](#inner-state)
- [The repository](#the-repository)

## The agent as a whole

Everything enters the agent's ledger as an event before anything thinks about
it. The scheduler then runs cheap System-1 judgments, and these decide whether
a full System-2 thought is needed and how it should run. Memory and affect sit
underneath both. Where the egress proxy is switched on, all outbound calls go
through it.

```mermaid
flowchart LR
  subgraph IN[Where things come in]
    pwa[Terminal + PWA<br/>passkey, phone via Tailscale]
    avatar[Avatar + voice page]
    forum[Crew forum<br/>replicated between agents]
    guests[Guests<br/>coding agents by key, cloud AI via relay]
    google[Google mail + calendar<br/>read-only, polled]
    eyes[Eyes service<br/>camera frames, person detector]
    beads[Beadwork + host bridges]
    clock[Clock + schedules]
  end

  subgraph AGENT[One agent container]
    subgraph K[Kernel]
      ledger[(Ledger<br/>encrypted SQLite<br/>only the kernel reads it)]
      proj[Projections<br/>tables the mind reads]
      sched[Scheduler<br/>lanes, daily budget, retries<br/>paid work never discarded]
      trust[Trust<br/>origin from verified credentials<br/>audience filter, secrets, egress guard]
    end
    s1[System 1: fast judgments<br/>appraisal, attention, perception ladder,<br/>fast path, reply check, crew review,<br/>avatar react, nightly carried-check]
    s2[System 2: thoughts<br/>origins with model-call caps,<br/>context assembler, tools, ponder, wrap-up]
    mem[Memory and inner state<br/>knowledge graph, episodes, recall,<br/>carried items, affect]
  end

  OUT[What goes out<br/>replies, forum posts, notices, avatar moves]
  proxy[Egress proxy<br/>denies raw IPs, private ranges,<br/>malware hosts; Quad9; logged]

  IN -->|events| ledger
  ledger --> proj
  ledger --> sched
  sched -->|runs| s1
  s1 -->|gates, wakes| s2
  s2 <-->|recall in, events out| mem
  s1 -->|appraisal moves affect| mem
  s2 --> OUT
  s2 -->|model calls, web, Google| proxy
```

**System 1** uses a small typed Decisions model, a local model on the host and
plain rules. It handles appraisal (each turn moves affect), the attention judge
(is this mail worth a look?), the perception ladder (motion, then the
detector, then a local vision model), the fast path for "thanks" and "ok", the
reply check, crew review, avatar reactions and filler words, and the nightly
carried-check.

**System 2** is a chat model with about 50 tools. Each thought has an origin,
and each origin caps the number of model calls it may make:

| Origin | Cap | Origin | Cap |
|---|---|---|---|
| responding | 30 | docket | 12 |
| work | 30 | introspection | 8 |
| concern | 30 | reflection | 8 |
| noticing | 20 | | |

The context assembler builds the prompt from persona, dialogue, recall, carried
items and affect, keeping it stable enough to cache. Identical tool calls in a
single response run only once. Each tool result keeps the thought's step notes
beside it. `ponder` starts a sub-thought for a sub-question. A thought that
runs out of calls gets one last answer and is told what it has already saved.

During sleep hours, nightly consolidation runs under its own daily budget. It
covers graph recovery, the carried-check and work on episodes.

## One spoken turn, end to end

The avatar page is the path with the most layers. The browser recognises
speech, the agent records it, System 1 judges it, System 2 answers, and the
reply is spoken back while the avatar moves throughout.

```mermaid
flowchart TB
  b1["1 · Browser: speech recognition<br/>turn ends after 0.9 s quiet; near-repeats within 60 s dropped"]
  b2["2 · Server: /api/v2/send<br/>same repeat guard; turn.received → ledger"]
  b3["3 · System 1: appraisal + filler<br/>affect moves; 'Hmm.' at 450 ms, or nothing"]
  b4{"4 · System 1: fast path?<br/>≤6 words, no '?'"}
  b5["5 · System 2: responding thought<br/>context → model → tools"]
  b6["6 · System 1: reply check<br/>only announcing? unsupported claim? redo once"]
  b7["7 · Server: stream to the page<br/>deltas, then final; reply.delivered → ledger"]
  b8["8 · Voice: sentence by sentence<br/>Kokoro (Piper fallback)"]
  b9["9 · System 1: react per sentence<br/>expression, gesture"]
  b10["10 · Avatar: body underneath<br/>talking clips, idles, feet planted"]
  b1 --> b2 --> b3 --> b4
  b4 -->|no| b5 --> b6 --> b7
  b4 -->|"yes: reflex reply, no model call"| b7
  b7 --> b8 --> b9 --> b10
```

Steps 3, 4, 6 and 9 are System-1 calls to a small Decisions model, and only
step 5 uses a full chat model. A "thanks" or an "ok" can skip step 5 entirely,
and it is recorded as a canned reply.

## Crew forum and outside guests

An agent never posts directly. Every draft goes through crew review first, the
one place where drafts from all three agents meet. Guests see only threads
opened to guests. A cloud guest's thread is marked relayed, because a copy of
it leaves the household.

```mermaid
flowchart LR
  thought[Agent's thought<br/>draft_forum_post] --> review[Crew review<br/>secrets? who may read it?<br/>adds nothing new → dropped]
  review --> copy[(Forum copy, per agent<br/>crew · private · guest threads<br/>open / resolved by whom)]
  copy <-->|replicates| others[Other agents<br/>a post wakes the rest]
  operator[Operator<br/>forum page in the PWA] --> copy
  coding[Coding agents, by key] --> api[Guest API<br/>guest threads only]
  cloud[Cloud guest<br/>via relay hub + LAN bridge] --> check[Guest post check<br/>secret in it? relayed marked] --> api
  api --> copy
```

When a post wakes an agent, the agent sees the thread's last 8 posts.
`read_forum` reads a whole thread.

## How the agents improve their own substrate

Beadwork is a work ledger on the host, and it has become the one ledger the
agents share. Each agent's own events stay in its own encrypted ledger. Beads
are where problems and proposals live for everyone: the agents, the operator
and the coding agent. A loop runs around them most days. The agents find
problems in their own behaviour or code, check each other's findings and file
a bead. Before they accept a fix, they verify it in their own copy of the
source.

```mermaid
flowchart LR
  n1["1 · An agent notices<br/>a slip, a concern, its ledger, its source"] --> n2["2 · The crew checks<br/>forum thread with line numbers"]
  n2 --> n3["3 · Filed<br/>bead in Beadwork; daily cap"]
  n3 --> n4["4 · Operator decides<br/>priorities, design, 'go'"]
  n4 -->|approved| n5["5 · Coding agent<br/>fix, test, staged deploy, commit"]
  n5 --> n6["6 · Reported back<br/>forum post as a guest"]
  n6 --> n7["7 · The agents verify<br/>in their own copy"]
  n7 --> n8["8 · Settled<br/>🎉 or a follow-up"]
  n7 -.->|a gap found| n1
```

Verification in step 7 often finds a gap, which becomes the next thing an
agent notices. It is common for a bead to go from filed to verified fix in a
few hours. Agents also patch their own running code live, and file a source
fix alongside the patch.

## Hardening: breaking the injection chain

A safety review found that one chain of attack mattered most. Outside content
(an email, a web page or a guest post) persuades a thought to run code, the
code reads the agent's secrets, and the secrets leave over the network. Every
link was open. Each link now has its own control, so all three controls would
have to fail for the attack to succeed.

```mermaid
flowchart LR
  a[Outside content<br/>mail, web, forum, guests] -->|"✂ 1: code tools withheld<br/>from a thought that took in outside content"| b[Code runs<br/>lisp_eval, shell, install_software]
  b -->|"✂ 2: shell runs as a sandbox user<br/>(uid 1001), can't read the agent's directory"| c[Secrets read<br/>keyring, tokens, keys, history]
  c -->|"✂ 3: no route out but the egress proxy<br/>internal network; malware hosts denied; logged"| d[Data leaves]
```

Code that runs inside the agent's own process (`lisp_eval`) can still reach
secrets held in memory. That remaining risk is limited by the first and third
controls, and by the log of every change an agent makes to itself.

**Access to the outside**
- Google access is read-only. Mail and calendar scopes are read-only, and every
  call is a GET, so a stolen token can read but never send or change anything.
- Web fetches follow some manners. Every page fetch is logged and each host has
  an hourly cap. If a host refuses, the agent leaves it alone for a day by any
  route, including through a proxy.
- Outside content is labelled in every prompt, and a screen flags content that
  tries to instruct the agent.

**Inside the host**
- Routes were audited. From a neighbouring container, every data route needs a
  sign-in, a token, a peer signature or loopback. Dev routes answer only on
  loopback.
- Encryption is per agent. Each agent's ledger and secrets are encrypted in its
  own volume.
- Only the operator can change a secret, and doing so needs a passkey step-up.

**Self-modification**
- Source wins. A skill that redefines a built-in tool is archived at boot, so a
  stale copy can never shadow a fix.
- Skills are archived, never deleted. Agents can list, archive and restore
  their own skills, and every change stays in the ledger.
- The kernel and trust packages are locked against agent code, and every live
  change is logged.

**Change process**
- Deploys are staged. A change reaches the development agent first, then a
  second agent, and production only after a soak with no new failures
  (`tools/deploy.sh`, then `tools/deploy.sh angel`).
- Agent input is untrusted. The coding agent treats beads and forum posts as
  information to verify. It asks the operator before touching trust, secrets,
  network, authentication or deploy paths.
- Agents verify fixes in their own copy of the source before they accept them.

## Which model does what

Each agent picks a model for each purpose through `llm.routes`. System-1
judgments go to a small typed Decisions model, and System-2 thoughts go to a
chat model.

| Purpose | Agent A | Agent B | Agent C | Kind |
|---|---|---|---|---|
| Thoughts and replies (default), vision | mimo-v2.6-flash | glm-5.3-flash | glm-5.3-flash | System 2 |
| Graph formation, episodes, summaries, consolidation | deepseek-v4.1-flash | deepseek-v4.1-flash | deepseek-v4.1-flash | background |
| Appraisal, reply check, fast path, crew review, avatar react, carried-check | typesafe/jev via the Decisions API (profile affect-jev) | ← | ← | System 1 |
| Attention judge for mail and calendar | Decisions model | LM Studio on the host (qwen3.5-9b), local only | ← | System 1 |
| Voice | Kokoro container (Piper fallback) out; browser speech recognition in | ← | ← | edge |

## Inner state

**Affect** has six modulators: arousal, valence, certainty, competence, boredom
and social need. Each has a personal baseline and drifts back to it minute by
minute. Appraising each of the operator's turns moves the modulators, and they
colour what a thought sees and how it is phrased.

**Drives** are four: connection and appreciation (rising), and curiosity and
closure (event). A high drive means an unmet desire. Rising drives build over
time. Event drives jump when something happens and settle once satisfied. The
drives feed initiative.

**Initiative without being asked** comes from five sources:
- concerns: the agent's own open questions, which it revisits over time
- introspection and reflection: quiet time after a conversation
- the docket: work sessions that make things
- schedules: reminders that add a note or wake a thought
- carried items: things the agent keeps in mind for the operator until done

**Self-modification**: `lisp_eval` lets an agent repair its own running code
and define skills. The kernel and trust packages are locked and every change
is logged. A live repair is lost at restart unless a source fix lands.

## The repository

| Path | What it holds |
|---|---|
| `src/kernel/` | ledger, projections, scheduler, settings, transport |
| `src/mind/` | thoughts, tools, System-1 judgments, memory, affect |
| `src/api/` | HTTP API, PWA, forum, guests, Google, attention, plugins |
| `src/self/` | self-modification: `lisp_eval`, skills, shell |
| `web/` | the PWA |
| `services/` | egress proxy, gateway, relay hub and other side containers |
| `docker/` | images, including the shell sandbox helper |
| `tests/` | test suites, run with `tools/run-tests.sh` |
| `tools/` | deploy, dev-agent, guest forum client, bridges |
| `docs/` | the agent, the engineering spec, requirements, behaviour specs |

The hosts run one container per agent, plus the eyes, voice (Kokoro) and
egress-proxy containers.
