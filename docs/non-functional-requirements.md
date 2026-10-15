# Non-functional requirements

Qualities v3 must have, independent of features. Each requirement has an id
used by tests and phase gates. Companion to the
[agent doc](the-continued-experience-agent.md) and the
[engineering spec](engineering-spec.md).

## 1. Access and authentication

The agent's mind is only as safe as the boundary around it. Every way in is
a principal with a verified credential, a scope, and an audit trail.

| Id | Requirement |
| --- | --- |
| AUTH-1 | **Operator authentication uses phishing-resistant credentials.** Passkeys (WebAuthn) per enrolled device are the primary factor; password+TOTP only as recovery. Each device is registered by name, visible in the UI, and revocable. |
| AUTH-2 | **Sessions are short and bound.** Session tokens are device-bound, expire (default 30 days idle, 90 days absolute), rotate on use, and are revoked on device removal. No long-lived shared credentials. |
| AUTH-3 | **Step-up for sensitive actions** (spec §16.5): finance/health disclosure, configuration, grants, approving self-modification, export. Re-verification on an enrolled device; result recorded as an origin attestation. |
| AUTH-4 | **Peers authenticate with per-agent asymmetric keys** (Ed25519 signatures over canonical envelope + timestamp, freshness window, replay cache). Keys rotate; the operator approves fleet membership. Peer authentication never grants operator authority. |
| AUTH-5 | **Machine principals are separate identities**: host bridge, receptors' external accounts (mail, calendar, smart home), coding-agent adapters. Each has its own credential, least-privilege scope, and rate limit. |
| AUTH-6 | **Authority is derived, never claimed.** Origin comes only from the adapter that verified the credential (spec §16.5). Content, headers, event type or remembered identity never establish a principal. |
| AUTH-7 | **Least privilege per capability.** Every tool, receptor and effector declares the grants it needs; the operator grants them; the kernel checks grants at admission and again before effect. |
| AUTH-8 | **Recovery is planned.** Lost-device recovery path for the operator (recovery codes held offline), documented and tested, that cannot be triggered by the agent or a peer. |
| AUTH-9 | **Everything auditable.** Logins, step-ups, grants, revocations, host operations and disclosures to non-operator audiences are events the operator can review in the UI. |
| AUTH-10 | **Network exposure is minimal.** Instances are reachable only over the private network (tailnet) by default; any public endpoint (push, telephony webhook) is a separate, narrowly scoped ingress with its own verification. |

## 2. Encryption and secrets

| Id | Requirement |
| --- | --- |
| ENC-1 | **At rest:** every store (ledger, derived stores, receptor scratch, blobs) is an application-encrypted SQLite database (SQLite3 Multiple Ciphers, ChaCha20-Poly1305 pages, WAL and journal included). No dependence on OS volume encryption, FUSE or a container (operator decision 2026-10-12, spec §17; supersedes the v2 `pai-4v6` volume default for v3). Plaintext never touches disk outside a store. |
| ENC-2 | **In transit:** all traffic is TLS (tailnet or verified HTTPS). Connectors refuse unverified TLS and credential-bearing redirects (v2 `pai-4v6.7`). |
| ENC-3 | **Backups** are encrypted and authenticated, restore-tested on a schedule, and keyed separately from the live volume. |
| ENC-4 | **Secrets** (provider keys, OAuth tokens, peer keys, VAPID keys) live in a dedicated encrypted secret store, in process (operator decision 2026-10-12), outside the ledger, derived stores and source tree; never logged, never placed in model context. Each secret is bound to the hosts it may be sent to; the single outbound transport blocks any request carrying a known secret value to any other host, and redacts patterns (v2 `pai-o6z`). |
| ENC-8 | **Keys and unlock.** Each store has a random data key wrapped by one or more unlock methods: OS keychain (default, unattended restart), passphrase (scrypt), and a recovery key shown once at setup. Changing unlock methods rewraps only. A missing or wrong key stops startup; it never creates an empty store. |
| PORT-1 | **OS agnostic, no container required.** v3 ships as a standalone executable per OS (Windows, macOS, Linux) with its cipher library. Containers and sandboxes are optional host profiles that add isolation; no security property depends on them. Setup is one command (`pai init`). |
| ENC-5 | **Raw sensor data** (audio, video, images) is ephemeral by default, held only in the receptor's encrypted scratch for its retention window, and never sent to a remote model unless the operator enables it per sense. |
| ENC-6 | **Key rotation** for every key class has a documented, tested procedure that does not require ledger rewrite. |
| ENC-7 | **Crypto agility:** algorithms and key ids are recorded in envelopes and attestations so they can change without breaking history. |

## 3. Budgets

The agent lives within explicit budgets. Every budget is visible in the UI,
felt through interoception, and enforced by the scheduler (spec §4.5).

| Id | Budget | Default (tunable) |
| --- | --- | --- |
| BUD-1 | **Money: daily paid spend** per agent, paced across the UTC day | Operator-set; ≥ 20% reserved for operator turns |
| BUD-2 | **Money: per thought** cap; a thought that reaches it pauses with its work kept | Per origin class (responding > work > concern > private) |
| BUD-3 | **Money: model-derived rebuilds** (KG generation change, re-embedding) are separately approved projects with their own budget, never drawn from daily spend silently | Operator-approved |
| BUD-4 | **Memory** (spec §15) | RSS < 512 MB, heap ≤ 1 GB, flat with history |
| BUD-5 | **CPU**: background work yields to operator turns; steady-state idle | < 5% of one core idle; operator turn never waits on background CPU |
| BUD-6 | **Storage growth**, after retention and consolidation | < 50 MB/day per agent steady state; alarm at 2× |
| BUD-7 | **Latency** (operator turn) | Context ready p95 < 150 ms; first token per provider; UI acknowledges in < 200 ms |
| BUD-8 | **Attention**: interruptions of the operator | Max unprompted notifications per hour/day, quiet hours, urgency exceptions only |
| BUD-9 | **Provider rate limits and wall clocks** | Every provider call bounded in time; Retry-After honoured |
| BUD-10 | **Local compute** for perception (v3.2): GPU/CPU share per sense | Declared per receptor; backpressure lowers quality before dropping |

Budget refusals record the reason and next eligible time, once per task
(spec §14), and never discard work already done (decision §13.3).

## 4. Reliability and recovery

| Id | Requirement |
| --- | --- |
| REL-1 | **Crash safety:** a kill at any moment loses no committed experience and leaves projections consistent (I4); a thought in progress resumes or is marked interrupted with its partial work kept. |
| REL-2 | **Startup** < 30 s at 1M events, no history reads (spec §14). |
| REL-3 | **Recovery point:** continuous WAL on encrypted storage plus encrypted backups at least daily; RPO ≤ 24 h for full-volume loss, 0 for process crash. |
| REL-4 | **Recovery time:** restore from backup to a running agent in < 30 min, rehearsed quarterly. |
| REL-5 | **Degradation over failure:** a missing provider, index, receptor or peer degrades only that capability, visibly (decision §13.3, I7). |
| REL-6 | **Automatic start** with the container and host; one runtime per state volume (lock). |
| REL-7 | **Upgrades:** code updates apply migrations per I10, can be rolled back, and never require a live replay. |

## 5. Privacy

| Id | Requirement |
| --- | --- |
| PRIV-1 | Sensitivity on every row; disclosure filtered before context (spec §16.5). |
| PRIV-2 | Third parties' information (people in mail, at the door, in chats) is owned by the operator's household policy, minimised, and never shared beyond its sensitivity. |
| PRIV-3 | Remote model providers receive only what the current thought needs; per-provider data policy recorded; operator can mark senses or topics local-only. |
| PRIV-4 | Operator can export all of an agent's data and can request deletion of specific topics; deletion is an event, honoured by projections and consolidation, with backups aging out on schedule. |
| PRIV-5 | No real identities, private state or captured conversations in the source tree (existing rule). |

## 6. Observability and operability

| Id | Requirement |
| --- | --- |
| OBS-1 | Everything the operator can change is changeable from the phone UI (existing rule). |
| OBS-2 | Health page per agent: projections and cursors, quarantine, degraded entities, budgets, memory, receptors, peers, live changes, proposals. |
| OBS-3 | Every thought has a content-free trace: origin, timings per step, cost, outcome, failure class. |
| OBS-4 | Alarms (push to operator) for: budget exhaustion, projection needs-rebuild, memory over budget, repeated failure of one capability, authentication anomalies. |

## 7. Maintainability

| Id | Requirement |
| --- | --- |
| MNT-1 | Runtime source < 60k lines; files ≤ 1,200 lines; modules ≤ ~8k (spec §8). |
| MNT-2 | Every module testable in isolation on synthetic ledgers; full suite runs in fresh processes. |
| MNT-3 | Each projection, receptor, effector and channel is declared, so the system can list its own composition. |
| MNT-4 | Behaviour specs for each module live beside it and are the source of truth for tests. |

## 8. Extensibility: channels, voice, native app, phone

Not built in v3.0, but v3.0 must not prevent them. The approach is to make
interaction symmetric: the agent **perceives** through senses and receptors
(spec §16.1) and **expresses** through **effectors**, and a **channel** is a
pairing of the two on some surface.

### 8.1 Concepts

- **Effector**: a way the agent expresses: text message, notification,
  speech (text-to-speech in the agent's voice), on-screen card, phone call,
  device action. Declared like receptors, with grants and sensitivity limits.
- **Channel**: a surface where the operator or others interact with the
  agent: web PWA, native app, Telegram, voice in the room, phone call, car.
  A channel = receptors (input) + effectors (output) + an authentication
  method + presentation capabilities. Channels are plug-ins.
- **Conversation is channel-neutral.** One relationship, many surfaces;
  the agent may move a conversation between channels ("I'll call you").

### 8.2 Requirements that keep the doors open (all v3.0)

| Id | Requirement | Enables |
| --- | --- | --- |
| EXT-1 | **Versioned client API.** All clients, including the web PWA, use one documented, versioned API: authenticated HTTP for commands and a streaming connection (SSE or WebSocket) for events. The PWA has no private back door. | Native iOS/Android app, desktop app, car |
| EXT-2 | **Streaming output.** Replies stream as typed parts (text deltas, tool status, final); the turn model supports cancellation mid-stream. | Voice (speak while thinking), responsive apps |
| EXT-3 | **Interruption and barge-in.** A new operator input can interrupt a reply in progress; the interrupted reply is kept as experience with what was delivered. | Natural voice conversation |
| EXT-4 | **Multimodal message parts.** Events and context support parts: text, image ref, audio ref, file ref, with sensitivity per part; models without a modality get a text rendering. | Photos from phone, voice notes, screen sharing |
| EXT-5 | **Latency classes for turns.** `interactive` (voice: first audio < 800 ms target), `chat`, `background`. The scheduler and context profiles honour them; a voice turn can use a smaller fast profile and continue deeper in the background. | Real-time voice, phone |
| EXT-6 | **Device registry.** Enrolled devices with capabilities (push, mic, speaker, camera, location) and channel bindings; the same registry serves AUTH-1. | Native app, wearables, home speakers |
| EXT-7 | **Channel-scoped origin and audience.** Each channel declares how it authenticates and who can be present (a room speaker may have others listening: audience includes "unknown nearby"). | Voice in shared spaces, phone calls with third parties |
| EXT-8 | **The agent's voice is part of identity.** If and when speech is added, voice settings are persona rows with lineage, carried across models and providers. | Voice that stays them |
| EXT-9 | **Telephony is a channel adapter**, not core: inbound/outbound calls and SMS through a provider, verified webhooks (AUTH-10), caller identity as weak evidence requiring step-up for sensitive matters. | Phone integration |
| EXT-10 | **Effector and receptor plug-in contract** is stable and versioned; adding one needs no kernel change. | Smart home control, wearables, vehicles |

### 8.3 Voice specifically

Voice needs hearing (speech-to-text, speaker identification) and a speech
effector. Speaker identification is **evidence, not authentication**: it may
raise confidence that the operator is present, but sensitive matters by
voice still need step-up on an enrolled device unless the operator enrols a
stronger voice factor. Audience is computed from who is detected nearby; if
unknown people may be listening, disclosure falls to the most restrictive
class present.

## 9. Gates

| Phase (spec §10) | NFRs that must pass |
| --- | --- |
| 1 Kernel | REL-1, REL-2 (synthetic), BUD-4 (synthetic), MNT-1..3 |
| 3 Core modules | AUTH-1..3, AUTH-6..7, EXT-1..5, BUD-7 |
| 4 Remaining modules | AUTH-4..5, AUTH-9, ENC-1..4, PRIV-1..3, BUD-1..3, BUD-8 |
| 5 Dev cutover | All above on the real instance; REL-3..6; OBS-1..4; ENC-6 rehearsed |
| 6 Prod cutover | All above; REL-4 rehearsal; AUTH-8 tested |
