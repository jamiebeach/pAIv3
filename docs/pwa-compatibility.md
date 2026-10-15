# Keeping the v2 PWA on v3

Operator decision (2026-10-12): v3 serves the existing PWA instead of a new
one. The terminal and observability pages are used heavily and must keep
working. This is the inventory of what the pages call and how v3 answers
each route. Bead v3-clp.5.

Source: v2 `src/adapters/web/assets` (about 2,000 lines: terminal, login,
observability, graph explorer, affect, notifications, settings, shell,
service worker) and the handlers in `src/adapters/web` and
`src/mind/observability/dashboard.lisp`.

## Principles

- The assets are copied into v3 unchanged where possible. Where a route has
  no v3 meaning, the page changes minimally rather than v3 imitating v2
  internals.
- Every route reads projections, never the ledger (spec §11). Pages that
  listed raw events get a bounded projection made for them.
- All routes sit behind v3 sessions (passkeys). The `X-PAI-Request:
  same-origin` header the pages already send is kept as the CSRF guard.
- The compatibility routes live in one file (`src/api/pwa.lisp`) over the
  same functions as `/v1`, so the versioned API stays the contract (EXT-1).

## Routes

Plan column: **serve** (v3 answers in the v2 shape), **adapt** (page
changes), **later** (belongs to another bead; page shows it as unavailable
until then).

### Shell and static

| Route | Plan |
| --- | --- |
| `/`, `/terminal`, `/dashboard`, `/graph`, `/g`, `/affect`, `/notifications`, `/settings`, `/login` | serve the HTML |
| `/app-shell.js`, `/viewport.js`, `/service-worker.js`, page scripts, `/manifest.webmanifest`, `/pwa-icon.svg` | serve; manifest name per instance |

### Sign-in

| Route | v2 | Plan |
| --- | --- | --- |
| `/api/v2/login` | username and password | **adapt**: login page uses the v3 passkey ceremony (`/v1/auth/...`), recovery-code entry, and device enrolment. Passwords are not carried over. |

### Terminal

| Route | v2 | v3 source | Plan |
| --- | --- | --- | --- |
| `/api/v2/history?before&limit` | presentation ring: `{events, has_more, turn_in_flight}` | `working_dialogue`, thought rows | serve |
| `/api/v2/stream?since` | SSE, one JSON event per message, `id` for resume | v3 SSE broadcast | serve, translating event kinds |
| `/api/v2/send` | `{message, images}`; leading `/` is a command | `receive-turn` + responding thought | serve; images refused as in v2 |
| `/api/v2/files`, `/file`, `/upload`, `/download` | workspace file browser | none yet | later (host bridge, v3-clp.11) |

Terminal event kinds and their v3 origin:

| Kind | From |
| --- | --- |
| `user` | `turn.received` |
| `final` | `reply.delivered` |
| `notice` | `notice.posted` (reminders included) |
| `thinking`, `accepted`, `queued` | thought requested / work started |
| `stream-progress` | reply deltas and provider heartbeat |
| `tool` | `thought.step` tool calls |
| `error` | `thought.failed`, `provider.call.failed` on an operator thought |
| `command` | slash-command output |
| `operational`, `ambient` | runtime alarms (v3-fu8.8) and senses (v3-clp.9); empty until then |

Slash commands: v2's set is replaced by a small v3 set over the same
functions as the agent's tools (status, settings, inspect, schedule,
budget). Unknown commands answer with the list.

### Observability

| Route | v2 fields used by the page | Plan |
| --- | --- | --- |
| `/api/dashboard/observability/live` | `as_of`, `attention`, `work_docket`, `runtime_parameters`, `peer_inbox` | serve: docket from `v3_work`, attention from thoughts in flight, parameters from settings and affect, peer inbox empty until v3-clp.12 |
| `/api/dashboard/observability/history?hours&type` | `activity.buckets` (per-hour counts by series), `events` (recent, filterable by type) | serve from a new bounded `observed_activity` projection: hourly counters and a capped recent-events table, retention by setting |
| `/api/dashboard/model-context` | last assembled context | serve from `context.assembled` (a small projection of the latest few manifests and digests) |

### Other pages

| Route | Plan |
| --- | --- |
| `/api/settings/runtime` | **adapt**: settings page lists and changes v3 settings (`/v1/settings`), with step-up |
| `/api/relationship/affect/reset`, `/tone`, `/policy`, `/pause`, `/resume` | affect from the v3 affect projection (serve); tone, policy, pause and resume with the persona and initiative work (v3-clp.7, v3-okh.1) |
| `/api/v2/graph/search` | later (v3-clp.6) |
| `/api/notifications/*` (policy, pending, state, test, push key and subscriptions, telegram) | later (v3-clp.10); web push keys become secrets |
| `/board-view`, `/board/*` | later (v3-clp.12 peer messaging) |

## Order of work

1. Static assets, sessions and the passkey login page.
2. Terminal: history, stream, send, event translation, slash commands.
3. Observability: live and history with the `observed_activity`
   projection; model context.
4. Settings and affect pages.
5. The rest follow their modules.

Each step is tested against the real page scripts (a headless browser run
of the terminal and observability pages) before the next.
