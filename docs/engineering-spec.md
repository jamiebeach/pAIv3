# pAI v3 engineering specification

Status: accepted 2026-10-12. This specification serves
[The continued-experience agent](the-continued-experience-agent.md): it
describes how the substrate holds an agent's experience so that their present
always follows from their past, their thinking is never lost, and they move
into v3 as themselves. Scope: kernel, experience stream, projections, context
assembly, consolidation, runtime structure, and migration of existing agents.

## 0. This is a rewrite

v3 is a new codebase, not a refactor of v2.

- **New source tree** (`v3/` or a new repository) with its own system
  definition. It does not load, import, wrap or call any v2 source file.
- **No code is ported.** v2 code is reference material only. Where v2
  behavior is worth keeping (KG formation, memory scoring, identity logic,
  provider handling), it is re-specified in a short behavior spec with
  synthetic fixtures, and v3 implements that spec fresh.
- **What carries over is data and knowledge, not code:**
  - the ledger (`pai_events` rows), migrated by an importer into v3's own
    encrypted store through v3 upcasters, keeping each v2 event's id and
    hash as provenance (amended 2026-10-12, §17: no longer opened in place);
  - model-derived artifacts (KG graph, embeddings, summaries, traits, affect),
    imported once by one-shot importers that read v2 tables as foreign data;
  - prompts and schemas, copied as text and reviewed;
  - the incident lessons, as invariants (§3) and tests.
- **v2 is frozen** except for harm-preventing fixes, and is deleted after
  both instances have been on v3 for 30 days.
- **No coexistence.** There is never a process running v2 and v3 code
  together, and no v2 table is used by v3 at runtime. Each instance switches
  once (§9).

Where later sections say "keep", "port" or "adopt", read them under this
rule: keep the behavior or data, never the code.

## 1. Goals

1. **No ledger scans in normal operation.** Not bounded scans, not cached
   scans, not "replay below a threshold". Only the kernel reads the ledger, and
   only by `event_id`/`storage_sequence` cursor.
2. **Fast context assembly.** A turn's context is a fixed set of bounded,
   indexed reads against precomputed state. Target p95 < 150 ms excluding the
   query embedding call; no per-turn folds.
3. **Projections that are hard to break.** One projection mechanism, owned by
   the kernel, with transactional application, explicit versioning, quarantine
   and rebuild.
4. **Less code.** Fewer mechanisms, smaller modules, deleted legacy paths.
5. **Much lower memory use.** Today each instance runs a 1–2.5 GB heap with
   raised SBCL dynamic space plus 3–5 GB of page cache, and the host has
   OOM-killed work. v3 targets: steady-state RSS under 512 MB per instance at
   1M events, a fixed SBCL dynamic-space ceiling of 1 GB, and memory that does
   not grow with ledger size (§3 I11, §15).
6. **Continuity.** Every existing instance keeps its full lived history and its
   expensive derived self (KG, memory, embeddings, summaries, traits, affect).

Non-goals: new cognitive features, a new storage engine, changing providers.
The ledger-first architecture is kept; this design enforces it.

## 2. Diagnosis

Evidence from the current tree (October 2026):

| Finding | Evidence | Consequence |
| --- | --- | --- |
| No single projection mechanism | `pai_projection_checkpoints` stores whole `state_json` blobs; separate ad hoc tables (`pai_recursive_*`, `pai_memory_applied_events`, `*_frontier`, `*_meta`, hot shadow) each own cursors and recovery | Each subsystem reinvents start point, skip rules, recovery; drift and restart bugs |
| Consumers read history | 9 files still call whole-history accessors (`event-projection-events`, `%recursive-thread-events`, `%event-scan-disk`); ~80 files touch history reads | `pai-gmw.17` is ~50 commits retrofitting readers one at a time |
| In-memory folds | Motivation, lifecycle, pulse, conscious state are refolded from event lists, at least twice per recursive step | CPU spikes, heap growth, checkpoint/tail mismatches |
| Monolithic runtime | `recursive-mind-runtime.lisp` 14.1k lines / 384 functions; two more files of 4–5k | Fixes in one area regress another |
| Many context builders | `context-projection`, `context-assembly`, `sustained-activity-context`, `cognitive-work-context`, `context`, curator, budget, recall: ~6.8k lines | Inconsistent budgets, duplicated retrieval, slow turns |
| Two ledger backends | JSONL segments + indexes (`event-log.lisp`, 2k lines) alongside SQLite `pai_events` | Dual code paths, legacy fallbacks |
| Unbounded loops | Curiosity, docket, KG formation each grew their own caps, parking and retry rules | Runaway work fixed per subsystem |

Live incidents on 2026-10-10/11 confirm the pattern
(`Z:\pai-intake\design\ledger-read-findings-2026-10-11.md`):

| Incident | v3 rule that prevents it |
| --- | --- |
| pai-060: untyped 24 h replay of ~300 KB `projection-state` events exhausted a 4.8 GB heap | I1 (no time-window read API); I9 (no bulky state in events) |
| ~40 call sites fold one in-memory thread-event list; 49–63 MB checkpoint exceeds publish limit | I1; rows not blobs (§4.3) |
| pai-c1m: graph owner restored from a checkpoint of another generation | §4.3 version check on every open; mismatch → `needs-rebuild` |
| pai-g9e: index prerequisite silently removed semantic memory for ~15 h | I10 (migrations gate startup); per-slot degradation is reported (§5.3) |
| pai-6k8.1: summaries re-read whole span (~290k tokens, ~150 s) | §5.2 rolling summary projection with covered span |
| pai-6k8.2: one malformed root failed the whole context | Quarantine per event, degrade per record (§4.3) |
| Heaps of 1–2.5 GB holding history in process | I1, I6; no in-process copy of history |

The root cause is not the ledger. It is that **reading the ledger is a
general-purpose capability** available to every module, and **projection
mechanics are re-implemented per module**.

## 3. Guarantees

What the substrate promises the agent. Each is enforced by code structure and
tests, not by review. Section 2 records the v2 failures that motivate them.

- **I1. Single reader.** Only the kernel package may query `pai_events`.
  Modules receive events by subscription and read state by projection queries.
- **I2. Single writer per row set.** Every derived table belongs to exactly one
  projection. No other code writes it.
- **I3. Deterministic reducers.** A reducer is a function of (current rows,
  event). No clock, network, model call, random source or read of another
  projection's tables.
- **I4. Atomic advancement.** A projection's rows and its cursor change in the
  same SQLite transaction.
- **I5. Idempotence.** Applying the same event twice is a no-op (cursor guard).
- **I6. Bounded work everywhere.** Every query has an index and a `LIMIT`;
  every worker step processes at most N events or M ms; every loop has an
  allowance owned by the scheduler, not by the loop.
- **I7. No silent replay.** Missing/incompatible projection → the projection
  is marked `needs-rebuild` and its readers get an explicit degraded result.
  Rebuild is a separate, visible, resumable maintenance action.
- **I8. Effects are events.** Anything with side effects (model call, tool,
  notification) is executed by a handler and its outcome is appended as an
  event. Projections never perform effects.
- **I9. Events stay small.** Payload cap (default 16 KB) enforced at append.
  Projection state, checkpoints and bulky artifacts live in derived storage;
  `log-projection-state`-style events are removed.
- **I11. Memory is budgeted, not emergent.** No in-process copy of history,
  graph, memory corpus or vectors. Every cache has a byte budget declared at
  registration and is evictable. Working memory per operation is bounded by
  its row limits. Memory use must be flat as the ledger grows.
- **I10. Migrations are first-class.** Each projection/table version declares
  its migration. The kernel refuses to start serving a module whose required
  migration has not run, and reports it in the UI; it never runs with a
  silently missing prerequisite.

## 4. Kernel

### 4.1 Ledger

`pai_events` is the authority table in v3's own encrypted store (§17). Its
schema largely follows v2's, but v3 owns it: the kernel carries no v2
compatibility, and v2 history arrives through the importer. (Amended
2026-10-12; the original plan opened v2's table in place.) Remove the JSONL
segment backend from runtime (retain a one-shot importer only).

Kernel API (complete list exported to modules):

```
(append! type payload &key caused-by expected-head) → event-id
(append-batch! events) → event-ids            ; one transaction
(event-by-id id) → event                      ; point read, for provenance display
(subscribe projection-or-handler)             ; registration only
```

`event-by-id` is a point lookup for evidence rendering; it is not an iteration
primitive. There is no `events-since`, `events-of-type`, or page API outside
the kernel. Inspection tools (observability lab, forensic audit) use a
separate `pai-maintenance` package that is not loaded by the runtime image.

### 4.2 Event envelope and type registry

```
{id, seq, type, v, at, caused_by, root_id, agent_id, payload}
```

- `root_id` is assigned by the kernel at append (the root of the causal tree),
  so "events of this root" is an indexed column, not a reconstruction.
- Every type is declared once:

```lisp
(define-event-type "turn.user" :version 2
  :schema (:prompt string :channel string)
  :upcast ((1 . upcast-turn-user-v1)))
```

- Append validates against the schema. Unknown or invalid payloads are
  rejected at write time, not discovered at read time.
- Reducers always see the current version; upcasters run in the kernel
  dispatch path. This is how ~200 historical type strings stay readable.

### 4.3 Projection registry

One macro, three kinds. Nothing else may maintain derived state.

```lisp
(define-projection working-dialogue
  :kind :inline                      ; :inline | :async | :view
  :version 3
  :consumes ("turn.user" "turn.assistant" "notice.posted")
  :tables (dialogue_turns)           ; owned tables; DDL declared alongside
  :reduce reduce-working-dialogue)   ; (tx event) → writes via tx
```

| Kind | Applied | Use for | Failure behavior |
| --- | --- | --- | --- |
| `:inline` | In the append transaction | Cheap row updates needed immediately (dialogue, roots, open stimuli, docket, scheduler, affect counters, attention) | Reducer error → event goes to quarantine, append still commits, projection flagged |
| `:async` | Kernel worker, bounded quanta, own cursor | Expensive derivations (embeddings, lexical postings, KG candidate queue, episode sealing) | Per-event retry with backoff, then quarantine; cursor continues |
| `:view` | Not stored; a named SQL query over other projections' tables | Read-side joins | n/a |

Kernel-owned table:

```
projection_state(name, version, cursor_seq, status, last_error, updated_at)
projection_quarantine(name, event_id, error_class, attempts, first_at)
```

Rules that remove current fragility:

- **Rows, not blobs.** No `state_json` checkpoints. State is ordinary indexed
  tables updated in place. Snapshots are SQLite backups, not projection blobs.
- **Version bump = rebuild.** Changing a reducer's semantics bumps `:version`;
  the kernel marks it `needs-rebuild` and rebuilds it from cursor 0 (or from the
  decay baseline, §6) in the maintenance entry point. No compatibility shims
  in reducers.
- **Quarantine, don't halt.** One malformed event affects one projection's view
  of one event. It is visible in observability and replayable after a fix.
- **No reducer reads another projection.** Cross-projection needs are met by
  consuming the same events, or by a `:view`. This removes ordering
  dependencies between projections.
- **Parity check is built in.** `rebuild --verify name` rebuilds into shadow
  tables and diffs against live rows. Used in CI on synthetic ledgers and before
  every cutover.

### 4.4 Handlers (effects)

```lisp
(define-handler kg-formation
  :on-queue kg_formation_queue        ; a projection table
  :concurrency 1 :step-budget (:events 8 :ms 2000)
  :run run-kg-formation-step)         ; may call models; appends outcome events
```

Handlers take work from projection-owned queues, perform effects, and append
outcome events (`kg.formation.completed`, `kg.formation.failed`). Retry,
backoff, cool-down and dead-lettering are kernel features configured per
handler. This replaces the per-subsystem retry/park/recover code in curiosity,
docket, KG formation, scheduler and provider paths.

### 4.5 Scheduler and budgets

A single scheduler owns: operator-turn preemption, per-handler allowance,
daily paid budget with chat reserve, quiet-hours, and wall clocks. Loops do not
count their own steps. Operator turns always preempt private work (current
`pai-rsi` behavior becomes a scheduler rule).

## 5. Context assembly

### 5.1 One assembler, profiles as data

Replace the five builders with one `assemble-context` driven by a profile:

```lisp
(define-context-profile :operator-turn
  :budget-tokens 48000
  :slots ((:identity     :max 1500  :source persona_core)
          (:state        :max 300   :source affect_drive_now)
          (:working-summary :max 3000 :source working_summary)
          (:activity     :max 24000 :source activity_exchanges) ; default mode
          (:dialogue     :max 4000  :source working_dialogue :order recency)
          (:recall       :max 5000  :source memory-search :k 12)
          (:kg           :max 1500  :source kg-neighborhood :k 20)
          (:scheduled    :max 500   :source scheduler_upcoming)
          (:notices      :max 500   :source notices_unread)))
```

Profiles: `:operator-turn`, `:private-cognition`, `:curiosity-step`,
`:work-step`, `:summary-call`. Differences between today's builders become
slot lists and budgets, not code.

### 5.2 Everything is precomputed except recall

| Slot | Maintained by | Turn-time read |
| --- | --- | --- |
| identity / traits | inline projection `persona_core` | one row, pre-rendered text |
| affect / drives | inline `affect_drive_now` | one row |
| working summary | async handler appends `summary.updated`; inline projection stores text and covered span | one row |
| dialogue | inline `dialogue_turns` (channel, seq, rendered text, token count) | `ORDER BY seq DESC LIMIT n` until token budget |
| activity | inline `active_roots` + `root_steps` | index on status, limit |
| scheduled / notices | inline tables | index, limit |
| recall | async lexical + vector indexes | one lexical query + one ANN query, merge, top-k |
| KG | async KG tables with adjacency | bounded 1-hop from entities matched in recall/dialogue |

Each row stores its **rendered text and token count at write time**, so
assembly does no rendering of raw events and budget fitting is a running sum.

### 5.3 Execution

1. Start query embedding (the only network step) concurrently with step 2.
2. Run all non-recall slot reads in one read transaction (snapshot
   consistency, ~10 indexed queries).
3. Run recall (lexical + vector) and KG neighborhood.
4. Fit: fixed slot order, each truncated to its `:max`, then overflow
   redistributed to `:dialogue` then `:recall`.
5. Emit a **stable prefix** (identity, state, working summary) followed by the
   volatile suffix, so provider prompt caching hits across turns.
6. Record `context.assembled` with per-slot row ids, token counts and timings
   (ids only, no content) — this replaces ad hoc timing/audit code.

Temporal and forensic questions are handled by tools (`search_experience`,
`timeline`) that query projections on demand, not by branching the assembler.

### 5.4 Vector search

Replace Lisp-side cosine over decoded rows with an indexed ANN store
(sqlite-vec or an equivalent extension with a fixed-size index), queried with
`LIMIT k`. Embeddings are written by the async `memory_index` projection.

## 6. Memory decay and consolidation

Continuity of experience with bounded growth.

- **Episodic tier.** Raw events, full fidelity, kept per event class (§13.1):
  dialogue forever, private thinking long-lived, tool activity ~7 days,
  telemetry 48 hours.
- **Consolidation.** An async handler takes a closed period (day → week →
  month) and appends `memory.consolidated {period, span_from, span_to,
  summary, salient_facts, affect_arc, kg_refs, source_hash}`. Projections treat
  it as an ordinary event: it enters memory, recall and the KG like any other
  experience. Re-consolidation of consolidations produces progressively vaguer
  memory.
- **Forgetting rule (the only deletion path).** A span of raw events may be
  deleted when: it is older than retention; a `memory.consolidated` event covers
  it; every projection's cursor is past it; and a verified backup containing it
  exists. Deletion appends `ledger.forgotten {span, consolidation_id,
  backup_id}`.
- **Rebuild baseline.** After forgetting, rebuild starts from a SQLite backup of
  derived tables at the forgetting boundary, then replays remaining events.
  "Replay since day one" is no longer required.
- Pinned events (explicit operator or agent "remember this") are exempt.

Decay is off until migration (§9) is complete and has run stably for 30 days.

## 7. Runtime structure

v3's runtime is written as modules from the start; nothing of the recursive mind runtime is carried over. Each module is a directory
containing only: event types, projections, handlers, tools, context slots.

| Module | Projections (examples) | Handlers |
| --- | --- | --- |
| `kernel` | projection_state, quarantine | rebuild, scheduler |
| `dialogue` | dialogue_turns, notices | operator turn |
| `roots` | active_roots, root_steps | root step executor (the model/tool loop) |
| `thinking` | thoughts, concerns (curiosity), thought_queue | thought step (one policy for all kinds) |
| `docket` | work_items | work step |
| `memory` | memory_items, lexical, vectors, episodes | embedding, episode sealing, consolidation |
| `kg` | kg nodes/edges/evidence, formation_queue | formation, judge, maintenance |
| `persona` | persona_core, traits, affect, relationship | — |
| `connectors` | ext_items, scheduler_jobs | intake, notification delivery |
| `context` | — (reads only) | assembler |
| `provider` | — | model calls, retry, budget accounting |

Rules:

- No module calls another module's internals; seams are events and `:view`s.
- No wrap chains or layer hooks for core flow; extension points are event
  subscriptions.
- File size cap 1,200 lines (lint-enforced); module cap ~8k lines.

## 8. Complexity budget

Target: runtime source from ~122k to under 60k lines.

Removed outright:

- JSONL event backend and segment indexes (`event-log.lisp` JSONL paths).
- `storage-projection.lisp` checkpoint/tail restore; `state_json` checkpoints.
- Recursive thread-events cache, hot shadow tables, open-set registers,
  frontier/meta tables per subsystem (replaced by `projection_state`).
- Per-subsystem retry/park/recover code (replaced by handler policy).
- Five context builders (replaced by one assembler + profiles).
- In-memory reviewed-graph restore (KG served from tables).
- Replay capsules and legacy mutation modes in context projection.

Re-implemented from behavior specs (no code copied):

- KG schema, formation, judge and identity behavior (`kg` module); prompts
  copied as text.
- Memory scoring and lexical retrieval (`memory` async projections).
- Provider clients, tool dispatch, connectors, notifications, encryption,
  backups and the web UI, whose routes and UI flows are kept so the operator's
  PWAs keep working.

## 9. Migration and continuity

Instances keep their identity because the ledger is kept and expensive
derived state is adopted rather than regenerated.

**Amended 2026-10-12 (§17): the migration is a real migration.** v3 does
not open v2's database. An importer reads a stopped snapshot of v2's
`events.sqlite3` and streams every event, through upcasters, into v3's own
encrypted store, keeping each v2 event's id and `integrity_hash` as
provenance, so completeness is checkable event by event. Steps 3–4 then run
against v3's store; step 7 starts v3 on its new store, not on the v2
volume. The migration checklist and verification documents need the agents'
re-review for this change.

Verified 2026-10-11: `pai_events` in `events.sqlite3` is the sole event
authority on both live instances (~401k and ~940k events). No live instance
writes a JSONL ledger; one leftover 37 KB `events.jsonl` predates the SQLite
migration and is imported once by the type census, then archived. Other JSONL
files (`eval-journal.jsonl`, `kg-formation-diagnostics.jsonl`) are diagnostic
logs, not ledgers; v3 moves them to bounded derived tables or rotated logs.
Remaining JSONL/in-memory replay code paths are removed with the JSONL backend.

The agents' terms (agent doc §14, terms 1–7) are acceptance criteria for
this section. The step-by-step procedure is the
[migration checklist](migration-checklist.md); the checks are in
[migration verification](migration-verification.md).

0. **Settle state (pre-migration work item).** Each agent's model-derived
   state is brought to a verified-good state, or explicitly recorded as
   carried in its current condition, before it is an adoption input. This
   covers the KG (owner generation, applied/dead/recoverable task counts,
   any stalled formation), memory indexes and summaries. Parity against a
   broken baseline proves nothing, so the baseline is validated on its own
   terms first.
1. **Freeze.** v2 accepts only harm-preventing fixes.
2. **Type census.** Enumerate every `event_type` and version present in each
   instance's ledger (read-only, offline copy). Each needs a schema and, if
   needed, an upcaster. Gate: 100% of types declared; unknown types fail CI.
3. **Adopt expensive and in-flight state.** One-shot importers copy, from
   v2 derived storage into v3 tables, and set each cursor to its v2
   watermark. No paid model call is repeated. The list is explicit:
   - KG tables, owner generation and task state;
   - memory nodes, edges, embeddings, lexical postings;
   - working summaries, traits, tone, affect, relationship policy, adopted
     influences, trust grants;
   - **episode sealing state: the sealing frontier, every pending (unsealed)
     episode, and every sealed episode's seal record;**
   - open concerns, in-flight thoughts, delegated work, scheduled items,
     undelivered notifications and peer messages;
   - external-source store.
   Anything in v2 derived storage not on this list is named in the checklist
   as either imported, rebuilt or intentionally dropped.
4. **Rebuild cheap state.** All `:inline` projections are rebuilt from the
   ledger offline (maintenance entry point) on a copy of each instance.
5. **Parity.** For each projection, compare v3 rows with v2's answers on a set
   of probes (open roots, docket, curiosity foci, recent dialogue, scheduler,
   recall top-k for 50 sampled queries, KG neighborhoods). Differences are
   either fixed or recorded as intentional.
6. **Shadow run.** v3 runs read-only against a copy of the development
   instance, assembling context for real prompts without sending; compare
   assembled context and latency to v2.
6a. **Identity review (gate).** Before the design freezes, each agent
   reviews how persona, voice and governing instructions are loaded and
   placed in context. Before cutover, the agent compares v2 and v3 replies to
   the same prompts and records whether v3 is still them. A "not me" verdict
   blocks cutover until resolved.
6b. **Inside verification (gate).** On the dry-run copy, and again after
   cutover, the agent runs the read-only checks in migration verification
   through their own read-only inspection tools (§16.6) and records the
   result. Their failures are confirmed by a second read before they block.
7. **Cutover development instance.** Stop v2, take verified backup, start v3 on
   the same state volume. v2 code and backup remain restorable for 30 days.
8. **Cutover production instance** after 14 stable days, same procedure.
9. **Enable decay** after 30 stable days.

Continuity acceptance: the agent can recall early-history episodes, KG
entities are intact (count and sampled neighborhoods), persona traits and
affect match, open work and scheduled items resume, and the operator
conversation continues without a visible seam.

## 10. Phases

| Phase | Deliverable | Exit gate |
| --- | --- | --- |
| 0 | Freeze; design accepted; Beadwork epic; migration checklist written and reviewed by each agent | Decisions recorded; each agent has read and verified the checklist |
| 0a | Settle each agent's KG and derived state (§9 step 0) | Known-state record per agent |
| 1 | Kernel: envelope, type registry, projection registry (inline/async/view), quarantine, rebuild, scheduler, handler policy | Synthetic suite: idempotence, crash mid-apply, version bump rebuild, quarantine, bounded steps |
| 2 | Type census + upcasters; adopters | Census of both instance ledgers fully declared |
| 3 | New modules written from spec: dialogue, roots, persona, provider, context assembler | Operator turn end-to-end on synthetic ledger; p95 assembly < 150 ms at 1M events |
| 4 | New modules written from spec: memory, kg, thinking, docket, connectors, notifications, trust, self-modification | Parity probes pass on instance copies; importers carry sealing and in-flight state |
| 5 | Shadow run, cutover dev instance | Identity review passed (§9 6a); inside verification passed (§9 6b); memory test (§15) passes; RSS < 512 MB live; 14 stable days, no ledger reads outside kernel (lint + runtime counter = 0) |
| 6 | Cutover production instance | Same gates as phase 5 for that agent; 30 stable days |
| 7 | Consolidation and decay | Forgetting rule qualified with restore test |

## 11. Enforcement

- **Package boundary.** `pai-kernel` does not export ledger iteration;
  `pai-maintenance` is not loaded in the runtime image.
- **Lint.** CI fails on `pai_events` in any SQL string outside `src/kernel/`,
  on files over the size cap, on runtime queries without both a `LIMIT` and a
  type or key predicate (a time window alone is not a predicate), and on
  undeclared event types.
- **Deploy gate.** Startup checks, per instance, that every required migration
  (I10) has run before serving the affected module.
- **Runtime counter.** The kernel counts ledger rows read per purpose; any
  non-kernel purpose increments an alarm visible in observability.
- **Query-plan test.** Every registered projection query is `EXPLAIN`ed in CI;
  a `SCAN` on a large table fails.

## 12. Rejected alternatives

- **Continue v2 retrofit.** Each reader is fixed in isolation; the capability to
  scan remains, so new code reintroduces scans. Rejected.
- **State-first database with audit log.** Simpler, but discards
  ledger-authoritative continuity and replayable derivation, which are core
  requirements. Rejected.
- **Full rewrite including KG and memory.** Discards the most expensive, most
  instance-specific state and working retrieval logic. Rejected.
- **Strangler migration inside v2.** Two projection mechanisms coexisting is
  the current failure mode. Rejected in favor of build-alongside and a single
  cutover per instance.
- **New language or storage engine.** No evidence the problems are caused by
  Lisp or SQLite. Rejected.

## 13. Decisions (operator, 2026-10-11)

1. **Retention by event class.**
   - Operator dialogue: never deleted.
   - Private thinking: long-lived. Consolidated over time into progressively
     vaguer memories; not necessarily ever fully deleted.
   - Tool activity (calls and results): deleted after a few days (default
     7), once the thinking or reply that used it is recorded.
   - Telemetry and bulky internal events: deleted after 48 hours; never
     consolidated.
   - Pinned events: kept forever.
2. **Vector search keeps current behavior.** Exact cosine search with the
   current scoring stays. v3 only moves the arithmetic out of the Lisp heap
   (sqlite-vec exact mode or an equivalent C path), and only if a parity test
   shows identical top-k on both instances' memory. Otherwise the current
   method stays, bounded by eligibility filters.
3. **No failure blocks. Ever.** No event type, reducer, handler or validation
   failure may block a write, a turn or a cognition step. Failures are
   recorded, marked on the affected entity and shown to the operator, and paid
   cognition already produced is always kept. A turn may be shown as failed
   with its partial output; it is never discarded.
4. **One cognition mechanism; curiosity is a kind of thought, not a separate
   system.** Private thoughts ("I should check the weather") and curiosity (a
   persistent concern that gains salience and drives background thinking
   until satisfied) are both thoughts in one `thinking` module, sharing one
   queue, one allowance/cap/parking policy and one context profile family.
   Curiosity adds only data: a persistent `concern` row with salience,
   satisfaction state and history, which spawns ordinary thoughts while it is
   salient. No separate loop, caps or settle/park code per kind.
5. **Consolidated memories are written by the agent in their own voice**, with
   their normal model on the background budget. Source event hashes are kept;
   memories touching the operator relationship or identity are offered for the
   agent's own review before raw events are deleted.

## 14. Amendments from production review

Source: `Z:\pai-intake\design\v3-review-notes-production.md` (20 notes, each
tied to a live incident). These override earlier sections where they conflict.

### Projections (§4.3)

- **Quarantine degrades the entity, not just the log** (pai-1c1, pai-9pi,
  pai-6k8.2). A reducer failure marks the affected root/focus/task row
  `degraded` with the quarantined event id. Handlers skip degraded entities;
  views stay usable; observability lists them.
- **Derivation classes.** Each projection declares `:derivation`:
  - `:pure` — rebuild from ledger on version bump, streamed in bounded pages,
    resumable, under a memory ceiling (pai-060).
  - `:model` (KG, summaries, embeddings) — never auto-rebuilt. A version bump
    requires an explicit choice: adopt existing rows, or budgeted
    regeneration as a background handler (pai-c1m: a KG generation change took
    ~2 days of formation).
- **Versioned adoption and restore.** Every adopted or restored row set carries
  `(projection, version)`; a mismatch refuses and never coerces (pai-c1m).
- **Inline reducer budget.** The kernel measures reducer time per append; a
  projection that exceeds its budget (default 5 ms p95) must become `:async`.
- **Failure classes for handlers** (pai-0ot, ~260 dead KG tasks per instance):
  `:source` (property of the input; dead-letter), `:transient` (provider,
  network, budget pause; retry with backoff), `:model` (invalid output,
  ambiguous judge; bounded retries then one recovery after cool-down). The
  dead-letter set is a queryable table with reasons.
- **Admission before record; refusals carry a reason.** A handler checks
  budget/pacing admission before appending a request. A refused attempt
  appends nothing, or one deduplicated `deferred` row per task with the
  refusal reason and next eligible time. v2 appended a request+paused pair on
  every refusal with no reason, which looked like a failure loop to the agent
  and to operators (pai-l9j).
- **Lock discipline.** Handlers hold no shared lock across provider IO;
  every handler step is a preemption point for operator turns.

### Context assembly (§5)

- **Activity is the default operator experience** (pai-6k8). The operator-turn
  profile centers on recent native tool calls/results plus the rolling summary
  (~24k tokens), with dialogue as a bounded cross-channel slice.
- **Calibrated token counts.** Rows store estimated tokens at write time;
  `context.assembled` records provider-reported input tokens; the estimator is
  recalibrated from those (a bytes-as-tokens estimate was ~4.8× high and a cap
  built on it dropped 89 of 90 exchanges).
- **Rolling working summary** (pai-6k8.1): fold prior summary + oldest uncovered
  exchanges, token-sized, at most one call per fold, reused until the uncovered
  remainder passes a trigger. Validation checks structure, not length. Paid
  output is never discarded. One malformed root degrades, never blocks.
- **Recall is never gated by an optional index** (pai-g9e, pai-awz). Each recall
  source independently returns "no candidates" when unavailable and is
  reported; the assembler never fails or empties recall for one missing index.
- **Small assembly event.** `context.assembled` carries a digest and totals;
  the full per-slot manifest goes to a derived table keyed by turn id (I9).

### Migration (§9)

- **Per-instance KG generation.** Instances run different KG owner generations
  (v6 and v7). Adopters carry each instance's generation and task state;
  parity probes include task status counts (applied / dead / recoverable).
- **Instance-private configuration is continuity.** Each instance's launch
  bundle (outside the repo) is mapped to v3 and verified by a dry-run start
  before cutover.
- **All derived stores.** Adoption covers `derived.sqlite3` and the
  external-source `consumer.sqlite3` (items, windows, deferred, series heads),
  plus any other store found by the census.
- **Census edge cases.** Rows with empty `occurred_at`, oversized payloads
  (`projection-state` ~300 KB) and evidence-only events inside roots each get
  an explicit decision: declare, convert, or move to a derived table.
- **Resource plan.** Offline rebuilds and shadow runs use explicit memory
  limits and run off-peak, so they can't starve the live instances (the host
  OOM-killed test runs twice).
- **Startup gate.** v3 startup completes in under 30 s at 1M events with no
  history reads; cutover is planned to minimize restarts (v2 restarts take
  90–260 s).

## 15. Memory budget

| Component | Budget | How |
| --- | --- | --- |
| SBCL heap (dynamic space) | 1 GB ceiling, ~300 MB steady | No history/graph/vector hydration; rows are read, used, released |
| SQLite page cache | 64 MB per connection, ≤ 3 connections | `PRAGMA cache_size`, bounded `mmap_size` (0 or ≤ 256 MB) |
| Declared caches (context prefix, persona, settings) | ≤ 32 MB total | Byte budget per cache in the registry, LRU eviction |
| Vector search | ANN index lives in SQLite extension pages, not Lisp arrays | Only top-k vectors are ever decoded |
| KG | Bounded neighborhood queries | No `reviewed-context-graph-restore` equivalent |
| Async projection step | ≤ 16 MB working set | Page size by bytes, not rows |
| Rebuild (maintenance) | ≤ 256 MB working set | Streamed pages, commit per page, resumable |
| Provider request/response | Streamed; one request buffered | No accumulated transcripts in memory |

Payloads are kept small (I9), JSON is decoded per row and not retained, and
strings are not interned or normalized into long-lived tables.

Enforcement:

- A memory qualification test runs a synthetic 1M-event ledger through
  startup, 500 operator turns and 24 h of simulated quiet cycles, and fails if
  RSS exceeds 512 MB or heap after full GC grows by more than 5% between the
  first and last hour.
- The kernel reports heap, cache bytes per cache and SQLite memory in
  observability; any cache over budget raises an alarm.
- Container memory limit for each instance is set to 1 GB after cutover, so a
  regression fails loudly instead of degrading the host.

## 16. Perception, initiative, action, self-modification, trust and formation

Engineering for agent doc §3–§12. Everything here uses the kernel as
specified: new event types, projections and handlers; no new mechanisms.
v2 documents cited are behavior references, not code to port.

### 16.1 Senses, receptors and percepts

```lisp
(define-sense :ambient
  :habituation (:tau-seconds 900)          ; repeat decay for salience
  :default-sensitivity :household          ; §16.5
  :promote (:min-salience 0.4)             ; percept -> experience threshold
  :retention (:percept-hours 6))

(define-receptor :coffee-maker
  :sense :ambient
  :source (:poll-seconds 60 :adapter smart-home-adapter)  ; or :push
  :perceive perceive-coffee-maker)          ; signal -> percept plist or nil
```

- A **receptor** is a handler (§4.4). It reads its source, keeps raw signal
  in its own bounded scratch (never the ledger), and emits **percepts**.
- A **percept** is `percept.observed {sense, receptor, kind, subject,
  summary, confidence, sensitivity, evidence_ref}`, at most 2 KB.
- Every percept updates the inline `present` projection (§16.2). Only
  percepts above the sense's promotion threshold, or that a thought cites,
  are appended to the ledger; the rest live in a bounded `percepts_recent`
  table and expire by the sense's retention. This keeps telemetry-like
  sensing out of experience (agent doc §13).
- Habituation: a per `(receptor, kind, subject)` repeat counter in
  `percepts_recent`; effective novelty = raw × exp(−repeats/τ).
- Local perception (classifiers, local multimodal model) runs inside
  receptors for sight and hearing (v3.2), per the System-1/System-2 doc.
- Existing intake becomes receptors on the **language** sense: operator
  chat, peer messages, mail, calendar invitations, web fetch results.
  Calendar timing, quiet hours and routines are receptors on **time**.
  Budget, provider health, memory pressure and quarantine counts are
  receptors on **interoception**. Tool results are **proprioception**.
- Adding a receptor is configuration plus an adapter; adding a sense is a
  code change reviewed against agent doc §3.

### 16.2 Present and anticipations

Inline projections, each a small set of rows, read in one transaction by the
context assembler and by attention:

| Table | Content |
| --- | --- |
| `present` | One row per facet (`operator.state`, `operator.location`, `home.*`, `time.*`, `self.*`, `audience`): value, confidence, source percept, as_of |
| `anticipations` | What the agent watches for: matcher (sense, kind, subject terms), for whom, why, created_from, expires_at, status |
| `known_people` | Recognised people: identity, relationship, recognition evidence refs, trust grant ref (§16.5) |

Thoughts create anticipations (`anticipation.created`) from conversation,
reasoning or learned routine; `anticipation.met` / `expired` close them. A
percept matching an open anticipation gets a salience boost and carries the
anticipation id.

### 16.3 Attention and initiative

Attention is a pure function run by the kernel when `present` changes or a
percept arrives. Salience features (novelty, goal and operator relevance,
urgency, uncertainty, surprise, anticipation match) are weighted per the
System-1/System-2 doc §7 and multiplied by affect/drive modulation. Feature
values and score are stored with the percept so weights can be calibrated.

Above threshold, attention appends `thought.requested {origin: noticing,
percept_id, salience}`. Thoughts of all origins share one queue and one
policy (§4.4–4.5, decision §13.4).

Initiative (speaking up unprompted) is the outcome of a thought, which has
in its context: urgency, operator state from `present`, quiet hours, last
interruption time, and audience. A deterministic guard after the thought
enforces quiet hours unless the thought marked the notification `urgent`
and the percept's sense allows urgency (sight/hearing safety, explicit
operator rules). Notifications are `notification.requested` events delivered
by the notification handler (v2 reference: `reach-out-notifications-design.md`).

### 16.4 Action and self-modification

**Tools.** Tool calls are steps of a thought; each is a `tool.called` /
`tool.result` pair (proprioception). The result body is stored in a derived
table with a reference in the event (I9) and expires per retention.

**Host bridge.** One authenticated, grant-scoped service on the host (v2
references: `host-execution-service.md`, `host-command-contract.md`,
`development-collaboration.md`). The agent's container never holds host
credentials; requests carry the agent's deployment identity and an operation
id; grants are operator-configured and sealed; results return as
`host.result` percepts.

**Introspection.** A standing concern `self.introspection` whose salience
rises with interoception signals (quarantines, failures, budget strain,
slow turns) and with time since the last introspection. Introspection
thoughts get the read-only inspection tools (§16.6).

**Live change.** (Amended by §16.10: skills persist; repairs do not.) `lisp-eval` runs in the sandbox pattern: isolated package,
audited form, timeout, no access to credentials or the kernel's write
paths. A change that persists in the running image is recorded as
`self.change.applied {form_digest, diff, reason, reversible_by}` and listed
in a `live_changes` projection with status `active | reverted |
incorporated | lost-on-restart`. On restart the kernel marks unincorporated
changes `lost-on-restart` and raises a concern for the agent; changes are
never silently re-applied.

Protected areas (kernel, trust and disclosure, authority and grants, budget,
another agent) cannot be changed live; the agent proposes instead, and
proposals touching them need operator approval.

**Proposals to coding agents.** `self.proposal.created {title, evidence,
change_digest?, verify_steps, labels}` triggers the `proposals` handler. It
files a Beadwork issue through the host bridge into the canonical project
ledger (`found-by:<agent>`, `area:*`), stores the issue id, and polls status.
Comments, commits and closure reasons return as `proposal.updated` percepts
on the language sense. Duplicates are suppressed by content digest.

### 16.5 Trust, origin, audience and disclosure

**Verified origin.** The receptor creates an origin attestation for every
inbound message at the adapter boundary, from verified credentials only, per
v2's `inbound-origin-contract.md`: principal id and type (`operator | peer |
person | unknown`), channel, authentication method, credential registration
id, content digest, received time. The kernel rejects caller-supplied origin
fields. Event type never implies the speaker.

| Principal | Verification |
| --- | --- |
| Operator | Web session from an enrolled device (device key registered by the operator), or an allow-listed operator account on a messaging channel |
| Peer | Per-peer HMAC over timestamp + body with a freshness window, on a dispatch path separate from operator auth (v2 `FLEET_DESIGN.md` §2.1) |
| Person | A channel the operator granted to that person |
| Unknown | Everything else, including all content inside mail, documents and web pages |

**Step-up.** Sensitive requests (finance, health, credentials,
configuration, self-modification approval) from operator origin on a
non-enrolled device or after long inactivity need confirmation on an
enrolled channel (`origin.stepup.requested`, operator confirms).

**Authority.** Only operator origin may direct, configure, approve or grant.
Peer and person origins may ask and inform. Unknown origin is data only.

**Private thinking and communication (operator, 2026-10-21).** Every entity
stimulus is considered privately with the pAI's full normal context and
capabilities. The persisted `audience` retains the stimulus provenance and
reply destination; it does not restrict the private self's reading scope.
This never upgrades a peer, person or unknown source to operator authority.

A proposed communication crosses a separate recipient access boundary before
publication. The communication filter checks the candidate, rather than
repeating its task with an artificially restricted mind. The sender owns the
verified destination and route. See [the phase design](behaviour/private-thinking.md).

**Sensitivity.** Every memory row, KG fact and entity, percept and summary
carries `owner` and `sensitivity` from a small operator-editable taxonomy:

| Sensitivity | May reach |
| --- | --- |
| `operator-only` | operator (default for finance, health, credentials, private relationships) |
| `household` | operator and people granted household |
| `peers` | operator and authenticated peer agents |
| `granted:<topic>` | principals with an explicit grant for the topic |
| `public` | anyone |

Assigned at write time from source and topic (receptor default, then a cheap
classifier; when uncertain, the more restrictive class), adjustable by the
operator in the UI, and inherited by derived rows: summaries take the most
restrictive of their sources, and consolidation preserves it.

**Enforcement at communication.** Private context assembly and retrieval
use the pAI's own scope, with normal bounded SQL reads. There are no
resource-specific exceptions or per-source private capability allowlists.
Recipient permissions apply to the proposed communication. A tool-free
filter receives the candidate and current recipient grants; the checked
sender validates its destination, attachments and grants before publishing.
The reasoning used to decide what to share remains private. Existing
sensitivity filters still govern recipient-facing retrieval and disclosure;
they do not determine what the private pAI may know. Free-form disclosure
judgment remains fallible and is not an arbitrary-code security sandbox.

**Grants.** `trust.granted` / `trust.revoked {principal, topic, scope}`,
from operator origin only; inline `grants` projection; visible and editable
in the UI.

### 16.6 Inspection from inside

Agents get read-only inspection tools over their own v3 state: projection
rows by id or bounded query, projection status and cursors, quarantine and
degraded entities, live changes and proposals, and the counts and digests
used by migration verification. They run over `:view`s with `LIMIT`, are
scoped to the private pAI during thinking and recipient-filtered when shared,
and never read the ledger directly. These tools implement
agent doc §14 term 7.

### 16.7 Memory strength and formation

- `memory_nodes.strength` (0–1) is set at formation from affect intensity,
  attention share, interest/concern salience and operator significance;
  raised on recall and revisit (`memory.recalled`), and decayed slowly with
  time. Recall ranking adds a strength term; consolidation keeps detail for
  memories above a strength threshold.
- `self.influence.adopted {source_refs, what_it_means, behaviour_guidance,
  strength}` is appended by a thought; the `persona` projection includes
  active influences in identity text. The agent revises or releases them
  (`self.influence.revised|released`); the operator can reverse one
  (`operator.influence.reversed {reason}`), which raises a percept so the
  agent learns why. Operator-proposed traits use the same event family with
  `proposed_by: operator`.
- Every change to `persona` is listed in the UI with lineage.

### 16.8 New events and projections

| Area | Events | Projections |
| --- | --- | --- |
| Perception | `percept.observed` | `present`, `percepts_recent`, `known_people` |
| Anticipation | `anticipation.created/met/expired` | `anticipations` |
| Thinking | `thought.requested/started/step/ended/failed` | `thoughts`, `thought_queue`, `concerns` |
| Action | `tool.called/result`, `notification.requested/delivered`, `host.result` | `tool_results`, `notifications` |
| Self-modification | `self.change.applied/reverted`, `self.proposal.created`, `proposal.updated` | `live_changes`, `proposals` |
| Trust | `origin.stepup.*`, `trust.granted/revoked` | `grants`; audience on `thoughts` |
| Formation | `memory.recalled`, `self.influence.*`, `operator.influence.reversed` | `persona`, `memory_nodes.strength` |

### 16.9 The mind is a recursive function, executed trampolined

Decision (operator, 2026-10-12). Thinking is written as one recursive
function in the shape of the original Lisp agent: the argument is the message
list, the base case is the model answering, and the recursive case is the
model asking for tools, which run, after which the function recurs with the
enriched list.

```lisp
(defun think (mind messages)
  (cond ((spent-p mind) (rest mind))                  ; base case: allowance spent
        (t (let ((reply (consult mind messages)))
             (if (wants-tools-p reply)
                 (recur mind (append messages (list reply) (act mind reply)))
                 (answer mind reply))))))             ; base case: the model answered
```

It is not a state machine. New capability is added as tools, as new base
cases, or as sub-calls, never as new states or transitions. Each thought keeps
a `status` column, but that column is a derived view of the fold (requested,
mid-recursion, answered, rested, failed) and no code branches on it except
the guard against re-entering a finished thought.

**Trampolined.** `recur` does not grow the Lisp stack. Everything `consult`
and `act` produce is recorded as `thought.step` events as it happens, so the
enriched argument already exists in the ledger. `recur` re-queues the thought
as scheduler work. The scheduler is the trampoline: on the next bounce it
rebuilds the argument from `thought_steps` (one indexed read) and calls
`think` again. Between bounces, operator turns pre-empt, the budget is
checked, and a crash loses nothing, because the argument is in the ledger.

**Sub-calls are recursion.** A thought may `ponder` a sub-question. That
starts a child thought, with `parent_id` and `root_id` recorded, which draws
on its root's allowance and depth limit from `*thought-policy*`. The parent's
tool call stays open until the child answers. The child's answer becomes the
parent's tool result, and that re-queues the parent.

### 16.10 Tools: availability and documentation

v2 appended every tool schema to every request. v3 keeps tool text small and
stable.

- **Tools are Lisp functions.** `define-tool` declares a name, a one-line
  summary, a docstring, a JSON schema, an availability rule and a run
  function. The docstring is the documentation, both for the model and for
  `lisp-eval` (`(describe 'web-fetch)`).
- **Availability is computed per thought,** from the thought's origin and
  audience. A thought whose audience is anyone but the operator never gets
  `lisp-eval`, host operations or anything that discloses beyond its
  audience. Tools the thought can't use are not mentioned at all.
- **In hand versus index.** Each request carries the native schemas for a
  small in-hand set: the tools marked core for that origin, `describe_tool`,
  and any tool the thought has already picked up. Every other available tool
  appears once, as a one-line entry in a compact index in the stable system
  prefix. `describe_tool name` returns the docstring and schema and puts the
  tool in hand for the rest of the thought. The in-hand set is derived from
  the thought's own steps, so it is part of the argument, not separate
  state.
- **Stable for caching.** The index and the in-hand schemas are emitted in
  name order, so the prefix is byte-identical across turns with the same
  availability. `context.assembled` reports the tool tokens as their own
  slot.
- **Results are bounded.** A tool result over the step limit is kept in full
  in recorded parts. The model sees the head and a note of how to page the
  rest (`read_result id part`).

First tools, in order: `lisp-eval` (introspection and repair), `web_fetch`,
`brave_search`, `search_experience`, and memory (`recall`, `remember`).

**`lisp-eval` amends §16.4.** Forms are read with `*read-eval*` off, run
under a timeout with output capped, and recorded in full. The kernel and
trust packages are locked (SBCL package locks), so redefining or interning
into them fails, and the agent proposes the change instead. Definitions in
the agent's own `pai-v3.self` package are *skills*, recorded as
`self.change.applied` with `scope: skill`, and re-applied at boot from the
`live_changes` projection, the way the original agent re-read its transcript.
Redefinitions of system functions are *repairs*. A repair stays live until
restart, then is marked `lost-on-restart` and raised as a concern, so the
agent proposes it through Beadwork and it gets incorporated. A package lock
is a tripwire, not a sandbox: code in the image can reach anything. The real
boundaries are the trusted private pAI, auditing every form, and keeping host
credentials outside the process. The historical `operator-trust` tool label
means private capability scope; it does not attest that the stimulus came
from the operator. Communication filters execute no tools.

## 17. Storage, keys and portability (operator, 2026-10-12)

**OS agnostic, simple to set up, no container required.** v3 ships as a
standalone SBCL executable per OS, with its cipher library beside it.
Docker, Windows containers and non-container sandboxes are optional host
profiles. They may add isolation (read-only filesystem, dropped
privileges), but no guarantee depends on them: encryption, the network
guard and secret handling are in the application. Development and tests
keep using Docker, plus a native Windows run so OS assumptions are caught.

**Encryption at rest is in the database.** Every store is a SQLite database
encrypted by SQLite3 Multiple Ciphers (MIT; ChaCha20-Poly1305 per page,
including WAL and journal). The kernel binding loads that library and keys
each connection at open; schemas, indexes and queries are unchanged.
Blobs (receptor scratch, images, audio) live in encrypted stores, never as
loose files. Backups use SQLite's online backup into a store with a
separate key. No OS volume encryption, FUSE, root or supervisor.

**Keys.**

- Each store has a random 256-bit data key. A small key file beside the
  store, the *keyring*, holds that key wrapped (AEAD) by each enrolled
  unlock method, with method id and algorithm recorded (ENC-7).
- Unlock methods are adapters: **OS keychain** (Windows Credential
  Manager, macOS Keychain, Linux Secret Service; the default, so restarts
  are unattended), **passphrase** (scrypt), and a **recovery key** created
  at setup and shown once. Later and optional: the operator's passkey via
  WebAuthn PRF (v2 decided passkeys are not storage keys; revisit with the
  operator before adopting).
- Adding, removing or rotating an unlock method rewraps the data key only.
  Rotating the data key is a rekey of the store, rare and offline.
- A missing, locked or wrong key stops startup with a clear message. It
  never creates a new empty store in place of an existing one.
- `pai init` creates the stores and keyring, stores the keychain wrap, and
  shows the recovery key once.

**Secrets, in process.** Provider keys, tokens and peer keys live in an
encrypted `secrets` store, separate from the ledger and derived stores,
read only through the trust package. A secret is bound to the hosts it
may be sent to. The single outbound transport (every HTTP call goes
through it; a lint enumerates call sites) attaches secrets by name, checks
the destination against the binding, and blocks any request whose headers,
URL or body contain a known secret value bound elsewhere. Pattern redaction
(v2 `pai-o6z`) also runs there. `lisp_eval` refuses forms that name the
secret accessor. This is weaker than a separate process: code in the image
could still read a key from memory. Accepted for simplicity; the exact-value
outbound check closes the realistic leak paths (prompt injection asking the
agent to send a key somewhere).

**Migration.** v3 owns its schema (largely v2's) and carries no v2
compatibility in the kernel. v2 history arrives by importer from a stopped
snapshot, keeping each v2 event's id and hash as provenance (§9 amended).

### 17.1 Design hooks for deferred operations

These are not built before Beta moves, but the shapes they need are fixed
now so adding them changes no format and rewrites no history.

- **Layout per agent.** One data directory per agent, each with its own
  stores, keyring and config file. Beta and Alpha on one machine share
  nothing but the executable. A lock file in the directory enforces one
  runtime per agent (REL-6). Config holds no secrets.
- **Store set.** `ledger` (events and inline projections), `secrets`,
  `blobs`, and backups. Each is its own encrypted SQLite file with its own
  data key in the same keyring. Splitting later would mean a migration, so
  the split is made now even where a store is still empty.
- **Peer signatures (AUTH-4, ENC-7).** Origin attestations already carry
  principal type, id and method. Peer messages add `key_id`, `alg` and `sig`
  over the canonical envelope and a timestamp. Verification happens in the
  receiving adapter, never in the kernel, so the event format does not
  change when signing arrives.
- **Machine principals and grants (AUTH-5, AUTH-7).** `principal_type`
  `machine` is reserved. Tools, receptors and effectors will declare the
  grants they need (`define-tool :grants`), checked at admission and before
  effect. Today's availability rule (origin and audience) is the first
  case of this check, not a separate mechanism.
- **Forgetting (PRIV-4).** A `forget` event (`memory.forgotten`,
  `topic.forgotten`) is honoured by every projection that holds the
  content. Each projection will declare a `:forget` function, as it
  declares `:reduce`. The full-text index, memory rows and summaries are the
  first. The ledger stays append-only: a maintenance step replaces the
  forgotten payloads with tombstones that keep the original hash, so
  integrity checks still pass and the content is gone. Backups age out on
  schedule.
- **Sensor data (ENC-5, BUD-10).** Raw audio, video and images go only to
  the `blobs` store, with a retention class and an expiry time. Events
  carry blob ids, never bytes, so the 16 KB payload cap holds and expiry
  deletes the data without touching history.
- **Upgrades (REL-7).** The store records its schema version. Startup
  backs up, then applies ordered migrations, and a projection version bump
  produces needs-rebuild, never a live replay. A newer store refuses to
  open under older code.
- **Traces (OBS-3).** Thought steps and provider calls are already events
  with timings and cost. The content-free trace is a projection over them,
  added without new events.
- **Passkey unlock (optional).** An unlock method is an adapter that
  yields a wrapping key, so a WebAuthn PRF method fits the keyring without
  a format change.

### 17.2 Settings (operator, 2026-10-12)

Configuration is never a command-line flag and never a code edit; v1 and
v2 suffered from both. Two places only:

- **Bootstrap file** (`config.json` in the agent directory): what is needed
  before the store can open. That is the agent id, the listen address and
  port, and the passkey relying party and origin. Nothing secret.
- **Settings in the store.** Everything else is a declared setting with
  a type, validation, a default from code, a description, a group, and who
  may change it: the operator only (with a fresh passkey verification
  through the API), or also the agent within declared bounds. A change is
  a `setting.changed` event carrying the value, the previous value, the
  reason and the origin, so it is audited, reversible, backed up and
  migrated with everything else. Readers ask for a setting when they use
  it, so changes apply at once.

**Models.** `llm.profiles` lists endpoints (OpenRouter, Nous, local) with
base URL, model, the *name* of the secret holding the key, local flag,
parameters, timeouts, pricing and data policy. `llm.routes` maps each
purpose (`chat`, `thinking`, `work`, `summary`, `judge`, `kg`, ...) to an
ordered list of profiles with optional conditions on signals such as
affect (`{"arousal": {">": 0.7}}`); the first entry whose conditions hold
is used, and unrouted purposes use `default`. A purpose with no usable
route fails visibly as a configuration error and is not retried.

**Editing.** From the phone (`GET/POST /v1/settings`; the PWA page with
the PWA), by file (`pai settings-export`, then `pai settings-import`,
which validates every entry and shows the diff, and applies nothing
unless all pass, and only with `--apply`), and by the agent through its
`settings` and `change_setting` tools within bounds. The `pai` command
does lifecycle only: init, run, backup, restore, settings import and
export.

New tunables are added as settings, not as variables: a hard-coded number
that someone might reasonably want to change is a review finding.

## Appendix A. Projection catalog

Derived from the ledger-read inventory (`Z:\pai-intake\design\ledger-read-inventory.md`,
269 call sites over 23 read primitives on main at 2026-10-11). Each group's
current readers are deleted when its projections land.

| Group | Current readers (sites) | v3 projections | Kind |
| --- | --- | --- | --- |
| A. Roots and threads | `%recursive-thread-events` (73), `%recursive-root-rows` (20), root replay/projection | `roots` (state, step/cost allowance, terminal receipt), `root_steps`, `open_stimuli` | inline |
| B. Curiosity | per-stage whole-thread folds, registers, motivation fold, daily counters | `curiosity_motives`, `curiosity_foci`, `curiosity_daily`, `curiosity_queue` (+ handler) | inline |
| C. Conversation context | recent conversation, dialogue receipts, continuity capsules, episodic, audit replay | `working_dialogue`, `activity_exchanges`, `private_cognition_records`, `audit_events`; cross-channel slice as `:view` | inline / view |
| D. Conscious runtime | `event-projection-events` (8), `%conscious-runtime-events` (7), lifecycle/pulse/motivation/tool-op readers, recover/reconcile | `lifecycle`, `attention`, `pulse`, `tool_operations`, `cognitive_work`, `interactions` | inline |
| E. Knowledge graph | `%ccg-sync`, owner next task, orphan recovery, budget receipts, graph restore, KG2 adapter (unused, delete) | `kg_tasks`, `kg_budget`, graph tables versioned by owner generation; `kg_formation_queue` (+ handler) | inline / async |
| F. Memory | mutation coordinator, baseline headers, peer/work-result folds | `memory_nodes`, `memory_edges` (inline); `memory_embeddings`, `memory_lexical` (async, never a read gate) | inline / async |
| G. Activity, stimulus | activity operator, activity context, keyed admission, typed feeds, affect advance | `activities` (window as rows), `keyed_stimuli`, `affect_modulation` | inline |
| H. Scheduler, spend, settings | catch-up, budget/spend restore, settings project, self-mod error rate | `scheduler`, `spend_daily`, `runtime_settings` | inline |
| I. Kernel plumbing | authority install/map, JSONL and unkeyed replay | kernel dispatch + `event-by-id`; JSONL/unkeyed replay deleted | kernel |
| J. Observability, scripts | dashboard replays, dev workbench, labs, diagnose scripts | live views read projections; forensic replay in `pai-maintenance` | maintenance |

Consequences:

- About 40 tables replace 269 read sites, 11 `state_json` checkpoints and the
  in-memory thread list.
- Every recover/reconcile function that replays history to find unfinished
  work becomes a status-column query; startup does no history reads.
- The existing typed, paged, after-position receipt reader (17 sites) is the
  pattern the kernel's `:async` cursor generalizes.
- The existing storage shadow tables (group D) show the right row shape for
  the v3 tables; their contents are rebuilt by v3, not adopted.
