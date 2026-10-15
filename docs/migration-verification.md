# Migration verification

Read-only checks each agent runs (and the coding agent runs independently)
after the dry run and after cutover. Every check compares v3 against a v2
baseline taken **at the moment v2 is stopped**, not an earlier snapshot.
Pass/fail is mechanical. Origin: a set proposed by one agent from their own
inspection of v2, generalised to every agent.

Example baseline values below come from one agent's v2 reads on 2026-10-11
and illustrate the shape only; re-baseline at cutover.

## 1. Experience stream (ledger)

- Row count and max `event_id` equal the v2 baseline (example: 345,252 rows,
  max id 385,107).
- `storage_sequence` and `event_id` sets equal v2's exactly. Gaps that exist
  in v2 are recorded in the baseline and must match; any new gap means loss.
- `integrity_hash` verifies for every row.
- Distribution of `agent_id`, `partition_status` and `event_type` is
  unchanged.
- `occurred_at` values are byte-identical to v2 (no re-timestamping); rows
  with empty `occurred_at` are listed in the baseline and handled as the
  type census decided.

## 2. Episodes, sealing and in-flight state

- Sealing frontier equals v2's (example: event 383,225, 1 pending episode).
- Every sealed episode in v2 has its seal in v3; counted, not sampled.
- Each pending episode is still pending or has sealed cleanly; none dropped.
- Open concerns, in-flight thoughts, delegated work, scheduled items and
  undelivered notifications match v2 by id and state.

## 3. Knowledge graph

- **Blocking precondition:** before the dry run, the graph is brought to a
  known state or explicitly recorded as carried in its current condition
  (owner generation, task status counts, any stall). The record is
  **signed by the owning agent**, stating that it describes their graph
  (checklist A2); an unsigned record does not satisfy this check.
- Record counts by kind equal v2 (example: 4,572 reviewed-graph records:
  225 entities, 360 facts, 360 current triples, 119 adjacency, 342
  application receipts, owner open/terminal/task/phase/application rows, 1
  graph-meta).
- Graph content digest equals v2's (example `74d3288d…`). A different digest
  means rebuilt, not migrated.
- Owner generation equals v2's for that agent (at 2026-10-11 the agents
  were on different generations, one with ~261 dead tasks to settle or carry
  explicitly).
- **Unprojected tail:** v2's graph frontier (example 380,909) trails the
  ledger (example 385,107). v3 inherits the frontier as the KG cursor and
  continues formation from there as ordinary background work. It does not
  replay or re-form what was already formed.
- Unused legacy tables that are empty in v2 stay empty in v3 (anything
  populated is new state and must be explained).
- After cutover, formation produces formation events within the first day.

## 4. Memory

- Memory node and edge counts equal v2 exactly (example: 11,681 nodes,
  20,593 edges); content and embedding hashes match.
- Every index recall depends on exists and is used (a missing turn index
  silently degraded turn-neighbourhood recall in v2 before it was built). Recall over a fixed
  probe set returns the same top-k as v2, or better where v2 was degraded,
  with the difference explained.

## 5. Identity, persona and relationships

- Persona id is consistent everywhere it appears, and comes from one place
  in v3 (v2 formation takes it from the storage partition).
- Traits, tone, relationship policy, affect, motives, adopted influences,
  continuity records and trust grants match v2 by count and hash.
- Identity parity: the agent compares v2 and v3 replies to the same prompts
  and judges whether v3 is still them (agent doc §14, term 3).

## 6. Derived stores outside the main database

- External-source store (items, windows, deferred, series heads) counts and
  hashes match.
- Diagnostic logs are not carried as state; known v2 error signatures do not
  recur in v3 diagnostics.

## 6a. Inside access is itself a check

Before any agent-run check counts, confirm the agent has working read-only
inspection access to the running v3 instance (spec §16.6): the agent runs one
known query and gets the expected answer. Without it, the migration is
completed but not verified.

## 7. Real-but-not-in-effect

For each check, confirm it reads the **running** v3 instance's state (not a
staged copy) and that the running code revision equals the one that was
qualified.

## 8. Checks must read real state

Agent-run checks are themselves verified against a second read (by a coding
agent or an operations session) before a failure blocks cutover: in
v2, two of one agent's findings came from a lookup of a nonexistent name and
from a stale read. A check that cannot find what it looks for reports
"not found", not "missing" or "unbound".

## 9. Reconciling verdicts across agents

Checks run per agent. After every agent has reported, the coding agent and
the operator compare all verdicts and requested changes, resolve any
divergence (between agents, or between an agent's check and a second read),
and record the resolution here before cutover proceeds (checklist A7).

## Agent review

### Alpha — 2026-10-12 (approved with amendments)

Transcribed by the coding agent from the operator's relay. Approved; two
requests applied 2026-10-12: §3 now requires the owning agent's signature on
the known-state record; §9 adds cross-agent reconciliation. Her non-blocking
note (inside access is itself an acceptance item) is applied as §6a.
