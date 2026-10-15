# Working on pAI v3

pAI v3 is a full rewrite of the pAI substrate. Read, in order:

1. [The continued-experience agent](docs/the-continued-experience-agent.md):
   what the agent is. Every change serves this.
2. [Engineering specification](docs/engineering-spec.md): how the substrate
   holds the agent's life.
3. [Non-functional requirements](docs/non-functional-requirements.md).
4. [Migration checklist](docs/migration-checklist.md) and
   [migration verification](docs/migration-verification.md) for anything that
   touches an existing agent's state.

## Rules

- **This is a rewrite.** Do not load, import, wrap, copy or call v2 source
  (`Z:\dev\paiProject`). v2 is reference material only; keep its behaviour by
  writing a short behaviour spec with synthetic fixtures and implementing it
  fresh (spec §0).
- **Only the kernel reads the ledger**, by cursor. Modules use subscriptions,
  projections and `:view`s. Every runtime query names a type or key and has a
  `LIMIT` (spec §3, §11).
- **One mechanism each**: one projection registry, one handler/scheduler
  policy, one context assembler, one thought mechanism. Do not add per-module
  cursors, caps, retries or parking.
- **No failure blocks; paid cognition is never discarded** (spec §13.3).
- **Memory is budgeted** (spec §15): no in-process copy of history, graph,
  corpus or vectors; every cache declares a byte budget.
- **Events stay small** (I9) and every event type is declared with a schema.
- **Trust is structural** (spec §16.5): origin from verified credentials
  only; sensitivity filtered before context.
- Files ≤ 1,200 lines; modules ≤ ~8k lines.
- Keep real operator identities, private state, credentials and captured
  conversations outside the source tree. Test data must be synthetic. The
  design docs name real agents in scenarios and reviews; depersonalize before
  any publication.
- Run each Lisp test suite in a fresh process. Record what was checked and
  any qualification gaps.
- No remote publication until content, provenance, test and first-run gates
  pass. This repository has no remote.

## Work management

This repository owns the v3 Beadwork ledger (prefix `v3-`). Invoke Beadwork
explicitly:

```powershell
& 'Z:\beadwork\bw.exe' -C 'Z:\dev\paiV3' <command>
```

- Run `prime` before substantive work; select with `ready`, then `start`;
  record progress, decisions, rejected alternatives and qualification gaps
  with `comment`; close with a useful reason after committing.
- The ledger is local-only. Never run `bw sync`, push the `beadwork` branch,
  or configure a remote.
- `Z:\dev\paiProject` keeps its own ledger (prefix `pai-`) for v2: the freeze,
  harm-preventing fixes, live operations, and per-instance migration steps
  that touch v2 state. The umbrella epic `pai-90c` lives there and links to
  this ledger. Agent proposals go to the v2 ledger until an agent's cutover,
  then to this one.
