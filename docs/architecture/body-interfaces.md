# The interfaces a body plugs into

Bead v3-baw.1.1, for [Having a body](../embodied_somatic_awareness_specification.md)
§6.5. Source of truth is the code; this note says what was decided and why.

A body (today the avatar plug-in) needs three things from the core. Each is
now a declared, exported interface with tests
(`tests/mind-percepts-test.lisp`). Nothing behaves differently yet: the
first users arrive with v3-baw.1.2, 1.3 and 1.6.

| Need | Interface | Where |
| --- | --- | --- |
| Say something in the present | `pai-v3.api:define-present-facet` | `src/api/attention.lisp` |
| Report what was perceived | `pai-v3.mind:define-receptor`, `emit-percept`, `recent-percepts` | `src/mind/percepts.lisp` |
| Read the agent's feeling | `pai-v3.mind:body-feeling` | `src/mind/affect.lisp` |

## 1. A facet of the present

```lisp
(define-present-facet avatar-self-lines (kernel)
  (list "..."))            ; lines, or nil when there is nothing to say
```

Facets are named functions, called by name, so a live reload replaces one in
place. The senses' and location's facets now register the same way. Before,
`perception.lisp` set the whole list, which on reload would have dropped any
facet a plug-in had added.

Unchanged: facets are shown only to the operator's own thinking, share the
present slot's token budget after the calendar, and a facet that fails is
skipped. Whether the agent's own facet should also reach thoughts for other
audiences is for v3-baw.1.3 to decide; the interface does not settle it.

Rule for anyone writing one: a facet composes its lines from known states.
It never passes on text another program sent.

## 2. Percepts

**Decision: this is the first tenant of the general recent-percepts table**
of the engineering specification §16.1, not a smaller private table. The
table is named `percepts_recent` and keyed as the specification describes,
so sight can move onto it later without a second mechanism.

```lisp
(define-receptor "avatar"
  :sense "proprioception"
  :kinds (("speech" "done" "interrupted" "not-shown" "not-heard" "failed")
          ("view" "opened" "closed"))
  :retention-seconds 3600)

(emit-percept kernel "avatar" "speech" "not-heard"
              :subject "act-1041" :salience 0.9
              :numbers (obj "played_ms" 0))        ; => id, or NIL if dropped

(recent-percepts kernel :receptor "avatar" :kind "speech" :min-salience 0.4)
```

**Codes and numbers only.** A percept is a receptor, a kind, a code, a
subject and up to eight named numbers. Every one of the first four is a
*token*: lower-case letters, digits and `. _ : -`, at most 64 characters.
There is no summary field. A view of the body runs in a browser, and what it
sends is untrusted; with no field that can hold a sentence, there is nothing
for it to say to the agent. Whatever the agent reads about a percept is
composed on its own side from the code (specification §5.4, engineering
specification §16.5).

**A closed set per receptor.** A receptor declares its kinds and codes.
Anything else is dropped, silently: `emit-percept` returns NIL and stores
nothing. It does not signal, so a malformed report cannot become an error
message that carries its text somewhere.

**Never the ledger.** `emit-percept` appends no event. Routine perceiving is
not experience. What a surprising outcome becomes (v3-baw.1.4) is decided
there and uses the ordinary paths.

**Bounds.**

| | |
| --- | --- |
| Rows | at most 256; the oldest go first |
| Row size | at most 512 bytes (enforced by the token and number limits) |
| Byte budget | 128 KB |
| Expiry | per percept, at most the receptor's retention, itself at most 6 hours |
| Pruning | on write only; nothing ticks |
| Read | by receptor or by sense, through an index; newest first, at most 64 rows a call |

**Habituation.** A percept stores its raw salience and how many percepts of
the same receptor, kind and code came before it inside the receptor's
habituation window (15 minutes unless declared). What is read back is
`salience / (1 + repeats / 2)`, the curve noticing change already uses for
surprise (`orienting.lisp`). The engineering specification keys the repeat
counter by subject; here the subject is usually the id of one act, different
every time, so the repeat that matters is "the same thing happened again".

**Where it lives in the store.** The table belongs to a projection named
`percepts` that consumes no events, with a new derivation, `:scratch`:
bounded working state that was never in the ledger. It is created and
inspected like any projection's tables, is never rebuilt (there is nothing to
rebuild it from, and `rebuild-projection` says so), and losing its rows is
not a fault. Writes take the kernel lock, as every transaction does.

**Checkable against something that is not this table** (the crew's
condition, 2026-11-15). This table is the agent's working account. It is not
the only one: v3-baw.1.2 keeps the raw report beside it, so a percept can be
checked against what was actually received.

## 3. The agent's feeling, for a body

```lisp
(body-feeling kernel)   ; => {"arousal" 0.3 "valence" 0.5 "boredom" 0.2
                        ;     "curiosity" 0.0 "social_need" 0.3}
```

Five numbers, computed on read from `affect-now`, each rounded to two
decimal places so a view does not twitch with noise. Valence runs -1 to 1;
the rest 0 to 1. Curiosity is the drive; the others are modulators. A body
reads this and nothing else of affect, so the tables behind it can change.

## 4. What the avatar plug-in still reaches into

Not part of this bead, listed so they are not forgotten:

- `pai-v3.mind::*voice-turn-notes*` (the line telling the agent what its
  avatar can perform);
- `pai-v3.mind::decide` and `pai-v3.mind::%profile` (the per-sentence
  judgment of expression, which v3-baw.3.5 changes).

## 5. Tests

`tools/run-tests.sh tests/mind-percepts-test.lisp`: a declared percept is
kept and read back, and the ledger head does not move; an undeclared
receptor, kind or code, a sentence as a subject, text where a number
belongs, and more than eight numbers are each dropped; repeats habituate and
the window passes; percepts expire, the table stays at 256 rows and a read
at 64; the table is working state; the feeling is five numbers in range; a
facet is registered once and shown to the operator's own thinking only.
