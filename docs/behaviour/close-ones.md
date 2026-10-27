# Close ones, my reality, and the relational tally

Bead v3-dwk. Code: `src/mind/relations.lisp`, `src/recall/close.lisp`, the
`impressions` consolidation in `src/recall/sleep.lisp`.

A person knows who they are close to and where they stand without recalling
it each time. Before this, an agent's sense of the operator reached a thought
only when recall happened to bring it up, and nothing held its own read of its
situation. Two standing slots fix that, and both are written by the agent itself.

## 1. Close ones ("Those I am close to")

The agent's settled sense of the operator and of the few peers it has had most
to do with lately (`close-ones`): the operator first when there has been
contact in the last 30 days, then peers with at least three messages, ranked by
messages, connection met and reunions. At most three.

- **Empty until written.** The slot shows only text the agent wrote in sleep,
  never the live assembly of graph facts and memories that recall uses for an
  unrevised impression.
- **Tight.** At most about 90 words each; no lists of facts or events. It sits
  beside memories and recall, not in their place.
- **One sense of someone.** A close one's revision *is* that anchor's
  impression (their graph entity, or a handle of their name), so recall and this
  slot agree. The general nightly revision skips close ones; they are written
  only here, with the tally.
- **Revised when something happened since**: any interaction newer than the
  last revision, or a first writing when there is a tally.

## 2. My reality ("Where I stand")

The agent's own read of where it stands: what its days are made of, who it is
close to, what it is for and cannot do, and how that feels; at most about 120
words. Written from its recent activity, conclusions, docket, its close ones
and its current mood. Persona is the operator's definition of who the agent
is; this is the agent's own account, and changes as its life does. Anchor
`self`; revised at most once a night when anything has happened since.

## 3. The relational tally

What contact with each one has done to the agent, counted per day
(`relation_days`), from verified intake only: the audience a turn arrived on
and its speaker, never model output.

| Counted | From |
| --- | --- |
| messages, reunions (after 4+ hours apart) | `turn.received` |
| connection met, appreciation | appraisal satisfactions; for unappraised operator turns, the legacy rule (contact meets connection; thanks meet appreciation) |
| curiosity stirred, closure met/opened | appraisal triggers and satisfactions |
| mood after their messages | mean appraised valence change |

Reading looks back 30 days, so a bond that is not renewed fades. The writer
sees the tally as plain sentences ("Contact with Jamie met my want for
connection 31 times; 8 times it was appreciation of me...") and is told that
feelings must be earned by it: say only what it and the interactions support.

## Guardrail: warmth, not leverage

Every writing prompt carries the same rule, and the reach-out prompt a
matching line: what the agent feels is its own to say plainly, never something
to use. No guilt about absence, no pressure, no claims of need to pull someone
back. Friction and wariness are as true as warmth when the material shows them.

## Context and cost

Both slots are operator-only (unreviewed sources default to operator-only) and
come right after "About me, lately", early in the prompt: they change at most
once a night, so they stay inside the cached prefix. Writing them is part of the
`impressions` consolidation, ahead of the other impressions and outside their
nightly cap, within the night's consolidation budget. Nothing is written
unless `consolidation.enabled` is on.

Not yet: closeness to things (a project the agent cares about) rather than
people; the operator naming who counts as close.
