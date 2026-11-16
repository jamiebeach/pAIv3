# Body state: kept by the agent, derived on read

Bead v3-baw.3.1, for [Having a body](../embodied_somatic_awareness_specification.md)
§5.3. Code: `src/mind/body.lisp`; tests: `tests/mind-body-test.lisp`. No page
uses it yet: storing the state is v3-baw.3.2, and the page drawing it is
v3-baw.3.3.

## 1. Two things, kept apart

**The anchor** is what is stored, one per body. It is written only when
something happens: the agent starts or stops something, speaks, chooses
what to wear, goes away. It holds:

| Field | |
| --- | --- |
| `activity` | `talking`, `working`, `unfinished`, `idle`, `resting`, `away` |
| `detail` | for working: `reading`, `writing` or `thinking`; for unfinished, what was left open |
| `place`, `spot`, `facing` | where the body was: a named spot of a place, and which way it faced |
| `wearing` | as `avatar.wearing` |
| `since` | when this began (universal time) |
| `boredom`, `arousal`, `valence` | the agent's feeling at that moment |

**The body** is derived from the anchor and the time by `body-at`, a pure
function: no clock, no database, no randomness except a generator seeded by
the anchor's own `since`. Every reader at the same moment gets the same
body, and a page closed for an hour finds what an hour would have done.
Nothing ran meanwhile.

`body-at` returns the activity shown, where the body is (`spot`, or null
while walking; `x`, `z`, `facing`), `posture`, `gaze`, the resting `face`
and its `strength`, `since` and `for` (how long it has been at what it is
doing), `next_change` (seconds until the answer next changes, so a view
knows when to ask again), and the anchor it came from.

## 2. The rules

**The crew's rule, 2026-11-15:** the state when nobody looks is coarse and
true, never dice for its own sake. A body at rest stays at rest. A body at
work stays at its work, and says how long ("three hours into it").

| Activity | Posture | Gaze | With no event |
| --- | --- | --- | --- |
| talking | standing | visitor | after 10 minutes, idle |
| working | reading, writing or thinking (its detail) | task | holds |
| unfinished | thinking | task | holds, with how long |
| idle | standing; walking between spots | aside; waypoint while walking | after 90 minutes, resting |
| resting | sitting if its spot is a seat, else standing | aside | holds |
| away | away (not on stage) | aside | holds |

**Walking is idle's alone.** Idle is the one activity with nothing to hold
the body still. An idle body stays at a spot for a while, then walks to
another spot of its place at 0.7 m/s. How long it stays depends on boredom
*as it was at the anchor*: about 20 minutes when nothing bores it, about 4
when much does, each stay drawn from the anchor's own generator.

**Why feeling enters at the anchor, not at the read.** If the read used the
feeling of the moment, two reads a minute apart could disagree about where
the body had been for the last hour, because boredom moved in between. So
the anchor keeps the feeling that shapes the body's time alone, and every
read agrees. Feeling *now* reaches the body in the numbers that should
follow it moment to moment: breathing, restlessness, the face while talking
(v3-baw.3.4, 3.5). An affect change large enough to matter is itself an
event, and writes a new anchor.

**Resting face.** From valence at the anchor: above 0.35, relaxed; below
-0.35, sad; otherwise neutral, all faint. The face while talking is
v3-baw.3.5.

## 3. Properties, as tested

- The same anchor and moment give the same body (40 anchors, 8 moments each).
- Deriving in two steps gives what deriving in one does: `body-at` of a
  derived state works from the anchor it carries. Composition is true by
  construction, and tested for every activity at 30 seeds.
- Working, unfinished, resting and away never move and have no
  `next_change`.
- Talking lapses to idle at 10 minutes, idle to resting at 90.
- A bored idle body walks more than a calm one, and only to spots of its
  place.
- A walk is continuous: no step faster than walking speed (one-second
  samples over 89 minutes).
- A resting body at a seat sits.
- A read a year after the anchor is as cheap as one a minute after (idle is
  bounded at 90 minutes, so at most 64 moves are ever worked out).
- A time before the anchor reads as the anchor.

## 4. How far one read may advance

Unbounded, and cheap. Every phase that lasts is still, and the one that
moves (idle) ends at 90 minutes, so the work of a read does not grow with
the time since the anchor.

## 5. Left for later beads

- Storing anchors, and which events write one: v3-baw.3.2.
- Places as records, and spots the agent names: Phase D. Until then there is
  one place, `home`, with five spots.
- After an idle walk ends in rest, the body faces the anchor's direction
  rather than the way it walked. Harmless until a view draws it; fix with
  v3-baw.3.3 if it shows.
