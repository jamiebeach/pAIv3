# The reply check: what the senses perceive now counts as evidence

Bead v3-baw.1.5, for [Having a body](../embodied_somatic_awareness_specification.md)
§5.7. **Draft for Specter's review; no code until it is agreed** (the crew,
2026-11-15). The check is `%reply-problem` in `src/mind/thought.lisp`.

## 1. The problem

Before a final reply to the operator goes out, one small judgment asks: does
it state the operator's present state, plans or doings as fact with nothing
they said behind it? Its evidence is `asked` (the message answered) and
`operator_said` (what they said in the last six hours). Nothing else.

So "you're back at your desk", said because a camera sees the operator at
the desk, is judged ungrounded. The reply is held back and rewritten as a
guess, though it was not a guess. The senses were right; the check could
not see them.

## 2. The change

One more field for the judge, `perceived_now`, present only when a sense has
something current:

```json
"perceived_now": [
  {"sense": "sight",    "what": "Desk camera: 1 present, possibly the operator (similarity 0.82)", "minutes_ago": 0},
  {"sense": "location", "what": "Phone: driving (62 km/h)",                                         "minutes_ago": 1}
]
```

**What counts.** Only a current perception by one of the agent's senses:

| Counts | Why |
| --- | --- |
| Who a camera sees now, or that it sees no one | sight: an observation, with its own expiry |
| Where the operator's phone is now: a named place, or driving | location: a fix, with its age |

**What does not**, and why each is excluded on purpose:

| Excluded | Why |
| --- | --- |
| The calendar | A plan, not a perception. "You're in your 10:00" from a calendar is exactly the assumption this check exists to catch. |
| Anything the agent inferred or remembers | Not a perception now. |
| A camera that is off or lost | It perceives nothing; saying "no one is there" from it would be wrong. |
| The agent's own body percepts (v3-baw.1.2) | About the agent, not the operator. |
| A visitor's view of the agent's place (Phase D) | A camera angle is not a person. Never evidence about the operator's body. |
| Anything older than 15 minutes | A perception goes stale. Sight already expires its rows; a location fix older than this is left out. |

**Where it comes from.** The mind cannot read the senses directly (they
live in `src/api/`, which loads after it). It gets a hook:
`*perceived-now*`, functions of the kernel each returning a few
`(:sense :what :minutes-ago)` items. `perception.lisp` and `location.lisp`
register one each. The lines are composed on the agent's side from their own
rows (as the present's lines already are), never from text a client sent. At
most eight items and 1,200 characters; past that the oldest go.

**The question.** Its instruction gains one sentence, only when the field is
present:

> What the agent's own senses perceive now is in perceived_now, with how
> long ago. A statement that one of those perceptions supports is grounded.
> A plan, an appointment or a habit is not a perception and supports nothing
> about what they are doing now.

The first criterion becomes "nothing the operator said, and nothing
perceived now, supports it". When `perceived_now` is absent the request is
byte for byte what it is today.

**The rewrite.** If a reply is still caught, the note that asks for it
(`+grounding-nudge+`) names what was missing: "nothing they said, and nothing
your senses perceive now".

Unchanged: one judgment per thought, the 3-second timeout, never a problem
when the judgment fails, the operator's audience only.

## 3. Tests (synthetic)

1. A camera sees the operator at the desk; "you're back at your desk" passes.
2. Only a calendar entry for a meeting; "you're in your meeting" is caught.
3. The same claim with the camera off ("lost"); caught.
4. A guess or a question ("are you back at your desk?") passes, as today.
5. No senses configured: the request sent to the judge is identical to
   today's, compared byte for byte.
6. A location fix 20 minutes old is left out; one minute old is included.
7. A perception line can never carry text another program sent: a
   recognition candidate is a name from the operator's own list
   (`perception.people`), so a candidate that is not on it is shown as
   "not known".

## 4. Qualification, before it reaches Aria and Angel

`inspect flags` lists replies the check caught. On Specter, take the last 30
caught for grounding, and replay each through the judge with and without
`perceived_now` as it stood then (where a camera or fix was live). Record:
how many change verdict, and for each that does, whether the senses really
supported it. A changed verdict with no real support is a defect, and stops
the rollout.

## 5. Questions for Specter

1. Is a location fix a perception of the operator, or only of the phone?
   (Proposal: of the phone, and the line says so: "Phone: driving". The
   judge then decides whether "you're driving" is supported, as it would
   for a person who saw the phone move.)
2. Fifteen minutes: right for both senses, or should sight keep its own,
   shorter, expiry?
3. Should "no one present" from a working camera count? It supports "you've
   stepped away", which is a claim about the operator. Proposal: yes, with
   its age.
