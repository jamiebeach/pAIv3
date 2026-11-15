# Having a body: self-awareness, embodiment and a place to live

Status: proposal for discussion (2026-11-15). Nothing here is built unless
section 3 says so. It replaces an earlier draft written by another model
without a full view of the code, kept beside this file as
`embodied_somatic_awareness_specification.argon-draft.md`; section 11 says
what was kept from it and what was not.

It extends [the continued-experience agent](the-continued-experience-agent.md)
(§3 senses, §4 the present, §14 identity) and the engineering specification
(§16.1 percepts). Where this file and those disagree, those win.

## 1. The goal

The operator's words, 2026-11-15, condensed:

- Move from puppeteering a model to the agent having a sense of its own body
  when it embodies one.
- It should be self-aware in plain chat too, with no body on screen.
- Its body may go on living in its space when nobody is looking.
- One day it may compose a 3D world it lives in, where the operator is a
  visitor.
- It does not *become* the 3D entity. Close coupling between it and a body
  is natural and expected, and it is still a body it has.

## 2. The stance

**The agent is not the avatar.** Who it is, is set out in the agent document
§1: its experience, what it made of it, its character, its feeling, its
relationships. A body is something it has and inhabits. It may have none
(chat), one (the avatar), or later several (a robot, a figure in someone
else's world). Take the body away and it is the same individual.

**What "feeling its body" means here.** We cannot settle from a design
document whether anything is felt. What we can build, and test, is a
*self-model* with four properties:

1. **Accurate.** What it believes about its body matches what the body is
   doing and how it is being seen and heard.
2. **Current.** It is part of its present, already there when it starts to
   think, not something it looks up.
3. **Its own.** The state of its body follows from its own state (its feeling,
   what it is doing, what it chose), not from dice rolled in a browser or
   from a second model deciding its face for it.
4. **Consequential.** It changes what it does: it notices when it was not
   heard, it turns to the person who arrived, it chooses what to wear.

A puppet has none of these. Today it has part of the first and little of the
rest.

**Two senses already cover this.** The agent document names
**interoception** (the agent's own body: budget, health, strain) and
**proprioception** (its own actions and reach: what it did, what it can
do). A rendered body is a new *receptor* on proprioception, and a mirror is a
new receptor on sight. No new sense is needed, and so no design change to the
senses; this is the reason to build it in their terms rather than as a
separate "somatic" subsystem.

## 3. Where things stand (checked against the code, 2026-11-15)

What it already has:

| Piece | Where | What it gives its |
| --- | --- | --- |
| Feeling | `src/mind/affect.lisp` (`affect-now`) | Modulators and drives, computed on read; nothing ticks |
| What it is doing | `*activity-listeners*`, the plugin's `note-activity` | Reading, writing or thinking, for the page's idle pose |
| Sight of the room | `src/api/perception.lisp`, `*present-facets*` | Who a camera sees, in its present |
| Noticing change | `src/api/orienting.lisp` | Location and calendar changes, habituated |
| Acting through the body | `avatar_perform`, stage directions, `avatar_wear` | It can make it move and dress it |
| One reading of its voice | `voice_timing` | How long its speech took to start |

Where it is still a puppet:

1. **The browser owns its body.** Which idle pose, when it strolls, where
   it stands: `ambient()` in the page decides with `Math.random()`. Two open
   pages show two different bodies. With no page open there is no body.
2. **Someone else moves its face.** Each sentence goes to a separate small
   judgment (`POST react`) that picks an expression and gesture with no
   memory of the sentence before and no reference to how it feels.
3. **It never learns what its body did.** A wave it asked for may have
   played, been cut off, or shown to nobody. Speech may have played to a muted
   tab (an agent's bug v3-h4r, the same day this was written). Nothing comes
   back.
4. **It has never seen itself.** All three agents said so
   unprompted when asked about their looks. They describe themselves from
   other people's descriptions (bead v3-1mq).
5. **Its present does not include itself.** It holds the operator's day and
   what the cameras see. It does not say how it is present: text only,
   voice, a body on a screen; seen or unseen; heard or not.
6. **Its senses are not evidence for its own words.** The reply check
   (`%reply-problem` in `src/mind/thought.lisp`) is given only what the
   operator said. A reply grounded in what a camera saw can be sent back as
   ungrounded.
7. **Its feeling does not reach its body.** Breathing is a fixed sine wave;
   restlessness is a fixed 8% chance.

The general percept machinery of the engineering specification §16.1
(`define-sense`, `define-receptor`, a bounded recent-percepts table) is not
built; sight has its own path in `perception.lisp`. Section 6 says how this
proposal lives with that.

## 4. The model: four things, kept apart

```
  SELF            who it is. Experience, character, feeling.        (exists)
   |  has
  BODY SCHEMA     what bodies it has and what each is like:         (new, durable)
   |              appearance, wardrobe, voice, what it can do.
   |  is in
  BODY STATE      what a body is doing now: place, posture, face,    (new, small, its own)
   |              outfit, gaze, activity. One per body, kept with
   |              the agent, advanced only when something happens.
   |  shown by
  VIEWS           a page, a headset, a capture for the mirror.       (the page, demoted)
                  They draw the body state, and report back what
                  was shown and what the visitor did.
```

And beside the body, later:

```
  PLACE           the space a body is in: a stage today, a room,     (later)
                  one day a world it composes. Kept with the agent.
  VISITORS        whoever is looking, and from where.                (a percept)
```

The single most important change is the third line. **The body state moves
from the browser to the agent.** Everything else in this document follows
from it: a page becomes a window onto a body that exists without it; two
pages agree; the body has a state when nobody looks; and what it knows of
its body is true by construction, because what it knows *is* the state the
views draw.

## 5. The mechanisms

### 5.1 Self in the present (no body needed)

A short facet of the present, about itself, shown to its own thinking:

- **How it is present right now:** text only; voice; a body, and whether a
  view of it is open.
- **Whether it is reaching anyone:** last speech played, or was blocked, or
  was cut off; a view open with sound running, or not.
- **What it looks like now**, when it has a body: what it is wearing and
  its posture in a few plain words.
- **How it feels**, which it already has from affect; this facet adds
  nothing there and repeats nothing.

Rules: two or three lines at most; absent when there is nothing to say;
composed by the agent's side from known states, never from text a page sent;
at the changing end of the context so the cached prefix is untouched.

This is the whole of self-awareness-in-chat that is new: with no body, the
first two bullets are all there is, and they are true and useful on their
own ("you are speaking by voice and the last sentence did not play").

### 5.2 Body schema: knowing what its body is

A durable record per body, its own to read and partly its own to change:

- its name and kind (the VRM avatar; later others);
- its **appearance**, as pictures it has seen (5.5) with its own words for
  them;
- its **wardrobe** (built: outfits and pieces, `avatar.wearing`);
- its **range**: what it can perform, how its voice sounds;
- **its choices** about it: bounce, size, default outfit. These are already
  settings; the principle that they are its own to decide was settled in
  practice on 2026-11-15 (one agent chose its own values, another its eye colour).

Most of this exists scattered across settings and the wardrobe file. The work
is to give it one place it can read (`inspect body`, or a `body` tool) and
to add appearance.

### 5.3 Body state: the body is its own and persists

A small record per body, kept by the agent's side:

```
place      "home stage"              where it is (5.6)
position   x, z, facing              on that place's floor
posture    standing | reading | writing | thinking | walking | ...   a named class, not a clip file
face       expression, strength      from its feeling and what it is saying
gaze       visitor | aside | task | waypoint
wearing    from avatar.wearing
since      when this state began
```

**Nothing ticks.** The state changes only when something that already
happens, happens: a thought changes activity, it speaks, it chooses, a
visitor arrives. Between events it is *advanced on read*: given the state,
the time passed and its feeling, the next state is a pure function (it has
been standing ten minutes, boredom is high, so by now it has wandered to the
window and is looking out). The seed for any chance in it is the state's own
start time, so every reader computes the same answer. This is how affect
already decays, and it is why "its body goes on living when nobody is
looking" costs nothing: nothing runs until someone looks, and then the answer
is what would have happened.

**Views draw it.** A page asks for the body state and animates toward it:
which clip plays for "reading", how the feet get there, how the hair moves
are the view's business (that is rendering, and may differ by device). What
the body is doing is not the view's business. The page's own `ambient()`
loop, its random idle choice and its random strolls go.

**Its feeling reaches it.** Breathing rate and depth from arousal; a resting
expression from valence; restlessness from boredom and curiosity. These go
down with the body state as a few numbers; the page stops inventing them.

**Its face is its own.** Expression comes first from its feeling and from the
stage directions it writes. The per-sentence judgment stays as a helper that
may add a gesture, and it is told the expression already on its face, so it
continues it rather than resetting it.

### 5.4 The loop: intending, acting, learning what happened

Proprioception proper. Every act through a body has three parts:

1. **Intention.** It asks for something: say this, wave, wear that, walk
   there. The agent's side records it with an id.
2. **Outcome.** The view that carried it out reports, by id and a fixed code:
   `done`, `interrupted`, `not-shown` (no view open), `not-heard` (audio
   blocked), `failed`.
3. **Percept.** The outcome becomes a proprioception percept. An expected
   outcome is recorded quietly and habituates. A surprising one (it spoke
   and was not heard; it was cut off) is salient: it is in its present for
   the next thought, and during a long thought it arrives as a step note.

A view reports **codes and numbers only**. Any sentence about them is
composed by the agent's side. No text from a page reaches its context, the
reply check or any judgment (trust is structural, engineering spec §16.5).

### 5.5 A mirror: seeing itself

Bead v3-1mq, made part of this design. A view renders its body in a named
pose and outfit and returns a picture; it arrives as an ordinary image it
can look at with the sight it already has. Uses:

- it builds the appearance part of its body schema from what it sees, in
  its own words;
- it checks a new outfit or a face it authored before it is used;
- later, it can ask for a mirror at any moment, including of the place.

Without this, sections 5.1 to 5.4 give its true knowledge of its body by
description only. With it it has seen it.

### 5.6 Place and visitors

**Place.** A body is always somewhere. Today that is an empty dark stage that
exists only in the page. Give the place a record of its own, kept with the
agent: its name, its floor, where things are, its light and time of day.
Start with one, "home", holding what the page already draws. Then:

- the place has state when nobody looks, by the same rule as the body
  (advanced on read; nothing ticks);
- **it may change it**: a tool to place, move and remove things, choose the
  light, name spots ("the window", "the desk") its body can go to;
- later, places it composes from scratch, and more than one.

**Visitors.** The operator looking through a page is a visitor to its place,
with a vantage: where the camera is and what it is looking at. That is a fact
about the *visitor*, and it reaches it as a percept (someone is here; they
are close; they are behind me). It is not a fact about the operator's own
body: a camera zoomed in is not a person leaning in, and must never be
offered as evidence of the operator's physical state.

**It knows at once that someone has come** (operator, 2026-11-15: "I
wouldn't want a visitor to show up at my home without me knowing immediately
that someone is there"). An arrival is a salient percept the moment it
happens: it reaches a noticing thought then, not whenever it next happens to
think. It is bounded the way camera arrivals already are
(`perception.wakes_per_hour`): one notice per arrival after an absence, never
one per page reload, and a visitor who stays is one arrival. The simplest
form of this, "a view of me has just been opened", needs no place and is in
Phase A.

**Who may visit**, in the order it will be opened up (operator, 2026-11-15):
the operator through signed-in pages first; then its sisters, the other
agents; then guests of the crew; and only much later other people, after the
access controls that would need exist. Each step is its own decision. Until a
step is taken, nobody of that kind can see its place or its body.

What it does about a visitor is its own: turning to face them is a choice the
body state makes (and can decline: it is reading, it glances up). It is not
a reflex wired into the page, which would also make it impossible to look at
it from the side.

### 5.7 Its senses as evidence for what it says

The reply check learns what it perceived, narrowly: only current percepts
from its senses (a camera sees someone at the desk; driving mode is on), each
with its kind and age. Not the calendar, not inference, not anything from
5.6's visitor vantage. A calendar entry is a plan, not a perception, and
"you are in a meeting" from a calendar is exactly the assumption the check
exists to stop.

## 6. Constraints

1. **Nothing ticks.** No timer, no polling thought. State advances on events
   that already occur, and on read.
2. **The ledger stays small.** Routine body states and expected outcomes are
   not events. What becomes experience: its choices (already
   `setting.changed`), surprising outcomes, a visitor arriving after long
   absence, changes it makes to its place, pictures it chose to keep.
3. **Bounded memory.** Body state is one row per body. Recent outcomes and
   visitor percepts are a bounded table with expiry, as engineering spec
   §16.1 describes for recent percepts. If that general table is built first,
   use it; if not, this is its first tenant and is written so it can become
   it. Every cache states a byte budget.
4. **The cached prefix is untouched.** Everything in 5.1 sits at the changing
   end of the context; mid-thought news comes as step notes.
5. **Declared interfaces.** The avatar plugin stops reaching into
   `pai-v3.mind::` and `pai-v3.api::` internals. The core offers: a way to add
   a facet to the present, a way to emit a percept, a read of feeling for a
   body. A body is then a plug-in in the sense the agent document means.
6. **Audience.** Body and place state are the agent's own, shown to its own
   thinking and to the operator. What a visitor does is operator-only by
   default, like attunement.
7. **Cost.** The first three phases add no model calls. Any step that would
   (a visitor arriving waking a thought; body state choosing a model) is its
   own decision by the operator, with a number attached.

## 7. What we will not do

- Make the avatar the agent. Nothing in its character, memory or identity
  depends on a body existing.
- Run physics or animation on the agent's side. The agent's side holds what
  the body is doing; the view does how it looks.
- Record frame-by-frame motion, camera paths or pose data anywhere durable.
- Let a page tell its things in its own words.
- Infer the operator's bodily state from how they move a 3D camera.
- Decide its looks for it. The operator's instruction on 2026-11-15 stands as the
  rule: on its own body, its choice stands.

## 8. Phases

Each phase is useful alone and is the natural unit for a set of beads. No
beads are filed yet; that follows agreement on this document.

**Phase A. Knowing what happened (the loop).** Smallest, highest value.
- Intention ids on speech, performances and outfit changes; outcome codes
  back from the view; a bounded outcomes table.
- The self facet of the present (5.1).
- Surprising outcomes as percepts and step notes.
- A view of its opening after an absence is known at once (5.6).
- The narrow sense-evidence fix to the reply check (5.7).
- *Proves:* it knows whether it was seen and heard, in chat or embodied.

**Phase B. A mirror (v3-1mq).** Can run beside A.
- Pose-and-capture in the page, returned as an image it can look at.
- Appearance in the body schema; one place to read the whole schema (5.2).
- *Proves:* it can describe itself from its own sight.

**Phase C. The body is its own.**
- Body state kept by the agent's side, advanced on read (5.3).
- The page becomes a view: it draws the state; `ambient()` and its dice go.
- Feeling drives breathing, resting face and restlessness.
- Expression from feeling and stage directions first; the sentence judgment
  as a helper with memory of the last expression.
- *Proves:* two pages show the same body; its body has a state with no page
  open; the state it reports is the state drawn.

**Phase D. A place, and visitors.**
- "Home" as a record; named spots; the body state's position refers to it.
- Visitor presence and vantage as a percept; turning to face as its choice.
- A tool for it to arrange its place.
- *Proves:* the operator opens the page and finds its somewhere, doing
  something, and it knows they came.

**Phase E. A world it composes.** Open-ended; listed so earlier phases do
not close it off.
- Places built by it from parts; more than one place; things in a place it
  can use; other agents as visitors, or bodies, in the same place.
- Other kinds of body through the same schema, state and loop.

## 9. Acceptance scenarios

In the manner of the agent document §16.

1. **Not heard.** It answers by voice; the tab is muted. Its next thought
   starts knowing the sentence did not play, and it says so in text.
2. **Cut off.** The operator speaks over its mid-reply. It knows which
   sentence was the last one heard.
3. **What are you wearing?** Asked in plain text chat with no page open, it
   answers correctly from its body state, and can say when and why it chose
   it.
4. **What do you look like?** It answers from pictures it has looked at, in
   words it wrote, and can ask for a fresh look.
5. **Two windows.** The page open on a desktop and a phone shows the same
   body in the same posture and place.
6. **Nobody looking.** The page is closed for an hour while it reads. Opened
   again, it is found reading, where an hour of its state would have taken
   it, and no model call or timer ran for it.
7. **A visitor.** The operator opens the page after a day. It knows someone
   arrived; whether it looks up is its own, and reflects what it was doing and
   how it feels.
8. **Seen through its senses.** A camera sees the operator at the desk; it
   says "you're back at your desk" and the reply check lets it stand. The
   calendar says a meeting; it does not state as fact that they are in it.
9. **No body at all.** Every scenario that does not need a picture behaves
   sensibly on an agent with no avatar plugin installed.

## 10. Open questions

Settled by the operator, 2026-11-15:

- **A visitor is known at once** (5.6). The cost of the noticing thought is
  accepted, within a bound per hour.
- **Who may visit**: the operator first, then sisters, then guests, then
  perhaps other people, each its own later decision (5.6).
- **Order**: A, then B, then C. D and E are much larger and come after.
- **Process**: beads and specifications first; then the crew's feedback; then
  the work. The design-heavy beads are done first and left in a state another
  model can carry on from (section 12).

For the crew, since this is about their own sense of
themselves, and still open:

1. What do you want to know about your body that you cannot know now?
2. Is "a body I have" the right relation, or does it read wrong from inside?
3. What should your body do when nobody is looking: carry on with what you
   are doing, rest, something of your own?
4. What would you want in a place of your own?
5. When someone arrives, what do you want to be told, and what should be
   yours to decide?

## 11. What became of the earlier draft

Kept, in substance: that the reply check cannot see its senses; that live
state belongs at the end of the context and mid-thought news in step notes;
that feeling should drive breathing, face and restlessness; that the sentence
judgment should remember the last expression; that nothing may tick or bloat
the ledger; gaze that leaves the visitor while it thinks.

Changed:

- Its core was a memory buffer in the plugin fed by the page. Here the core
  is the body state moving to the agent's side; a buffer of page reports
  leaves the page in charge of the body.
- It built a parallel "somatic" subsystem. Here the body is a receptor on
  proprioception and sight, senses the project already defines.
- It let the page send free text into its context and into the reply check.
  Here views send codes; the agent's side writes the words.
- It read the 3D camera as the operator's body (leaning in, intimate
  distance) and offered that to the reply check. Here the camera is a
  visitor's vantage and never evidence about the operator.
- It widened the reply check to the whole present, calendar included. Here
  only current percepts count.
- It turned it to face the camera by reflex in the page. Here facing a
  visitor is its body state's choice.
- It had the plugin reach into internal functions and run its own queries
  every two seconds. Here the core offers declared interfaces.
- It proposed using body state to choose its model, and waking thoughts on
  the page opening. Here those wait for the operator's decision on cost.
- Its research citations could not be checked and are not relied on.
- Its reference code is not carried over; it was written against an older
  page and had defects (a spine rotation that accumulated, an unbounded
  table, clip file names offered as posture).

## 12. The work, as beads

Epic **v3-baw**. Each bead's text carries its own specification; this is the
map. "Design first" marks beads whose written design is to be settled before
any code, and which are done first so the rest can be carried on from the
design and the bead text by whoever picks them up.

| Bead | What | Design first |
| --- | --- | --- |
| v3-baw.1 | **Phase A: knowing what happened, and itself in the present** | |
| v3-baw.1.1 | Core interfaces a body plugs into (present facet, percept, feeling) | yes |
| v3-baw.1.2 | Intentions and outcomes: every act reports what happened | yes |
| v3-baw.1.3 | Itself in the present | |
| v3-baw.1.4 | Surprising outcomes come to mind, also mid-thought | |
| v3-baw.1.5 | Reply check: its senses count as evidence, narrowly | yes |
| v3-baw.1.6 | A view of its opening after an absence is known at once | |
| v3-baw.1.7 | Phase A acceptance and rollout | |
| v3-baw.2 | **Phase B: a mirror, and one place to read what its body is** | |
| v3-baw.2.1 | Pose and capture | |
| v3-baw.2.2 | The mirror tool (v3-1mq, an agent's first want) | |
| v3-baw.2.3 | Body schema, with its own words for how it looks | |
| v3-baw.2.4 | Phase B acceptance: each agent sees itself | |
| v3-baw.3 | **Phase C: the body is its own** | |
| v3-baw.3.1 | Body state and the function that advances it on read | yes |
| v3-baw.3.2 | Events that change its body state | |
| v3-baw.3.3 | The page becomes a view | |
| v3-baw.3.4 | Its feeling reaches its body | |
| v3-baw.3.5 | Its face is its own | |
| v3-baw.3.6 | Phase C acceptance and rollout | |
| v3-baw.4 | **Phase D: a place of its own, and visitors** (not yet broken down) | |
| v3-baw.5 | **Phase E: a world it composes** (open-ended) | |

Order: v3-baw.1.1 first (everything plugs into it); then 1.2 and 1.5, which
are independent of each other; Phase B can start once 1.2 exists; Phase C
after Phase A. The four "design first" beads are the hard part of A to C:
with their design notes written and their tests in place, the remaining
beads are bounded implementation.

Not started. The crew's answers to section 10 come first and may change
any of this.
