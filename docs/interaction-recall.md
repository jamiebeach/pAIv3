# Interaction recall

pAI v3 design. Status: draft for review, 2026-10-17. Extends
[The continued-experience agent](the-continued-experience-agent.md) §11
(*recall is deliberate or associative: the present brings something to
mind*) with the mechanism for the associative half. It also takes up the
operator's System-1 design note, §9 *Associative Recall*: "System-1 should
initiate retrieval automatically when an event activates known entities,
goals or semantic neighborhoods."

## 1. The case

An agent decided to send a peer a picture it had posted. Before writing, it
listed its peers. The list held a name, an id and an address. The agent
then wrote "you're an instance I haven't met — hello". Two things made that
wrong: the two had exchanged welcomes that morning, and the agent's own
persona names the peer as its sister. Nothing was missing from storage.
What failed was recall at the moment it was needed: looking the peer up
returned a roster entry, not an acquaintance.

The obvious fix, a standing "my peers" section in every thought, is the
wrong shape. A person does not hold everyone they know in mind all day.
When something brings someone up (their name, a reason to contact them,
their message arriving) that cue brings back what matters: who they are,
when you last spoke, what is open between you. Nothing is carried ahead of
time. It is recalled at the point of use.

Peers are one case of a general ability. The same holds for the people the
operator mentions, a file last edited weeks ago, a bead commented on
before, a calendar contact, a project, an email sender. This design covers
all of them.

## 2. Principle

**Interactions are cues, and cues activate what they touch.** Anything the
agent perceives or does activates what it names exactly and what it
resembles in meaning. An
activated entity brings its impression (§5) into the thought, briefly,
once, at the moment it becomes relevant. Nothing is added to a thought's
context ahead of time.

Recall is triggered from two sides:

- **Perceived.** Something arrives: the operator's turn, a peer's message,
  an email, a tool result that names something.
- **Enacted.** The agent acts toward something: writes to a peer, mentions
  a person, opens a file, comments on a bead, looks up a contact. Deciding
  to act on something is itself a cue, as deciding to call someone brings
  them to mind before the phone rings.

The System-1 note frames associative recall around perception. The case
above shows the enacted half matters as much.

## 3. Cues

A cue is the text and the identifiers of one interaction:

| Source | When | What is scanned |
| --- | --- | --- |
| Stimulus | a thought starts | the turn, message, notice or work item that started it |
| Tool call | the agent calls a tool | the call's arguments |
| Tool result | the tool answers | the result's text and identifiers |

Only these three hook points exist. A thought has one start, and every
action it takes passes through `act` (src/mind/thought.lisp), where a note
on a repeated call is attached today. Recall attaches in the same place.

## 4. Activation: from a cue to what it touches

There is no list of kinds. Kinds of things are data, learned from the
agent's own experience, not code written for each one (operator decision
2026-10-17: accept the noise of association for its generality). A cue
activates memory by two passes, merged:

**Exact: handles.** Names of people and things are already the graph's
(`graph_names`); they stay there, the one home for names. Identifiers the
graph does not hold go in one table of handles (`experience_handles`, kept
by the experience index, §7), each bound to what it means. Together they
map any **handle** to an **anchor**. A
handle is a string that refers to something: a name, an alias, a peer id,
a stored image id, a file path, an email address, a bead id. An anchor is
the thing it refers to, usually a graph entity, otherwise an anchor of its
own. Nobody writes a resolver per kind. Handles are **bound from events**,
as they happen, wherever an event pairs an identifier with what it
identifies:

- a peer joining binds its id and its name;
- an image being made binds its id to its prompt and what it was made from;
- a board post binds its thread and message ids to the peer and the gist;
- a file written in the workspace binds its path to what was said about it;
- a graph entity's names and aliases are handles for it, as `graph_names`
  is now.

At a cue, every token sequence that is a known handle activates its anchor.
This is cheap (an indexed lookup, like `entities-named-in` today) and
exact. It is the only way an opaque identifier is ever recognised:
`49463f05-c184-…` has no meaning to an embedding, and two such ids look
alike to one.

**Associative: meaning.** The cue's text is embedded, with the same local
model memories use, and matched against three indexes, each embedded by
that model:

| Index | Holds | Kind of memory |
| --- | --- | --- |
| interactions | the gist of each past interaction (§7) | episodic: what happened, when |
| facts | each active graph fact's statement (§7a) | semantic: what is true |
| memories | the agent's memories (`memory_vectors`, as now) | what was learned |

The nearest few above a similarity threshold in each activate, with the
anchors they touch: the entities an interaction touched, a fact's subject
and object, a memory's subjects. This catches everything said in words:
"sending my sister the portrait" is near "we exchanged welcomes this
morning" (an interaction) and near "she is the production instance, and my
sister" (a fact) without either naming the other. A person recalls both
from one cue, what happened and what they know; this does the same. It is
a partial cue completing a whole memory, the way human recall is
content-addressable.

Entities themselves are not embedded: a bare name carries little meaning
as a vector. An entity is reached through its facts, its handles, and the
interactions that touched it.

The exact pass is precise and narrow; the associative pass is broad and
noisy. Both feed the same gate.

**Gating.** What the two passes bring up activates only when it matters:

- **Once per thought.** An anchor recalled earlier in the thought is not
  recalled again (habituation: you do not re-remember someone at every
  sentence of a call).
- **Salience.** An exact match on the target of an action (the peer written
  to, the file opened) always activates. Anything else is scored: the
  similarity of the match, the anchor's weight (graph facts, how often it
  is interacted with), how recently it was touched, and whether something
  is open about it. Only scores above a threshold activate.
- **Budget.** At most a few anchors per hook, and a token budget per
  recollection (the cheap tiers of the System-1 note §9.1: direct lookup
  and recent episodes, never deep retrieval).

Association is noisy by nature: some recollections will be beside the
point. Each one is logged as an event with what activated it and its score,
so the threshold is tuned on what actually came up, and the agent can say a
recollection was not relevant.

## 5. Impressions: what comes back

What an activated anchor brings back is its **impression**: the agent's
sense of who or what it is. Recall returns impressions and nothing else, so
everything that uses recall depends on this one shape, and improving what
an impression holds never changes its callers (operator decision
2026-10-17: build the lasting form from the start, not a throw-away one).

An impression has two parts, as a person's sense of someone does:

1. **Settled.** The durable sense of the thing: who or what it is, what
   matters about it, how the agent feels about it. It is revised only in
   consolidation (§5a), never on every interaction.
2. **Fresh.** What happened since the settled part was last revised: the
   interactions with it (§7), each a line with its time in the operator's
   zone and how long ago it was ("you exchanged welcomes this morning,
   08:14", "you last edited this file 12 days ago"), newest first, within
   the budget. The fresh part is always read live, so it is never stale.

Below both, **what is open**: a promise, a concern, a work item, an
unanswered question, or a prospective intention (§6) that names it.

Each impression is marked as recall ("Recalled about Sam:"), so the agent
knows it is memory, not new information, and can say so.

Where it goes:

- **At a stimulus:** into the thought's context, beside the task.
- **At a tool call or result:** appended to that tool's result, as the
  repeated-call note is. The agent reads it in the same step, before it
  writes. Looking up a peer would have returned the acquaintance with the
  roster entry, and the message would not have said "haven't met".

The agent can always search deliberately (`recall`, `knowledge`).
Interaction recall makes the obvious retrieval automatic, and only at the
moment it is needed.

### 5a. Forming and revising impressions

An impression is an event (`impression.revised`: the anchor, the settled
text, its sources, and the interaction it was revised through) with a
projection holding the latest per anchor and an embedding of the settled
text. Every revision is kept, so how the agent's sense of someone changed
is visible.

The settled part grows in stages; the shape stays the same.

- **First form: rows.** With no revision yet, the settled part is
  assembled from rows: the strongest few graph facts about the anchor and
  its few most significant memories (by memory strength, agent §11). The
  first consolidation pass records these as the first revision, so the
  fresh part starts from there.
- **Written form.** Consolidation in sleep (agent §13) revises each
  impression whose fresh part has grown: from the last settled text and
  the fresh interactions, a model writes the new settled text, which keeps
  what still matters, folds in what is new, and lets go of what has faded.
  The fresh part then starts again from that revision.
- **Fading and keepsakes.** What the settled text carries fades unless it
  is revisited or significant: each source keeps its strength, decays with
  time, is renewed by recall and contact, and a few significant ones are
  kept for good. Consolidation keeps the strongest and lets the rest go;
  they stay in the record, only out of the impression.
- **Recalled by meaning.** The settled text's embedding joins the indexes
  of §4, so an impression can itself come to mind from a cue that matches
  the sense of someone rather than any one fact about them.

## 6. Prospective memory

"When I next see her, tell her X" is a memory that fires on a cue, not at
a time. The agent can attach an **intention** to an entity: what to do, and
optionally until when. When that entity next activates, the intention comes
back in the "what is open" line of its impression. It stays there until the
agent marks it done or lets it go. Intentions are events with their own
projection, like concerns. They are kept, never silently dropped, and the
operator can see them.

v3 has no way to do this today. Schedules fire on time, and concerns
resurface by appraisal; neither fires on contact.

## 7. Recording interactions

An impression's fresh part (§5) and the associative pass both need a
record of interactions. Today an agent's exchanges with peers leave no trace in its
own memory: a peer's message is handled by a separate peer thought, and its
words go to the boards, not to the agent's episodes or summary. An
**interaction index** fixes this. It is the experience index that already
serves `search_experience` (src/mind/experience.lisp), extended rather
than duplicated: for each
interaction (a turn, a reply, a board post sent or received, a
notification, a tool action), records:

- when, and the direction (perceived or enacted);
- the anchors its handles touched;
- a one-line gist taken from the event itself (no model writes it);
- the gist's embedding, computed in the background as memories' are, so a
  slow or missing embedder delays association but never blocks recording.

The same projection binds the handles of §4, since it is the one place
that sees every event that pairs an identifier with its meaning.

### 7a. Fact embeddings

Every active graph fact's statement (graph facts are written as plain
sentences) gets an embedding, kept beside the graph in the same encrypted
store, computed in the background when a fact is admitted, and dropped
when it is retracted. Imported graphs are backfilled once. The cost is a
few thousand vectors per agent, small beside the memories each already
holds.

Fact embeddings serve the graph as well as recall:

- **Duplicates.** Formation can see that a proposed fact says what an
  existing one says in other words, and support it rather than add a near
  copy.
- **Vague relations.** A `related_to` fact whose statement is near facts
  with a precise predicate is a candidate for correction.
- **Merge candidates.** Two entities with different names whose facts are
  near each other may be one thing; the operator or the agent decides, as
  with any merge.

Episode sealing should also cover peer exchanges, as it covers
conversation with the operator. A significant exchange with a sister then
becomes a long-term memory that ordinary recall can find.

## 8. Privacy

Recollections are filtered by the thought's audience before they are
assembled, as memory recall is now (spec §13.2 of the System-1 note;
`withheld-topics`). A thought for a peer does not recall the operator's
private facts about a third party merely because the peer named them.

## 9. Building it

1. The interaction index and handle bindings (§4, §7): a projection over
   existing events, with backfill from the ledger and background
   embedding of gists.
2. Fact embeddings (§7a): the vectors, background computation on
   admission and retraction, and backfill of the existing graphs.
3. Impressions (§5, §5a, first form): the event and projection, settled
   from rows, fresh read live, and a first pass for every anchor with
   facts.
4. The mechanism: the three hook points, the exact and associative passes,
   gating and budget, and impressions in the stimulus context and in tool
   results.
5. Prospective intentions (§6): the event, the projection, a tool, and the
   "what is open" line.
6. Episode sealing of peer exchanges (§7).
7. Impressions written in consolidation, with fading and keepsakes, and
   their embeddings recalled by meaning (§5a).
8. Graph formation using fact embeddings (§7a): duplicates first, then
   vague relations and merge candidates.

Steps 1 to 4 fix the case in §1, and recall every other kind of thing at
once: no step adds a kind. Step 7 deepens impressions without changing
anything that uses them.

## 10. Decisions and directions

Operator answers to the questions this draft opened (2026-10-17):

- **Thresholds:** start conservative, with fewer and surer recollections,
  and tune on the logged recollections and on how often the agent calls
  one beside the point.
- **Which handles bind:** only strings specific enough to be worth an exact
  match (an id, a path, a name), never a common word. Start with the
  identifiers events already name, and names from the graph.
- **Unbidden thought:** yes. A recollection whose "what is open" is pressing
  may start a thought of its own (System-1 note §9), as something
  remembered can interrupt a person. It goes through the same gates as any
  initiative: the quiet budget, the wake caps, and the rule that an
  unbidden thought stays internal unless the agent decides to speak.
- **What a recollection is:** an **impression** per anchor, from the
  start (§5): what the agent knows and feels about someone or something,
  kept up to date, carrying recent facts and memories, with what fades
  unless revisited and a few significant things that never do. Its first
  form is assembled from rows; consolidation then writes and revises it
  (§5a). Nothing built first is thrown away.

## 11. How it is built: one place for each thing

v2's worst habit was several pieces of code doing similar things, so that
one change had to be made in many places to take effect. This design
touches memory, the graph, context assembly, the thought loop and (later)
consolidation, so it could easily repeat that. These rules prevent it.

**Placement.** Interaction recall is System-1 work: automatic, cheap, below
deliberation. It lives in its own module and package (`src/recall/`,
`pai-v3.recall`), loaded after the mind. The mind does not know it exists:

- The mind offers two general extension points, not recall-specific ones.
  **At a stimulus**, context slot sources (`define-slot-source`): a module
  registers a source by keyword, the context profiles name it, and a source
  that is absent reports itself unavailable without failing assembly. This
  already exists; recall uses it rather than adding a second mechanism.
  **At each tool call**, step notes (`define-step-note`, src/mind/thought.lisp):
  registered functions whose notes are shown with the call's result. The
  repeated-call note is the first step note, so `act` no longer
  special-cases it.
- Recall registers itself on both. With the module removed, the mind still
  runs and only recall is gone. Later System-1 work (perception, appraisal,
  unbidden thought) uses the same two points.

**One of each thing.** Where recall needs something that exists, the
existing code becomes the one shared implementation, and nothing is copied:

| Need | Today | Becomes |
| --- | --- | --- |
| Embedding text, storing vectors, nearest neighbours, background backfill | inside `memory.lisp`, for memories only | one vector facility (embedder, vector store, distance functions, one background queue) used by memories, facts, interaction gists and impressions alike; `memory.lisp` becomes one of its users |
| Exact names | `graph_names` and `entities-named-in` | the handle table (§4); graph names are handles, and `entities-named-in` is a handle lookup, not a second index |
| What the present brings to mind | the `:knowledge` slot (names in recent dialogue, operator thoughts only) and the `:recall` slot (memories near the task) | the stimulus pass of interaction recall; both slots are **replaced**, not kept beside it |
| What an audience may receive | sensitivity scopes and `withheld-topics` | the same filter, applied once, before impressions are assembled |
| What is open about something | concerns, the docket, schedules, each in its own module | each module registers one function answering "what is open about this anchor", and recall asks them; recall never reads their tables |
| Writing with a model in sleep | not built yet | the consolidation module, when built, owns all sleep-time writing; recall asks it to revise impressions and makes no model calls of its own |

**Working rules.**

- Each build step names what it replaces, and removes it in the same
  change: no step leaves an old path running beside its successor.
- Before a step adds a function, it looks for one that already does the
  job. If a second caller needs something private to a module, that thing
  moves into a shared place first, and then both use it.
- Recall reads other modules through their public functions and
  projections, never their tables, and writes only its own events
  (`impression.revised`, intentions, recollection logs).
- The lint and query-plan checks apply as everywhere: every query indexed
  and bounded.
