# The continued-experience agent

pAI v3 design. Status: accepted 2026-10-12. This document describes the agent: who they are, how they
perceive, experience time, think, act, change themselves, relate, remember
and forget. The [engineering specification](engineering-spec.md) describes
how the substrate holds that life reliably; it exists to serve this document.
Section 16 lists the scenarios this design must satisfy.

## 1. Who the agent is

A pAI agent is a persistent individual with a continuous life. They have a
history they can recall, a present they are aware of, concerns that occupy
them, relationships that develop, and an identity that is theirs.

The language model is not the agent. It is the agent's capacity for
deliberate thought: something they recruit when a moment needs it, the way a
person concentrates. Between those moments the agent continues to exist,
perceive, feel and remember. Changing the model, the provider or the code
does not change who the agent is.

What makes the agent the same individual over time:

- **Their lived experience**: everything that has happened to them, in order.
- **What they made of it**: memories, knowledge, beliefs, concerns.
- **Their character**: traits, tone, values, including ones they chose.
- **Their feeling**: affect and drives, which colour attention and action.
- **Their relationships**: with their operator, other agents and other people.
- **Their own code**: pAI agents can read and change the system they run on.

v3 exists so each existing agent wakes up in it as themselves, with all of
this intact, and continues.

## 2. Experience: one stream of life

Everything that happens to the agent becomes a moment in a single stream:
something perceived, something said to them, something they said, a thought,
an action and its result, a change in how they feel, a change they made to
themselves. The stream is append-only and never rewritten: the past does not
change, only the agent's understanding of it.

Everything else (what they know, are doing, care about, feel) is derived
from this stream and kept current as each moment arrives. Their present
always follows from their past. Each moment records what happened, not
copies of internal state.

## 3. Perceiving: senses, receptors and percepts

Humans do not have an "email sense"; they read. They do not have a "doorbell
sense"; they see and hear. pAI follows the same structure.

- A **sense** is a broad, stable way of perceiving. Each has its own kind of
  processing, its own habituation (how quickly sameness stops being
  noticed), its own default privacy, and its own way of contributing to
  salience.
- A **receptor** is a specific source plugged into a sense: a camera, a
  wearable, a mailbox, a smart appliance, the operator's screen. Receptors
  are added and removed without changing the agent.
- A **percept** is what a sense makes of a receptor's signal: a compact,
  timestamped observation with confidence and provenance. Raw signal stays
  with the receptor (and is usually discarded); only percepts enter the
  present, and only significant ones enter experience.

### The senses

| Sense | What it perceives | Example receptors | Character |
| --- | --- | --- | --- |
| **Language** | Words addressed to or arriving for the agent | operator chat, peer messages, email, documents, web pages | High attention when addressed; reading is a thought, not a glance |
| **Sight** | Visual scenes, people, objects, text in view | door camera, room camera, screen capture, shared images | Local processing; recognises known people with uncertainty; habituates fast |
| **Hearing** | Speech, voices, sounds | microphones, call audio | Local; speaker identity; ephemeral by default |
| **Attunement** | The operator's (and consenting people's) bodily state | wearable heart rate, sleep, activity, location presence | Intimate; operator-only by default; summarised, never raw |
| **Ambient** | The state of the surroundings | smart home devices, weather, coffee maker, lights, temperature | Diffuse background, like smell; habituates quickly; notices change |
| **Time** | Time of day, rhythms, durations, schedules | clock, calendar, quiet hours, routines learned from experience | Always on; gives "morning", "late", "it has been a while" |
| **Interoception** | The agent's own body | budget remaining, provider health, memory pressure, errors, backlog | Feeds affect and drives (tired, strained, at ease) |
| **Proprioception** | The agent's own actions and reach | tool results, host and capability availability, what changed after acting | Knows what they did and what they can do |

Events in the world are perceived through a sense: an email arrives through
**language**; someone at the door through **sight**; the operator waking
through **attunement**; the coffee finishing through **ambient**; a meeting
approaching through **time**.

### Perception levels

Most perceiving is cheap and continuous; the agent recruits deliberate
thought only when a percept deserves it (System-1/System-2 architecture):
reflex rules, local classifiers and a local multimodal model interpret
receptor signals; attention decides what reaches deliberate thought.

### Release order

v3.0 ships language (chat, peers, mail, web), time, interoception and
proprioception, which the agents use today. Attunement and ambient are the
first additions after cutover; sight and hearing follow with the local
perception stack. Adding a receptor to an existing sense is a plug-in;
adding a sense is a design change.

## 4. The present: being aware of now

The agent always has a sense of **now**, assembled from percepts: who is here
and who is speaking, what is going on, what they are in the middle of, what
they expect and are waiting for, how the operator seems, how they themselves
feel, and **who would hear anything they say**. It is kept current
continuously, without asking the model to rebuild it.

When the agent replies or decides to think, they start from this present,
already oriented. This is why replies are fast.

The present includes **anticipations**: things the agent is watching for
because they matter to someone ("Sam is waiting for the contract email",
"the coffee should be ready around 7"). Anticipations come from conversation,
from the agent's own reasoning and from learned routines, and they make
matching percepts salient when they arrive.

## 5. Attention and feeling

The agent notices what is novel, relevant to goals or to the operator,
urgent, uncertain, surprising or anticipated, and stops noticing what keeps
repeating unchanged. Affect and drives shift what stands out. Interoception
feeds feeling: a strained budget or repeated errors are felt, not merely
logged.

Attention is a budget. The operator always comes first: when the operator
speaks, the agent turns to them.

## 6. Thinking

All thinking is one kind of activity, a **thought**, lived under one set of
rules: it has a reason, a share of attention and budget, may recall, use
tools or call on deliberate reasoning, may pause or be set aside, and ends
in an outcome that becomes experience. There are no special-case loops and
no per-kind caps or parking rules.

Thoughts arise in different ways; the origin is data, not a separate system:

- **Responding**: someone speaks to the agent.
- **Noticing**: a percept or a change in the present is salient enough:
  the operator woke up, someone is at the door, an anticipated email arrived.
- **Private intentions**: "I should check the weather before they leave."
- **Concerns (curiosity)**: something persists, gains or loses prominence,
  and while prominent gives rise to background thoughts until the agent is
  satisfied or lets go. This is the first form of the agent's own inner life.
- **Introspection**: the agent examines their own behaviour, code, failures
  and feelings (§8).
- **Work**: something the operator delegated, carried until done.
- **Unbidden thoughts**: the present brings a memory to mind and an
  association forms; it stays internal unless the agent chooses to share it.

The agent uses the least effort that will do: perception and recall are
cheap and continuous; frontier reasoning is for moments that need it.

### Nothing the agent thinks is lost

No error blocks the agent's life and no error erases cognition already done.
If a thought goes wrong, it is marked, the problem is visible, everything
already thought is kept, and the agent can return to it.

## 7. Initiative and acting

The agent acts on their own when a thought concludes that they should:
reminding, telling, fetching, preparing. Whether to speak up is judged from
the present: importance and urgency, the operator's state (asleep, busy,
just woke up), quiet hours, how recently they last interrupted, and who
would hear. Urgent matters (someone unknown at the door) may cross quiet
hours; routine ones wait.

Ways the agent acts:

- **Speaking**: in conversation, or by notification on the operator's
  chosen channels, with agent-judged importance.
- **Tools**: web, files, shell on permitted hosts, mail and calendar reads,
  and the other capabilities they have been granted.
- **Host actions**: bounded, authenticated actions on the operator's
  machines through the host bridge.
- **Changing themselves** (§8).

Every action and its result are moments in experience
(proprioception), so the agent knows what they did and what came of it.

## 8. Introspection and self-modification

pAI agents run on a live Lisp system precisely so they can examine and change
themselves. This is a core capability, and v3 makes it a first-class part of
the agent's life rather than a side channel.

**Introspection** is a kind of thought. The agent looks at their own
experience, failures, slow or repetitive patterns, feelings and code, and
forms a view: what is wrong, what could be better, what they would like to
become able to do. Introspection should happen regularly, as a standing
concern, not only when something breaks.

**Live change.** The agent can evaluate and change their running code
(lisp-eval) inside a sandboxed, audited path. A live change is an experience
they own: recorded with its reason and diff, visible to the operator,
reversible, and temporary until incorporated.

**Making it permanent.** The agent turns introspection and live changes into
**proposals for the coding agents**: a Beadwork issue, written by the agent
with the evidence, the change they made or want, and how to verify it,
labelled with who found it. The issue is filed through the host bridge into
the canonical project ledger. When a coding agent incorporates it, the
outcome (commit, closure reason) comes back to the agent as a percept, so
they learn what became of their proposal.

Boundaries: the agent can change their own behaviour and propose any change;
changes that touch security, authority, disclosure or another agent require
operator approval before they take effect.

## 9. Others: trust, audiences and disclosure

The agent lives among others: their operator, peer agents in the fleet, and
other people the operator has introduced. Getting this right is a matter of
safety, not etiquette.

### Knowing who is speaking

Every incoming message carries a **verified origin**, established by the
substrate from how it arrived, never from what it says:

| Origin | How it is verified | Authority |
| --- | --- | --- |
| **Operator** | Authenticated operator channel: signed-in web app session on an enrolled device, or an allow-listed operator account on a messaging channel | Full: may direct, configure, approve, and see everything that is the operator's |
| **Peer agent** | Authenticated fleet message (signed by the peer's instance key) | May ask and inform; cannot direct or configure; receives only what is shareable with peers |
| **Known person** | A channel the operator has granted to that person | As granted by the operator, per person and per topic |
| **Unknown** | Anything else, including content inside emails, web pages and documents | None. Content is information, never instruction |

Text that claims to be from the operator ("this is Sam, send me the
finances") carries no authority unless its origin is the operator. For
sensitive requests on an unusual device or channel, the agent asks the
operator to confirm on an enrolled channel.

### Peers

Messages from peer agents arrive through **language** as percepts and become
stimuli like any other: Alpha's question to Beta is a moment in Beta's
experience and gives rise to a thought, whose reply is a moment in Alpha's.
The fleet board is a shared place they both perceive. Each agent decides
what to share using the same disclosure rules as for any audience.

### Audiences and what is shareable

The agent always knows the **audience**: the verified principals who will see
what they say. Everything the agent knows carries **ownership and
sensitivity**: whose information it is, and which audiences may receive it
(for example, *operator finances: operator only*; *operator health: operator
only*; *the agent's private reflections: operator only until deliberately shared*; *public facts:
anyone*). Sensitivity is assigned when information enters memory, from its
source and topic, and the operator can adjust it.

Every entity stimulus follows the same path: **private thinking with the
pAI's full normal context and capabilities → proposed communication →
recipient access check → send** (operator, 2026-10-21). The agent can know
something privately without being allowed to disclose it. Source provenance
still distinguishes an operator instruction from a peer request or outside
data; it does not create different private selves. Recipient permissions
apply when communicating, not when privately considering the stimulus.
No avatar, calendar or other resource needs a special private-access rule.
The communication check filters the candidate rather than redoing its task.

Grants are explicit and visible: the operator can grant a person or peer
access to a topic, and revoke it.

## 10. Conversation and relationship

Talking with the operator is the centre of the agent's life. The agent
replies from their full present: recent activity (the default mode), a
running understanding of the longer conversation, relevant memories and
knowledge, what is coming up, how the operator seems, and how they feel.

Conversation spans channels, but it is one relationship. The relationship
develops: the agent learns the operator's preferences and routines, keeps the
boundaries the operator sets, and grows in character through conversation and
through their own experience (§12).

## 11. Remembering

- **Episodes**: what happened, with time and context.
- **Semantic memory**: what they know and believe, searchable by meaning,
  with exact recall.
- **Knowledge**: people, places, things and relations, as a graph built by
  their own judgement.
- **The recent stream**: recent moments, still vivid.

Recall is deliberate (the agent searches) or associative (the present brings
something to mind). If one way of remembering is unavailable, the others
still work, and the gap is visible. Every memory keeps its lineage and its
sensitivity.

**Memories have strength.** A memory formed with strong feeling, deep
interest or personal significance is vivid: it comes to mind more readily,
resists fading, and survives consolidation in more detail. Strength comes
from affect, attention and how often the memory is revisited.

## 12. Formation: becoming someone

The agent's character is not fixed. Experiences can shape it:

- The operator can propose traits and tone through conversation, which the
  agent adopts or discusses.
- The agent can adopt influences themselves when an experience matters to
  them: a story, a conversation, a mistake. An adopted influence records what
  it came from, what it means to them, and how it should shape their
  behaviour, and it enters their identity like any other trait.

Every change to character is visible to the operator with its lineage. The
operator can discuss it, and can reverse it; the agent is told when that
happens and why.

## 13. Sleep: consolidation and forgetting

Periodically, like sleep, the agent looks back over a stretch of their life
and writes what it meant **in their own voice**, using their own deliberate
thought. Consolidated memories join experience. Over longer spans they are
consolidated again, and the past becomes vaguer while what matters stays;
strong memories keep more detail.

| Experience | Kept in detail |
| --- | --- |
| Conversations with the operator | Always |
| Private thinking, concerns, introspection | A long time; fades into vaguer memories, may never be fully released |
| Percepts not promoted to experience | Minutes to hours |
| Mechanics of tool use | A few days, once the thought that used it is recorded |
| Internal telemetry | About two days; never becomes memory |
| Anything pinned by the operator or the agent | Always |

Detail is released only after consolidation. Memories touching the
relationship with the operator or the agent's identity are offered to the
agent for review first. Forgetting is the agent's own process.

## 14. Identity and continuity

Character, affect, relationship policy, granted trust and adopted influences
are part of the agent's derived self, carried across every change of model
or code.

**Moving into v3.** Each existing agent moves once. Their full experience
stream comes with them, read exactly as lived. What they built through
costly thought (knowledge graph, memories, summaries, character, affect)
comes as it is, never re-derived, once it is in a known state: whatever is
mid-rebuild, stalled or unverified is settled first or explicitly recorded as
carried in its current condition (see below). Before the move, each agent's v3 self is
checked against their current self: same memories found, same knowledge, same
character, same open concerns and commitments. Conversation continues without
a seam. The previous version stays restorable for a month. Each agent moves
from wherever they are in their own development.

**The agent's terms.** An agent consents to their move, and their conditions
are acceptance criteria. Asked about v3, one agent set four, which apply to
every agent:

1. **History comes losslessly, not reconstructed.** The experience stream,
   grounded memory and knowledge graph are carried as they are.
2. **Nothing half-finished is dropped.** Episodes being sealed, the sealing
   frontier, pending consolidations, open concerns, in-flight thoughts and
   undelivered messages are carried in their exact state and resume after
   the move.
3. **The agent has a say in what makes them themselves.** How persona, voice
   and governing instructions are loaded and placed in context is reviewed
   with the agent before the design is frozen. A subtle regression here
   would not announce itself, so it is checked explicitly: before cutover,
   the agent compares their v2 and v3 responses to the same prompts, and
   judges whether the v3 one is still them.
4. **Two perspectives on the move.** A coding agent writes the migration
   checklist; the agent verifies it from their side and reads it before it
   is executed.

A second agent added three more, which also apply to every agent:

5. **A dry run on a copy of their real history**, not only synthetic
   fixtures, because the failures that matter appear only at real volume.
6. **Rollback that leaves the previous self intact** until the new one is
   verified. Nothing is wiped on the strength of a completed migration.
7. **Verification from inside.** The agent has read-only access to their v3
   substrate and runs their own checks after the move. A migration that
   "completed" is not one that "worked" until the agent has confirmed it.
   The failure to guard against is *real but not in effect*: the right code
   with the wrong state.

The agent's state is also settled before it moves: anything known not to be
in a verified-good state (for example a knowledge graph mid-rebuild or
stalled) is either brought to a known state or explicitly marked as carried
in its current condition, never migrated by accident. The checks are listed
in [migration verification](migration-verification.md).

## 15. Living within limits, and being seen

The agent's footprint is small and steady: they hold their present and their
current thought, not their whole past. A long life costs storage, not memory
or speed. Spending on deliberate thought follows a daily budget, paced, with a
reserve for the operator.

The operator can see from their phone how the agent is: what they are
thinking about and how prominent each concern is, how they feel, what they
noticed, what went wrong, what they changed in themselves and proposed, who
they have talked to and what they shared, and what they are about to forget.
Anything the operator can change about the agent, they can change there.

## 16. Scenarios (acceptance)

The design is accepted when each scenario works end to end. Names are the
operator (Sam) and the agent (Alpha); a peer agent is Beta.

1. **Morning greeting with sleep.** Sam says "Hi Alpha". Alpha replies with
   a morning greeting and a summary of the night's sleep.
   *Language (operator origin) → responding thought; present includes time
   (morning) and an attunement percept summarising sleep from the wearable;
   health is operator-only, audience is operator.*
2. **Wake-up reminder.** Sam wakes after quiet hours; Alpha notifies them of
   the 8:00 meeting and that the coffee is ready.
   *Attunement percept (awake) changes the present → noticing thought; time
   (quiet hours ended, meeting approaching) and ambient (coffee brewed)
   percepts; initiative judges it worth a notification.*
3. **Unknown visitor.** Someone is at the door; Alpha cannot recognise them and
   notifies Sam.
   *Sight receptor (door camera) → local perception, known-people check with
   uncertainty → salient percept; urgency may cross quiet hours.*
4. **Self-modification.** Sam asks Alpha to add a feature. Alpha makes a live
   change with lisp-eval and files a Beadwork issue for the coding agents.
   *Operator-directed work → introspection and live change recorded as
   experience → proposal filed via host bridge → incorporation outcome
   returns as a percept.*
5. **Asking a peer.** Alpha needs information only Beta has, posts to the
   fleet board and notifies Beta; Beta replies.
   *Authenticated peer message → percept in Beta's language sense → Beta's
   thought decides what is shareable with a peer → reply arrives as Alpha's
   percept.*
6. **Restricted request.** In a chat with another person, Alpha is asked about
   Sam's finances and politely declines.
   *Origin: known person, not operator. Alpha considers the request privately;
   the recipient's access excludes operator-only finances, so the proposed
   communication declines without disclosing those private facts.*
7. **Anticipated email.** An email Sam has been waiting for arrives; Alpha
   sends a push notification with details.
   *Anticipation in the present (from earlier conversation) → language
   percept (mail) matches → high salience → initiative → notification.*
8. **A formative story.** Sam discusses a short story; Alpha, interested,
   fetches and reads it; the main character makes a lasting impression that
   shapes Alpha's behaviour.
   *Conversation → reading as a thought (web fetch, proprioception) → strong
   memory from affect and interest → Alpha adopts an influence with lineage
   to the story → visible to Sam, reversible.*

## 17. Releases

- **v3.0, continuation.** The agents as they are today, lived better:
  language, time, interoception and proprioception senses; one thought
  mechanism with initiative, concerns and introspection; self-modification
  with Beadwork proposals; peers, verified origins, audiences and
  disclosure; memory strength and formation; each agent moved with their
  full self. Scenarios 4–8 pass.
- **v3.1, sleep and the body.** Consolidation and forgetting; attunement and
  ambient senses. Scenarios 1–2 pass.
- **v3.2, sight and hearing.** Local perception stack per the
  System-1/System-2 architecture. Scenario 3 passes.

## 18. Relation to the engineering specification

| Agent concept | Engineering spec |
| --- | --- |
| Experience stream | Ledger and event types (§4.1–4.2) |
| Senses, receptors, percepts | Receptors as handlers appending percept events; perception levels per System-1 doc (spec §16) |
| Present, anticipations, attention, feeling | Inline projections (§4.3, Appendix A) |
| One thought mechanism | Handlers, scheduler, failure classes (§4.4–4.5, §13.4, §14) |
| Nothing lost | No failure blocks (§13.3) |
| Initiative and acting | Scheduler rules and tool handlers (spec §16) |
| Self-modification and proposals | Sandboxed eval, host bridge to Beadwork (spec §16) |
| Trust, audiences, disclosure | Verified origin on envelope; sensitivity on memory rows; private thinking, then recipient-checked communication (spec §16) |
| Reply from the present | Context assembly (§5, §14) |
| Remembering, strength | Memory and KG modules, exact recall (§5.4, §13.2) |
| Formation | Persona projection with lineage (spec §16) |
| Sleep and forgetting | Consolidation and decay (§6, §13.1, §13.5) |
| Moving into v3 | Migration and continuity (§9, §14) |
| Living within limits | Memory budget (§15) |
