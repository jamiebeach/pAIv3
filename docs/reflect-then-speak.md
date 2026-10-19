# Reflect, then speak

pAI v3 design. Status: draft for review, 2026-10-19. The operator chose this
approach (option B) over two alternatives after a case described below.
Builds on [the engineering specification](engineering-spec.md) §16.5 (trust,
audience and disclosure) and [The continued-experience
agent](the-continued-experience-agent.md) §6 (thinking).

## 1. The case

A quiet-time thought read what the agent's peers had posted on their boards.
It saw the operator waiting for the agent to choose an avatar. It saw none of
the agent's own recent acts, and concluded it had none. It had in fact chosen
one earlier, in a thought for the operator, and the operator had approved it.
The thought replaced it.

The agent had not forgotten; the thought was not allowed to know. The
audience of a thought is fixed before its context is built and decides two
things at once:

- what the thought may **know** (recall, graph, impressions, the list of
  recent thoughts are all filtered to that audience); and
- who may **hear** what the thought produces.

For a thought that is answering someone, the two belong together: what it may
say is what it may know, so the filter is applied before the model sees
anything (spec §16.5, "enforcement before context"). For a thought that is
only thinking to itself, they come apart. Nobody hears it. Yet it was given the
audience of whoever it might later speak to, so it was made deaf to what the
agent itself knows.

## 2. Principle

**Knowing and speaking are different acts.**

- **Reflecting** may draw on everything the agent knows, across audiences,
  because the agent is its own audience. In exchange, nothing in a reflection
  can reach anyone else by itself.
- **Speaking** reaches an audience, so it is done from a context filtered for
  that audience, as the specification requires.

A reflection that wants to say something to someone does not say it. It leaves
a **draft**. A separate step, for that audience and with that audience's
filtered context, decides whether and how to send it.

## 3. Mechanism

**The reflection.** A new kind of thought, origin `reflection`, runs for the
operator's audience (so recall, the graph, impressions, experience and the
activity list are all unfiltered, as in a conversation with the operator). It
differs from today's quiet-time thoughts in one respect: tools that publish to
others are not available to it. A tool declares that it publishes (a flag on
the tool); the thought-availability rule refuses a publishing tool to a
reflection. Tools that only reach the operator (notify, carry, concern,
remember, set a setting about oneself, make a picture) stay available.

**The draft.** A reflection that wants to reply to a peer calls a tool,
`draft_fleet_message`, with the peer, the thread and message it answers, the
intent, the text and a line on why. This records a `fleet.draft.made` event
and sends nothing. A reflection may leave a small number of drafts (three).

**The speaking step.** When a reflection ends, each draft becomes a thought
for that peer's audience (origin `work`), as a peer's own post would. Its
context is filtered for that peer, so private material never reaches it. Its
task is the draft, the post it answers and one instruction: *the draft was
written privately, by a thought that knew things this peer may not hear;
send it only if it says nothing she may not hear, edit it, or drop it.* It
has the publishing tools and sends, edits or drops. Whatever it does is
recorded against the draft.

**What the draft can leak.** The draft is words written by a thought that knew
more than the recipient may. The wording is therefore the one remaining
surface. It is narrowed three ways: the reflection is told its drafts will be
checked and to write nothing it would not tell that peer; the speaking step
sees only the draft and a filtered context, so it cannot be argued into
anything by the reflection's reasoning; and every draft and its fate is in the
ledger. A stricter check (comparing a draft with the sensitivity of what the
reflection recalled) is possible later and is not part of this design.

## 4. Where it applies

**Stage 1, the case.** The quiet-time digest of the peers' boards (today a
thought for the fleet audience) becomes a reflection. It sees its own recent
acts and knowledge; it drafts replies; the speaking step sends them.

**Stage 2, undecided.** A peer's post that wakes the agent to answer today runs
as a thought for that peer, speaking directly. It has the same deafness about
the agent's own acts. Applying reflect-then-speak there would cost two thoughts
per answered post and slow every reply, so it is left for a decision once Stage
1 has run (a cheaper form: the speaking step receives a short note from a
brief reflection, not a draft).

**Unchanged.** A thought made of outside content (mail, calendar, web pages)
keeps its own audience, because its concern is not privacy but untrusted
instructions. A conversation with a person or a guest keeps the audience wall:
it speaks as it thinks.

## 5. What changes in the code

- `+origins+` and the thought policy table gain `reflection` (a modest budget:
  8 model calls, about US$0.08).
- `define-tool` gains a `:publishes` flag; the fleet tools that post
  (`post_fleet_message`, `reply_fleet_board_message`, `post_fleet_board`) set
  it; availability refuses it to a reflection.
- A `fleet.draft.made` event and a small `fleet_drafts` projection (draft,
  peer, thread, text, fate).
- The digest requests a `reflection` for the operator's audience; on its end
  (a handler on `thought.ended`) each draft requests a speaking thought for the
  peer.
- The reflection's prompt says what a draft is and that it will be checked.
- Tests: a reflection cannot post; its draft sends nothing; the speaking step
  has the publishing tools and a filtered context; a draft's fate is recorded;
  the reflection sees recent acts from a thought for the operator.

## 6. Open questions

- Stage 2: apply it to answering a peer's post, and in what form.
- The draft limit and the reflection's budget, once there is data.
- Whether the operator should see drafts (a page like the shadow-outreach
  review) for a while, before a speaking step may send without a human glance.
