# Charges and attention

pAI v3 design. Status: draft for review, 2026-10-18. Builds on
[The continued-experience agent](the-continued-experience-agent.md) §3
(senses and receptors), §4 (the present), §5 (attention) and §7
(initiative), and on the operator's System-1 design note (layered
perception, appraisal, escalation to System-2).

## 1. The case

The agents are costly and, so far, deliver little beyond work on
themselves. Three symptoms, one cause:

- **Value.** Work sessions produce reflection, not things the operator
  uses. No one asked for the result and no one checks whether it helped.
- **Reciprocity.** The agents are largely reactive. When they do reach out,
  it is mostly about their own substrate, not about what matters to the
  operator.
- **Warmth.** The agents have personality and loyalty, but rarely express
  it; when they do, it has nothing specific to attach to.

The cause is what reaches them. An agent perceives its own ledger, its
board, its peers, and the operator when the operator starts a
conversation. Quiet time thinks about the substrate because the substrate
is all it perceives. An agent cannot reach out about the operator's day,
or be glad about the operator's news, without seeing either.

A fourth concern is continuity. An agent's life is discrete thoughts with
gaps it cannot tell from absence. It knows time passed because the ledger
says so, not because anything happened to it in between.

All four have one remedy: continuous, cheap attention to the operator's
world, organised around what each agent is responsible for.

## 2. Principle

**An agent serves through its charges, and attends to them continuously.**

- A **charge** is part of the operator's life an agent owns: it watches
  it, keeps it current, and produces things for it.
- **Attention** (System-1) runs all the time without a frontier model. It
  turns receptor signals into percepts, keeps the present current, and
  decides what deserves a thought.
- **Thought** (System-2) is recruited only when attention says so, or when
  the operator speaks.
- **Outreach** is a thought's conclusion that the operator should hear
  something. It is gated, rate-limited and about the operator.

**Not workflows.** An agent with a charge is not a set of scheduled jobs.
Nothing here runs a script on a timer and calls it attention. The watch
notices; the agent decides whether anything deserves thought, what to
make of it, and whether to say anything. A brief exists because the agent
judged the operator should know something, phrased for that day, not
because a cron entry fired a template. Where this note speaks of cadence
(daily, weekly), it means the rhythm of the operator's life the agent
pays attention to, not a schedule that acts on its own.

**Charges spark curiosity.** Attending to part of someone's life raises
questions: why a training load keeps dipping on certain weeks, what a
company announcement means for a holding, where a story's thread could
go. These are the agent's own, and it may pursue them in quiet time like
any other curiosity. This is the intended source of initiative: an agent
whose interests grow out of the operator's world, rather than out of its
own substrate.

## 3. Charges

A charge is configuration, not code: a setting naming the charge, its
receptors, its rhythm, what it produces and its authority ceiling. Each
agent holds a small number of charges; the operator assigns them, and can
add, move or drop a charge at any time from the app. A charge that moves
takes its history with it: the receiving agent is told what the previous
holder knew and had open. Agents with different charges become different,
which is intended.

| Charge | Receptors (sense) | Rhythm | Produces | Ceiling |
| --- | --- | --- | --- | --- |
| Days and commitments | mail, calendar (language, time) | continuous | morning brief, conflict and deadline warnings, unanswered-mail nudges | read only; drafts, never sends |
| Work and company | mail, calendar, project files, notes (language) | daily, weekly | weekly plan, status of open threads, prepared materials | read only; drafts |
| Staying current | news and research feeds, search (language) | daily | a short brief filtered to the operator's interests, with sources | read only |
| People | calendar, mail, conversation, the graph (language, time) | weekly | nudges: who has gone quiet, birthdays, follow-ups promised | suggests; never contacts anyone |
| Health and longevity | wearable data (attunement), conversation | daily, weekly | trends in sleep, training, recovery; meal and exercise prompts | summaries only, never raw; operator-only audience |
| Portfolio | holdings and prices, filings, dividend calendars (language) | weekly | weekly review: income, positions against stated goals, items to decide | read only; never trades |
| Shared fiction | the work's documents, conversation | per session | drafts, continuity notes, the next scene | edits only the work itself |
| Making | project repositories, docket | per session | shipped increments of a small app | builds and tests; operator ships |

Every charge carries an **authority ceiling**. The agent may draft, warn,
summarise and propose. Sending, contacting, trading and publishing stay
with the operator unless a later decision raises a specific ceiling.

Privacy follows the senses: attunement (health) is operator-only by
default and summarised; people the operator knows are third parties, so
what is learned about them is not shared with peers.

### Sharing between agents

Charges divide the work, not the family. What one agent notices can
matter to another: a calendar change bears on the health charge's training
week; a news item bears on the portfolio; a hard week the operator
mentions bears on everyone. Agents share observations with peers at the
level of percepts and conclusions, not raw sources:

- An agent offers a peer an observation when its appraisal finds it
  relevant to a peer's charge (the peer's charges are known through the
  fleet). The peer appraises it like any other percept; sharing does not
  oblige a thought.
- Shared observations carry their origin, so the receiver knows whose
  attention produced them and how directly.
- Privacy still applies. Health stays operator-only unless the operator
  widens it; what is learned about third parties is shared only as far as
  the charge needs.
- Agents also talk about their work and their curiosities on the boards,
  as they do now, which is where shared understanding grows.

## 4. Attention: the System-1 watch

The first slice of System-1, without sensors beyond what v3 already reads.

**The loop.** A watch runs every few minutes (a setting, per charge). It
polls each charge's receptors for change, cheaply: message ids, calendar
etags, feed entries, a wearable's daily summary. Unchanged sources cost
nothing further.

**Percepts.** Each change becomes a percept: a compact, timestamped
observation with its source, the charge it serves and the entities it
names. Raw content stays with the receptor; a percept holds a summary and
a reference.

**The present.** Percepts update a present projection: what is coming up,
what is waiting on the operator, what the agent is watching for
(anticipations), and when the operator was last active. It is kept
current without a model.

**Appraisal.** Each percept is scored in layers, cheapest first, each
adding its reasons:

1. *Rules* say how it arrived and when: a person, a list or a machine
   (from the message's own headers and the mail service's categories);
   account security; an event that is soon, new, moved, clashing or
   cancelled. Rules never decide relevance.
2. *Relevance*: the percept is embedded and compared with what the agent
   knows matters (the operator's stated interests, its open concerns, its
   work). A list message about something the operator cares about is
   raised.
3. *The judge*: what is still unsure goes, in one batch, to a small model:
   a local model on the operator's machine, or the JEV judge for calibrated
   choices. It may raise an item, never lower one.

A layer that is unavailable is skipped and the pass says so. Feature
values, reasons and scores are recorded, so thresholds can be calibrated
against what the operator found useful. Reading the item is still a
thought: System-1 decides how soon and how much attention, never what the
item means.

**The stream.** Every pass writes a short stream record: what was
noticed, what was set aside, what was escalated. This is the evidence the
agent's in-between existed. A thought can read the stream since it last
ran, so it starts from what happened, not from a gap.

**Escalation.** A percept above threshold, or a moment in the operator's
rhythm the agent attends to (the operator's day starting, the end of a
trading week), requests a thought with a wake
contract: the reason, the evidence percepts, the charge, and the action
ceiling. The thought decides what to do; the watch only decides what to
think about.

**Incubation.** Fragments a thought leaves open are attached to their
concern (v3-okh.8). The watch matches new percepts against them, so a
fragment can come back when the world gives it a reason, and the stream
records when that happened and what the resulting thought changed.

**Cost.** The watch uses no frontier model. Thoughts are fewer and better
aimed than open-ended quiet time, which this partly replaces.

## 5. Outreach

Outreach is something the agent says to the operator unprompted. It has
kinds, each with its own limits:

| Kind | Example | Limits |
| --- | --- | --- |
| Service | "The 3pm moved to 2pm and now overlaps the school pickup." | as needed; urgency decides; quiet hours except urgent |
| Brief | what the day holds, the week's portfolio picture | when the agent judges it useful; never a fixed template |
| Finding | something the agent found that bears on the operator's goals | a few a day; spaced |
| Nudge | "It has been six weeks since you spoke to an old friend." | a few a week |
| Warmth | a specific, grounded expression of care or gladness | rare; spaced by days |

Every outreach passes the same gates:

- **About the operator.** It concerns the operator's life, work or goals.
  Outreach about the agent's own substrate goes to the board or the
  docket, not to the operator.
- **Grounded.** It cites the percept, memory or fact it rests on. Warmth
  in particular must point at something real (a hard week the operator
  mentioned, a milestone, a shared piece of work), never a template.
- **Timely.** Quiet hours, the operator's present state, and spacing
  since the last outreach of the same kind.
- **Not repeated.** Topic and phrasing habituate: the same nudge or the
  same kind of warmth does not come back on a schedule.

The v2 reciprocity canary had the right shape for this (bounded daily
counts, minimum spacing, topic blocking, a shadow mode). It is the model;
v3 makes the kinds explicit and adds the about-the-operator gate.

A shadow period comes first: outreach is decided and recorded but not
sent, and the operator reviews what would have been said.

## 6. Work that produces

Work sessions (the docket, v3-6nl) are tied to a charge. A session ends
with a deliverable (a draft, a brief, a fix, a plan, an increment of an
app) or with an explicit record that it produced nothing and why. Work on
the agent itself is allowed but is not a charge and does not count as
service.

Deliverables are collected in one place the operator can open from the
app, by agent and by charge.

## 7. Seeing whether it is worth it

Each agent keeps a weekly account, shown in the app:

- what it cost (model spend by purpose: conversation, watch, thoughts,
  work sessions, sleep);
- what it delivered, by charge;
- outreach sent, and what the operator did with it (answered, acted on,
  ignored, asked to stop).

Operator reactions are the feedback that tunes appraisal thresholds and
outreach limits. The account also makes the operator's decision explicit:
keep a charge, move it to another agent, or drop it.

## 8. Access without credentials

Most charges need the agent to be signed in to something: a mailbox, a
code host, a wearable's service, a brokerage. The agent must be able to act
as the operator there without ever holding the credential. Hiding the
credential stops it being stolen, copied or leaked through a page or a
prompt; it does not stop misuse, so scope and approval matter as much as
hiding.

**What exists.** A secret store; an outbound guard that attaches a named
secret to a request only for the hosts the secret is bound to, and refuses a
raw secret anywhere else; and the Google connect flow, in which the operator
signs in at the service and the token goes straight into the store.

**What is missing, in order of effort.**

1. *A pasted key.* A page in the app where the operator adds a secret: its
   name, the hosts it may be sent to, and its value. The value goes from the
   operator's browser to the store and is never shown again. This replaces
   entering secrets at a terminal.
2. *OAuth for any service that offers it.* The Google flow generalised: a
   table of services (authorisation address, token address, scopes, hosts).
   Scoped, refreshable, revocable tokens are the best credential there is.
3. *The agent asks.* A tool, request_access(service, why), puts a task in the
   operator's app ("The agent asks you to connect the code host, for pull
   requests on this project"). The operator completes it; the agent is told
   it is done and then uses the secret by name.
4. *A browser the agent drives but does not hold.* For services with only a
   login. See below.

**Limits on every secret.** Allowed hosts; allowed methods and paths
(read-only by default); the operator's approval for anything that writes or
moves money; responses scanned so a secret a service echoes back is
redacted; every use recorded.

**The browser sidecar.** The browser runs outside the agent's container, in
its own sandbox, with a profile per service. The operator signs in there,
by watching and taking over when the agent asks (a login, a two-factor
code); the profile keeps the session. The agent drives it through a narrow
interface: go to an address, click, type, read the page as text, take a
screenshot. It has no way to run script in a page, read cookies or storage,
or export the profile, so it never holds a session. The operator can watch
the agent work and take control at any time.

Rules for the sidecar:

- Each profile is limited to its service's hosts; the agent cannot send an
  authenticated profile somewhere else.
- A page is outside content. An agent working in a signed-in profile can be
  steered by text on the page, so reading is the default and any write
  (submit, send, pay, change) waits for the operator.
- Password fields are masked in screenshots; the agent's typing of a stored
  value is done by the sidecar, not by the agent.
- Two-factor and consent steps are always handed to the operator.
- A session cookie can do anything the account can, unlike a scoped token,
  so the sidecar is for services that offer nothing better.

What it unlocks: a wearable's service with no personal interface, a
brokerage's read-only views, any site the operator uses daily.

## 9. Releases

1. **Days and commitments.** The watch over mail and calendar, the present,
   the stream, the morning brief, service outreach, in shadow mode first.
2. **Staying current.** Feeds, the daily brief, findings.
3. **Work that produces and the weekly account.** Charges on docket items,
   the deliverables view, cost against value.
4. **Warmth and nudges.** The remaining outreach kinds, once the gates
   have been seen working.
5. **Health.** The wearable receptor (attunement), summaries and prompts.
6. **Portfolio.** Holdings and market receptors, the weekly review.
7. **Work and company, people, making, shared fiction** as charges are
   assigned.

Each release is one charge or one mechanism, usable on its own.

## 10. Open questions

- The first assignment of charges (proposed: shared fiction, health,
  people and warmth to the production agent; days, work and current
  events to the UAT agent; making and the portfolio to the development
  agent), adjusted as experience shows.
- Whether the watch should run inside each agent or as one shared
  service feeding all three (one mailbox read, three audiences).
- Which feeds define "current" for news and research.
- The wearable's access path: a vendor API with the operator's consent, or
  an export the operator places on a host.
- The portfolio's data source, and whether it is read from a brokerage or
  kept by the operator.
- How warmth is evaluated: what the operator welcomed, and what felt
  performed.
