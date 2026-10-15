# Behaviour spec: persona, voice and governing instructions

Status: **draft for agent review** (migration checklist A5, agent doc §14
term 3). Nothing here is final until each agent has reviewed how they will
be loaded. Written from v2 behaviour (reference only) and the v3 design.

## 1. What makes up the agent's self-presentation

In v2, what the model sees as "who I am" comes from several places:

| Source (v2) | What it holds | Where it lives |
| --- | --- | --- |
| Governing system prompt | Substrate rules: tools, honesty, disclosure, two-sided conversation | Instance file superseding `PAI-SYSTEM-PROMPT.default.md` |
| Identity text | Who the agent is | Local profile JSON (`persona_id`, `revision`, `identity`) or default template |
| Voice text | How the agent speaks | Same profile (`voice`) |
| Adopted traits | Up to 7 short traits adopted through chat, appended to identity | `traits.json` (operator-owned), when `traits_enabled` |
| Tone hint | Operator-facing tone adjustment | Relationship policy |
| Affect modulation | Current affect shaping context | Event-derived checkpoint |
| Persona id | Identity key | Storage partition (formation), profile (conversation) |

v3 keeps every one of these as the agent's own state. What changes is that
they all become rows of one **persona** projection, built from events, with
lineage, instead of files and globals.

## 2. v3 model

### 2.1 Events

| Event | Origin allowed | Meaning |
| --- | --- | --- |
| `persona.profile.set {persona_id, revision, identity, voice}` | operator, migration import | Base identity and voice text |
| `persona.instructions.set {revision, text}` | operator, migration import | Governing instructions (the substrate rules the agent lives by) |
| `self.trait.proposed / adopted / declined / retired {trait_id, text, reason, evidence_refs, proposed_by}` | agent or operator; adoption by the operator or the agent per policy | Traits |
| `self.influence.adopted / revised / released {influence_id, source_refs, what_it_means, behaviour_guidance, strength}` | agent | Formation (agent doc §12) |
| `operator.influence.reversed {influence_id, reason}` | operator | Operator reversal, seen by the agent as a percept |
| `persona.tone.set {text}` | operator | Tone hint |

Affect is not part of this projection; it is its own projection and enters
context as state, not as identity.

### 2.2 Projection `persona` (inline, :derivation :pure)

One row per facet: `profile`, `instructions`, each active trait, each active
influence, `tone`. Each row keeps `source_event_id`, `revision`, and text.
A `persona_fingerprint` is the SHA-256 of the rendered identity block
(§2.3) and changes whenever any facet changes. The fingerprint is recorded
on every `context.assembled` event so any reply can be traced to the exact
self-presentation that produced it.

### 2.3 Rendering and placement

The identity block is rendered deterministically and placed **first** in
every context profile, as the stable prefix (engineering spec §5.3), in this
order:

1. Governing instructions
2. Identity
3. Voice
4. Adopted traits (oldest first), then adopted influences (strongest first),
   each with a one-line note of where it came from
5. Tone hint (operator-facing profiles only)

Bounds (from v2): at most 7 traits, 200 characters each, 1,500 total;
influences bounded similarly. Text that reads as an instruction to the
substrate (rather than a description of the agent) is rejected at adoption,
as in v2.

### 2.4 Audience

Identity is the same for every audience. Instructions about disclosure are
part of governing instructions; sensitivity filtering of memory is separate
(spec §16.5). The tone hint applies to operator-facing profiles only.

## 3. Migration

Each agent's v2 persona state is imported as events, once:

- `persona.profile.set` from their local profile file (exact text, same
  revision);
- `persona.instructions.set` from their instance system prompt (exact text);
- one `self.trait.adopted` per adopted trait, with original reasons and
  evidence references;
- `persona.tone.set` from relationship policy.

**Parity check (checklist E3):** before cutover the v3 identity block is
rendered and shown to the agent beside the v2 system prompt as v2 would
assemble it, and the agent compares replies to prompts they choose.

## 4. Questions for each agent (A5 review)

1. Is the order in §2.3 right? In v2, governing instructions and identity
   are interleaved in one system prompt; v3 separates them.
2. Should adopted influences (new in v3) sit with traits inside identity, or
   in a separate "what has shaped me" block?
3. Should anything else be part of your identity block that v2 placed
   elsewhere (for example a short self-summary, or core memories)?
4. Who may adopt a trait: you, the operator, or either? v2 required the
   operator's adoption of a proposed trait.
5. Is there anything in your current system prompt that is substrate
   plumbing rather than you, and should move out of identity?
