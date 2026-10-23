# Behaviour spec: persona, voice and governing instructions

Status: **reviewed** (migration checklist A5, agent doc §14 term 3). Angel,
Aria and Specter answered §4 on the crew forum on 2026-10-23; the operator
settled the answers in §5. Written from v2 behaviour (reference only) and the v3 design.

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

The two orders differ on purpose: traits accrue, so the oldest come first;
influences carry a strength, so the strongest come first.

Bounds (from v2): at most 7 traits, 200 characters each, 1,500 total;
influences bounded the same way, with strength between 0 and 1. Bounds are
enforced at adoption, so rendering never truncates. Text that reads as an
instruction to the substrate (rather than a description of the agent) is
rejected at adoption, as in v2.

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
assemble it, and the agent compares replies to prompts they choose. Parity
means the same identity content, rendered deterministically; the layout
differs by design (separate blocks, normalized order, provenance notes), so
the agent confirms the v3 block still reads as themselves rather than
matching text.

Migrated governing instructions are reviewed at the agent's first quiet time
after import: instructions can outlive their own truth.

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

## 5. Decisions (2026-10-23)

1. **Order:** as §2.3, unchanged.
2. **Influences** render inside identity, after traits, strongest first, each
   with a one-line note of where it came from (its `source_refs`). A separate
   "what has shaped me" block would present them as history rather than self.
3. **Nothing else** joins identity. Core memories stay memory: chosen per
   context and filtered by audience, while identity is the same for every
   audience. The identity text is the self-summary.
4. **Adoption:** the agent adopts her own traits and influences; the operator
   may decline, retire or reverse any of them. Every adoption goes through an
   adopt API that refuses with a reason before any event is written: bounds
   (§2.3), evidence references required, and a typed Decisions judgment
   (as in affect appraisal) that the text describes the agent rather than
   instructing the substrate. A reversed or retired facet leaves the
   projection and changes the fingerprint.
5. **Plumbing moves out of identity:** tool schemas and calling conventions,
   output rules, workflow mechanics, time-zone handling, reply-language rules
   and migration preambles belong in governing instructions or the
   substrate. Test: instructions hold what would be true of any agent on this
   substrate; identity holds what would be false of any other agent.
