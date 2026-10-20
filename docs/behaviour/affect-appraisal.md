# Semantic message appraisal with Decisions models

New operator and authenticated fleet messages can receive one typed semantic
appraisal before response work runs. This is perception on the existing kernel
work queue, not another conversation loop or a reflection after the conversation.
The response model receives the updated ordinary affect context. It is not
asked to perform this appraisal or to make an additional `feel` round trip.

## Intake and ordering

- `mind/dialogue.lisp` records a verified operator chat turn with the optional
  `semantic_affect` marker, then invokes its registered intake seam.
- `api/boards.lisp` queues appraisal for authenticated operator board posts.
- `api/fleet.lisp` queues it after authenticating/recording a peer message,
  before deciding whether the message should wake a reply. Relayed operator
  words retain peer authority. A peer fyi can matter without requiring a reply.
- `mind/appraisal.lisp` records a source-keyed request and kernel work item in
  one transaction, before the reply work. Operator appraisals have operator
  priority; peer appraisals have work priority. Existing ordered scheduling
  therefore runs appraisal first. Normal budget admission still applies.

A repeated peer delivery uses the same authenticated peer/message key. Operator
chat uses its durable turn ID; operator board posts use their message ID. New
message IDs are separate experiences, even if the text is identical.

Requests capture only the new text and bounded prior context. Chat history is
filtered by audience and strictly precedes the source event. Peer board history
examines at most eight indexed thread rows and excludes operator posts and other
peers. Bounded prior peer messages and completed work summaries use explicit
same-audience/source-before indexes, including when there is no local board
thread. No private operator concern, mail, drive target or future message enters
a peer appraisal. Identity comes from the same persona identity facet already
available to that audience. This version appraises text, not unseen image content.
JSON/UTF-8 budgets retain whole characters and indicate message truncation. The
request stays below the provider's documented short-context threshold.

## One typed call

`mind/decisions.lisp` calls OpenRouter's Decisions API through the kernel's
existing outbound transport and secret references. It is not a chat-completions
request and requires no SDK dependency. Authentication, egress guards, provider
call accounting, spend events and observability captures use existing mechanisms.
The request includes provider preferences denying data collection and requiring
ZDR; there is no automatic privacy relaxation or provider fallback.

The operator selects a named `llm.profiles` entry through
`affect.appraisal.profile`. Its `model` can be any model supporting the Decisions
API's Score contract, such as `typesafe/jev-1.13` or `inception/mercury-decide`.
There is no vendor allowlist. The returned model name is required and recorded
with the outcome, including provider canonical builds. Stored judgments and
receipts remain usable after a profile change. Questions and criteria live together in
`+appraisal-questions+` in `mind/appraisal.lisp`. One call asks six independent
Score questions: appreciation, connection, curiosity, closure, valence and arousal.
Each scale has concrete descriptions, with neutral/insufficient-evidence levels.
Instructions distinguish sarcasm, negation, quoted praise, the recipient of
praise, and the authority of a peer. State/message text is evidence, not commands.

The parser validates each type, score range, confidence, probability distribution
and consistency of its weighted score. Extra fields confer no authority. The model
cannot choose source identity, write a setting, call tools or directly set state.

## From judgments to personal state

Probability of an experience is different from its strength. The gate uses the
probability mass supporting the direction of a change (default .8). A split
between specific and strong appreciation may still establish that appreciation
occurred; uncertainty about strength alone does not discard it. The raw generic
confidence is retained for inspection, not mistaken for emotional intensity.

For supported factors, the probability-weighted score supplies strength:

| Factor | Effect |
| --- | --- |
| Appreciation | Satisfy this pAI's appreciation drive by strength times its personal satisfaction_drop |
| Connection | Satisfy connection by strength times personal satisfaction_drop |
| Curiosity | Stir curiosity by strength times personal trigger_amount |
| Closure | Open matters stir closure; resolving matters satisfies it, using those personal dials |
| Valence | At most a signed .08 nudge |
| Arousal | At most a signed .06 nudge |

Neutral or ambiguous factors produce no change. Routine acknowledgements are
distinguished from meaningful engagement. Numeric effects are stored in the
finished event and applied atomically; replay does not ask JEV again or reinterpret
old scores under new policy. `feel` remains available for further experiences or
later reinterpretation, with guidance against repeating an already appraised
adjustment just by rereading the message.

The deterministic operator-contact timestamp still updates, so the existing
operator-silence floor on social_need remains operator-specific. Peer connection
can satisfy the connection drive. Time dynamics and tool-outcome effects remain
unchanged; this does not add triggers, notification quotas or spending allowances.

## Durability, failure and qualification

Typed raw output is retained in small durable chunks. Receipt, provider invoice
and spend are one transaction, before semantic validation or effect application.
An invalid semantic result is still accounted for and retained. Appraisal outcomes
and affect application independently enforce first-terminal-outcome/source-key
idempotence. New projections consume only new event types; their empty tables can
initialize without replaying old history. Existing affect gains an additive empty
deduplication table and consumption of a new event; its old event interpretation
is unchanged.

A started call is recorded before HTTP. After interruption, the owning work item
can reuse its complete durable receipt. Without one it records uncertainty and
never automatically buys the call again. No exactly-once remote-service guarantee
is claimed across a lost network response. Failed HTTP calls may have unknown
upstream billing, as with existing provider failures. Normal scheduler/accounting
records remain authoritative for known spend.

The HTTP call has a total deadline (default two seconds). Unavailable profiles,
provider/egress refusal, malformed output or timeout record a terminal failure and
leave affect unchanged. Reply work continues under its normal admission rules;
appraisal does not override a budget refusal to the conversation itself. There
are no automatic retries or escalations to an expensive model. Work deferred by
budget can expire as stale after five minutes; it then applies no late change.

## Configuration and compatibility

`affect.appraisal.enabled` defaults false for safe unconfigured first runs. Set
`affect.appraisal.profile` to a named Decisions-compatible entry in `llm.profiles`, then enable it.
There is no fallback to the default chat route. The ordinary settings interface
owns timeout and directional probability threshold too.

Enabled operator turns carry `semantic_affect=true`; only those skip the legacy
appreciation substring matcher and flat connection-satisfaction rule. Old unmarked
events retain their original meaning on rebuild. A failed enabled appraisal does
not silently fall back to keywords. Explicitly disabling the feature returns new
chat turns to legacy behavior. No old message is automatically backfilled or
purchased again.

Synthetic contracts verify payload shape, accounting/raw retention, order,
probability gates, per-agent mapping, failure/deadline behavior, deduplication,
privacy and rebuild parity. Separate bounded live tests of the pinned model use
synthetic messages to check actual semantic quality, usage and latency before
activation or switching models. Probability calibration differs between models;
rerun the synthetic acceptance set with the same gate before switching. Existing
installations retain their selected JEV profile until the operator changes it.
Scores and their concentration are judgments, not proof of subjective
experience; real conversational fit still needs observation.

Sources: [OpenRouter Decisions API](https://openrouter.ai/docs/api/api-reference/alphadecisions/submit-a-decisions-request),
[JEV tutorial](https://openrouter.ai/docs/guides/community/jev-tutorial),
[TypeSafe Score](https://docs.typesafe.ai/primitives/score).

For example, add a profile named `affect-mercury` with base URL
`https://openrouter.ai/api/v1`, model `inception/mercury-decide`, an existing
OpenRouter secret reference, and the appropriate per-token pricing. Select it
with `affect.appraisal.profile="affect-mercury"` after qualification. Preserve
other profiles and conversation routes. Unsupported models fail the typed
contract without changing affect; they are never sent a chat request instead.
Privacy preferences, accounting, deadlines and duplicate protection are the same
for every model. Model selection does not relax ZDR requirements.

See [OpenRouter's Decisions model selection guidance](https://github.com/OpenRouterTeam/skills/blob/main/skills/openrouter-decisions/references/models.md).
