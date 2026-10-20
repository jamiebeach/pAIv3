# Initiative guidance

Initiative language lives in `src/mind/initiative.lisp`. The scheduler still
chooses what gets a thought, when, and within which allowance. These families
organize guidance; they are not additional thinking loops or budgets.

| Family | Existing origins | Purpose |
| --- | --- | --- |
| Reflect | introspection, reflection | Review conversation or peer activity; wander over memories and ideas. |
| Explore | concern, private, unbidden | Investigate a question, pursue an intention, or think privately. |
| Make | docket | Resume useful work and produce a durable result. |
| Notice | noticing | Consider a carried item or scheduled matter. |
| Work | work | Carry out the task at hand. |

`initiative-guidance` supplies one stable system-prefix section for these
origins **only when the audience is the operator**. It asks the agent to
consider explicit and possible unspoken needs, check earlier actions and work,
choose a proportionate contribution, and use concern, carry, work_docket,
document or selective outreach. Affect and drives can inform a choice; they
are not commands to act or spend. Inferred needs remain possibilities rather
than facts or permission. Doing nothing remains an explicit valid outcome.
Operator response turns retain their existing speaking-while-working guidance.
Peer, guest, coding-agent and external-content thoughts do not receive the
operator initiative section.

## Prompt builders and trigger ownership

All six initiative task builders are in the same module as the shared guidance:

| Builder | Caller that owns the trigger and bounded inputs |
| --- | --- |
| conversation-review-prompt | mind/concerns.lisp: %start-review; new operator conversation, once per stretch |
| concern-visit-prompt | mind/concerns.lisp: %concern-thought; selected concern, horizon and call allowances |
| rumination-prompt | mind/concerns.lisp: %start-rumination; bounded memory seeds and summary, paced quiet time |
| docket-work-prompt | mind/docket.lisp: %start-work; eligible maintained item and remaining allowance |
| carried-consideration-prompt | api/carried.lisp: consider-carried; due carried items outside notification quiet hours |
| fleet-digest-prompt | api/fleet.lisp: fleet-digest; bounded unseen peer board activity in quiet time |

Conversation review can notice an operator or peer problem, a promise, a
passing plan, or a possible unasked need. It can carry an operator need or open
useful work, while retaining its limit of at most two concerns. A peer's need
is not automatically an operator commitment. Concern visits can selectively
notify the operator, draft for peers, or turn a useful outcome into work.
Rumination can consider reality, relationships, memories, ideas, affect and
drives; inspiration can become a creation. Docket work must resume actual
artifacts and record progress rather than merely describe a deliverable.

Event-specific inputs such as an intention, scheduled text, received message
or draft remain with their owning modules. They are not a second copy of the
initiative guidance. The assembler adds shared guidance to eligible thoughts.
Context slots and audience filters remain owned by the existing assembler and
slot sources; this change does not widen them.

## Capabilities and deliberate outreach

`work_docket` and `draft_fleet_message` schemas are directly in hand for private
initiative origins; `document` is directly in hand for docket work. Other
available tools remain discoverable through the tool index and `describe_tool`.
The draft allowance and all thought, concern, docket and daily spending limits
are unchanged. There is no creation quota, new trigger, paid probe or forced
notification.

`private-initiative-p` covers operator-audience introspection, reflection,
concern, docket, noticing, unbidden and private thoughts. These cannot use fleet
publishing tools. They can leave a draft using the existing bounded
[reflect-then-speak](../reflect-then-speak.md) mechanism. After the private
thought ends, a separate peer-audience thought checks, edits, sends or drops
it. Only draft text crosses that handoff; private `why` does not. A draft's
wording can still contain an accidental disclosure: the existing model check
is not a deterministic semantic privacy proof. No new peer-post reflection
stage is introduced.

Ordinary authenticated operator tasks and peer-audience work retain their
existing tool permissions. Reflection alone retains its special cross-audience
activity context; adding a draft route to other private initiative does not
grant them that context flag.

## Qualification

`mind-initiative-test.lisp` checks assembled guidance, audience exclusions,
capabilities in hand and specific builder behavior. `mind-concerns-test.lisp`
exercises review and rumination through their real quiet-time triggers.
`api-fleet-test.lisp` checks private initiative drafts stay unsent until thought
completion and use the existing peer handoff without private reasoning.
These tests establish wiring and boundaries; whether an agent notices a useful
opportunity and makes a welcome contribution still needs live observation.
