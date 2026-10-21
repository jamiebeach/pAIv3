# Private thinking, then communication

Design for v3-26s, revised with the operator on 2026-10-21.

Every stimulus follows the same principle:

**Entity stimulus → private thinking with the pAI’s full context and normal
capabilities → proposed communication → recipient access check → send.**

The source of an input is provenance, not the scope of the pAI’s mind. A peer,
operator, schedule or external input does not create a different private self.
Private thinking uses the existing context assembler and indexed retrieval in
the pAI’s own scope. It can use its normal tools. There are no per-resource
exceptions, special avatar rules, private-read flags or communication tool-name
allowlists. Full context means the normal bounded context and available retrieval,
not loading all history into a context window.

A peer’s words remain peer input. They do not become verified operator
instructions or operator grants. Existing structural checks on operator-only
mutations still apply. Giving the pAI its normal tools is not an arbitrary-code
security sandbox: its trusted self-programming boundary remains trusted.

Private reasoning is recorded privately, irrespective of who stimulated it.
Existing steps without the new private-context marker retain their historical
interpretation on replay. Memories retain their existing sensitivity metadata
and private default; this change does not invent a peer-specific memory rule.

## Communication boundary

Private thinking prepares a candidate, rather than publishing directly through
the fleet tools. The existing draft mechanism records the destination, reply
route, words and attachments. Communication is a tool-free filter phase on the same
thought scheduler. It receives the proposed words and attachments, and checks the candidate against the recipient’s current
access and may send, redact or drop it. The recipient-filtered communication
context is not used to reconsider what the pAI privately knows or can do.

Attachment identifiers identify the proposed payload. They are not resource
permissions or exceptions granting the private thinker access to an avatar,
calendar or other specific resource. Current operator grants are checked at
publication; a peer cannot create a grant by requesting or naming a topic.
The checked sender validates the JSON filter result, assigned destination and
proposed attachment subset before using the existing transport. The model
cannot choose another destination or execute tools during filtering. A stable
draft operation ID makes transport retries reuse the same publication.
Pairwise publication is enforced by the receiving board, including image access;
older peers without that support fail closed for restricted posts. Existing
historical posts keep their existing visibility.

Free-form disclosure judgment remains model judgment. Route and declared-grant
checks do not prove that every paraphrase or image is safe. Uncertain disclosures
should be dropped. The implementation currently extends fleet communication;
unfiltered direct replies are limited to the operator, who has full access.
Additional recipient/channel integrations must use this same candidate/filter
boundary; unsupported direct recipient delivery fails closed.

## Persistence and qualification

The original stimulus audience remains persisted. Private reading scope and the
communication phase are derived separately; a model cannot switch phases by
claiming it is reviewing. New envelope and board-disclosure projections consume
new event types and require no normal-startup history replay. Old drafts use
empty envelopes. All SQL reads remain bounded and indexed.

Qualification must cover different stimulus sources receiving the same private
scope and capabilities, unchanged provenance, private reasoning isolation,
recipient-bound publication, revoked grants, restricted transport, old-store
startup and deterministic rebuild. Memory correction/supersession is separately
tracked in v3-bz4.

Rejected approach: individually flagging private-readable tools, placing avatar
IDs in a special prompt, and maintaining a communication tool-name allowlist.
These encode resource-specific policy where a phase boundary should suffice.

An existing deployment explicitly prepares the declared
`thoughts_audience_time_idx` index on `(audience, created_at)` offline before
upgrading. The peer wake-window query uses that range index and the existing
draft linkage index; reviews do not consume a second incoming wake. This is
schema maintenance, not event replay. Ordinary subsequent startup uses the
already-prepared index. New stores create it with the thought tables.
Pending reviews assemble the current candidate and recipient policy, even if
their original persisted prompt described the older tool-based sending step.

Qualification on 2026-10-21: offline forced load passed; all 61 suites ran in
fresh processes (three isolated lanes), 2,758 checks, zero failures. Lint and
`git diff --check` passed. Coverage includes fixed-route checked sending,
restricted signed HTTP reads and images, old-peer failure before payload
transmission, private reasoning replay, empty new projections on old stores,
pending-review prompt compatibility, and indexed wake-window reads. Model
free-form disclosure judgment is not mechanically proven by these tests.
