# Acts and outcomes: what the agent asked of its body, and what happened

Bead v3-baw.1.2, for [Having a body](../embodied_somatic_awareness_specification.md)
§5.4. It uses the percepts table of [the interfaces a body plugs
into](body-interfaces.md). The code is in the avatar plug-in
(`src/acts.lisp`, `www/app.js`); this note is the design it is built to.

## 1. What an act is

Three kinds, each given an id by the agent's side:

| Kind | When | Id |
| --- | --- | --- |
| `speech` | A view asks for the audio of one sentence of a reply | `st<thought>-<offset>` (or `se<event>-<offset>` for a reply that did not stream): the reply and where in it the sentence starts |
| `performance` | `avatar_perform`, or the stage direction opening a reply | `p<n>` from the queue; a stage direction uses its sentence's `s` id |
| `wear` | `avatar_wear` | `p<n>` from the queue |

A speech id is worked out from the reply and the sentence, not counted, so
two views speaking the same sentence are speaking the same act. The view
works it out (only it knows where a sentence starts) and sends it with the
request for the audio; the agent's side checks its form there and records
the act. An id of any other form records nothing.

The plug-in keeps the last 128 acts in memory (about 40 KB; an act is its id,
kind, time, best outcome so far, and for speech at most 160 characters of the
sentence). A restart forgets them; a report for an act it does not know is
dropped.

## 2. What a view reports

`POST outcome` with exactly: the view's id, the act's id, the kind, one code,
and numbers.

| Code | Speech | Performance | Wear |
| --- | --- | --- | --- |
| `done` | played to its end | the clip played | at least one piece is shown |
| `interrupted` | the operator cut in; `played_ms` says how far it got, 0 if it never started | | |
| `not-heard` | the browser was not letting sound play when it started | | |
| `failed` | the audio could not be fetched or decoded | no clip matches, or it did not load | nothing in the model matches (`matched` 0) |
| `not-shown` | | recorded by the agent's side when no view is open | likewise |

Numbers allowed, and nothing else: `played_ms`, `total_ms`, `matched`,
`pieces`. A report with any other field, a code not in its kind's column, a
string where a number belongs, an unknown view or an unknown act is dropped
whole. **No text from a view is stored, logged or shown to the agent.**

A view gets its id when it opens (the first `GET perform`). At most eight
views are remembered.

**What a view cannot know.** A browser does not tell a page that its tab or
the computer is muted. `not-heard` means the browser was blocking sound (the
case of bead v3-h4r). A muted tab reports `done`. This is a limit of the
report, stated here and in the agent's own words for it (v3-baw.1.3), so
`done` is read as "played", never as "the operator heard it".

## 3. Several views

The best outcome reported for an act stands:
`done` > `interrupted` > `failed` > `not-heard` > `not-shown`.
Heard anywhere is heard; it was not heard only if every view says so. The
first report for an act, and any later report that betters it, becomes a
percept. A later report that is the same or worse goes to the raw record
only.

## 4. Where an outcome goes

1. **A percept** (`emit-percept`, receptor `avatar`): kind, code, the act's id
   as subject, the numbers. Salience before habituation: `done` 0.1,
   `not-shown` 0.6, `interrupted` 0.7, `failed` 0.8, `not-heard` 0.9. Kept an
   hour. Never a ledger event. What makes a surprising one come to mind is
   v3-baw.1.4.
2. **The raw record** (section 5).

An act nobody reported on has no percept. Reading it back says so
("unreported") once it is 30 seconds old; nothing is invented for it.

## 5. The raw record: an account that is not the percepts

The crew's condition on Phase A (2026-11-15): the agent's account of its
body must be checkable against something that is not that account, or a
wrong report becomes its picture of itself.

`avatar-acts.log` on the agent's volume, beside `avatar-timing.log`. One
line for every act asked, every report received (accepted, or dropped with
the reason as a code), and every outcome the agent's side recorded itself.
Tokens and numbers only, by the same validation; a dropped report is logged
with its size, not its content. Trimmed to its newer half past 200 KB.

```
2026-11-15T23:10:02Z asked speech s8841-0 chars=42
2026-11-15T23:10:03Z report v3fa91c20 speech s8841-0 not-heard played_ms=0 kept
2026-11-15T23:10:09Z report v77b0e1aa speech s8841-0 done played_ms=1840 total_ms=1840 kept
2026-11-15T23:10:09Z report v3fa91c20 speech s8841-0 done played_ms=1840 total_ms=1840 same
2026-11-15T23:11:40Z dropped unknown-act bytes=118
2026-11-15T23:12:00Z self wear p41 not-shown
```

The agent reads it with the `avatar_record` tool, and the percepts with
`inspect`. The two are written by different code paths from the same
validated report, so a disagreement between them is a defect that can be
seen.

## 6. Quoting what was heard

"It knows which sentence was the last one heard" (acceptance scenario 2)
needs the words. The sentence a view asked to have spoken is text from a
view. It is used for one thing: to find that sentence in the agent's own
reply. When it is there word for word, what is quoted is the agent's own
text. When it is not (the view strips markup before speaking), nothing is
quoted and the place in the reply is given instead. Done in v3-baw.1.3; this
bead only keeps what it needs.

## 7. Tests

In the plug-in (`tools/run-tests.sh tests/acts-test.lisp`, 54 checks), through
the function the route calls, with synthetic reports: each accepted code becomes one percept and one `kept` line that
agree; text in any field is refused and appears nowhere; an unknown view,
act, code or number name is dropped; the better of two views' reports
stands and the worse is logged `same` or `worse`; the act table and the log
stay within their bounds; with no view open, a performance and an outfit
change are recorded `not-shown` by the agent's side; an act with no report
reads as unreported.

Not covered by automated tests: the page's own reporting (`play`, `hush`,
`wearOutfit`). It is checked by hand in a browser against the raw record
for each code, and that check is recorded on the bead.
