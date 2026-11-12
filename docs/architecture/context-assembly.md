# Context assembly: what reaches a thought

Pseudocode of the path from a stimulus to the messages a model call is sent.
Source of truth is the code; each step names where it lives. Keep this file
in step when the path changes.

## 1. One thought, one frame

```
on thought T (an operator turn, a reminder, quiet time, a forum post, ...):
    tools_text = tool index for T                          # thought.lisp
    frame      = thought_frame(T, tools_text)              # built once per thought, cached
    loop:                                                  # the tool loop
        messages = frame ++ replay(T)                      # replay grows each step
        reply    = model(messages)
        if reply has tool calls: run them, record results; continue
        else: post-reply checks (step 4); end
```

Every model call of a thought re-sends the same frame; only the replay at the
end changes. The frame is cached per thought (thought-frame, thought.lisp).

## 2. Building the frame

```
assemble_thought_context(T, tools_text):                   # context.lisp
    if T is private initiative:
        tools_text += "Private thought. Stimulus source: <audience> ..."
    if initiative guidance for T's origin:                  # initiative.lisp
        tools_text += "## Thinking: <family>" + guidance
    tools_text += injection caution, if the stimulus carries outside content
    tools_text += (T answers a turn ? SPEAKING_WHILE_WORKING : INITIATIVE_COMPLETION)

    profile = communication-review   if T reviews an outgoing post
            | operator-turn          if T answers a turn
            | private-cognition      otherwise

    task = the outgoing post under review
         | the operator's own words, rendered with local time   # the last user message
         | T's reason                                           # "It is 06:45: <reminder text>", ...

    return assemble_context(profile, audience = who will read T's output,
                            thought = T, tools_text, task)
```

## 3. assemble_context

```
assemble_context(profile, request):                        # context.lisp
    identity = render_identity()                           # persona.lisp: persona rows by facet (instructions,
                                                           # identity, voice, traits, influences, standing rules, tone),
                                                           # tone only when operator-facing
    sections = []
    for slot in profile.slots (in order):
        rows = source[slot](request)                       # newest / most relevant first
        if slot is optional (activity, recollection):
            rows = cull(rows)                              # cull.lisp: once per thought, a Decisions
                                                           # judgment drops rows unrelated to T;
                                                           # newest 5 always kept; fails open
        keep rows while their tokens fit slot.max          # the oldest fall off the end
        if slot.order == oldest-first: reverse rows        # transcripts read forward
        if rows: sections += "## <slot.title>\n" + rows
    record context.assembled (per-slot counts, tokens, status, digest)
    return [ system: identity + tools_text,                # stable prefix: provider cache hits
             system: join(sections),                       # changes thought to thought
             user:   task ]
```

A slot whose source is missing or fails reports itself "unavailable" in the
record; assembly never fails. Each source checks the audience first
(`*slot-audience-policy*`): operator-only slots never reach a thought whose
output another audience will read.

## 4. The operator-turn profile, in order

Slowest-changing first (for the cache), the present last (nearest the answer).
Token budgets are maxima.

| # | Title | Budget | Source | What fills it |
|---|---|---|---|---|
| 1 | About me, lately | 120 | myself (avatars.lisp) | whether I have an avatar; pictures made lately |
| 2 | Where I stand | 200 | reality (close.lisp) | my settled self-impression, with its age |
| 3 | Those I am close to | 500 | close (close.lisp) | settled impressions of those closest, with ages |
| 4 | Earlier, in summary | 3000 | summary (summary.lisp) | rolling summary, framed "[Covers our conversation up to …, yesterday]" |
| 5 | Things I am carrying for the operator | 700 | carried (carried.lisp) | open carried items: since, due, waiting for |
| 6 | What I understand the operator needs | 400 | needs (needs.lisp) | my own view, each with the operator's words |
| 7 | Work I am keeping | 600 | docket (docket.lisp) | docket items, parked flag, next step |
| 8 | What I am looking into | 600 | concerns (concerns.lisp) | open concerns, steps, next look |
| 9 | What I have been doing | 24000 | activity (context.lisp) | thoughts from the last `context.activity_hours` (24), never fewer than `context.activity_floor` (10) nor more than 40, reasons and outcomes (culled); what ages out is looked over once in quiet time (`%start-aged-review`) |
| 10 | Conversation | 4000 | dialogue (context.lisp) | recent transcript, oldest first |
| 11 | From the crew forum, where the operator took part | 900 | forum (forum.lisp) | last 5 posts of up to 3 threads the operator posted in, 48 h |
| 12 | What comes to mind | 6000 | recollection (recall.lisp) | impressions of anchors named in or associated with the task + last 4 turns; top-5 recalled memories (culled) |
| 13 | Images I have seen lately | 400 | seen (seeing.lisp) | recent images with captions |
| 14 | Coming up | 500 | scheduled (schedule.lisp) | context-mode reminders that went off in the last `schedule.fired_visible_hours` (24), full text; then the next 10 scheduled |
| 15 | The operator's day | 300 | present (attention.lisp) | next calendar events; sensed present; where the operator is (location.lisp): a fix younger than `location.fresh_minutes` as where they are, an older one as where they were, with its age |
| 16 | What I noticed between thoughts | 600 | stream (attention.lisp) | the watch's last 8 passes |
| 17 | Not for this audience | 200 | restricted (memory.lisp) | topics withheld from this audience |
| 18 | How I am | 300 | state (affect.lisp) | modulators and drives, with meanings and trends |
| 19 | Now | 160 | clock (schedule.lisp) | local time, zone, UTC, the next 8 days |
| 20 | Before you answer | 200 | turn (context.lisp) | voice rule on a voice turn (plus "The operator is driving …" while a fix under 5 minutes old says so); "A message with no tool call ends this thought …" |

private-cognition is the same with smaller budgets, plus "What I have concluded
lately" after the summary, and without the Coming up and Not for this
audience slots. communication-review carries only Now.

## 5. The replay (what grows during a thought)

```
replay(T):                                                 # thought.lisp, %replay-messages
    for step in T's recorded steps, in order:
        model text + its tool calls  -> one assistant message
        tool result                  -> tool message (long ones paged: "Call read_result …")
        nudge / said_note / provenance_note -> user message
    older long results shortened to their first 1200 chars, except the newest 4
    and anything re-read with read_result
```

## 6. Checks that add to the replay before a reply goes out

```
on a final reply (no tool calls) to the operator:          # thought.lisp, provenance.lisp
    if it only announces work, or isn't grounded: one nudge, go round again
    if provenance_check judges it claims what I read/checked/saw/started
       (Decisions score >= thought.announce_check.min_probability, 0.7):
        provenance_note = my web history + memory notes on it; go round once
on a voice turn, words already said alongside a tool call: said_note
```

Each check fires at most once per thought.

## 7. What this means when reading a specimen

- The frame is fixed at the thought's first call. What was in it then shaped
  every later call of the thought.
- Position matters: slots near the end (Coming up, How I am, Now, Before you
  answer) sit closest to the task, and a fast model weighs the end most.
- Imperative text in any slot (a reminder's runbook, the turn rule) competes
  with the operator's message. Descriptive text (persona, impressions) does
  not instruct.
- Times are stored in UTC and shown local; a section that shows past events
  should say how old they are (summary does; Coming up's fired reminders
  show only an absolute stamp).
