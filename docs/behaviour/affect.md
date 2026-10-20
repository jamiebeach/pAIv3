# Affect: meaning and individual dynamics

The event-derived `affect` projection owns six modulators (arousal, valence,
certainty, competence, boredom, social_need) and four drives (connection,
curiosity, closure, appreciation). State is calculated from a bounded set of
indexed rows and elapsed time. Reading it neither appends events nor calls a
model. There is no emotional timer thread.

## What a thought sees

`render-affect` in `src/mind/affect.lisp` renders each number with a deterministic
semantic description and its passive direction over the next minute. For example:

```text
boredom=0.92 (very high boredom; settling)
social_need=0.81 (very high need for connection; rising)
appreciation=0.12 (very low unmet desire; rising)
curiosity=0.76 (about a synthetic question) (high unmet desire; settling)
```

Intensity bands are very low below .2, low below .4, moderate below .6, high
below .8, and very high otherwise. Valence uses its signed scale: very negative
at or below -.6, negative below -.2, neutral through .2, positive below .6,
very positive otherwise. Certainty and competence are *felt* state, not proof
of knowledge or ability. Drive numbers mean unmet desire: a low appreciation
drive does not prove that praise was received, and a high one does not mean
feeling appreciated. No label claims a recent emotional episode or requires action.

Directions describe passive evolution under current dials: rising, settling
(numeric decrease), or steady. They do not infer a historical trajectory from
conversation. Future events can change the direction. Numeric routing and
rumination inputs retain their existing meaning; this adds no action threshold,
spending allowance, model call, or automatic outreach.

The existing 300-token state slot is retained. Targets are clipped to 80
characters and omitted if needed to keep all ten semantic values in the slot.
Only operator context includes target text; non-operator context and `feel`
results exclude it. Very small requested budgets can return no state row.

## Personal settings

Settings are durable operator-controlled events. Each pAI can have different
values through the existing settings interface/import. An update replaces the
whole object; read/copy the existing object before changing selected entries if
other personalized entries must be preserved. A partial object uses source
defaults for omitted names/fields; it is not a merge with the last saved object.

`affect.modulators` already supports independent baseline, decay_rate, min and
max for each modulator. Decay rate is the share of remaining distance to baseline
closed per minute. `social_need.silence_seconds` now independently controls the
operator-silence floor: `1 - exp(-seconds_since_operator / silence_seconds)`.
Its default is 14400 seconds (four hours to roughly .63), not a four-hour
half-life. Changing this dial applies it to the current silence interval.

`affect.drives` accepts these independent dials for each drive:

| Dial | Meaning | Fallback when absent |
| --- | --- | --- |
| resting_level | Initial value when no row exists; level toward which persistence decays | Initial legacy start; decay target 0 |
| rise_per_hour | Amount built per hour, independently of legacy baseline | Rising mode: legacy baseline / 4; event mode: 0 |
| retention_per_five_minutes | Fraction of distance to resting_level retained per five minutes | Rising: 1; event: .985 |
| trigger_amount | Addition when `feel` stirs this drive | .4 |
| satisfaction_drop | Fraction removed on satisfaction, including automatic operator contact/appreciation | .7 |

All these values are in [0,1]. `mode` remains rising or event; legacy `baseline`
and `start` remain accepted. Without new fields, old values and equations are
preserved. With retention 1, a drive builds linearly and resting_level only sets
its initial value: choose retention below 1 to give an existing drive a pull
toward its resting level. Retention 0 means immediate settling for positive
elapsed time. Stored values are not overwritten just because a resting level
changes. A drive without a recorded row stays at its initial value; its first
state event anchors elapsed-time growth, as before.

Growth and decay combine as a continuous linear system, so splitting elapsed
time into several intervals produces the same result (before saturation). With
retention `r` between 0 and 1, decay constant `k=-ln(r)/300`, hourly rise `g`,
and resting level `b`, the value after `t` seconds is:

`b + (old-b)*exp(-k*t) + (g/3600)*(1-exp(-k*t))/k`, bounded to [0,1].

`feel` writes its configured trigger amount into the event, so replay does not
reinterpret the amount under later settings. The reducer applies the satisfaction
fraction in force at the event. A setting change first advances existing stored
values under the old dials, then installs the new dials. Rebuild consumes the
same ordered events and reproduces the result. No schema or projection-version
change is required: old events/dials retain their old interpretation. No runtime
rebuild is introduced.

## Illustrative configurations

These are synthetic examples, not assignments to any existing agent. A pAI
with a stronger pull toward connection could use:

```json
{
  "connection": {
    "resting_level": 0.35,
    "rise_per_hour": 0.20,
    "retention_per_five_minutes": 0.99,
    "satisfaction_drop": 0.70
  }
}
```

A pAI whose curiosity is more easily stirred and lasts longer could use:

```json
{
  "curiosity": {
    "resting_level": 0.05,
    "trigger_amount": 0.55,
    "retention_per_five_minutes": 0.995,
    "satisfaction_drop": 0.70
  }
}
```

Curiosity does not need to grow merely because time passes; explicit rise is
available if deliberately configured. Existing agents keep their saved tuning
until an operator chooses changes. Invalid names, keys, ranges, inverted bounds
and invalid modes are rejected before a setting event is appended. These
personality dials do not alter the agent's authority or spending limits.

## Qualification

Synthetic affect tests cover legacy parity, independent trajectories, trigger
and satisfaction behavior, time partitions, settings boundaries, validation,
semantic interpretation, slot budget and private target filtering. Maintenance
tests explicitly rebuild through dial changes and compare state. These establish
mechanics; subjective fit of labels and personal tuning needs operator/agent
review over real use.

## Semantic message appraisal

The optional [JEV appraisal stage](affect-appraisal.md) replaces new messages'
keyword and flat connection updates with bounded, personal semantic effects.
It runs before response work; the numeric/semantic rendering and time dynamics
documented here remain the same. Old event interpretation is preserved.
