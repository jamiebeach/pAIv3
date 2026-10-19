# Concern revisit horizons

`concern` action `note` accepts `next_look_at`, a canonical UTC timestamp
(`YYYY-MM-DDTHH:MM:SSZ`). Omission preserves an existing horizon; an empty
string clears it. Invalid timestamps leave the concern unchanged. Prose in
a note is never interpreted as a schedule.

An open concern becomes eligible at the later of its explicit horizon and
its last attendance plus `quiet.revisit_seconds`. Other quiet-time budget,
grace, and lifetime limits still apply. A future horizon also prevents idle
rotation and immediate failure-repair selection from bypassing the wait.
Other eligible concerns may proceed. Notes and thought outcomes preserve
the horizon until explicitly changed. Past horizons simply leave the global
revisit floor in effect.

Projection `concerns` version 2 adds a nullable `next_look_at`. Historical
events without that field replay with no horizon. Deployment requires an
explicit rebuild; historical prose is not converted into guessed dates.

Synthetic fixtures in `mind-concerns-test.lisp` cover a 24-hour wait,
the exact due boundary, shorter horizons, explicit clearing, invalid dates,
preserving a wait across notes, rotation, and selection of another concern.
