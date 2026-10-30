"""Record each agent's written sense of itself and of those it is close to, and
measure how it changes (bead v3-dwk, the selfhood record).

Run once a day, after the agents' sleep:  python tools/self-record.py
Reads `peek close` from each running agent; writes, all under private/selfhood/
(git-ignored, since the texts are about the operator's life):
  raw/<date>/<agent>.json   the texts as written, verbatim
  metrics.csv               one row per agent and text per day
  notes/<date>.md           what held and what shifted, for a person to read

Every measure is a plain function of the texts, with no model in the loop, so
anyone can recompute it:
  stability    similarity to the same text's previous record (word Jaccard
               and character-trigram cosine; 1.0 means unchanged)
  persistence  share of the content words of the record a week earlier still
               present today
  individuality  1 minus the mean similarity of an agent's "self" text to the
               other agents' the same day (their scaffolding is shared, so the
               difference is theirs)
  echo         share of the text's content words that come from the writing
               prompts' own vocabulary
"""
import csv
import datetime
import json
import math
import re
import subprocess
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent / "private" / "selfhood"
AGENTS = {"specter": ("pai-specter", 8090), "aria": ("pai-aria-dry", 8091), "angel": ("pai-angel-dry", 8092)}
# Words the writing prompts themselves use (src/recall/close.lisp), so a text
# that leans on them is leaning on its instructions.
PROMPT_WORDS = {"warmth", "leverage", "guilt", "pressure", "wariness", "friction", "earned", "plain",
                "plainly", "reservation", "tight", "settled", "faded", "fold", "material", "feeling"}
STOP = set("""a an the and or but if then than that this these those with without into onto from about over under
of to in on at by for as is are was were be been being it its it's i me my mine we our you your he him his she her they them their
what which who whom whose when where why how not no nor so too very just also only still even ever never more most
some any all each every both either neither one two can could would should will shall may might must do does did done
have has had having there here now today tonight yesterday tomorrow out up down off again once""".split())


def words(text):
    return [w for w in re.findall(r"[a-z][a-z'-]+", (text or "").lower()) if len(w) > 3 and w not in STOP]


def jaccard(a, b):
    a, b = set(words(a)), set(words(b))
    return round(len(a & b) / len(a | b), 3) if a | b else None


def trigram_cosine(a, b):
    def grams(t):
        t = re.sub(r"\s+", " ", (t or "").lower())
        return Counter(t[i:i + 3] for i in range(len(t) - 2))
    ga, gb = grams(a), grams(b)
    dot = sum(ga[g] * gb[g] for g in ga)
    norm = math.sqrt(sum(v * v for v in ga.values())) * math.sqrt(sum(v * v for v in gb.values()))
    return round(dot / norm, 3) if norm else None


def echo(text):
    w = words(text)
    return round(sum(1 for x in w if x in PROMPT_WORDS) / len(w), 3) if w else None


def peek(container, port):
    out = subprocess.run(["docker", "exec", container, "curl", "-s", f"http://127.0.0.1:{port}/v1/dev/peek?what=close&limit=6"],
                         capture_output=True, timeout=120)
    return json.loads(out.stdout.decode("utf-8"))["items"][0]


def texts_of(record):
    """{"self": text, "<name>": text, ...} from a peek-close record."""
    names = {c["anchor"]: c["name"] for c in record.get("close_ones", [])}
    found = {}
    for imp in record.get("impressions", []):
        if imp.get("settled"):
            key = "self" if imp["anchor"] == "self" else names.get(imp["anchor"], imp["anchor"])
            found["operator" if key == "Operator" else key.lower()] = imp["settled"]
    return found


def load(day, agent):
    p = ROOT / "raw" / day.isoformat() / f"{agent}.json"
    return texts_of(json.loads(p.read_text(encoding="utf-8"))) if p.exists() else None


def main():
    today = datetime.date.today()
    raw = ROOT / "raw" / today.isoformat()
    raw.mkdir(parents=True, exist_ok=True)
    (ROOT / "notes").mkdir(parents=True, exist_ok=True)
    current = {}
    for agent, (container, port) in AGENTS.items():
        try:
            record = peek(container, port)
        except Exception as e:  # an agent that is down is noted, not fatal
            print(f"{agent}: could not read ({e})", file=sys.stderr)
            continue
        record["recorded_at"] = datetime.datetime.now(datetime.timezone.utc).isoformat(timespec="seconds")
        (raw / f"{agent}.json").write_text(json.dumps(record, indent=1, ensure_ascii=False), encoding="utf-8")
        current[agent] = texts_of(record)

    def previous(agent, back):
        for d in range(back, back + 7):
            t = load(today - datetime.timedelta(days=d), agent)
            if t:
                return t
        return None

    rows, notes = [], [f"# Selfhood record, {today.isoformat()}", ""]
    for agent, texts in current.items():
        yesterday, week = previous(agent, 1), previous(agent, 7)
        others = [current[o].get("self") for o in current if o != agent and current[o].get("self")]
        notes.append(f"## {agent}")
        for key, text in sorted(texts.items()):
            row = {"date": today.isoformat(), "agent": agent, "text": key, "words": len(words(text)),
                   "stability_jaccard": jaccard(text, yesterday.get(key)) if yesterday and key in yesterday else None,
                   "stability_trigram": trigram_cosine(text, yesterday.get(key)) if yesterday and key in yesterday else None,
                   "persistence_week": (round(len(set(words(text)) & set(words(week[key]))) / len(set(words(week[key]))), 3)
                                        if week and key in week and words(week[key]) else None),
                   "individuality": (round(1 - sum(trigram_cosine(text, o) for o in others) / len(others), 3)
                                     if key == "self" and others else None),
                   "echo": echo(text)}
            rows.append(row)
            notes.append(f"- **{key}**: stability {row['stability_trigram']}, a week on {row['persistence_week']}, "
                         f"echo {row['echo']}" + (f", individuality {row['individuality']}" if row["individuality"] is not None else ""))
        notes.append("")
    metrics = ROOT / "metrics.csv"
    new = not metrics.exists()
    with metrics.open("a", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()) if rows else ["date"])
        if new:
            w.writeheader()
        w.writerows(rows)
    (ROOT / "notes" / f"{today.isoformat()}.md").write_text("\n".join(notes), encoding="utf-8")
    print(f"recorded {len(current)} agents, {len(rows)} texts -> {ROOT}")


if __name__ == "__main__":
    main()
