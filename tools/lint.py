#!/usr/bin/env python3
"""Structural lint for pAI v3 (engineering spec §11). Dependency-free.

Fails on:
  - a source file over 1,200 lines;
  - pai_events referenced outside src/kernel/ and src/maintenance/;
  - a runtime SELECT without LIMIT;
  - outside src/kernel/ and src/maintenance/, a SELECT without a type or key predicate
    (an equality or IN in WHERE; a time window alone is not enough);
  - an HTTP client used anywhere but the kernel's one outbound transport
    (src/kernel/transport.lisp, spec section 17), so no request escapes the
    egress guard.
"""
import pathlib, re, sys

ROOT = pathlib.Path(__file__).resolve().parent.parent
MAX_LINES = 1200
STRING = re.compile(r'"((?:[^"\\]|\\.)*)"', re.S)
problems = []

for path in sorted((ROOT / "src").rglob("*.lisp")):
    rel = path.relative_to(ROOT).as_posix()
    text = path.read_text(encoding="utf-8")
    lines = text.count("\n") + 1
    if lines > MAX_LINES:
        problems.append(f"{rel}: {lines} lines (max {MAX_LINES})")
    in_kernel = rel.startswith("src/kernel/")
    # Maintenance pages through whole tables by design (spec §4.3); it is
    # never loaded at runtime, so only the LIMIT rule applies to it.
    in_maintenance = rel.startswith("src/maintenance/")
    # The secrets store is its own small file, not the ledger; it lists its
    # rows by name range.
    in_secrets_store = rel == "src/trust/secrets.lisp"
    if rel != "src/kernel/transport.lisp":
        for n, src_line in enumerate(text.splitlines(), 1):
            code = src_line.split(";", 1)[0]
            if re.search(r"\b(dex|dexador|drakma|usocket):", code, re.I) or "socket-connect" in code:
                problems.append(f"{rel}:{n}: HTTP or socket client outside the outbound transport")
    for m in STRING.finditer(text):
        s = m.group(1)
        line = text.count("\n", 0, m.start()) + 1
        # Maintenance (never loaded at runtime) may read the ledger for
        # census, rebuild and inspection (spec §4.1); nothing else may.
        if "pai_events" in s and not in_kernel and not in_maintenance:
            problems.append(f"{rel}:{line}: pai_events outside the kernel")
        if not re.match(r"\s*SELECT\b", s, re.I) or not re.search(r"\bFROM\b", s, re.I):
            continue
        if not re.search(r"\bLIMIT\b", s, re.I):
            problems.append(f"{rel}:{line}: SELECT without LIMIT")
        if not in_kernel and not in_maintenance and not in_secrets_store:
            where = re.search(r"\bWHERE\b(.*)", s, re.I | re.S)
            if not where or not re.search(r"=\s*\?|\bIN\s*\(|\bMATCH\s*\?", where.group(1), re.I):
                problems.append(f"{rel}:{line}: SELECT without a type or key predicate")

for p in problems:
    print(p)
print(f"lint: {len(problems)} problem(s)")
sys.exit(1 if problems else 0)
