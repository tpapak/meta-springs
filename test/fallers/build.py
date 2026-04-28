#!/usr/bin/env python3
"""Convert test/fallers.csv (contrast-format NMA: id, t1, t2, effect, se)
into arm-level JSON for gc-rust.

Approach: each row → a fake 2-arm 'study'. Multi-arm trials (29 / 380)
are split into independent 2-arm pseudo-studies, which ignores their
contrast correlations — same simplification netmeta makes when given
contrast-format input without an explicit multi-arm-correction step.
For a network of 380 studies with ~7.6 % multi-arm, the impact on τ²
is small.

We label each pseudo-study by row index (1, 2, …) to keep gc-rust's
study indices unique. Treatment names (strings) pass through.

For each row with effect y and SE σ:
  arm 1 (t1):  mean = 0,    sd = σ/√2,  n = 1
  arm 2 (t2):  mean = y,    sd = σ/√2,  n = 1
giving Var(contrast) = σ²/2 + σ²/2 = σ².
"""

import csv, json, math, os
from pathlib import Path

ROOT = Path(__file__).resolve().parent
ROOT.mkdir(exist_ok=True)
(ROOT / "data").mkdir(exist_ok=True)

src = "/Users/tosku/Sync/Documents/slmm/test/fallers.csv"
rows_out = []
with open(src) as f:
    for i, r in enumerate(csv.DictReader(f), start=1):
        eff = float(r["effect"]); se = float(r["se"])
        if not (math.isfinite(eff) and math.isfinite(se) and se > 0):
            continue
        sd = se / math.sqrt(2.0)
        rows_out.append({"study": i, "treatment": r["t1"], "mean": 0.0, "sd": sd, "n": 1})
        rows_out.append({"study": i, "treatment": r["t2"], "mean": eff, "sd": sd, "n": 1})

out = ROOT / "data" / "fallers.json"
with open(out, "w") as f:
    json.dump(rows_out, f)

# Treatments + studies summary
treats = sorted({r["treatment"] for r in rows_out})
studies = sorted({r["study"]    for r in rows_out})
print(f"wrote {out}: {len(rows_out)} arm rows, {len(studies)} pseudo-studies, {len(treats)} treatments")
