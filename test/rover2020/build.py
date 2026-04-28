#!/usr/bin/env python3
"""Build arm-level JSON for the Crins et al. 2014 dataset (Röver 2020,
Table 3) so gc-rust can read it as a pairwise meta-analysis of log-ORs.

Each study has one (yi, σi). We emit two arms with means 0 and yi,
shared per-arm sd σi/√2, n = 1. Then var(arm2 − arm1) = σi². The
bayesmeta() prior was HN(τ; scale = 0.5), corresponding to gc-rust's
HalfNormal(τ; σ = 0.5)."""

import json, math, os

# Röver 2020, Table 3 (pp. 20–21):  yi = log-OR, σi = se
table3 = [
    ("Heffron2003",   -2.31, 0.60),
    ("Gibelli2004",   -0.46, 0.56),
    ("Schuller2005",  -2.30, 0.88),
    ("Ganschow2005",  -1.76, 0.46),
    ("Spada2006",     -1.26, 0.64),
    ("Gras2008",      -2.42, 1.53),
]

rows = []
for i, (lbl, y, se) in enumerate(table3, start=1):
    sd_per_arm = se / math.sqrt(2.0)
    rows.append({"study": i, "treatment": 1, "mean": 0.0, "sd": sd_per_arm, "n": 1})
    rows.append({"study": i, "treatment": 2, "mean": y,   "sd": sd_per_arm, "n": 1})

os.makedirs("test/rover2020/data_rust", exist_ok=True)
with open("test/rover2020/data_rust/rep_01.json", "w") as f:
    json.dump(rows, f)
with open("test/rover2020/data/crins2014.json", "w") as f:
    os.makedirs(os.path.dirname(f.name), exist_ok=True) if False else None
    f.write(json.dumps(rows))
print(f"wrote {len(rows)} arm rows for {len(table3)} studies")
