#!/usr/bin/env python3
"""Build an arm-level continuous JSON file from the Higgins, Thompson,
Spiegelhalter (2009) Table 1: 14-study set-shifting meta-analysis.

The published table reports SMD and SE per study only. We fake two
arms per study so that gc-rust's continuous likelihood reads it
correctly:

  arm "control"          : y = 0,        sd = se/√2, n = 1
  arm "eating_disorder"  : y = SMD_i,    sd = se/√2, n = 1

Then  variance(contrast) = sd²/n + sd²/n = se²,  matching the paper.
The reference group for relative effects is treatment "control", so
the GC's reported contrast (1 → eating_disorder) IS the SMD.
"""

import json, math
from pathlib import Path

ROOT = Path(__file__).resolve().parent
ROOT.mkdir(parents=True, exist_ok=True)

# Higgins, Thompson, Spiegelhalter (2009), Table 1.
# (study label, SMD, SE)
table1 = [
    ("Steinglass",       0.38, 0.40),
    ("Holliday",         0.07, 0.21),
    ("Tchanturia",       0.52, 0.29),
    ("Tchanturia_s1",    0.85, 0.25),
    ("Tchanturia_s2",    0.45, 0.29),
    ("Murphy_s1",        0.01, 0.35),
    ("Murphy_s2",       -0.58, 0.36),
    ("Mathias_Kent",     0.44, 0.25),
    ("Kingston",         0.46, 0.22),
    ("Thompson",         0.93, 0.47),
    ("Jones_s1",         0.28, 0.24),
    ("Jones_s2",         0.20, 0.28),
    ("Jones_s3",         0.46, 0.23),
    ("Witt",             0.59, 0.36),
]

rows = []
for i, (label, smd, se) in enumerate(table1, start=1):
    sd_per_arm = se / math.sqrt(2.0)
    # control arm
    rows.append({"study": i, "treatment": 1,
                 "mean": 0.0, "sd": sd_per_arm, "n": 1})
    # eating-disorder arm
    rows.append({"study": i, "treatment": 2,
                 "mean": smd, "sd": sd_per_arm, "n": 1})

out_dir = ROOT / "data"
out_dir.mkdir(exist_ok=True)
fpath = out_dir / "higgins2009_setshifting.json"
fpath.write_text(json.dumps(rows, indent=None))
print(f"wrote {fpath} ({len(rows)} arm rows for {len(table1)} studies)")

# Also dump as rep_*.json for gc-rust multi-rep mode (keeps schema uniform)
rep_dir = ROOT / "data_rust"
rep_dir.mkdir(exist_ok=True)
(rep_dir / "rep_01.json").write_text(json.dumps(rows, indent=None))
print(f"wrote {rep_dir}/rep_01.json")
