#!/usr/bin/env python3
"""Build binomial arm-level JSON for the Crins 2014 dataset.
This is the EXACT data that bayesmeta processes via metafor::escalc()
into log-ORs.  gc-rust's binomial path uses the exact binomial
likelihood + per-arm logit-scale RE — same model class as bayesmeta
but without the normal-approximation step."""

import json, os
# Crins 2014 (Röver 2020 Table 3): IL-2RA group (events a, total n1) vs Control (c, n2)
table3_counts = [
    ("Heffron2003",   14, 61, 15, 20),
    ("Gibelli2004",   16, 28, 19, 28),
    ("Schuller2005",   3, 18,  8, 12),
    ("Ganschow2005",   9, 54, 29, 54),
    ("Spada2006",      4, 36, 11, 36),
    ("Gras2008",       0, 50,  3, 34),
]

rows = []
for i, (lbl, a, n1, c, n2) in enumerate(table3_counts, start=1):
    rows.append({"study": i, "treatment": 1, "events": c, "n": n2})  # Control = treatment 1 (ref)
    rows.append({"study": i, "treatment": 2, "events": a, "n": n1})  # IL-2RA = treatment 2

os.makedirs("test/rover2020/data", exist_ok=True)
with open("test/rover2020/data/crins2014_bin.json", "w") as f:
    json.dump(rows, f)
print(f"wrote {len(rows)} arm rows")
