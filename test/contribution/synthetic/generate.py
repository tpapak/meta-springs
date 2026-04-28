"""Synthetic 5-treatment binary NMA dataset for the CINeMA round-trip test.

Network: A-B-C-D-E with 12 studies, one 3-arm. Connected and a couple of
loops so the contribution matrix has interesting structure.
"""
import json
import random
import sys

random.seed(42)

# (study_id, [(treatment, events, n)])
studies = [
    (1,  [("A", 12, 100), ("B", 18, 100)]),
    (2,  [("A", 25, 200), ("B", 35, 200)]),
    (3,  [("A", 8,  80),  ("C", 14, 80)]),
    (4,  [("B", 20, 150), ("C", 24, 150)]),
    (5,  [("B", 16, 120), ("D", 28, 120)]),
    (6,  [("C", 14, 100), ("D", 19, 100)]),
    (7,  [("C", 9,  90),  ("E", 16, 90)]),
    (8,  [("D", 11, 90),  ("E", 14, 90)]),
    (9,  [("A", 7,  60),  ("D", 13, 60)]),
    (10, [("B", 19, 130), ("E", 26, 130)]),
    (11, [("A", 6,  70),  ("B", 11, 70), ("C", 9, 70)]),  # 3-arm
    (12, [("A", 5,  50),  ("E", 13, 50)]),
]

indata = []
for sid, arms in studies:
    for (t, r, n) in arms:
        indata.append({
            "id":           sid,
            "study":        f"study_{sid}",
            "t":            t,
            "r":            r,
            "n":            n,
            "rob":          1,
            "indirectness": 1,
        })

payload = {
    "indata": indata,
    "type":   "long_binary",
    "model":  "random",
    "sm":     "OR",
}
json.dump(payload, sys.stdout)
