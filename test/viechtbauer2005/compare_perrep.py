#!/usr/bin/env python3
"""Per-replica side-by-side: gc-rust REML mode (truncated, ≥ 1e-9)
vs metafor REML (untruncated). Same simulated data; both fits read
the same rep_NN.json files. Prints delta-distribution and timing.
"""

import csv, json, statistics as st
from pathlib import Path
from collections import defaultdict

ROOT = Path("/Users/tosku/Sync/Documents/slmm/test/viechtbauer2005")

# load metafor per-rep CSV
mf_rows = []
with open(ROOT / "results" / "metafor_perrep.csv") as f:
    for r in csv.DictReader(f):
        mf_rows.append({
            "condition": r["condition"],
            "rep":       int(r["rep"]),
            "tau2_mf":   float(r["tau2_reml_untrunc"]),
            "wall_mf":   float(r["wall_sec"]),
        })
mf_by = {(r["condition"], r["rep"]): r for r in mf_rows}

# load gc-rust per-rep
gc_by = {}
for cond_label in {r["condition"] for r in mf_rows}:
    d = json.load(open(ROOT / "results" / f"{cond_label}.json"))
    for r in d["results"]:
        gc_by[(cond_label, r["rep"])] = {
            "tau2_gc":   r["reml_mode_iterative"],
            "wall_gc":   r["wall_sec"],
        }

# join
joined = []
for k in sorted(mf_by.keys()):
    if k not in gc_by: continue
    j = {**mf_by[k], **gc_by[k]}
    j["delta"] = j["tau2_gc"] - j["tau2_mf"]
    joined.append(j)

print(f"Joined {len(joined)} replicas across {len({r['condition'] for r in joined})} conditions")
print()

# Per-condition summary
by_cond = defaultdict(list)
for r in joined: by_cond[r["condition"]].append(r)

print(f"{'condition':<22} {'n':>5} {'mean Δ':>10} {'sd Δ':>10} {'max|Δ|':>10}  "
      f"{'gc median ms':>14} {'mf median ms':>14}  {'speedup':>8}")
print("-"*120)
for cond in sorted(by_cond):
    rs = by_cond[cond]
    deltas = [r["delta"] for r in rs]
    gc_t   = [r["wall_gc"]*1000 for r in rs]
    mf_t   = [r["wall_mf"]*1000 for r in rs]
    print(f"{cond:<22} {len(rs):>5} "
          f"{st.mean(deltas):>+10.5f} {st.pstdev(deltas):>10.5f} "
          f"{max(abs(d) for d in deltas):>10.5f}  "
          f"{st.median(gc_t):>14.2f} {st.median(mf_t):>14.2f}  "
          f"{st.median(mf_t)/st.median(gc_t):>8.1f}x")

# Overall delta histogram (text)
print()
all_d = [r["delta"] for r in joined]
print(f"All-replica delta (gc-rust − metafor) on the SAME data:")
print(f"  n        = {len(all_d)}")
print(f"  mean     = {st.mean(all_d):+.5f}")
print(f"  sd       = {st.pstdev(all_d):.5f}")
print(f"  max |Δ|  = {max(abs(d) for d in all_d):.5f}")
print(f"  fraction |Δ| < 1e-3:  {sum(1 for d in all_d if abs(d) < 1e-3)/len(all_d)*100:.1f}%")
print(f"  fraction |Δ| < 1e-6:  {sum(1 for d in all_d if abs(d) < 1e-6)/len(all_d)*100:.1f}%")

# Where does delta come from?
neg_metafor = [r for r in joined if r["tau2_mf"] < 0]
pos_metafor = [r for r in joined if r["tau2_mf"] >= 0]
print()
print(f"  replicates where metafor went negative:  {len(neg_metafor)} ({len(neg_metafor)/len(joined)*100:.1f}%)")
if neg_metafor:
    nd = [r["delta"] for r in neg_metafor]
    print(f"     mean delta on those: {st.mean(nd):+.5f}  (gc-rust clamps to ≈ 0, so delta = -metafor τ² ≈ |negative|)")
if pos_metafor:
    pd = [r["delta"] for r in pos_metafor]
    print(f"  replicates where metafor stayed positive: {len(pos_metafor)} ({len(pos_metafor)/len(joined)*100:.1f}%)")
    print(f"     mean delta on those: {st.mean(pd):+.6f}  (this is the pure algorithmic disagreement)")
    print(f"     sd                : {st.pstdev(pd):.6f}")
    print(f"     max |Δ|           : {max(abs(d) for d in pd):.6f}")

# total wall comparison
total_gc = sum(r["wall_gc"] for r in joined)
total_mf = sum(r["wall_mf"] for r in joined)
print()
print(f"Total wall time (sum over {len(joined)} fits):")
print(f"  gc-rust:  {total_gc:.2f} s   ({total_gc/len(joined)*1000:.2f} ms/fit)")
print(f"  metafor:  {total_mf:.2f} s   ({total_mf/len(joined)*1000:.2f} ms/fit)")
print(f"  speedup:  {total_mf/total_gc:.1f}x")
