#!/usr/bin/env python3
"""Combine GC + spring-REML (Haskell) and netmeta (R) outputs and compute
bias / RMSE / coverage of the τ² estimators against the simulation truth.

Reads:
  test/sim_t20/results/gc_sreml.json
  test/sim_t20/results/netmeta.json
  test/sim_t20/data/manifest.json

Writes:
  test/sim_t20/results/combined.json
  Prints a bias/RMSE table to stdout.
"""

import json
import statistics as st
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent

gc_rows = json.load(open(ROOT / "results" / "gc_sreml.json"))["results"]
nm_rows = json.load(open(ROOT / "results" / "netmeta.json"))["results"]

key = lambda r: (r["tau2_idx"], r["rep"])
gc_by = {key(r): r for r in gc_rows}
nm_by = {key(r): r for r in nm_rows}

combined = []
for k, g in sorted(gc_by.items()):
    n = nm_by.get(k, {})
    combined.append({
        "tau2_idx":     g["tau2_idx"],
        "tau2_true":    g["tau2_true"],
        "rep":          g["rep"],
        "k_studies":    g["k_studies"],
        "multi_arm_k":  g["multi_arm_k"],
        "gc_mode_tau2":     g.get("gc_mode_tau2"),
        "spring_reml_tau2": g.get("spring_reml_tau2"),
        "netmeta_tau2":     n.get("netmeta_tau2"),
    })

with open(ROOT / "results" / "combined.json", "w") as f:
    json.dump({"results": combined}, f, indent=2)

# Bias / RMSE per τ² level
def rms(xs): return (sum(x*x for x in xs)/len(xs))**0.5

groups = defaultdict(list)
for r in combined: groups[r["tau2_true"]].append(r)

def stat(xs, true):
    err = [x - true for x in xs if x is not None]
    if not err: return None
    return {
        "n":     len(err),
        "mean":  st.mean([x + true for x in err]),  # mean(estimate)
        "bias":  st.mean(err),
        "rmse":  rms(err),
        "sd":    st.pstdev(err),
        "min":   min(x + true for x in err),
        "max":   max(x + true for x in err),
    }

print("=" * 96)
print(f"Simulation: T=20 treatments, S=100 studies, 50 replicas per τ², per-arm iid RE (Lu-Ades)")
print("=" * 96)

for tau2_true in sorted(groups):
    rows = groups[tau2_true]
    print(f"\nτ²_true = {tau2_true}   (n={len(rows)} replicas, multi-arm avg = "
          f"{st.mean([r['multi_arm_k'] for r in rows]):.0f}/{rows[0]['k_studies']})")
    print(f"  {'estimator':<14}  {'mean(τ̂²)':>10}  {'bias':>10}  {'RMSE':>10}  "
          f"{'sd':>10}  {'[min, max]':>22}")
    print("  " + "-"*84)
    for label, fld in [("GC mode",     "gc_mode_tau2"),
                       ("spring-REML", "spring_reml_tau2"),
                       ("netmeta REML","netmeta_tau2")]:
        xs = [r[fld] for r in rows if r[fld] is not None]
        s = stat(xs, tau2_true)
        if s is None:
            print(f"  {label:<14}   (no data)"); continue
        print(f"  {label:<14}  {s['mean']:>10.5f}  "
              f"{s['bias']:>+10.5f}  {s['rmse']:>10.5f}  {s['sd']:>10.5f}  "
              f"[{s['min']:.4f}, {s['max']:.4f}]")

# Pairwise agreement
print("\n" + "=" * 96)
print("Inter-estimator agreement (mean ± sd of pairwise differences, all 150 replicas)")
print("=" * 96)

def diff_stats(name, fa, fb):
    ds = [r[fa] - r[fb] for r in combined if r[fa] is not None and r[fb] is not None]
    if not ds: print(f"  {name}: no data"); return
    print(f"  {name:<28}  n={len(ds):3d}  mean={st.mean(ds):+.5f}  "
          f"sd={st.pstdev(ds):.5f}  RMS={rms(ds):.5f}  "
          f"max|Δ|={max(abs(d) for d in ds):.5f}")

diff_stats("GC mode − spring-REML",   "gc_mode_tau2",     "spring_reml_tau2")
diff_stats("GC mode − netmeta REML",  "gc_mode_tau2",     "netmeta_tau2")
diff_stats("spring-REML − netmeta",   "spring_reml_tau2", "netmeta_tau2")
