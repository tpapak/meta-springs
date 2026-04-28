#!/usr/bin/env python3
"""Read /tmp/{gc,mn}_compare.json and print head-to-head + bias vs truth."""
from __future__ import annotations
import json

# --- truth (from test/synth_bin/synth_meta.json synth_041 entry) ---------
TAU2_TRUE = 0.3904
D_TRUE    = [0.0, -0.5764, -0.563, 1.207, 0.2127, -0.0233]
def truth_pair(a, b):
    return D_TRUE[int(b) - 1] - D_TRUE[int(a) - 1]

gc = json.load(open("/tmp/gc_compare.json"))
mn = json.load(open("/tmp/mn_compare.json"))

# --- timing + tau^2 ------------------------------------------------------
print(f"Dataset: synth_041  (k=20 studies, T=6 treatments, A=40 arms)")
print(f"Truth: τ²={TAU2_TRUE}  d_true={D_TRUE}\n")

print(f"{'method':<10}  {'wall (s)':>10}  {'τ² mode':>9}  {'τ² median':>10}  "
      f"{'τ² mean':>9}  {'τ² 95% CI':>20}")
print("-" * 78)
for nm, src in [("GC", gc), ("multinma", mn)]:
    t2 = src["tau2"]
    print(f"{nm:<10}  {src['wall_sec']:>10.3f}  "
          f"{t2.get('mode', float('nan')):>9.3f}  "
          f"{t2['median']:>10.3f}  {t2['mean']:>9.3f}  "
          f"({t2['ci_lo']:>+.3f}, {t2['ci_hi']:>+.3f})")

print(f"\nTime ratio multinma/GC = {mn['wall_sec']/gc['wall_sec']:.2f}×")

print("\nτ² bias vs truth (= 0.3904):")
for nm, src in [("GC", gc), ("multinma", mn)]:
    t2 = src["tau2"]
    print(f"  {nm:<10}  median bias = {t2['median']-TAU2_TRUE:+.4f}   "
          f"mean bias = {t2['mean']-TAU2_TRUE:+.4f}")

# --- contrast posteriors -------------------------------------------------
gc_eff = {(e["from"], e["to"]): e for e in gc["effects"]}
mn_eff = mn["effects"]

# Both methods' 30 directional contrasts: align by key
keys = sorted(gc_eff.keys(), key=lambda k: (int(k[0]), int(k[1])))

# Show 15 unordered pairs (a < b)
shown = []
for (a, b) in keys:
    if int(a) >= int(b): continue
    shown.append((a, b))

print(f"\n{'pair':<6}  {'truth':>8}  "
      f"{'GC mean':>10}  {'GC 95% CI':>20}  "
      f"{'MN mean':>10}  {'MN 95% CI':>20}  {'|GC-MN|':>9}")
print("-" * 96)

gc_bias_sum = mn_bias_sum = 0.0
gc_se_sum  = mn_se_sum    = 0.0
deltas     = []
for (a, b) in shown:
    g = gc_eff[(a, b)]
    m = mn_eff[f"{a}->{b}"]
    t = truth_pair(a, b)
    gc_bias_sum += (g["mean"] - t)
    mn_bias_sum += (m["mean"] - t)
    gc_se_sum   += (g["mean"] - t) ** 2
    mn_se_sum   += (m["mean"] - t) ** 2
    deltas.append(abs(g["mean"] - m["mean"]))
    print(f"{a}->{b:<3}  {t:>+8.4f}  "
          f"{g['mean']:>+10.4f}  ({g['ci_lo']:>+.3f}, {g['ci_hi']:>+.3f})  "
          f"{m['mean']:>+10.4f}  ({m['ci_lo']:>+.3f}, {m['ci_hi']:>+.3f})  "
          f"{abs(g['mean']-m['mean']):>9.4f}")

n = len(shown)
print(f"\nEffect bias (mean of (post mean - truth) over {n} contrasts):")
print(f"  GC       mean bias = {gc_bias_sum/n:+.4f}   RMSE = {(gc_se_sum/n)**0.5:.4f}")
print(f"  multinma mean bias = {mn_bias_sum/n:+.4f}   RMSE = {(mn_se_sum/n)**0.5:.4f}")
print(f"\nGC vs multinma: max |Δposterior mean| = {max(deltas):.4f}, "
      f"mean = {sum(deltas)/n:.4f}")
