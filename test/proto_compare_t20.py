#!/usr/bin/env python3
"""Side-by-side GC vs multinma report on /tmp/t20.json (T=20)."""
from __future__ import annotations
import json

truth = json.load(open("/tmp/t20_truth.json"))
gc    = json.load(open("/tmp/gc_t20.json"))
mn    = json.load(open("/tmp/mn_t20.json"))

TAU2_TRUE = float(truth["tau2_true"])
D_TRUE    = list(map(float, truth["d_true"]))
T         = int(truth["T"])
def truth_pair(a, b): return D_TRUE[int(b)-1] - D_TRUE[int(a)-1]

print(f"Dataset: T={T} treatments, k={truth['k']} studies, A={truth['n_arms']} arms")
print(f"Truth: τ²={TAU2_TRUE:.4f}\n")

print(f"{'method':<10}  {'wall (s)':>10}  {'τ² mode':>9}  {'τ² median':>10}  "
      f"{'τ² mean':>9}  {'τ² 95% CI':>22}")
print("-" * 80)
for nm, src in [("GC", gc), ("multinma", mn)]:
    t = src["tau2"]
    print(f"{nm:<10}  {src['wall_sec']:>10.3f}  "
          f"{t.get('mode', float('nan')):>9.3f}  "
          f"{t['median']:>10.3f}  {t['mean']:>9.3f}  "
          f"({t['ci_lo']:>+.3f}, {t['ci_hi']:>+.3f})")

print(f"\nTime ratio multinma/GC = {mn['wall_sec']/gc['wall_sec']:.2f}×")

print(f"\nτ² bias vs truth ({TAU2_TRUE:.4f}):")
for nm, src in [("GC", gc), ("multinma", mn)]:
    t = src["tau2"]
    mode_b   = t.get('mode', float('nan')) - TAU2_TRUE
    med_b    = t['median'] - TAU2_TRUE
    mean_b   = t['mean']   - TAU2_TRUE
    print(f"  {nm:<10}  mode bias = {mode_b:+.4f}   "
          f"median bias = {med_b:+.4f}   mean bias = {mean_b:+.4f}")

# Per-contrast comparison
gc_eff = {(e['from'], e['to']): e for e in gc['effects']}
mn_eff = mn['effects']

# Iterate unordered pairs
ints = lambda s: int(s)
keys = sorted(gc_eff.keys(), key=lambda k: (ints(k[0]), ints(k[1])))
unord = [k for k in keys if ints(k[0]) < ints(k[1])]

deltas = []
gc_bias_sum = mn_bias_sum = 0.0
gc_se_sum = mn_se_sum = 0.0
n = 0
for (a, b) in unord:
    g = gc_eff[(a, b)]
    m = mn_eff.get(f"{a}->{b}")
    if m is None: continue
    t = truth_pair(a, b)
    deltas.append(abs(g['mean'] - m['mean']))
    gc_bias_sum += g['mean'] - t
    mn_bias_sum += m['mean'] - t
    gc_se_sum   += (g['mean'] - t) ** 2
    mn_se_sum   += (m['mean'] - t) ** 2
    n += 1

print(f"\nEffect bias (mean of (post mean - truth) over {n} contrasts):")
print(f"  GC       mean bias = {gc_bias_sum/n:+.4f}   RMSE = {(gc_se_sum/n)**0.5:.4f}")
print(f"  multinma mean bias = {mn_bias_sum/n:+.4f}   RMSE = {(mn_se_sum/n)**0.5:.4f}")
print(f"\nGC vs multinma: max |Δposterior mean| = {max(deltas):.4f}, "
      f"mean = {sum(deltas)/n:.4f}")
