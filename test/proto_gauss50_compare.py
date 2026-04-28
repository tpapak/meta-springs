#!/usr/bin/env python3
"""Side-by-side report: /tmp/{gc,mn}_g50.json + /tmp/g50_truth.json."""
from __future__ import annotations
import json

truth = json.load(open("/tmp/g50_truth.json"))
gc    = json.load(open("/tmp/gc_g50.json"))
mn    = json.load(open("/tmp/mn_g50.json"))

TAU2_TRUE = float(truth["tau2_true"])
D_TRUE    = list(map(float, truth["d_true"]))
T = int(truth["T"])
def truth_pair(a, b): return D_TRUE[int(b)-1] - D_TRUE[int(a)-1]

print(f"Dataset: T={T}, k={truth['k']}, A={truth['n_arms']} arms (Gaussian)")
print(f"Truth: τ²={TAU2_TRUE:.4f}\n")

print(f"{'method':<10}  {'wall (s)':>10}  {'τ² mode':>9}  {'τ² median':>10}  "
      f"{'τ² mean':>9}  {'τ² 95% CI':>22}")
print("-" * 80)
for nm, src in [("GC", gc), ("multinma", mn)]:
    t = src["tau2"]
    print(f"{nm:<10}  {src['wall_sec']:>10.3f}  "
          f"{t.get('mode', float('nan')):>9.4f}  "
          f"{t['median']:>10.4f}  {t['mean']:>9.4f}  "
          f"({t['ci_lo']:>+.4f}, {t['ci_hi']:>+.4f})")

print(f"\nTime ratio multinma/GC = {mn['wall_sec']/gc['wall_sec']:.2f}×")

print(f"\nτ² bias vs truth ({TAU2_TRUE:.4f}):")
for nm, src in [("GC", gc), ("multinma", mn)]:
    t = src["tau2"]
    print(f"  {nm:<10}  mode bias = {t.get('mode',float('nan'))-TAU2_TRUE:+.4f}   "
          f"median bias = {t['median']-TAU2_TRUE:+.4f}   "
          f"mean bias = {t['mean']-TAU2_TRUE:+.4f}")

# Per-contrast: GC has just (mean, sd) at the GC mode; multinma has full posterior.
gc_eff = {(e['from'], e['to']): e for e in gc['effects']}
mn_eff = mn['effects']

ints = lambda s: int(s)
unord = sorted({(a, b) for (a, b) in gc_eff.keys() if ints(a) < ints(b)},
               key=lambda k: (ints(k[0]), ints(k[1])))
gc_bs = mn_bs = 0.0
gc_se = mn_se = 0.0
deltas = []
n = 0
for (a, b) in unord:
    g = gc_eff[(a, b)]
    m = mn_eff.get(f"{a}->{b}")
    if m is None: continue
    t = truth_pair(a, b)
    gc_bs += g['mean'] - t; mn_bs += m['mean'] - t
    gc_se += (g['mean'] - t)**2; mn_se += (m['mean'] - t)**2
    deltas.append(abs(g['mean'] - m['mean']))
    n += 1

print(f"\nEffect bias (mean of (post mean - truth) over {n} contrasts):")
print(f"  GC       mean bias = {gc_bs/n:+.4f}   RMSE = {(gc_se/n)**0.5:.4f}")
print(f"  multinma mean bias = {mn_bs/n:+.4f}   RMSE = {(mn_se/n)**0.5:.4f}")
print(f"\nGC vs multinma: max |Δposterior mean| = {max(deltas):.4f}, "
      f"mean = {sum(deltas)/n:.4f}")
