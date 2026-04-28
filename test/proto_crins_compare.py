#!/usr/bin/env python3
"""Side-by-side: bayesmeta vs GC under same priors on Crins2014."""
import json

bm = json.load(open("/tmp/crins_bayesmeta.json"))
gc = json.load(open("/tmp/crins_gc.json"))

print(f"bayesmeta total: {bm['bayesmeta_total_sec']:.2f}s "
      f"({len(bm['results'])} priors)")
print(f"GC fit + reweights: {gc['fit_wall_sec']:.3f}s + "
      f"{gc['post_wall_sec']:.4f}s = {gc['fit_wall_sec']+gc['post_wall_sec']:.3f}s\n")

# Map labels (bayesmeta uses "Jeffreys", GC uses "Flat / REML mode")
bm_by = {r['label']: r for r in bm['results']}
gc_by = {r['label']: r for r in gc['results']}

# Match priors: bayesmeta label -> GC label
matches = [
    ("Jeffreys",          "Flat / REML mode"),
    ("HalfNormal(0,0.5)", "HalfNormal(0,0.5) on τ"),
    ("HalfNormal(0,1)",   "HalfNormal(0,1)   on τ"),
    ("HalfCauchy(0,0.5)", "HalfCauchy(0,0.5) on τ"),
]

print(f"{'prior':<20} {'method':<10} {'τ median':>10} {'τ mean':>10} "
      f"{'τ mode':>10} {'95% CI':>22}")
print("-" * 86)
for bm_lbl, gc_lbl in matches:
    b = bm_by[bm_lbl]
    g = gc_by[gc_lbl]
    print(f"{bm_lbl:<20} {'bayesmeta':<10} "
          f"{b['tau_median']:>10.4f} {b['tau_mean']:>10.4f} "
          f"{b['tau_mode']:>10.4f} ({b['tau_lo']:>+.4f}, {b['tau_hi']:>+.4f})")
    print(f"{'':<20} {'GC':<10} "
          f"{g['tau_median']:>10.4f} {g['tau_mean']:>10.4f} "
          f"{g['tau_mode']:>10.4f} ({g['tau_lo']:>+.4f}, {g['tau_hi']:>+.4f})")
    print()
