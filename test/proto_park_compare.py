#!/usr/bin/env python3
"""Side-by-side prior-sensitivity report on Parkinson NMA."""
import json
gc = json.load(open("/tmp/park_gc.json"))
mn = json.load(open("/tmp/park_mn.json"))

print(f"GC fit: {gc['fit_wall_sec']:.2f}s  (5 reweights {gc['post_wall_sec']:.4f}s)")
print(f"multinma: {mn['total_wall_sec']:.2f}s  (4 separate fits)\n")

print(f"{'prior':<26} {'method':<10} {'τ² mode':>9} {'τ² median':>10} "
      f"{'τ² mean':>9} {'95% CI':>22}")
print("-" * 90)
gc_by = {r['label']: r for r in gc['results']}
mn_by = {r['label']: r for r in mn['results']}

# Show GC's flat first (no multinma equivalent), then matched pairs
for r in gc['results']:
    lbl = r['label']
    print(f"{lbl:<26} {'GC':<10} "
          f"{r['tau2_mode']:>9.4f} {r['tau2_median']:>10.4f} "
          f"{r['tau2_mean']:>9.4f} ({r['tau2_lo']:>+.4f}, {r['tau2_hi']:>+.4f})")
    if lbl in mn_by:
        m = mn_by[lbl]
        print(f"{'':<26} {'multinma':<10} "
              f"{m['tau2_mode']:>9.4f} {m['tau2_median']:>10.4f} "
              f"{m['tau2_mean']:>9.4f} ({m['tau2_lo']:>+.4f}, {m['tau2_hi']:>+.4f})")
    else:
        print(f"{'':<26} {'multinma':<10}  (cannot do flat τ — improper posterior)")
    print()
