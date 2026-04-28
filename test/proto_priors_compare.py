#!/usr/bin/env python3
"""Side-by-side report: GC reweight vs multinma re-fits across M priors."""
import json

gc = json.load(open("/tmp/gc_priors.json"))
mn = json.load(open("/tmp/mn_priors.json"))

print(f"GC total wall   : {gc['total_wall_sec']:>7.2f} s "
      f"  (1 fit {gc['fit_wall_sec']:.2f}s + reweights {gc['post_wall_sec']:.4f}s)")
print(f"multinma total  : {mn['total_wall_sec']:>7.2f} s "
      f"  ({len(mn['summaries'])} fits)")
ratio = mn['total_wall_sec'] / gc['total_wall_sec']
print(f"ratio multinma/GC = {ratio:.2f}×\n")

# Align rows by prior label
gc_by = {s["prior"]: s for s in gc["summaries"]}
mn_by = {s["prior"]: s for s in mn["summaries"]}

print(f"{'prior':<22} {'method':<10} "
      f"{'mode':>8} {'median':>8} {'mean':>8} {'95% CI':>22}")
print("-" * 86)
for pr in [s["prior"] for s in mn["summaries"]]:
    g = gc_by[pr]; m = mn_by[pr]
    for nm, src in [("GC", g), ("multinma", m)]:
        print(f"{pr:<22} {nm:<10} "
              f"{src['mode']:>8.4f} {src['median']:>8.4f} {src['mean']:>8.4f} "
              f"({src['ci_lo']:>+.3f}, {src['ci_hi']:>+.3f})")
    print()
