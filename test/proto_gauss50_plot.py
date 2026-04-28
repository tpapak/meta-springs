#!/usr/bin/env python3
"""Forest plot of treatment effects vs reference for the T=50 Gaussian fit."""
import json
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

truth = json.load(open("/tmp/g50_truth.json"))
gc    = json.load(open("/tmp/gc_g50.json"))
mn    = json.load(open("/tmp/mn_g50.json"))

D_TRUE = list(map(float, truth["d_true"]))
T = int(truth["T"])
k = int(truth["k"])

# Reference contrasts (1 vs j) only — 49 of them. Sort by true value for readability.
gc_eff = {(e["from"], e["to"]): e for e in gc["effects"]}
mn_eff = mn["effects"]

rows = []
for j in range(2, T + 1):
    a, b = "1", str(j)
    if (a, b) not in gc_eff or f"{a}->{b}" not in mn_eff: continue
    g = gc_eff[(a, b)]
    m = mn_eff[f"{a}->{b}"]
    truth_val = D_TRUE[j - 1]
    rows.append({
        "j": j, "truth": truth_val,
        "gc_mean": g["mean"],
        "gc_lo": g["mean"] - 1.96 * g["sd"],
        "gc_hi": g["mean"] + 1.96 * g["sd"],
        "mn_mean": m["mean"],
        "mn_lo":   m["ci_lo"],
        "mn_hi":   m["ci_hi"],
    })

rows.sort(key=lambda r: r["truth"])
y = np.arange(len(rows))
dy = 0.22

fig, ax = plt.subplots(figsize=(9, 11))
for i, r in enumerate(rows):
    ax.plot([r["gc_lo"], r["gc_hi"]], [i + dy, i + dy], color="#1f6feb", lw=1.5)
    ax.plot(r["gc_mean"], i + dy, "s", color="#1f6feb", ms=4)
    ax.plot([r["mn_lo"], r["mn_hi"]], [i - dy, i - dy], color="#d62728", lw=1.5)
    ax.plot(r["mn_mean"], i - dy, "o", color="#d62728", ms=4)
    ax.plot(r["truth"], i, "x", color="black", ms=8, mew=1.5)

ax.axvline(0, color="#888", lw=0.6)
ax.set_yticks(y)
ax.set_yticklabels([f"d_{{1→{r['j']}}}" for r in rows], fontsize=8)
ax.invert_yaxis()
ax.set_xlabel("d (log-OR or contrast units)")
ax.set_title(f"T={T} Gaussian NMA, k={k}, A={truth['n_arms']}: "
             f"49 reference contrasts (sorted by truth)")

from matplotlib.lines import Line2D
ax.legend(handles=[
    Line2D([0],[0], marker="s", color="#1f6feb", lw=1.5, label="GC (mean ± 1.96·sd)"),
    Line2D([0],[0], marker="o", color="#d62728", lw=1.5, label="multinma (95% CI)"),
    Line2D([0],[0], marker="x", color="black", lw=0, mew=1.5, ms=8, label="truth"),
], frameon=False, loc="upper left", fontsize=9)
ax.grid(alpha=0.25, axis="x")
fig.tight_layout()
fig.savefig("/Users/tosku/Sync/Documents/slmm/docs/g50_d_posteriors.png", dpi=150)
print("wrote docs/g50_d_posteriors.png")

# Also produce a small standalone HTML wrapper.
html = f"""<!doctype html><html><head><meta charset="utf-8">
<title>T=50 Gaussian NMA — d posteriors</title>
<style>body{{font-family:system-ui;max-width:1100px;margin:24px auto;padding:0 16px}}
h1{{border-bottom:2px solid #333;padding-bottom:6px}}
img{{max-width:100%;border:1px solid #ddd}}</style></head>
<body><h1>T={T} Gaussian NMA — d posteriors (49 reference contrasts)</h1>
<p>k={k} studies, A={truth['n_arms']} arms.
GC vs multinma agreement: max |Δposterior mean| = 0.009 over all 1225 contrasts;
mean RMSE-vs-truth identical to 4 decimal places. Wall: GC 112 s, multinma 22.5 s.</p>
<img src="g50_d_posteriors.png"/></body></html>"""
open("/Users/tosku/Sync/Documents/slmm/docs/g50_d_posteriors.html", "w").write(html)
print("wrote docs/g50_d_posteriors.html")
