#!/usr/bin/env python3
"""T=100 30-rep bias study report — Rust GC vs multinma.  Reads the lite
schema multi-rep JSONs.  Writes docs/t100_reps_report.html."""
import base64, io, json, math, statistics
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

truth = json.load(open("/tmp/t100_reps_truth.json"))
gc    = json.load(open("/tmp/t100_reps_bin_gc.json"))
mn    = json.load(open("/tmp/t100_reps_bin_mn.json"))

tau2_true = truth["tau2_true"]
d_true    = truth["d_true"]
T, k, NR  = truth["T"], truth["k"], truth["nReps"]

PRIORS = ["HalfNormal(τ; σ=0.5)", "HalfNormal(τ; σ=1)", "HalfCauchy(τ; σ=0.5)"]

def parse_tid(s):
    s = str(s)
    if s.startswith("d[") and s.endswith("]"): s = s[2:-1]
    try: return int(s)
    except ValueError: return None

def collect(results):
    out = {}
    for rep in results:
        for pr in rep["priors"]:
            d = out.setdefault(pr["label"], {"tau2": [], "tau2_in_ci": [], "eff_bias": []})
            t = pr["tau2"]
            d["tau2"].append(t["mean"])
            d["tau2_in_ci"].append(int(t["ci_lo"] <= tau2_true <= t["ci_hi"]))
            for e in pr["effects"]:
                j = parse_tid(e["to"])
                if j is None or j < 1 or j > len(d_true): continue
                truth_eff = d_true[j-1] - d_true[0]
                bias = e["mean"] - truth_eff
                lo = e["mean"] - 1.96 * e["sd"]
                hi = e["mean"] + 1.96 * e["sd"]
                d["eff_bias"].append((j, bias, int(lo <= truth_eff <= hi)))
    return out

def stats(d):
    if not d["tau2"]: return None
    biases = [b for (_, b, _) in d["eff_bias"]]
    in_cis = [c for (_, _, c) in d["eff_bias"]]
    abs_bs = sorted(abs(b) for b in biases)
    return dict(
        tau_b=statistics.fmean([t-tau2_true for t in d["tau2"]]),
        tau_rmse=math.sqrt(statistics.fmean([(t-tau2_true)**2 for t in d["tau2"]])),
        tau_cov=statistics.fmean(d["tau2_in_ci"]),
        eff_b=statistics.fmean(biases) if biases else float("nan"),
        eff_med_ab=abs_bs[len(abs_bs)//2] if abs_bs else float("nan"),
        eff_p95_ab=abs_bs[int(0.95*len(abs_bs))] if abs_bs else float("nan"),
        eff_n_sep=sum(1 for b in biases if abs(b)>5),
        eff_cov=statistics.fmean(in_cis) if in_cis else float("nan"),
    )

gc_data = collect(gc["results"])
mn_data = collect(mn["results"])

def png_b64(fig):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=130, bbox_inches="tight")
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()

# Per-rep wall time
fig, ax = plt.subplots(figsize=(10, 3.5))
gc_w = [r["wall_sec"] for r in gc["results"]]
mn_w = [r["wall_sec"] for r in mn["results"]]
x = np.arange(min(len(gc_w), len(mn_w)))
width = 0.35
ax.bar(x - width/2, gc_w, width, color="#3a7bd5", label=f"GC (Rust, 4 priors via reweight)")
ax.bar(x + width/2, mn_w, width, color="#d54a3a", label=f"multinma (3 separate fits)")
ax.set_xlabel("replicate")
ax.set_ylabel("wall (s)")
ax.set_xticks(np.arange(0, NR, 5))
ax.legend(fontsize=9)
ax.grid(True, alpha=0.3, axis="y")
ax.set_title(f"Per-replicate wall time — T={T}")
fig.tight_layout()
plot_wall = png_b64(fig)

# τ² estimates per rep
fig, axes = plt.subplots(1, len(PRIORS), figsize=(13, 3.5), sharey=True)
for ax, lbl in zip(axes, PRIORS):
    if lbl in gc_data:
        ts = gc_data[lbl]["tau2"]
        ax.scatter([0.85] * len(ts), ts, s=18, alpha=0.7, color="#3a7bd5", label="GC")
    if lbl in mn_data:
        ts = mn_data[lbl]["tau2"]
        ax.scatter([1.15] * len(ts), ts, s=18, alpha=0.7, color="#d54a3a", label="multinma")
    ax.axhline(tau2_true, color="#444", linestyle="--", linewidth=1,
               label=f"truth={tau2_true}")
    ax.set_xticks([1])
    ax.set_xticklabels([lbl], fontsize=8)
    ax.set_xlim(0.5, 1.5)
    ax.grid(True, alpha=0.3)
    if ax is axes[0]:
        ax.set_ylabel("τ² posterior mean")
        ax.legend(fontsize=8, loc="upper right")
fig.suptitle(f"τ² posterior mean across {NR} replicates (truth τ²={tau2_true})",
             fontsize=11)
fig.tight_layout()
plot_tau = png_b64(fig)

def row(src, st):
    if st is None: return ""
    sep = (f"{st['eff_n_sep']}" if st['eff_n_sep']==0
           else f"<b style='color:#c33'>{st['eff_n_sep']}</b>")
    return (f"<tr><td></td><td>{src}</td>"
            f"<td>{st['tau_b']:+.3f}</td><td>{st['tau_rmse']:.3f}</td>"
            f"<td>{st['tau_cov']:.2f}</td>"
            f"<td>{st['eff_b']:+.3f}</td>"
            f"<td>{st['eff_med_ab']:.3f}</td>"
            f"<td>{st['eff_p95_ab']:.3f}</td>"
            f"<td>{sep}</td><td>{st['eff_cov']:.2f}</td></tr>")

rows = []
for lbl in PRIORS:
    rows.append(f"<tr><td colspan=10 class='prior-h'><b>{lbl}</b></td></tr>")
    rows.append(row("GC (Rust)", stats(gc_data[lbl]) if lbl in gc_data else None))
    rows.append(row("multinma", stats(mn_data[lbl]) if lbl in mn_data else None))

speedup = mn["total_wall_sec"] / gc["total_wall_sec"]

html = f"""<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<title>T={T} 30-rep binary bias study</title>
<style>
body {{ font-family: -apple-system, system-ui, sans-serif; max-width: 1200px;
        margin: 2em auto; padding: 0 1em; color: #222; }}
h1 {{ margin-bottom: 0.2em; }}
h2 {{ border-bottom: 1px solid #ddd; padding-bottom: 0.2em; margin-top: 1.5em; }}
.meta {{ color: #555; font-size: 0.95em; }}
table {{ border-collapse: collapse; margin: 1em 0; font-size: 0.92em; }}
th, td {{ padding: 0.4em 0.8em; text-align: right; border-bottom: 1px solid #eee; }}
th {{ background: #f4f4f4; text-align: center; }}
td:first-child, td:nth-child(2) {{ text-align: left; }}
tr.prior-h, td.prior-h {{ background: #fafafa; }}
.win {{ background: #e8f4ff; padding: 0.5em 1em; border-left: 3px solid #38c; }}
img {{ max-width: 100%; height: auto; }}
</style>
</head>
<body>
<h1>T={T} binary bias study — {NR} replicates</h1>
<p class='meta'>
  <b>Truth:</b> τ²={tau2_true}, d<sub>true</sub> ∈ N(0, 0.5²) for j ≠ 1<br>
  <b>Design:</b> T={T} treatments, k={k} studies (160 two-arm + 30 three-arm
  + 10 four-arm), well-behaved (baseline μ ≈ −1, no rare events).
  {NR} replicates with shared topology.<br>
  <b>GC (Rust):</b> 1 fit + 3 prior reweights per rep.
  <b>multinma:</b> 3 separate HMC fits (4 chains × 3000 iter).
</p>

<h2>Wall time</h2>
<table>
  <tr><th>method</th><th>wall (s)</th><th>per replicate</th><th># priors / rep</th></tr>
  <tr><td>GC (Rust)</td><td>{gc['total_wall_sec']:.1f}</td>
      <td>{gc['total_wall_sec']/NR:.2f}</td><td>4 (incl. REML)</td></tr>
  <tr><td>multinma</td><td>{mn['total_wall_sec']:.1f}</td>
      <td>{mn['total_wall_sec']/NR:.2f}</td><td>3</td></tr>
</table>
<p class='win'><b>GC (Rust) is {speedup:.0f}× faster overall</b>
({gc['total_wall_sec']:.1f} s vs {mn['total_wall_sec']:.0f} s)
while doing one more prior (Flat/REML, which multinma cannot represent).</p>

<h2>Per-replicate wall time</h2>
<img src='data:image/png;base64,{plot_wall}'>

<h2>Bias summary</h2>
<table>
  <tr><th>prior</th><th>method</th>
      <th>τ² bias</th><th>τ² RMSE</th><th>τ² 95% cov</th>
      <th>eff bias</th><th>med &#124;eff bias&#124;</th>
      <th>p95 &#124;eff bias&#124;</th>
      <th>#sep</th><th>eff 95% cov</th></tr>
  {''.join(rows)}
</table>

<h2>τ² posterior means (per replicate)</h2>
<img src='data:image/png;base64,{plot_tau}'>

</body>
</html>
"""

with open("docs/t100_reps_report.html", "w") as f:
    f.write(html)
print("Wrote docs/t100_reps_report.html")
