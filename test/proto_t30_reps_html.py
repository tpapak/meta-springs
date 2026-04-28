#!/usr/bin/env python3
"""Render the T=30 multi-arm bias study as a self-contained HTML report
covering BOTH binary and Gaussian regimes side-by-side.  Reads
/tmp/t30_reps_truth.json + /tmp/t30_reps_{bin,cont}_{gc,mn}.json,
writes docs/t30_reps_report.html.
"""
import base64, io, json, math, os, statistics
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

truth = json.load(open("/tmp/t30_reps_truth.json"))
bin_gc = json.load(open("/tmp/t30_reps_bin_gc.json"))
bin_mn = json.load(open("/tmp/t30_reps_bin_mn.json"))
cont_gc = json.load(open("/tmp/t30_reps_cont_gc.json"))
cont_mn_path = "/tmp/t30_reps_cont_mn.json"
cont_mn = json.load(open(cont_mn_path)) if os.path.exists(cont_mn_path) else None

tau2_true = truth["tau2_true"]
d_true    = truth["d_true"]
T, k, NR  = truth["T"], truth["k"], truth["nReps"]

PRIORS_ORDER = ["Flat (REML)", "HalfNormal(τ; σ=0.5)",
                "HalfNormal(τ; σ=1)", "HalfCauchy(τ; σ=0.5)"]

def parse_tid(s):
    s = str(s)
    if s.startswith("d[") and s.endswith("]"):
        s = s[2:-1]
    try:
        return int(s)
    except ValueError:
        return None

def collect(results):
    out = {}
    for rep in results:
        for pr in rep["priors"]:
            d = out.setdefault(pr["label"],
                {"tau2": [], "tau2_in_ci": [], "eff_bias": []})
            t = pr["tau2"]
            d["tau2"].append(t["mean"])
            d["tau2_in_ci"].append(int(t["ci_lo"] <= tau2_true <= t["ci_hi"]))
            for e in pr["effects"]:
                j = parse_tid(e["to"])
                if j is None or j < 1 or j > len(d_true):
                    continue
                truth_eff = d_true[j-1] - d_true[0]
                bias = e["mean"] - truth_eff
                lo = e["mean"] - 1.96 * e["sd"]
                hi = e["mean"] + 1.96 * e["sd"]
                d["eff_bias"].append((j, bias, int(lo <= truth_eff <= hi)))
    return out

def stats(d):
    n = len(d["tau2"])
    if n == 0:
        return None
    tau_b   = statistics.fmean([t - tau2_true for t in d["tau2"]])
    tau_rmse = math.sqrt(statistics.fmean(
        [(t - tau2_true) ** 2 for t in d["tau2"]]))
    tau_cov = statistics.fmean(d["tau2_in_ci"])
    biases  = [b for (_, b, _) in d["eff_bias"]]
    in_cis  = [c for (_, _, c) in d["eff_bias"]]
    eff_b   = statistics.fmean(biases) if biases else float("nan")
    # Robust effect-bias stats: median |bias| (insensitive to separation
    # outliers) + count of |bias|>5 (separation indicator).
    abs_bs  = sorted(abs(b) for b in biases)
    eff_med_ab = abs_bs[len(abs_bs)//2] if abs_bs else float("nan")
    eff_p95_ab = abs_bs[int(0.95*len(abs_bs))] if abs_bs else float("nan")
    eff_n_sep  = sum(1 for b in biases if abs(b) > 5)
    eff_rmse = (math.sqrt(statistics.fmean([b * b for b in biases]))
                if biases else float("nan"))
    eff_cov = statistics.fmean(in_cis) if in_cis else float("nan")
    return dict(n=n, tau_b=tau_b, tau_rmse=tau_rmse, tau_cov=tau_cov,
                eff_b=eff_b, eff_rmse=eff_rmse, eff_cov=eff_cov,
                eff_med_ab=eff_med_ab, eff_p95_ab=eff_p95_ab,
                eff_n_sep=eff_n_sep, eff_n=len(biases))

def png_b64(fig):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=130, bbox_inches="tight")
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()

# ------------------ collect both regimes ------------------
bin_gc_data  = collect(bin_gc["results"])
bin_mn_data  = collect(bin_mn["results"])
cont_gc_data = collect(cont_gc["results"])
cont_mn_data = collect(cont_mn["results"]) if cont_mn else {}

# ------------------ τ² strip plot, both regimes ------------------
def tau_strip(gc_data, mn_data, regime_label):
    priors_for_plot = PRIORS_ORDER
    fig, axes = plt.subplots(1, len(priors_for_plot), figsize=(13, 3.5),
                             sharey=True)
    for ax, lbl in zip(axes, priors_for_plot):
        if lbl in gc_data:
            ts = gc_data[lbl]["tau2"]
            ax.scatter([0.85] * len(ts), ts, s=18, alpha=0.7,
                       color="#3a7bd5", label="GC")
        if lbl in mn_data:
            ts = mn_data[lbl]["tau2"]
            ax.scatter([1.15] * len(ts), ts, s=18, alpha=0.7,
                       color="#d54a3a", label="multinma")
        ax.axhline(tau2_true, color="#444", linestyle="--", linewidth=1,
                   label=f"truth τ²={tau2_true}")
        ax.set_xticks([1])
        ax.set_xticklabels([lbl], fontsize=8)
        ax.set_xlim(0.5, 1.5)
        ax.grid(True, alpha=0.3)
        if ax is axes[0]:
            ax.set_ylabel("τ² posterior mean")
            ax.legend(fontsize=8, loc="upper right")
    fig.suptitle(f"τ² posterior mean — {regime_label}", fontsize=11)
    fig.tight_layout()
    return png_b64(fig)

# ------------------ Effect bias histogram, both regimes ------------------
def eff_hist(gc_data, mn_data, regime_label):
    priors_for_plot = PRIORS_ORDER
    fig, axes = plt.subplots(1, len(priors_for_plot), figsize=(13, 3.5),
                             sharey=True)
    bins = [-3 + 0.2 * i for i in range(31)]
    for ax, lbl in zip(axes, priors_for_plot):
        if lbl in gc_data:
            b = [bias for (_, bias, _) in gc_data[lbl]["eff_bias"]]
            ax.hist(b, bins=bins, alpha=0.5, color="#3a7bd5",
                    label="GC", density=True)
        if lbl in mn_data:
            b = [bias for (_, bias, _) in mn_data[lbl]["eff_bias"]]
            ax.hist(b, bins=bins, alpha=0.5, color="#d54a3a",
                    label="multinma", density=True)
        ax.axvline(0, color="#444", linestyle="--", linewidth=1)
        ax.set_title(lbl, fontsize=9)
        ax.set_xlabel("effect bias")
        ax.grid(True, alpha=0.3)
        if ax is axes[0]:
            ax.set_ylabel("density")
            ax.legend(fontsize=8)
    fig.suptitle(f"Effect bias — {regime_label}", fontsize=11)
    fig.tight_layout()
    return png_b64(fig)

plot_tau_bin  = tau_strip(bin_gc_data,  bin_mn_data,  "binary")
plot_eff_bin  = eff_hist (bin_gc_data,  bin_mn_data,  "binary")
plot_tau_cont = tau_strip(cont_gc_data, cont_mn_data, "Gaussian")
plot_eff_cont = eff_hist (cont_gc_data, cont_mn_data, "Gaussian")

# ------------------ Summary table ------------------
def row_html(src, st):
    if st is None:
        return (f"<tr><td></td><td>{src}</td>"
                f"<td colspan=8 style='color:#888'>"
                f"(cannot do flat τ — improper posterior)</td></tr>")
    sep_str = (f"{st['eff_n_sep']}" if st['eff_n_sep'] == 0
               else f"<b style='color:#c33'>{st['eff_n_sep']}</b>")
    return (f"<tr><td></td><td>{src}</td>"
            f"<td>{st['tau_b']:+.3f}</td><td>{st['tau_rmse']:.3f}</td>"
            f"<td>{st['tau_cov']:.2f}</td>"
            f"<td>{st['eff_b']:+.3f}</td>"
            f"<td>{st['eff_med_ab']:.3f}</td>"
            f"<td>{st['eff_p95_ab']:.3f}</td>"
            f"<td>{sep_str}</td>"
            f"<td>{st['eff_cov']:.2f}</td></tr>")

def regime_table(gc_data, mn_data, regime_label, gc_wall, mn_wall):
    rows = []
    for lbl in PRIORS_ORDER:
        rows.append(f"<tr><td colspan=8 class='prior-h'><b>{lbl}</b></td></tr>")
        rows.append(row_html("GC",
                    stats(gc_data[lbl]) if lbl in gc_data else None))
        if lbl in mn_data:
            rows.append(row_html("multinma", stats(mn_data[lbl])))
        elif lbl == "Flat (REML)":
            rows.append("<tr><td></td><td>multinma</td>"
                        "<td colspan=6 style='color:#888'>"
                        "(cannot do flat τ — improper posterior)</td></tr>")
    speedup = (mn_wall / gc_wall) if (gc_wall and mn_wall) else float("nan")
    return f"""
    <h3>{regime_label}</h3>
    <p class='meta'>GC wall: {gc_wall:.1f}s &nbsp;·&nbsp;
       multinma wall: {mn_wall if mn_wall else float('nan'):.1f}s
       &nbsp;·&nbsp; <b>GC {speedup:.1f}× faster</b></p>
    <table>
      <tr><th>prior</th><th>method</th>
          <th>τ² bias</th><th>τ² RMSE</th><th>τ² 95% cov</th>
          <th>eff bias</th><th>med &#124;eff bias&#124;</th>
          <th>p95 &#124;eff bias&#124;</th>
          <th>#sep</th><th>eff 95% cov</th></tr>
      {''.join(rows)}
    </table>
    """

bin_table = regime_table(bin_gc_data, bin_mn_data, "Binary",
                         bin_gc["total_wall_sec"], bin_mn["total_wall_sec"])
cont_table = regime_table(cont_gc_data, cont_mn_data, "Gaussian",
                          cont_gc["total_wall_sec"],
                          (cont_mn["total_wall_sec"] if cont_mn else 0))

cont_warning = ("" if cont_mn else
    "<p class='note'><b>Gaussian multinma still running</b> — table and plots "
    "show GC only for the Gaussian regime.</p>")
engine_note = ("<p class='note'><b>Binary GC numbers from gc-rust (Rust port).</b>"
               " The Rust binary reproduces the same Laplace posteriors as the"
               " Haskell library (validated on diabetes within 6e-4 on τ²,"
               " 4e-6 on effects) but uses LAPACK-backed LU, giving ~110×"
               " wall-time speedup at T=30. Gaussian numbers remain from the"
               " Haskell library; Rust has not yet ported the Gaussian path.</p>")

html = f"""<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<title>T={T} multi-arm NMA bias study — GC vs multinma</title>
<style>
body {{ font-family: -apple-system, system-ui, sans-serif; max-width: 1200px;
        margin: 2em auto; padding: 0 1em; color: #222; }}
h1 {{ margin-bottom: 0.2em; }}
h2 {{ border-bottom: 1px solid #ddd; padding-bottom: 0.2em; margin-top: 1.5em; }}
h3 {{ margin-top: 0.5em; }}
.meta {{ color: #555; font-size: 0.95em; }}
table {{ border-collapse: collapse; margin: 1em 0; font-size: 0.92em; }}
th, td {{ padding: 0.4em 0.8em; text-align: right; border-bottom: 1px solid #eee; }}
th {{ background: #f4f4f4; text-align: center; }}
td:first-child, td:nth-child(2) {{ text-align: left; }}
tr.prior-h, td.prior-h {{ background: #fafafa; }}
.note {{ background: #f8f8f0; padding: 0.5em 1em; border-left: 3px solid #c89; }}
img {{ max-width: 100%; height: auto; }}
.regime-grid {{ display: grid; grid-template-columns: 1fr 1fr; gap: 1em; }}
@media (max-width: 900px) {{ .regime-grid {{ grid-template-columns: 1fr; }} }}
</style>
</head>
<body>
<h1>T={T} multi-arm NMA — bias study</h1>
<p class='meta'>
  <b>Truth:</b> τ²&nbsp;= {tau2_true:.3f},
  d<sub>true</sub> ∈ N(0, 0.8²) for j ≠ 1<br>
  <b>Design:</b> {T} treatments, {k} studies — 48 two-arm + 9 three-arm + 3 four-arm,
  {NR} replicates with shared topology and shared latent draws across binary
  and Gaussian regimes (only the outcome process differs).<br>
  <b>GC:</b> spring grand-canonical, adaptive 100-pt log-spaced τ² grid,
      Laplace mixture posterior — one fit, four priors via reweighting<br>
  <b>multinma:</b> HMC (Stan), 4 chains × 3000 iter, 1000 warmup — one fit per prior
</p>

{cont_warning}
{engine_note}

<h2>Binary &amp; Gaussian summary</h2>
<div class='regime-grid'>
  <div>{bin_table}</div>
  <div>{cont_table}</div>
</div>
<p class='meta'>
  Bias point estimate = posterior mean.  τ² CI = 2.5th–97.5th percentiles of
  the posterior.  Effect 95% CI = mean ± 1.96·sd of the marginal mixture
  (GC) or HMC posterior (multinma).  Coverage is fraction of (rep, contrast)
  pairs where the CI contains the truth (target = 0.95).<br>
  <b>med &#124;eff bias&#124;</b> = median of |posterior mean − truth| across
  all (rep, contrast) pairs (robust to separation outliers).
  <b>p95 &#124;eff bias&#124;</b> = 95th percentile of the same.
  <b>#sep</b> = count of single contrasts with |bias| &gt; 5 (a proxy for
  separation/non-identifiability — typically a treatment with 0 events in
  all its trials in a given replicate).
</p>

<h2>τ² posterior means (per replicate)</h2>
<h3>Binary</h3>
<img src='data:image/png;base64,{plot_tau_bin}'>
<h3>Gaussian</h3>
<img src='data:image/png;base64,{plot_tau_cont}'>

<h2>Effect bias distribution</h2>
<p class='meta'>{NR} reps × ~{T-1} contrasts (1→j) per prior =
  ~{NR*(T-1)} bias points each (varies if a treatment is absent in a rep).</p>
<h3>Binary</h3>
<img src='data:image/png;base64,{plot_eff_bin}'>
<h3>Gaussian</h3>
<img src='data:image/png;base64,{plot_eff_cont}'>

<h2>Topology</h2>
<p class='meta'>
  Multi-arm coverage: 80% two-arm, 15% three-arm, 5% four-arm — matching
  the typical mix in published NMA datasets.  Each non-reference treatment
  appears in ≥1 reference-anchored two-arm study (round-robin allocation),
  with additional appearances coming from random multi-arm studies.
</p>

</body>
</html>
"""

with open("docs/t30_reps_report.html", "w") as f:
    f.write(html)
print("Wrote docs/t30_reps_report.html")
