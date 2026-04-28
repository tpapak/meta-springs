#!/usr/bin/env python3
"""Render posterior overlays (GC vs multinma) for one representative
replicate, both regimes.  Reads /tmp/t30_post_<regime>_{gc,mn}.json,
appends a `posteriors` section to docs/t30_reps_report.html — replacing the
existing version."""
import base64, io, json
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

truth = json.load(open("/tmp/t30_reps_truth.json"))
tau2_true = truth["tau2_true"]
d_true    = truth["d_true"]

def load_pair(regime):
    gc = json.load(open(f"/tmp/t30_post_{regime}_gc.json"))
    mn = json.load(open(f"/tmp/t30_post_{regime}_mn.json"))
    return gc, mn

def png_b64(fig):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=130, bbox_inches="tight")
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()

# τ²: GC posterior is a discrete weighted grid (τ²ᵢ, wᵢ).  The implied density
# is wᵢ / Δτ²ᵢ where Δτ²ᵢ is the local (log-spaced) grid spacing — no kernel
# smoothing needed.  multinma posterior is samples → histogram.
def tau2_overlay(regime, gc, mn):
    """Rebin BOTH posteriors onto a common linear grid so heights are
    directly comparable (equal area ↔ equal mass)."""
    ts  = np.array(gc["tau2"])
    ws  = np.array(gc["weight"])
    samples = np.array(mn["tau2_samples"])
    # Common x-range: 99.5th percentile of either posterior.
    order = np.argsort(ts); ts_s = ts[order]; ws_s = ws[order]
    cum = np.cumsum(ws_s)
    gc_q995 = ts_s[min(np.searchsorted(cum, 0.995), len(ts_s) - 1)]
    x_lo, x_hi = 0.0, 1.05 * max(np.quantile(samples, 0.995), gc_q995)

    nbins = 40
    edges = np.linspace(x_lo, x_hi, nbins + 1)
    # GC: interpolate the cumulative posterior CDF onto the linear edges.
    # cum[i] = ∑_{j ≤ i} wⱼ at sorted grid points; mass per linear bin =
    # CDF(right) − CDF(left).  Avoids the "missing bins" artefact that
    # appears if you just bucket the point masses.
    cdf_x = np.concatenate([[0.0], ts_s, [max(ts_s[-1], x_hi) + 1]])
    cdf_y = np.concatenate([[0.0], cum,  [1.0]])
    cdf_at = np.interp(edges, cdf_x, cdf_y)
    gc_mass = np.diff(cdf_at)
    gc_density = gc_mass / (edges[1] - edges[0])
    # multinma: histogram on the same edges.
    mn_density, _ = np.histogram(samples, bins=edges, density=True)

    fig, ax = plt.subplots(figsize=(8, 3.6))
    ax.stairs(gc_density, edges, color="#3a7bd5", linewidth=2,
              label="GC (Laplace mixture)", baseline=0, fill=True, alpha=0.35)
    ax.stairs(mn_density, edges, color="#d54a3a", linewidth=2,
              label="multinma (HMC)", baseline=0, fill=True, alpha=0.35)
    ax.axvline(tau2_true, color="#444", linestyle="--", linewidth=1,
               label=f"truth τ²={tau2_true}")
    ax.set_xlim(x_lo, x_hi)
    ax.set_xlabel("τ²")
    ax.set_ylabel("density (common-bin)")
    ax.set_title(f"τ² posterior — {regime}, rep 1, prior = {gc['prior_label']}")
    ax.legend(loc="upper right", fontsize=9)
    ax.grid(True, alpha=0.3)
    fig.tight_layout()
    return png_b64(fig)

# Effects: GC mixture is Σ wᵢ · N(μᵢ, σᵢ²); multinma has direct samples.
def effects_overlay(regime, gc, mn, n_show=6):
    # pick contrasts to show: the ones with largest |truth| (most informative)
    by_to = {c["to"]: c for c in mn["contrasts"]}
    candidates = []
    for c in gc["contrasts"]:
        try:
            j = int(c["to"])
        except ValueError:
            continue
        if j < 1 or j > len(d_true):
            continue
        truth_eff = d_true[j-1] - d_true[0]
        if c["to"] in by_to:
            candidates.append((abs(truth_eff), c["to"], truth_eff, c, by_to[c["to"]]))
    candidates.sort(reverse=True)
    selected = candidates[:n_show]

    cols = 3
    rows = (len(selected) + cols - 1) // cols
    fig, axes = plt.subplots(rows, cols, figsize=(13, 3.0 * rows))
    axes = axes.flatten() if rows * cols > 1 else [axes]

    for ax, (_, lbl, truth_eff, gc_c, mn_c) in zip(axes, selected):
        pts = gc_c["points"]
        ws  = np.array([p["weight"] for p in pts])
        mus = np.array([p["mean"]   for p in pts])
        vars_ = np.array([max(p["var"], 1e-9) for p in pts])
        sds = np.sqrt(vars_)
        samples = np.array(mn_c["samples"])
        x_lo = min(mus.min() - 3 * sds.max(), samples.min())
        x_hi = max(mus.max() + 3 * sds.max(), samples.max())
        # clamp to reasonable range to avoid extreme outlier dominance
        x_lo = max(x_lo, -10)
        x_hi = min(x_hi, 10)
        xs = np.linspace(x_lo, x_hi, 600)
        # GC: mixture of Gaussians
        gc_density = np.zeros_like(xs)
        for w, mu, sd in zip(ws, mus, sds):
            gc_density += w * np.exp(-((xs - mu) ** 2) / (2 * sd ** 2)) / (sd * np.sqrt(2 * np.pi))
        # multinma: histogram (KDE was unused; histogram is faster and clear)
        hist, edges = np.histogram(samples, bins=80, range=(x_lo, x_hi), density=True)
        ax.stairs(hist, edges, alpha=0.4, color="#d54a3a", fill=True, label="multinma (HMC)")
        ax.plot(xs, gc_density, color="#3a7bd5", linewidth=2, label="GC (Laplace mix)")
        ax.axvline(truth_eff, color="#444", linestyle="--", linewidth=1,
                   label=f"truth = {truth_eff:+.3f}")
        ax.set_title(f"contrast 1→{lbl}", fontsize=10)
        ax.set_xlabel("effect")
        ax.grid(True, alpha=0.3)
    for ax in axes[len(selected):]:
        ax.axis("off")
    axes[0].legend(fontsize=8, loc="upper right")
    fig.suptitle(f"Effect posteriors — {regime}, rep 1, prior = {gc['prior_label']}",
                 fontsize=11)
    fig.tight_layout()
    return png_b64(fig)

bin_gc, bin_mn   = load_pair("bin")
cont_gc, cont_mn = load_pair("cont")

plot_tau2_bin   = tau2_overlay("binary",   bin_gc,  bin_mn)
plot_tau2_cont  = tau2_overlay("Gaussian", cont_gc, cont_mn)
plot_eff_bin    = effects_overlay("binary",   bin_gc,  bin_mn)
plot_eff_cont   = effects_overlay("Gaussian", cont_gc, cont_mn)

addendum = f"""
<h2>Posterior overlays (rep 1, prior = HalfNormal(τ; σ=1))</h2>
<p class='meta'>
  These show the actual posterior densities — not bias summaries — for one
  representative replicate.  GC overlays the Laplace mixture (Σ<sub>i</sub> wᵢ·N(μᵢ,σ²ᵢ)
  over the τ² grid); multinma uses HMC samples directly.
</p>

<h3>τ² posterior</h3>
<img src='data:image/png;base64,{plot_tau2_bin}'>
<img src='data:image/png;base64,{plot_tau2_cont}'>

<h3>Effect posteriors (top {6} largest-truth contrasts shown)</h3>
<img src='data:image/png;base64,{plot_eff_bin}'>
<img src='data:image/png;base64,{plot_eff_cont}'>
"""

# Append to existing report (insert before </body>)
report_path = "docs/t30_reps_report.html"
with open(report_path) as f:
    html = f.read()
# Drop any previous addendum block to keep this idempotent
import re
html = re.sub(r"<h2>Posterior overlays.*?(?=</body>)", "",
              html, flags=re.DOTALL)
html = html.replace("</body>", addendum + "\n</body>")
with open(report_path, "w") as f:
    f.write(html)
print(f"Updated {report_path} (appended posterior overlays)")
