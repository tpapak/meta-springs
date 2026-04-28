#!/usr/bin/env python3
"""All-priors posterior overlay for one rep per regime (T=30 + T=50 rare).
GC-Rust grid mixture vs multinma HMC samples, all 3 informative priors
side-by-side, plus REML (Rust-only).  Includes per-fit wall time.

Inputs:
  /tmp/t30_post_gc_rust.json   (Rust single-rep, all 4 priors with full data)
  /tmp/t50_post_gc_rust.json
  /tmp/t30_allpriors_mn.json   (multinma 3 priors with samples + per-fit wall)
  /tmp/t50_allpriors_mn.json
  /tmp/t30_reps_truth.json
  /tmp/t50_rare_truth.json
Output: docs/posteriors_t30_t50.html
"""
import base64, io, json, os
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np

PRIORS = ["Flat (REML)", "HalfNormal(τ; σ=0.5)",
          "HalfNormal(τ; σ=1)", "HalfCauchy(τ; σ=0.5)"]

def png_b64(fig):
    buf = io.BytesIO()
    fig.savefig(buf, format="png", dpi=130, bbox_inches="tight")
    plt.close(fig)
    return base64.b64encode(buf.getvalue()).decode()

def load_regime(regime):
    """Returns dict with: gc, mn, hk, truth, tau2_true, d_true."""
    truth_path = (f"/tmp/t30_reps_truth.json" if regime == "t30"
                  else f"/tmp/t50_rare_truth.json")
    truth = json.load(open(truth_path))
    gc = json.load(open(f"/tmp/{regime}_post_gc_rust.json"))
    mn_path = f"/tmp/{regime}_allpriors_mn.json"
    mn = json.load(open(mn_path)) if os.path.exists(mn_path) else None
    # Haskell GC dump is HN(τ; σ=1) only (one prior, full grid).
    hk_path = f"/tmp/{regime}_post_gc_haskell.json"
    hk = json.load(open(hk_path)) if os.path.exists(hk_path) else None
    return dict(gc=gc, mn=mn, hk=hk, truth=truth,
                tau2_true=truth["tau2_true"],
                d_true=truth["d_true"])

def gc_block(gc, label):
    return next((p for p in gc["priors"] if p["label"] == label), None)

def mn_block(mn, label):
    if mn is None:
        return None
    return next((r for r in mn["results"] if r["label"] == label), None)

def tau2_overlay_panel(ax, gc_p, mn_p, tau2_true, hk_p=None):
    """Plot τ² posterior overlay on one axis. GC interpolated CDF, multinma
    histogram on common bins, optional Haskell GC interpolated CDF as a
    line outline."""
    # GC (Rust) grid
    pts = gc_p["tau2_grid"]
    ts  = np.array([p["tau2"] for p in pts])
    ws  = np.array([p["weight"] for p in pts])
    order = np.argsort(ts); ts_s = ts[order]; ws_s = ws[order]
    cum = np.cumsum(ws_s)
    samples = (np.array(mn_p["tau2_samples"]) if mn_p is not None else None)
    gc_q995 = ts_s[min(np.searchsorted(cum, 0.995), len(ts_s) - 1)]
    candidates = [gc_q995]
    if samples is not None:
        candidates.append(np.quantile(samples, 0.995))
    hk_ts_s = hk_cum = None
    if hk_p is not None:
        hk_ts = np.array(hk_p["tau2"])
        hk_ws = np.array(hk_p["weight"])
        hk_order = np.argsort(hk_ts)
        hk_ts_s = hk_ts[hk_order]; hk_ws_s = hk_ws[hk_order]
        hk_cum  = np.cumsum(hk_ws_s)
        candidates.append(hk_ts_s[min(np.searchsorted(hk_cum, 0.995), len(hk_ts_s) - 1)])
    x_hi = 1.05 * max(candidates)
    x_lo = 0.0
    nbins = 40
    edges = np.linspace(x_lo, x_hi, nbins + 1)
    cdf_x = np.concatenate([[0.0], ts_s, [max(ts_s[-1], x_hi) + 1]])
    cdf_y = np.concatenate([[0.0], cum,  [1.0]])
    cdf_at = np.interp(edges, cdf_x, cdf_y)
    gc_density = np.diff(cdf_at) / (edges[1] - edges[0])
    ax.stairs(gc_density, edges, color="#3a7bd5", linewidth=2,
              fill=True, alpha=0.30, label="GC (Rust)")
    if samples is not None:
        mn_density, _ = np.histogram(samples, bins=edges, density=True)
        ax.stairs(mn_density, edges, color="#d54a3a", linewidth=2,
                  fill=True, alpha=0.30, label="multinma")
    else:
        ax.text(0.5, 0.5, "(no multinma — improper)", transform=ax.transAxes,
                ha="center", va="center", color="#888", fontsize=10)
    if hk_p is not None and hk_ts_s is not None and hk_cum is not None:
        hk_cdf_x = np.concatenate([[0.0], hk_ts_s, [max(hk_ts_s[-1], x_hi) + 1]])
        hk_cdf_y = np.concatenate([[0.0], hk_cum,  [1.0]])
        hk_cdf_at = np.interp(edges, hk_cdf_x, hk_cdf_y)
        hk_density = np.diff(hk_cdf_at) / (edges[1] - edges[0])
        ax.stairs(hk_density, edges, color="#2a8", linewidth=2,
                  fill=False, label="GC (Haskell)")
    ax.axvline(tau2_true, color="#444", linestyle="--", linewidth=1)
    ax.grid(True, alpha=0.3)

def render_tau2(regime, ds):
    fig, axes = plt.subplots(1, len(PRIORS), figsize=(15, 3.4),
                             sharey=True)
    hk_label = "HalfNormal(τ; σ=1)"   # Haskell dump only covers this prior
    for ax, lbl in zip(axes, PRIORS):
        gc_p = gc_block(ds["gc"], lbl)
        mn_p = mn_block(ds["mn"], lbl)
        hk_p = ds.get("hk") if lbl == hk_label else None
        if gc_p is None:
            ax.set_visible(False); continue
        tau2_overlay_panel(ax, gc_p, mn_p, ds["tau2_true"], hk_p)
        ax.set_title(lbl, fontsize=9)
        ax.set_xlabel("τ²")
        if ax is axes[0]:
            ax.set_ylabel("density")
            ax.legend(fontsize=8, loc="upper right")
    fig.suptitle(f"τ² posterior — {regime} (truth τ²={ds['tau2_true']})",
                 fontsize=11)
    fig.tight_layout()
    return png_b64(fig)

def render_effects(regime, ds, n_show=6):
    """For each prior, pick the n_show contrasts with largest |truth|, plot
    GC mixture density vs multinma sample histogram on the same axes."""
    d_true = ds["d_true"]
    gc = ds["gc"]
    mn = ds["mn"]
    # Pick contrasts based on first prior's effects (same set across priors).
    first_gc = gc["priors"][0]
    candidates = []
    for c in first_gc["effects"]:
        try:
            j = int(c["to"])
        except ValueError:
            continue
        if j < 1 or j > len(d_true):
            continue
        truth_eff = d_true[j-1] - d_true[0]
        candidates.append((abs(truth_eff), c["to"], truth_eff))
    candidates.sort(reverse=True)
    selected = candidates[:n_show]

    out_imgs = []
    for lbl in PRIORS:
        gc_p = gc_block(gc, lbl)
        mn_p = mn_block(mn, lbl)
        if gc_p is None: continue
        cols = 3
        rows = (len(selected) + cols - 1) // cols
        fig, axes = plt.subplots(rows, cols, figsize=(13, 3 * rows))
        axes = axes.flatten()
        for ax, (_, to, truth_eff) in zip(axes, selected):
            # GC mixture density
            gc_e = next((e for e in gc_p["effects"] if e["to"] == to), None)
            if gc_e is None:
                ax.axis("off"); continue
            pts = gc_e["points"]
            ws = np.array([p["weight"] for p in pts])
            mus = np.array([p["mean"] for p in pts])
            sds = np.sqrt(np.maximum(1e-9, [p["var"] for p in pts]))
            # multinma samples
            mn_e = (next((c for c in mn_p["contrasts"] if c["to"] == to), None)
                    if mn_p else None)
            samples = (np.array(mn_e["samples"]) if mn_e else None)
            if samples is not None:
                x_lo = min(samples.min(), float(np.quantile(mus - 3*sds, 0.0)))
                x_hi = max(samples.max(), float(np.quantile(mus + 3*sds, 1.0)))
            else:
                x_lo = float(np.min(mus - 3*sds))
                x_hi = float(np.max(mus + 3*sds))
            x_lo, x_hi = max(x_lo, -10), min(x_hi, 10)
            xs = np.linspace(x_lo, x_hi, 600)
            gc_density = np.zeros_like(xs)
            for w, mu, sd in zip(ws, mus, sds):
                sd_eff = max(sd, 1e-6)
                gc_density += w * np.exp(-((xs - mu) ** 2) / (2 * sd_eff**2)) / (sd_eff * np.sqrt(2*np.pi))
            ax.plot(xs, gc_density, color="#3a7bd5", linewidth=2,
                    label="GC (Rust)")
            if samples is not None:
                ax.hist(samples, bins=50, range=(x_lo, x_hi), density=True,
                        color="#d54a3a", alpha=0.4, label="multinma")
            ax.axvline(truth_eff, color="#444", linestyle="--", linewidth=1,
                       label=f"truth={truth_eff:+.2f}")
            ax.set_title(f"contrast 1→{to}", fontsize=10)
            ax.grid(True, alpha=0.3)
        for ax in axes[len(selected):]:
            ax.axis("off")
        if axes[0].lines:
            axes[0].legend(fontsize=8, loc="upper right")
        fig.suptitle(f"Effect posteriors — {regime}, prior = {lbl} "
                     f"(top {n_show} largest-|truth|)", fontsize=11)
        fig.tight_layout()
        out_imgs.append((lbl, png_b64(fig)))
    return out_imgs

def timing_table(ds, label, haskell_wall_sec=None):
    gc = ds["gc"]
    mn = ds["mn"]
    rows = []
    rows.append("<tr><th>engine</th><th>priors</th><th>fit wall</th>"
                "<th>per-prior cost</th></tr>")
    rows.append(f"<tr><td>GC (Rust)</td><td>{len(gc['priors'])} (1 fit + reweights)</td>"
                f"<td><b>{gc['fit_wall_sec']*1000:.0f} ms</b></td>"
                f"<td>~free (μs)</td></tr>")
    if haskell_wall_sec is not None:
        rows.append(f"<tr><td>GC (Haskell, pure-Haskell LU)</td>"
                    f"<td>1 (HN(τ; σ=1) only here)</td>"
                    f"<td><b>{haskell_wall_sec:.0f} s</b></td>"
                    f"<td>same algorithm, slow LU + ill-conditioning at large τ²</td></tr>")
    if mn is not None:
        per_prior_ts = [r["wall_sec"] for r in mn["results"]]
        per_str = ", ".join(f"{t:.1f}s" for t in per_prior_ts)
        rows.append(f"<tr><td>multinma (HMC)</td><td>{len(mn['results'])} separate fits</td>"
                    f"<td><b>{mn['total_wall_sec']:.1f} s</b></td>"
                    f"<td>{per_str}</td></tr>")
        rows.append(f"<tr><td colspan=4 style='text-align:center;color:#28a'>"
                    f"<b>GC (Rust) is {mn['total_wall_sec'] / max(gc['fit_wall_sec'], 1e-9):.0f}× faster than multinma</b></td></tr>")
    return f"""
    <h3>{label}: timing</h3>
    <table>{''.join(rows)}</table>
    """

# ---------- main ----------
ds_t30 = load_regime("t30")
ds_t50 = load_regime("t50")

tau2_t30 = render_tau2("T=30 binary multi-arm, rep 1", ds_t30)
tau2_t50 = render_tau2("T=50 rare events, rep 4", ds_t50)
eff_t30 = render_effects("T=30 binary multi-arm, rep 1", ds_t30)
eff_t50 = render_effects("T=50 rare events, rep 4", ds_t50)

t30_timing = timing_table(ds_t30, "T=30 rep 1",
                          haskell_wall_sec=8.56)
# Haskell rep 4 dump took ~25 min CPU (the historical stall reproduced
# from pure-Haskell `Mat.inv` ill-conditioning).
t50_timing = timing_table(ds_t50,
                          "T=50 rep 4 (the historical Haskell stall)",
                          haskell_wall_sec=1102.0)

def eff_section(eff_imgs):
    out = []
    for lbl, img in eff_imgs:
        out.append(f"<h4>{lbl}</h4>")
        out.append(f"<img src='data:image/png;base64,{img}'>")
    return "\n".join(out)

mn_status_t30 = ("" if ds_t30["mn"] else
    "<p class='note'><b>multinma T=30 not yet available</b> — "
    "rendered GC-only.</p>")
mn_status_t50 = ("" if ds_t50["mn"] else
    "<p class='note'><b>multinma T=50 not yet available</b> — "
    "rendered GC-only.</p>")

html = f"""<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<title>Posterior overlays — T=30 and T=50 binary NMA</title>
<style>
body {{ font-family: -apple-system, system-ui, sans-serif; max-width: 1200px;
        margin: 2em auto; padding: 0 1em; color: #222; }}
h1 {{ margin-bottom: 0.2em; }}
h2 {{ border-bottom: 1px solid #ddd; padding-bottom: 0.2em; margin-top: 1.5em; }}
h3 {{ margin-top: 1em; }}
h4 {{ margin: 1em 0 0.3em 0; color: #555; }}
.meta {{ color: #555; font-size: 0.95em; }}
.note {{ background: #fff4e0; padding: 0.5em 1em; border-left: 3px solid #c80; }}
table {{ border-collapse: collapse; margin: 1em 0; font-size: 0.92em; }}
th, td {{ padding: 0.4em 0.8em; text-align: left; border-bottom: 1px solid #eee; }}
th {{ background: #f4f4f4; }}
img {{ max-width: 100%; height: auto; }}
</style>
</head>
<body>
<h1>Posterior overlays — T=30 and T=50 binary NMA</h1>
<p class='meta'>
  GC posteriors (blue) from <code>gc-rust</code> single-rep mode: one Newton
  fit + Laplace logZ on a 100-pt log-spaced τ² grid, reweighted under each
  prior (≈free).  multinma posteriors (red) from 3 independent HMC fits
  (4 chains × 4000 iter, 1500 warmup).  Truth marked with a dashed line.
</p>

<h2>T=30 binary multi-arm, rep 1</h2>
{mn_status_t30}
{t30_timing}

<h3>τ² posterior</h3>
<img src='data:image/png;base64,{tau2_t30}'>

<h3>Effect posteriors (top 6 largest-|truth| contrasts)</h3>
{eff_section(eff_t30)}

<h2>T=50 rare events, rep 4 (the historical Haskell stall)</h2>
{mn_status_t50}
{t50_timing}

<h3>τ² posterior</h3>
<img src='data:image/png;base64,{tau2_t50}'>

<h3>Effect posteriors (top 6 largest-|truth| contrasts)</h3>
{eff_section(eff_t50)}

<h2>Notes</h2>
<ul>
  <li>GC's <em>Flat (REML)</em> column has no multinma counterpart —
      multinma cannot represent an improper τ prior.</li>
  <li>multinma reports per-fit wall time; GC reports a single total because
      reweighting under additional priors is essentially free given the
      cached log-Z grid.</li>
  <li>For T=50 rare events, GC's regularization bounds separated-treatment
      posteriors that multinma's HMC stretches into wide unidentifiable
      tails.  This is the regime where their posterior shapes diverge most
      visibly.</li>
</ul>

</body>
</html>
"""

with open("docs/posteriors_t30_t50.html", "w") as f:
    f.write(html)
print("Wrote docs/posteriors_t30_t50.html")
