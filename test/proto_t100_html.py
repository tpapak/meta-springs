#!/usr/bin/env python3
"""Posterior overlay for T=100 single-rep binary NMA.  GC-Rust grid mixture
vs multinma HMC samples, all 3 informative priors + REML (Rust-only).
Reads /tmp/t100_post_gc_rust.json + /tmp/t100_allpriors_mn.json + truth.
Writes docs/t100_report.html.
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

truth = json.load(open("/tmp/t100_truth.json"))
gc    = json.load(open("/tmp/t100_post_gc_rust.json"))
mn_path = "/tmp/t100_allpriors_mn.json"
mn    = json.load(open(mn_path)) if os.path.exists(mn_path) else None
tau2_true = truth["tau2_true"]
d_true    = truth["d_true"]
T, k      = truth["T"], truth["k"]

def gc_block(label):
    return next((p for p in gc["priors"] if p["label"] == label), None)
def mn_block(label):
    if mn is None: return None
    return next((r for r in mn["results"] if r["label"] == label), None)

# --- τ² overlay across priors ---
fig, axes = plt.subplots(1, len(PRIORS), figsize=(15, 3.4), sharey=True)
for ax, lbl in zip(axes, PRIORS):
    gc_p = gc_block(lbl); mn_p = mn_block(lbl)
    if gc_p is None: ax.set_visible(False); continue
    pts = gc_p["tau2_grid"]
    ts  = np.array([p["tau2"] for p in pts])
    ws  = np.array([p["weight"] for p in pts])
    order = np.argsort(ts); ts_s = ts[order]; ws_s = ws[order]
    cum = np.cumsum(ws_s)
    samples = np.array(mn_p["tau2_samples"]) if mn_p else None
    gc_q995 = ts_s[min(np.searchsorted(cum, 0.995), len(ts_s) - 1)]
    candidates = [gc_q995]
    if samples is not None:
        candidates.append(np.quantile(samples, 0.995))
    x_hi = 1.05 * max(candidates); x_lo = 0.0
    edges = np.linspace(x_lo, x_hi, 41)
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
    ax.axvline(tau2_true, color="#444", linestyle="--", linewidth=1)
    ax.set_title(lbl, fontsize=9)
    ax.set_xlabel("τ²")
    ax.grid(True, alpha=0.3)
    if ax is axes[0]:
        ax.set_ylabel("density")
        ax.legend(fontsize=8, loc="upper right")
fig.suptitle(f"τ² posterior — T={T}, k={k} (truth τ²={tau2_true})",
             fontsize=11)
fig.tight_layout()
plot_tau2 = png_b64(fig)

# --- Effect overlay: top 6 contrasts with largest |truth| ---
def render_effects(n_show=6):
    candidates = []
    for c in gc["priors"][0]["effects"]:
        try: j = int(c["to"])
        except: continue
        if j < 1 or j > len(d_true): continue
        truth_eff = d_true[j-1] - d_true[0]
        candidates.append((abs(truth_eff), c["to"], truth_eff))
    candidates.sort(reverse=True)
    selected = candidates[:n_show]
    out_imgs = []
    for lbl in PRIORS:
        gc_p = gc_block(lbl); mn_p = mn_block(lbl)
        if gc_p is None: continue
        cols = 3; rows = (len(selected) + cols - 1) // cols
        fig, axes = plt.subplots(rows, cols, figsize=(13, 3.0 * rows))
        axes = axes.flatten()
        for ax, (_, to, truth_eff) in zip(axes, selected):
            gc_e = next((e for e in gc_p["effects"] if e["to"] == to), None)
            if gc_e is None:
                ax.axis("off"); continue
            pts = gc_e["points"]
            ws = np.array([p["weight"] for p in pts])
            mus = np.array([p["mean"] for p in pts])
            sds = np.sqrt(np.maximum(1e-9, [p["var"] for p in pts]))
            mn_e = (next((c for c in mn_p["contrasts"] if c["to"] == to), None)
                    if mn_p else None)
            samples = np.array(mn_e["samples"]) if mn_e else None
            if samples is not None:
                x_lo = min(samples.min(), float(np.min(mus - 3*sds)))
                x_hi = max(samples.max(), float(np.max(mus + 3*sds)))
            else:
                x_lo = float(np.min(mus - 3*sds))
                x_hi = float(np.max(mus + 3*sds))
            x_lo, x_hi = max(x_lo, -10), min(x_hi, 10)
            xs = np.linspace(x_lo, x_hi, 600)
            gc_density = np.zeros_like(xs)
            for w, mu, sd in zip(ws, mus, sds):
                sd_eff = max(sd, 1e-6)
                gc_density += w * np.exp(-((xs - mu) ** 2) / (2 * sd_eff**2)) / (sd_eff * np.sqrt(2*np.pi))
            ax.plot(xs, gc_density, color="#3a7bd5", linewidth=2, label="GC (Rust)")
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
        fig.suptitle(f"Effect posteriors — T={T}, prior = {lbl}", fontsize=11)
        fig.tight_layout()
        out_imgs.append((lbl, png_b64(fig)))
    return out_imgs

eff_imgs = render_effects()

# --- Timing table ---
gc_wall = gc["fit_wall_sec"]
mn_total = mn["total_wall_sec"] if mn else None
rows = ["<tr><th>engine</th><th>priors</th><th>fit wall</th><th>per-prior</th></tr>"]
rows.append(f"<tr><td>GC (Rust)</td><td>{len(gc['priors'])} via reweight</td>"
            f"<td><b>{gc_wall:.2f} s</b></td><td>~free</td></tr>")
if mn is not None:
    per_str = ", ".join(f"{r['label'][:18]}={r['wall_sec']:.1f}s" for r in mn["results"])
    rows.append(f"<tr><td>multinma (HMC)</td><td>{len(mn['results'])} fits</td>"
                f"<td><b>{mn_total:.0f} s</b></td><td>{per_str}</td></tr>")
    rows.append(f"<tr><td colspan=4 style='text-align:center;color:#28a'>"
                f"<b>GC (Rust) {mn_total/gc_wall:.0f}× faster than multinma</b></td></tr>")

mn_warn = ("" if mn else "<p class='note'><b>multinma still running — "
                          "rendered GC-only.</b></p>")

eff_html = "".join(f"<h4>{lbl}</h4>"
                   f"<img src='data:image/png;base64,{img}'>"
                   for lbl, img in eff_imgs)

html = f"""<!doctype html>
<html lang='en'>
<head>
<meta charset='utf-8'>
<title>T={T} binary NMA — GC vs multinma</title>
<style>
body {{ font-family: -apple-system, system-ui, sans-serif; max-width: 1200px;
        margin: 2em auto; padding: 0 1em; color: #222; }}
h1 {{ margin-bottom: 0.2em; }}
h2 {{ border-bottom: 1px solid #ddd; padding-bottom: 0.2em; margin-top: 1.5em; }}
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
<h1>T={T} binary NMA — single-rep posterior comparison</h1>
<p class='meta'>
  <b>Truth:</b> τ²={tau2_true}, d<sub>true</sub> ∈ N(0, 0.5²) for j ≠ 1<br>
  <b>Design:</b> T={T} treatments, k={k} studies (160 two-arm + 30 three-arm
  + 10 four-arm), {truth['n_arms']} arms total, {truth['zero_event_arms']}
  zero-event arms.  Baseline μ ≈ −1 → ref event probability ≈ 0.27.<br>
  <b>GC:</b> 1 fit + 4 prior reweights via <code>gc-rust</code>.
  <b>multinma:</b> 3 separate HMC fits (4 chains × 4000 iter, 1500 warmup).
</p>

{mn_warn}

<h2>Timing</h2>
<table>{''.join(rows)}</table>

<h2>τ² posteriors (across priors)</h2>
<img src='data:image/png;base64,{plot_tau2}'>

<h2>Effect posteriors (top 6 largest-|truth| contrasts)</h2>
{eff_html}

</body>
</html>
"""

with open("docs/t100_report.html", "w") as f:
    f.write(html)
print("Wrote docs/t100_report.html")
