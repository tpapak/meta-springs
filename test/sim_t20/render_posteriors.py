#!/usr/bin/env python3
"""Generate inline-SVG posterior summaries for the simulation report.

For each τ² level, produces histograms across the 50 replicas of:
  • GC posterior mode of τ² (Flat / REML prior), with the data-generating τ² marked
  • posterior mean of the two strongest treatment contrasts, with the
    cross-replicate consensus mean marked
  • per-replica gc-rust wall time

Top-2 contrasts are picked by mean-absolute posterior-mean across all
150 replicas (so "strongest" is by signal magnitude, not noise).

Output: writes a single SVG-block fragment (see make_html_block) and
also dumps it to test/sim_t20/results/posteriors_block.html. The
existing REPORT.html can include the fragment via a <object> tag or
the block can be inlined directly.
"""

import json, statistics as st, math
from pathlib import Path

ROOT = Path(__file__).resolve().parent
TAUS = [(1, 0.01), (2, 0.10), (3, 0.60)]

def load(idx):
    return json.load(open(ROOT / "results" / f"gc_rust_tau{idx}.json"))

# ---------- collect per-rep summaries ----------
all_data = {}  # tau2_idx -> dict of per-replica arrays
for ti, tau2 in TAUS:
    d = load(ti)
    reps = d["results"]
    # τ² mode under Flat prior (=REML)
    tau2_mode  = [r["priors"][0]["tau2"]["mode"]   for r in reps]
    tau2_med   = [r["priors"][0]["tau2"]["median"] for r in reps]
    tau2_mean  = [r["priors"][0]["tau2"]["mean"]   for r in reps]
    walls      = [r["wall_sec"] for r in reps]
    # effects under Flat prior, indexed by 'to' treatment id
    eff_means = {}
    eff_sds   = {}
    for r in reps:
        for e in r["priors"][0]["effects"]:
            eff_means.setdefault(e["to"], []).append(e["mean"])
            eff_sds.setdefault(e["to"], []).append(e["sd"])
    all_data[ti] = dict(tau2_true=tau2, tau2_mode=tau2_mode, tau2_med=tau2_med,
                        tau2_mean=tau2_mean, walls=walls,
                        eff_means=eff_means, eff_sds=eff_sds,
                        total_wall=d["total_wall_sec"])

# ---------- pick top-2 effects by mean-|mean| across all reps ----------
all_treats = set()
for d in all_data.values(): all_treats.update(d["eff_means"].keys())
def overall_mag(t):
    vs = []
    for d in all_data.values():
        vs.extend(d["eff_means"].get(t, []))
    return abs(st.mean(vs)) if vs else 0
top2 = sorted(all_treats, key=overall_mag, reverse=True)[:2]
print(f"Top-2 effects (by |mean| across all 150 reps): {top2}")
for t in top2:
    pooled = []
    for d in all_data.values(): pooled.extend(d["eff_means"].get(t, []))
    print(f"  contrast (1 → {t}):  mean across reps = {st.mean(pooled):+.4f}, "
          f"sd = {st.pstdev(pooled):.4f}, n = {len(pooled)}")

# ---------- SVG histogram helper ----------
def svg_hist(values, *, width=320, height=140, bins=15, lo=None, hi=None,
             vline=None, vline_label=None, x_label="", y_label="count",
             color="#003a78", vline_color="#b8002b", title=""):
    if not values: return ""
    if lo is None: lo = min(values)
    if hi is None: hi = max(values)
    if hi == lo: hi = lo + 1e-9
    pad_l, pad_r, pad_t, pad_b = 38, 8, 22, 28
    plot_w = width - pad_l - pad_r
    plot_h = height - pad_t - pad_b
    # build histogram
    counts = [0] * bins
    for v in values:
        k = int((v - lo) / (hi - lo) * bins)
        if k >= bins: k = bins - 1
        if k < 0:     k = 0
        counts[k] += 1
    cmax = max(counts) if counts else 1
    bw = plot_w / bins
    bars = []
    for i, c in enumerate(counts):
        h = c / cmax * plot_h if cmax > 0 else 0
        x = pad_l + i * bw
        y = pad_t + (plot_h - h)
        bars.append(f'<rect x="{x:.1f}" y="{y:.1f}" width="{bw-1:.1f}" '
                    f'height="{h:.1f}" fill="{color}" opacity="0.78"/>')
    bars = "\n      ".join(bars)
    # axis ticks: lo, mid, hi
    def xpos(v): return pad_l + (v - lo) / (hi - lo) * plot_w
    def fmt_t(v):
        if abs(v) >= 100: return f"{v:.0f}"
        if abs(v) >= 10:  return f"{v:.1f}"
        if abs(v) >= 1:   return f"{v:.2f}"
        return f"{v:.3f}"
    ticks = [lo, (lo+hi)/2, hi]
    tick_lines = [f'<line x1="{xpos(t):.1f}" y1="{pad_t+plot_h:.1f}" '
                  f'x2="{xpos(t):.1f}" y2="{pad_t+plot_h+4:.1f}" '
                  f'stroke="#666" stroke-width="0.5"/>'
                  f'<text x="{xpos(t):.1f}" y="{pad_t+plot_h+15:.1f}" '
                  f'font-size="9" text-anchor="middle" fill="#444">{fmt_t(t)}</text>'
                  for t in ticks]
    tick_lines = "\n      ".join(tick_lines)
    # y-axis
    y_max_label = (f'<text x="{pad_l-4:.1f}" y="{pad_t+8:.1f}" font-size="9" '
                   f'text-anchor="end" fill="#444">{cmax}</text>')
    y_min_label = (f'<text x="{pad_l-4:.1f}" y="{pad_t+plot_h+3:.1f}" font-size="9" '
                   f'text-anchor="end" fill="#444">0</text>')
    # vline (truth marker)
    vline_svg = ""
    if vline is not None and lo <= vline <= hi:
        vx = xpos(vline)
        vline_svg = (f'<line x1="{vx:.1f}" y1="{pad_t:.1f}" x2="{vx:.1f}" y2="{pad_t+plot_h:.1f}" '
                     f'stroke="{vline_color}" stroke-width="1.5"/>')
        if vline_label:
            vline_svg += (f'<text x="{vx:.1f}" y="{pad_t-5:.1f}" font-size="9" '
                          f'text-anchor="middle" fill="{vline_color}">{vline_label}</text>')
    # axis frame
    frame = (f'<line x1="{pad_l:.1f}" y1="{pad_t+plot_h:.1f}" '
             f'x2="{pad_l+plot_w:.1f}" y2="{pad_t+plot_h:.1f}" stroke="#666" stroke-width="0.7"/>'
             f'<line x1="{pad_l:.1f}" y1="{pad_t:.1f}" '
             f'x2="{pad_l:.1f}" y2="{pad_t+plot_h:.1f}" stroke="#666" stroke-width="0.7"/>')
    title_svg = (f'<text x="{width/2:.1f}" y="14" font-size="11" font-weight="600" '
                 f'text-anchor="middle" fill="#003a78">{title}</text>') if title else ""
    xlabel_svg = (f'<text x="{pad_l+plot_w/2:.1f}" y="{height-3:.1f}" font-size="9" '
                  f'text-anchor="middle" fill="#444">{x_label}</text>') if x_label else ""
    return (f'<svg viewBox="0 0 {width} {height}" xmlns="http://www.w3.org/2000/svg" '
            f'style="background:#fff; border:1px solid #d0d4dc; border-radius:4px">\n'
            f'  {title_svg}\n'
            f'  {frame}\n'
            f'  {bars}\n'
            f'  {tick_lines}\n'
            f'  {y_max_label}\n  {y_min_label}\n'
            f'  {vline_svg}\n'
            f'  {xlabel_svg}\n'
            f'</svg>')

# ---------- build the report block ----------
def fmt_secs(s):
    if s < 1: return f"{s*1000:.0f} ms"
    if s < 60: return f"{s:.2f} s"
    return f"{s/60:.1f} min"

rows = []
for ti, tau2_true in TAUS:
    d = all_data[ti]
    # τ² panel
    tau_lo = 0
    tau_hi = max(max(d["tau2_mode"]), tau2_true * 2.5)
    tau_panel = svg_hist(
        d["tau2_mode"], lo=tau_lo, hi=tau_hi,
        vline=tau2_true, vline_label=f"truth = {tau2_true:.2f}",
        x_label="GC τ̂² (Flat-prior mode)",
        title=f"τ²_true = {tau2_true:.2f}  ·  τ̂² across 50 reps",
        color="#003a78")
    # effect 1 panel
    e1 = d["eff_means"].get(top2[0], [])
    cons1 = st.mean(e1) if e1 else 0
    sd1 = st.pstdev(e1) if e1 else 1
    eff1_panel = svg_hist(
        e1, lo=cons1 - 4*sd1, hi=cons1 + 4*sd1,
        vline=cons1, vline_label=f"cons. = {cons1:+.3f}",
        x_label=f"GC posterior mean of effect 1→{top2[0]}",
        title=f"contrast 1→{top2[0]}  ·  posterior means across 50 reps",
        color="#1f6f3c", vline_color="#b8002b")
    # effect 2 panel
    e2 = d["eff_means"].get(top2[1], [])
    cons2 = st.mean(e2) if e2 else 0
    sd2 = st.pstdev(e2) if e2 else 1
    eff2_panel = svg_hist(
        e2, lo=cons2 - 4*sd2, hi=cons2 + 4*sd2,
        vline=cons2, vline_label=f"cons. = {cons2:+.3f}",
        x_label=f"GC posterior mean of effect 1→{top2[1]}",
        title=f"contrast 1→{top2[1]}  ·  posterior means across 50 reps",
        color="#1f6f3c", vline_color="#b8002b")
    # wall-time panel
    wmin, wmax = min(d["walls"]), max(d["walls"])
    wmid = st.median(d["walls"])
    wall_panel = svg_hist(
        d["walls"], lo=0, hi=wmax * 1.1 if wmax > 0 else 1,
        vline=wmid, vline_label=f"median = {wmid*1000:.0f} ms",
        x_label="per-replica wall time (s)",
        title=f"τ²_true = {tau2_true:.2f}  ·  gc-rust wall time / replica",
        color="#7a5230", vline_color="#003a78")

    # avg posterior sd from GC for top-2
    avg_sd1 = st.mean(d["eff_sds"].get(top2[0], [0]))
    avg_sd2 = st.mean(d["eff_sds"].get(top2[1], [0]))
    rows.append(dict(tau2_true=tau2_true, total_wall=d["total_wall"],
                     tau_panel=tau_panel, eff1_panel=eff1_panel, eff2_panel=eff2_panel,
                     wall_panel=wall_panel,
                     cons1=cons1, sd1=sd1, avg_sd1=avg_sd1,
                     cons2=cons2, sd2=sd2, avg_sd2=avg_sd2,
                     median_wall=wmid))

# Build HTML block
parts = []
parts.append('<h2>Posterior summaries from gc-rust (50 reps × 3 τ² levels)</h2>')
parts.append(f'<p>The two strongest treatment contrasts (by mean-absolute '
             f'posterior mean across all 150 replicas) are <strong>1 → '
             f'{top2[0]}</strong> and <strong>1 → {top2[1]}</strong>. The '
             f'panels below show, for each τ² level, the histogram across '
             f'50 replicas of the GC posterior mode of τ² and the GC '
             f'posterior means of those two contrasts; the red bar marks '
             f'the data-generating τ² (left column) or the cross-replicate '
             f'consensus mean (centre/right columns).</p>')
parts.append('<table style="border-collapse: collapse; margin-top: 1em">')
parts.append('<thead><tr>'
             '<th style="text-align:left">τ²_true</th>'
             '<th>τ̂² posterior mode</th>'
             f'<th>contrast 1 → {top2[0]}</th>'
             f'<th>contrast 1 → {top2[1]}</th>'
             '<th>per-rep wall time</th>'
             '</tr></thead><tbody>')
for r in rows:
    parts.append('<tr>'
                 f'<td style="text-align:left; font-weight:600; vertical-align:middle;">τ² = {r["tau2_true"]:.2f}<br><span style="font-size:.85em; color:#555">total wall {fmt_secs(r["total_wall"])}</span></td>'
                 f'<td>{r["tau_panel"]}</td>'
                 f'<td>{r["eff1_panel"]}</td>'
                 f'<td>{r["eff2_panel"]}</td>'
                 f'<td>{r["wall_panel"]}</td>'
                 '</tr>')
parts.append('</tbody></table>')

# Per-contrast summary table
parts.append('<h3>Posterior mean and sd of the top-2 contrasts (averaged across replicas)</h3>')
parts.append('<table>'
             '<thead><tr><th>τ²_true</th>'
             f'<th>1 → {top2[0]}: mean ± rep-sd</th>'
             f'<th>1 → {top2[0]}: avg posterior sd</th>'
             f'<th>1 → {top2[1]}: mean ± rep-sd</th>'
             f'<th>1 → {top2[1]}: avg posterior sd</th>'
             '</tr></thead><tbody>')
for r in rows:
    parts.append('<tr>'
                 f'<td>{r["tau2_true"]:.2f}</td>'
                 f'<td>{r["cons1"]:+.4f} ± {r["sd1"]:.4f}</td>'
                 f'<td>{r["avg_sd1"]:.4f}</td>'
                 f'<td>{r["cons2"]:+.4f} ± {r["sd2"]:.4f}</td>'
                 f'<td>{r["avg_sd2"]:.4f}</td>'
                 '</tr>')
parts.append('</tbody></table>')

# Per-rep wall summary
parts.append('<h3>gc-rust wall time per replica</h3>')
parts.append('<table>'
             '<thead><tr><th>τ²_true</th><th>median</th><th>min</th><th>max</th>'
             '<th>total over 50 reps</th></tr></thead><tbody>')
for r, (ti, tau) in zip(rows, TAUS):
    walls = all_data[ti]["walls"]
    parts.append(f'<tr><td>{tau:.2f}</td>'
                 f'<td>{fmt_secs(st.median(walls))}</td>'
                 f'<td>{fmt_secs(min(walls))}</td>'
                 f'<td>{fmt_secs(max(walls))}</td>'
                 f'<td>{fmt_secs(r["total_wall"])}</td></tr>')
parts.append('</tbody></table>')

block = "\n".join(parts)

out = ROOT / "results" / "posteriors_block.html"
out.write_text(block)
print(f"\nwrote {out} ({len(block)} bytes)")
print(f"top2 contrasts: {top2}")
