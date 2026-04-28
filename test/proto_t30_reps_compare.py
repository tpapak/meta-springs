#!/usr/bin/env python3
"""Bias study: T=30 binary NMA, 30 replicates. GC vs multinma across 4 priors
(GC) and 3 priors (multinma; cannot do flat τ).

Reports per prior:
  - τ² bias  : mean(tau²_hat - tau²_true)   [point estimate = posterior mean]
  - τ² RMSE  : sqrt(mean((tau²_hat - tau²_true)²))
  - τ² coverage : fraction of reps where 95% CI contains tau²_true
  - Effect bias  : mean over (rep, contrast) of (mu_hat - d_true)
  - Effect RMSE  : sqrt(mean((mu_hat - d_true)²))
  - Effect 95% coverage : fraction where mu ± 1.96·sd contains d_true
"""
import json, math, statistics

truth = json.load(open("/tmp/t30_reps_truth.json"))
gc = json.load(open("/tmp/t30_reps_gc.json"))
mn = json.load(open("/tmp/t30_reps_mn.json"))

tau2_true = truth["tau2_true"]
d_true = truth["d_true"]   # length T; d_true[0] = 0 (ref)

def summarize(results, source):
    """Group by prior label, then compute summary stats."""
    by_prior = {}
    for rep in results:
        for pr in rep["priors"]:
            lbl = pr["label"]
            d = by_prior.setdefault(lbl, {
                "n": 0, "tau2_hat": [], "tau2_in_ci": 0,
                "eff_bias": [], "eff_se2": [], "eff_in_ci": 0, "eff_count": 0,
            })
            t = pr["tau2"]
            t_hat = t["mean"]
            d["tau2_hat"].append(t_hat)
            if t["ci_lo"] <= tau2_true <= t["ci_hi"]:
                d["tau2_in_ci"] += 1
            d["n"] += 1
            for e in pr["effects"]:
                tid = str(e["to"])
                # multinma format is "d[N]"; GC stores plain integer string.
                if tid.startswith("d[") and tid.endswith("]"):
                    tid = tid[2:-1]
                try:
                    j = int(tid)
                except ValueError:
                    continue
                if j < 1 or j > len(d_true):
                    continue
                truth_eff = d_true[j-1] - d_true[0]
                bias = e["mean"] - truth_eff
                d["eff_bias"].append(bias)
                d["eff_se2"].append(bias**2)
                d["eff_count"] += 1
                lo = e["mean"] - 1.96 * e["sd"]
                hi = e["mean"] + 1.96 * e["sd"]
                if lo <= truth_eff <= hi:
                    d["eff_in_ci"] += 1
    rows = []
    for lbl, d in by_prior.items():
        tau_b = statistics.fmean([t - tau2_true for t in d["tau2_hat"]])
        tau_rmse = math.sqrt(statistics.fmean([(t - tau2_true)**2
                                               for t in d["tau2_hat"]]))
        tau_cov = d["tau2_in_ci"] / d["n"]
        eff_b = statistics.fmean(d["eff_bias"]) if d["eff_bias"] else float("nan")
        eff_rmse = math.sqrt(statistics.fmean(d["eff_se2"])) if d["eff_se2"] else float("nan")
        eff_cov = d["eff_in_ci"] / d["eff_count"] if d["eff_count"] else float("nan")
        rows.append((lbl, source, tau_b, tau_rmse, tau_cov,
                     eff_b, eff_rmse, eff_cov, d["n"]))
    return rows

gc_rows = summarize(gc["results"], "GC")
mn_rows = summarize(mn["results"], "multinma")

print(f"truth: τ²={tau2_true:.3f}  T={truth['T']} k={truth['k']}  "
      f"reps={truth['nReps']}")
print(f"GC total wall:       {gc['total_wall_sec']:.1f}s   ({gc['n_reps']} reps × 4 priors)")
print(f"multinma total wall: {mn['total_wall_sec']:.1f}s   ({mn['n_reps']} reps × 3 priors)\n")

hdr = (f"{'prior':<26}{'method':<10}"
       f"{'τ² bias':>10}{'τ² RMSE':>9}{'τ² 95cov':>9}"
       f"{'eff bias':>10}{'eff RMSE':>9}{'eff 95cov':>10}")
print(hdr)
print("-" * len(hdr))
# Order: GC's flat first, then matched pairs.
gc_by = {r[0]: r for r in gc_rows}
mn_by = {r[0]: r for r in mn_rows}
order = ["Flat (REML)", "HalfNormal(τ; σ=0.5)",
         "HalfNormal(τ; σ=1)", "HalfCauchy(τ; σ=0.5)"]
for lbl in order:
    if lbl in gc_by:
        _, src, tb, tr, tc, eb, er, ec, n = gc_by[lbl]
        print(f"{lbl:<26}{src:<10}{tb:>+10.4f}{tr:>9.4f}{tc:>9.2f}"
              f"{eb:>+10.4f}{er:>9.4f}{ec:>10.2f}")
    if lbl in mn_by:
        _, src, tb, tr, tc, eb, er, ec, n = mn_by[lbl]
        print(f"{'':<26}{src:<10}{tb:>+10.4f}{tr:>9.4f}{tc:>9.2f}"
              f"{eb:>+10.4f}{er:>9.4f}{ec:>10.2f}")
    elif lbl == "Flat (REML)":
        print(f"{'':<26}{'multinma':<10}  (cannot do flat τ — improper posterior)")
    print()
