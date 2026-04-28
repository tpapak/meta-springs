#!/usr/bin/env python3
"""Full bias / RMSE / coverage / prior-sensitivity report for the
simulation study. Reads gc-rust outputs (and netmeta if available).

Coverage = fraction of replicates where the 95 % posterior CI
under each prior contains the true τ².
"""

import json, statistics as st
from pathlib import Path

ROOT = Path(__file__).resolve().parent
TAUS = [(1, 0.01), (2, 0.10), (3, 0.60)]

def load_rust(idx):
    return json.load(open(ROOT / "results" / f"gc_rust_tau{idx}.json"))

def load_netmeta():
    p = ROOT / "results" / "netmeta.json"
    if not p.exists() or p.stat().st_size == 0: return {}
    by = {}
    for r in json.load(open(p))["results"]:
        by[(r["tau2_idx"], r["rep"])] = r.get("netmeta_tau2")
    return by

nm_by = load_netmeta()

def rms(xs): return (sum(x*x for x in xs)/len(xs))**0.5

def reldev(bias, tau): return f"{100*bias/tau:+.1f}%" if tau > 0 else "—"

print("=" * 100)
print("Bias / RMSE table  (T=20, S=100, 50 reps per τ², per-arm iid RE)")
print("=" * 100)

priors = ["Flat (REML)", "HN(τ;0.5)", "HN(τ;1.0)", "HC(τ;0.5)"]

for ti, tau2 in TAUS:
    rust = load_rust(ti)
    reps = rust["results"]
    print(f"\nτ²_true = {tau2:.2f}   (n={rust['n_reps']}, GC wall = {rust['total_wall_sec']:.1f}s)")
    print(f"  {'estimator':<28}  {'mean':>9}  {'bias':>10}  {'(rel)':>7}  {'RMSE':>9}  {'sd':>9}  {'cov95':>6}")

    def show(label, xs, ci_los=None, ci_his=None):
        err = [x - tau2 for x in xs]
        b = st.mean(err); s = (sum(e*e for e in err)/len(err))**0.5
        cov = ""
        if ci_los is not None and ci_his is not None:
            covered = sum(1 for lo, hi in zip(ci_los, ci_his) if lo <= tau2 <= hi)
            cov = f"{100*covered/len(ci_los):.0f}%"
        print(f"  {label:<28}  {st.mean(xs):>9.5f}  {b:>+10.5f}  {reldev(b,tau2):>7}  "
              f"{s:>9.5f}  {st.pstdev(err):>9.5f}  {cov:>6}")

    show("GC bisection REML",   [r["reml_mode_iterative"] for r in reps])

    for pi, plabel in enumerate(priors):
        mode = [r["priors"][pi]["tau2"]["mode"]   for r in reps]
        med  = [r["priors"][pi]["tau2"]["median"] for r in reps]
        mean = [r["priors"][pi]["tau2"]["mean"]   for r in reps]
        lo   = [r["priors"][pi]["tau2"]["ci_lo"]  for r in reps]
        hi   = [r["priors"][pi]["tau2"]["ci_hi"]  for r in reps]
        show(f"{plabel} — mode",   mode, lo, hi)
        show(f"{plabel} — median", med,  lo, hi)
        show(f"{plabel} — mean",   mean, lo, hi)

    # netmeta
    nm_vals = [nm_by.get((ti, r["rep"])) for r in reps]
    nm_vals = [v for v in nm_vals if v is not None]
    if nm_vals:
        show(f"netmeta REML  (n={len(nm_vals)})", nm_vals)

print()
print("=" * 100)
print("Notes:")
print("  • bias = mean(τ̂²) − τ²_true   |   RMSE = √(mean((τ̂² − τ²_true)²))")
print("  • cov95 = fraction of 95% posterior CIs (under that prior) that contain τ²_true.")
print("  • A well-calibrated estimator has bias ≈ 0 and cov95 ≈ 95%.")
print("=" * 100)
print(f"netmeta progress: {len(nm_by)}/150 replicas processed")
