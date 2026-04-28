#!/usr/bin/env python3
"""Bias / RMSE / coverage for the larger simulation (T=30, S=200,
200 reps × 7 τ² levels)."""

import json, statistics as st
from pathlib import Path

ROOT = Path(__file__).resolve().parent
TAUS = [(1, 0.001), (2, 0.005), (3, 0.01), (4, 0.05), (5, 0.10), (6, 0.30), (7, 0.60)]

def load(idx):
    return json.load(open(ROOT / "results" / f"gc_rust_tau{idx}.json"))

def stat(xs, true):
    if not xs: return None
    err = [x - true for x in xs]
    return dict(n=len(xs), mean=st.mean(xs), bias=st.mean(err),
                rmse=(sum(e*e for e in err)/len(err))**0.5,
                sd=st.pstdev(err))

def reldev(b, t): return f"{100*b/t:+.1f}%" if t > 0 else "—"

print("=" * 110)
print("Larger simulation: T=30, S=200, 200 reps per τ² (7 levels), per-arm iid RE")
print("=" * 110)
print(f"\n{'τ²_true':>9}  {'estimator':<28}  {'mean':>10}  {'bias':>11}  {'rel':>9}  {'RMSE':>10}  {'sd':>10}  {'cov95':>6}")
print("-"*110)

for ti, tau2 in TAUS:
    rust = load(ti)
    reps = rust["results"]
    mode_flat = [r["priors"][0]["tau2"]["mode"]   for r in reps]
    med_flat  = [r["priors"][0]["tau2"]["median"] for r in reps]
    mean_flat = [r["priors"][0]["tau2"]["mean"]   for r in reps]
    mode_hn05 = [r["priors"][1]["tau2"]["mode"]   for r in reps]
    med_hn05  = [r["priors"][1]["tau2"]["median"] for r in reps]
    mean_hn05 = [r["priors"][1]["tau2"]["mean"]   for r in reps]
    med_hn1   = [r["priors"][2]["tau2"]["median"] for r in reps]
    mean_hn1  = [r["priors"][2]["tau2"]["mean"]   for r in reps]
    lo_flat   = [r["priors"][0]["tau2"]["ci_lo"]  for r in reps]
    hi_flat   = [r["priors"][0]["tau2"]["ci_hi"]  for r in reps]
    cov_flat  = sum(1 for l,h in zip(lo_flat,hi_flat) if l <= tau2 <= h) / len(reps) * 100
    walls = [r["wall_sec"] for r in reps]

    def show(label, xs, cov=None):
        s = stat(xs, tau2)
        cstr = f"{cov:.0f}%" if cov is not None else ""
        print(f"  {tau2:>9.4f}  {label:<28}  {s['mean']:>10.5f}  {s['bias']:>+11.5f}  "
              f"{reldev(s['bias'], tau2):>9}  {s['rmse']:>10.5f}  {s['sd']:>10.5f}  {cstr:>6}")

    show("Flat — mode (REML)",  mode_flat, cov_flat)
    show("Flat — median",        med_flat,  cov_flat)
    show("Flat — mean",          mean_flat, cov_flat)
    show("HN(τ;0.5) — median",   med_hn05)
    show("HN(τ;0.5) — mean",     mean_hn05)
    show("HN(τ;1.0) — median",   med_hn1)
    show("HN(τ;1.0) — mean",     mean_hn1)

    twall = rust["total_wall_sec"]
    note  = f"(wall: median {st.median(walls)*1000:.0f}ms, total {twall:.0f}s)"
    print(f"  {'':>9}  {note}")
    print()

print("=" * 110)
