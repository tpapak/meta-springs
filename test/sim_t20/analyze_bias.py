#!/usr/bin/env python3
"""Bias / RMSE for GC estimators against the simulation truth.

Reads gc-rust multi-rep outputs (1 file per τ² level, 50 replicas each).
If netmeta.json is present, joins it in too.
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

def stat(xs, true):
    if not xs: return None
    err = [x - true for x in xs]
    return dict(n=len(xs), mean=st.mean(xs), bias=st.mean(err),
                rmse=(sum(e*e for e in err)/len(err))**0.5,
                sd=st.pstdev(err),
                lo=min(xs), hi=max(xs))

def fmt(s):
    if s is None: return "(none)"
    return f"mean={s['mean']:.5f}  bias={s['bias']:+.5f}  RMSE={s['rmse']:.5f}  sd={s['sd']:.5f}  range[{s['lo']:.4f}, {s['hi']:.4f}]"

print("=" * 100)
print("Simulation: T=20, S=100, 50 replicas per τ², per-arm iid RE (Lu-Ades / spring model)")
print("=" * 100)

for ti, tau2 in TAUS:
    rust = load_rust(ti)
    reps = rust["results"]
    print(f"\nτ²_true = {tau2:.2f}    (n={rust['n_reps']} replicas, total GC wall = {rust['total_wall_sec']:.1f}s)")

    # Estimators from Rust:
    gc_mode_flat = [r["priors"][0]["tau2"]["mode"]   for r in reps]
    gc_mean_flat = [r["priors"][0]["tau2"]["mean"]   for r in reps]
    gc_med_flat  = [r["priors"][0]["tau2"]["median"] for r in reps]
    gc_mean_hn05 = [r["priors"][1]["tau2"]["mean"]   for r in reps]
    gc_mean_hn1  = [r["priors"][2]["tau2"]["mean"]   for r in reps]
    gc_reml_iter = [r["reml_mode_iterative"]         for r in reps]

    nm_vals = [nm_by.get((ti, r["rep"])) for r in reps]
    nm_vals = [v for v in nm_vals if v is not None]

    rows = [
        ("GC mode (Flat) — REML",  gc_mode_flat),
        ("GC bisection REML",      gc_reml_iter),
        ("GC posterior median Flat", gc_med_flat),
        ("GC posterior mean Flat",   gc_mean_flat),
        ("GC mean HN(τ;0.5)",        gc_mean_hn05),
        ("GC mean HN(τ;1.0)",        gc_mean_hn1),
    ]
    if nm_vals: rows.append(("netmeta REML",        nm_vals))

    for label, xs in rows:
        s = stat(xs, tau2)
        print(f"  {label:<28}  n={s['n']:3d}  {fmt(s)}")

print()
print("=" * 100)
print(f"netmeta data: {len(nm_by)} rows joined" if nm_by else "netmeta: not run yet")
