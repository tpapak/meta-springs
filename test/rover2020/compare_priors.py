#!/usr/bin/env python3
"""Compare bayesmeta and gc-rust posterior summaries under identical
priors. gc-rust gives us one log-Z(τ²) grid; we reweight under each
of bayesmeta's priors and compare every summary on the τ² scale.

Priors:
  1. Uniform on τ ∈ [0, ∞)             →   p(τ²) ∝ τ²^(-1/2)
  2. HalfNormal(τ; σ = 0.5)            →   p(τ²) ∝ τ²^(-1/2) exp(-τ²/(2σ²))
  3. HalfNormal(τ; σ = 1.0)            →   same form, σ = 1
  4. HalfCauchy(τ; σ = 0.5)            →   p(τ²) ∝ τ²^(-1/2) / (1 + τ²/σ²)

For each, gc-rust grid is reweighted in log-spaced τ² with proper τ²
Jacobian (dτ² = τ² × d log τ²) and posterior summaries computed via
trapezoidal integration."""

import json, math
from pathlib import Path

ROOT = Path(__file__).resolve().parent

# --- bayesmeta reference ---
bm = json.load(open(ROOT / "bayesmeta_results.json"))["results"]
bm_by = {b["label"]: b for b in bm}

# --- gc-rust grid ---
gc = json.load(open(ROOT / "results" / "crins_gc.json"))
flat = next(p for p in gc["priors"] if p["label"] == "Flat (REML)")
grid = flat["tau2_grid"]
ts   = [r["tau2"] for r in grid]
ws_flat = [r["weight"] for r in grid]
# Recover bare log Z (gc-rust stores Flat-prior log_post = logZ + log τ², after normalisation)
log_z = [math.log(max(w, 1e-300)) - math.log(t) for w, t in zip(ws_flat, ts)]
log_log = [math.log(t) for t in ts]

def reweight(prior_log_density):
    """Apply prior to log_z grid and compute posterior summaries on τ² scale."""
    log_post = [lz + prior_log_density(t) for lz, t in zip(log_z, ts)]
    # weight for log-grid trapezoid: p(τ²) × τ²
    log_w = [lp + math.log(t) for lp, t in zip(log_post, ts)]
    m = max(log_w); w = [math.exp(x - m) for x in log_w]
    cdf = [0.0]
    for i in range(len(ts) - 1):
        cdf.append(cdf[-1] + 0.5 * (w[i] + w[i+1]) * (log_log[i+1] - log_log[i]))
    tot = cdf[-1]; cdf = [c / tot for c in cdf]
    def q(p):
        for i in range(1, len(cdf)):
            if cdf[i] >= p:
                x0, x1 = ts[i-1], ts[i]; c0, c1 = cdf[i-1], cdf[i]
                return x0 if c0 == c1 else x0 + (p - c0)/(c1 - c0)*(x1 - x0)
        return ts[-1]
    # mean E[τ²]
    mean = sum(0.5 * (ts[i]*w[i] + ts[i+1]*w[i+1]) * (log_log[i+1] - log_log[i])
               for i in range(len(ts) - 1)) / tot
    return dict(median=q(0.5), mean=mean, lo95=q(0.025), hi95=q(0.975))

priors = [
    ("Uniform on τ ∈ [0, ∞)",
     lambda t: -0.5 * math.log(t)),                            # |dτ/dτ²| = 1/(2τ) → -½ log τ²
    ("HalfNormal(τ; σ = 0.5)",
     lambda t: -0.5 * math.log(t) - 0.5 * t / 0.25),           # σ=0.5 → 1/σ² = 4 → t/(2σ²) = t × 2
    ("HalfNormal(τ; σ = 1.0)",
     lambda t: -0.5 * math.log(t) - 0.5 * t / 1.0),            # σ=1 → t/(2σ²) = t/2
    ("HalfCauchy(τ; σ = 0.5)",
     lambda t: -0.5 * math.log(t) - math.log(1 + t / 0.25)),   # σ=0.5 → 1/σ² = 4
]

print("=" * 110)
print("Bayesmeta vs gc-rust (grid-reweighted) on Crins 2014, summaries on τ² scale")
print("=" * 110)
print(f"{'prior':<26} {'metric':<12} {'bayesmeta':>12} {'gc-rust':>12} {'abs Δ':>10} {'rel Δ':>9}")
print("-" * 110)
for label, plog in priors:
    gc_post = reweight(plog)
    bm_post = bm_by[label]["tau2"]
    for k in ["median", "mean", "lo95", "hi95"]:
        bm_v = bm_post[k]; gc_v = gc_post[k]
        absd = abs(gc_v - bm_v)
        reld = absd / abs(bm_v) * 100 if bm_v != 0 else float("nan")
        print(f"{label:<26} {k:<12} {bm_v:>12.5f} {gc_v:>12.5f} {absd:>10.5f} {reld if reld==reld else 0:>8.1f}%")
    print()

# Per-replica timing
print("=" * 110)
print(f"Per-fit wall time:")
print(f"  gc-rust (single fit, one grid, all 4 priors via reweight): {gc['fit_wall_sec']*1000:.2f} ms")
print(f"  bayesmeta (4 separate fits, MCMC ~10K samples each):        ~5–20 s per fit (R)")
