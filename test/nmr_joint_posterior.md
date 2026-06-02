# Joint (β, τ²) posterior for network meta-regression — `nmaRegressionPosterior`

Removes the earlier corner (GC/REML had β **profiled out** and handed in). Here β is
**integrated**, not profiled: a genuine joint posterior over the regression.

## Method (exact linear-Gaussian marginalisation)
Flat priors on (μ, d, β, τ²). β enters linearly and Gaussian, exactly like μ and d, so:
- On a τ² grid, the fixed effects (μ, d, β) are integrated **analytically**. The τ²
  posterior is `∝ exp(REML logLik(τ²))` — the REML evidence (mode refined off-grid by
  parabolic interpolation).
- `β | τ²` is Gaussian with mean = GLS estimate, variance = `[(XᵀΣ⁻¹X)⁻¹]_ββ`.
- β's marginal posterior is therefore a **τ²-mixture of Gaussians**
  `∫ N(β; β̂(τ²), Var(β|τ²)) p(τ²|y) dτ²` — closed form, no MCMC, no profiling.

Returns posterior summaries: β (mean, sd, 95% credible), τ² (mode, mean, 95% credible),
and the full grid `(τ², weight, β̂(τ²), Var(β|τ²))`. See `Data.Meta.NMA.RegPosterior`.

## Validation — vs `netmetareg` only (20 sim reps; truth β=0.30, τ²=0.10)

**Point estimates reproduce netmetareg exactly**
| comparison | max&#124;diff&#124; | cor |
|---|---|---|
| β posterior mean vs netmetareg β̂ | 0.00005 | 1.0000 |
| τ² posterior mode vs netmetareg τ̂² (REML) | 0.00050 | 1.0000 |

(τ² diff is grid resolution; the posterior **mode** = the REML point estimate, the
β posterior **mean** = the GLS/REML β̂.)

**Uncertainty — the reason to build it**
| quantity | joint posterior | netmetareg |
|---|---|---|
| β SD / SE (mean) | 0.0693 | 0.0606 (Wald, conditional on τ̂²) |
| β 95% width (mean) | 0.2751 | 0.2376 |
| τ² 95% interval (mean) | [0.058, 0.320] (credible) | [0.049, 0.248] (profile) |

The β posterior is **1.14× wider** than the Wald SE because it **integrates τ²
uncertainty** instead of fixing τ²=τ̂². The τ² credible interval has a heavier right
tail (flat-prior posterior vs profile likelihood).

**Coverage of truth** (β=0.30, τ²=0.10), 20 reps:
| | joint posterior | netmetareg |
|---|---|---|
| β covered | 19/20 | 18/20 (Wald) |
| τ² covered | 19/20 | 18/20 (profile) |

## β written into the GC solution (not profiled)
`nmaRegressionPosterior` **is** the grand-canonical solution with β added as a
coordinate. At each τ² the partition function `Z(τ²)` is the Gaussian integral over the
free coordinates — and β is just one more free linear coordinate (coupled through the
lever `x`), integrated analytically alongside (μ, d). So the τ² posterior is
`∝ Z(τ²)·prior(τ²)` with β **integrated, never profiled**, and `β | τ²` falls out of the
same solve. A `Tau2PriorR` argument (`FlatT2` = REML evidence; `HalfNormalTauR σ` on the
SD τ) sets the heterogeneity prior; a midpoint τ² grid keeps the τ-prior Jacobian finite.

## Cross-check against gemtc (matched prior) + a 5-treatment multi-arm network
With a **matched half-normal(0.5)** prior on τ, the spring joint posterior and gemtc's
MCMC posterior are overlaid in `test/nmr_posteriors.html` (data in
`test/nmr_posterior_data/`, built by `nmr_posterior_dump.hs` + `nmr_posterior_ref.R`;
5-treatment multi-arm network from `nmr5_multiarm_gen.R`). netmetareg (flat/REML) marks
the **mode**.

| network | quantity | spring (HN) | gemtc (HN) | netmetareg (mode) | truth |
|---|---|---|---|---|---|
| 3-trt, 2-arm | β | 0.404 [0.275,0.533] | 0.399 [0.272,0.527] | 0.404 ±0.061 | 0.30 |
| 3-trt, 2-arm | τ² | 0.122 (mode .093) [.049,.256] | 0.111 [.051,.258] | 0.104 [.047,.243] | 0.10 |
| 5-trt, multi-arm | β | 0.235 [0.093,0.377] | 0.230 [0.090,0.372] | 0.235 ±0.068 | 0.30 |
| 5-trt, multi-arm | τ² | 0.063 (mode .049) [.023,.138] | 0.058 [.022,.136] | 0.055 [.021,.128] | 0.08 |

The spring and gemtc densities are **essentially superimposed** for both β and τ² on both
networks — including the multi-arm network (multi-arm correlation is handled by the free
study baselines μ_i plus iid per-arm random effects, τ²ₐᵣₘ = τ²/2). On the 5-treatment
single dataset all three methods agree with each other (β≈0.23, τ²≈0.05); the gap to truth
(0.30, 0.08) is sampling noise in that one dataset, not method disagreement.

## What this does and does not claim
- The joint posterior is a genuine flat-prior Bayesian posterior over (β, τ²) by exact
  marginalisation — **β is integrated, not profiled** (the corner is gone).
- Its point summaries equal netmetareg's REML point estimates; it adds honest,
  slightly wider uncertainty by accounting for τ² uncertainty.
- Still a flat prior on τ² (improper); a proper half-normal/half-Cauchy prior is a
  one-line change (the grid weights already accept a prior factor).
- The data-generating process is the spring model itself (best case); not a
  misspecification stress test.

Reproduce: `stack runghc -- test/nmr_joint_posterior.hs 1 20` →
`test/nmr_gc_reml_results/joint.csv`; reference `test/nmr_joint_netmetareg_ref.R` →
`nmr_ref.csv`; comparison numbers above.
