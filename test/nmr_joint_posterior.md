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
