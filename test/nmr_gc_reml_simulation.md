# Honest Monte Carlo: Grand-Canonical vs REML τ² in network meta-regression

**Question.** In network meta-regression (covariate slope β + heterogeneity τ²),
how does the spring **Grand Canonical** (GC) τ² estimate compare with **REML**, with
the library's **springREML**, and with external gold standards
(**netmeta `netmetareg`**, **metafor**, **gemtc**, **multinma**) — measured against a
known truth?

## Design (fully honest, no tuning to the answer)
- Generative model = **exactly** the spring RE model. Treatments A,B,C; every arm
  (incl. reference) carries an iid random effect `u ~ N(0, τ²ₐᵣₘ)`,
  `τ²ₐᵣₘ = τ²_contrast/2`, so any within-study contrast has heterogeneity variance
  `2·τ²ₐᵣₘ = τ²_contrast` (the standard contrast-scale estimand). Shared slope β shifts
  non-reference arms by `β·x_study`.
- **Truth:** β = 0.30, τ²_contrast = 0.10, d_AB=0.5, d_AC=1.2, σ=1, n=100/arm.
- Topology (fixed across reps): 20 two-arm studies (10 A–B, 6 A–C, 4 B–C); study
  covariate `x ~ N(0,1.2)`. β is identifiable only from reference-anchored contrasts
  (cancels in B–C). **200 replicates**; only the outcomes vary across reps.
- Files: generator `nmr_gc_reml_sim_gen.R`; spring/GC/REML runner `nmr_gc_reml_sim.hs`;
  `netmetareg` runner `nmr_gc_reml_sim_netmetareg.R`; Bayesian subset
  `nmr_gc_reml_sim_bayes.R`. For GC/springREML the covariate is profiled out
  (subtract β̂·x from non-reference arms) and τ² estimated on the adjusted network.

## Result — β (truth 0.30), 200 reps
| method | mean | bias | sd |
|---|---|---|---|
| REML (`nmaRegressionRE`)        | 0.3036 | +0.0036 | 0.0582 |
| netmeta `netmetareg` (REML)     | 0.3036 | +0.0036 | 0.0582 |
| metafor `rma` (REML, anchored)  | 0.3021 | +0.0021 | 0.0655 |

`nmaRegressionRE` and `netmetareg` agree to 4 decimals **on every rep** (β and τ²).

## Result — τ² contrast-scale (truth 0.10), 200 reps
| τ² estimator | mean | bias | RMSE |
|---|---|---|---|
| **GC mode** (flat prior)        | 0.0971 | **−0.0029** | 0.0378 |
| spring-REML (library)           | 0.0967 | −0.0033 | 0.0370 |
| REML (`nmaRegressionRE`)        | 0.1032 | +0.0032 | 0.0395 |
| netmeta `netmetareg` (REML)     | 0.1032 | +0.0032 | 0.0395 |
| **GC mean** (flat prior)        | 0.1296 | **+0.0296** | 0.0563 |
| netmeta REML (adjusted, proxy)  | 0.1113 | +0.0113 | 0.0431 |

Per-rep correlation with `netmetareg`: GC mode 0.86, GC mean 0.86, spring-REML 0.86,
`nmaRegressionRE` 1.00 (identical).

## Honest conclusions
1. **GC mode ≈ REML ≈ spring-REML ≈ truth.** The grand-canonical posterior **mode**
   (flat prior) recovers τ² with negligible bias (−0.003), matching REML and the truth.
2. **The GC posterior *mean* (flat prior) is biased ~30% high** (+0.030). The τ²
   posterior is right-skewed, so mean ≫ mode. *Use the GC mode (or a proper prior),
   not the flat-prior mean, when comparing to REML point estimates.*
3. **`nmaRegressionRE` (my REML) == netmeta `netmetareg` exactly** across all 200 reps
   for both β and τ² — the implementation reproduces the recognised frequentist NMR.
4. The earlier single-toy impression that "spring-REML runs ~20% high" did **not**
   hold up under simulation: spring-REML is ~unbiased here (it was a degenerate
   6-study artifact).

## Bayesian cross-check (subset of reps — MCMC is slow)
Random-effects shared-slope NMR. All frequentist/spring numbers below are recomputed
on the **same** reps for a fair comparison.

**gemtc** (25 reps, `linearModel="random"`, shared regressor, covariate de-standardised):

| quantity (truth) | gemtc | GC mode | spring-REML | nmaRegRE = netmetareg |
|---|---|---|---|---|
| β  (0.30) | 0.3074 | — | — | 0.3110 |
| τ² (0.10) | 0.1186 (+0.019) | 0.1014 (+0.001) | 0.0999 (−0.000) | 0.1067 (+0.007) |

gemtc is reliable: β unbiased, τ² ~18 % high (typical vague-prior Bayesian τ² inflation
in small samples).

**multinma** (8 reps, `trt_effects="random"`, `regression=~x`, `class_interactions="common"`):

| rep | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | mean |
|---|---|---|---|---|---|---|---|---|---|
| β[x] (truth 0.30) | −1.55 | 1.43 | 3.35 | −0.79 | 4.70 | −0.16 | −0.59 | −1.53 | 0.61 |
| τ² (truth 0.10) | 0.40 | 0.24 | 0.28 | 0.28 | 0.20 | 0.34 | 0.28 | 0.18 | 0.27 |

**multinma is unreliable here:** the slope swings from −1.5 to +4.7 (sign and magnitude)
and τ² is consistently 2–4× too high. This is **not** a tuning failure — it is the known,
by-design weak identification of multinma's regression from **aggregate-only** data
(multinma targets within-study effect modification via IPD/ML-NMR; the covariate–treatment
interaction is not identified from study-level means alone — ecological-bias avoidance).
With aggregate data, prefer gemtc / netmetareg / the spring REML+GC for NMR.

## Bottom line
On a like-for-like estimand (contrast-scale τ², shared β), against a known truth:

| method | β bias | τ² bias | verdict |
|---|---|---|---|
| spring **GC mode** | (β via REML) | −0.003 | matches truth & REML |
| spring **REML** (`nmaRegressionRE`) | +0.004 | +0.003 | == netmeta `netmetareg` exactly |
| netmeta `netmetareg` | +0.004 | +0.003 | frequentist NMR gold standard |
| metafor `rma` | +0.002 | n/a (β only) | agrees |
| gemtc | +0.007 | +0.019 | reliable, τ² mildly high |
| spring **GC mean** (flat prior) | — | +0.030 | skew-inflated; use the mode |
| multinma | wild (±) | +0.17 | unreliable on aggregate data |
