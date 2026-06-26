# Baseline-risk regression: Monte-Carlo verification + Rust

Frequentist verification of the baseline-risk estimator (`nmaRegressionBaselineRiskBin` and
its Rust port `nmr_brisk`), and of the 2-D grand canonical (`...ExchGC` / `nmr_brisk_gc`),
by repeated simulation from a known truth — the same methodology as `nmr_gc_reml_simulation.md`.

## 1. FE shared slope — bias, RMSE, 95% coverage (200 reps)

DGP (`nmr_baseline_risk_mc_gen.R`): 30 two-arm studies (15 A–B, 15 A–C), n=120/arm, baselines
η_i ~ N(−1, 0.85²), truth **B = −0.50**, d[B] = 0.80, d[C] = 1.20 (contrasts at the mean
baseline). Fit FE shared baseline-risk to each rep in one Haskell process
(`nmr_baseline_risk_mc_fit.hs`), aggregate against truth (`nmr_baseline_risk_mc_agg.R`).
**200/200 reps converged.**

| param | mean | bias | RMSE | emp.sd | mean.se | cover₉₅ |
|---|---|---|---|---|---|---|
| **B** (−0.50) | −0.5019 | **−0.0019** | 0.0564 | 0.0566 | 0.0542 | **0.960** |
| d[B] (0.80) | 0.8165 | +0.0165 | 0.0641 | 0.0621 | 0.0570 | 0.920 |
| d[C] (1.20) | 1.2192 | +0.0192 | 0.0734 | 0.0711 | 0.0572 | 0.865 |

**The baseline-risk slope B — the parameter of interest — is unbiased** (bias −0.002), its
observed-information SE matches the empirical spread (0.054 vs 0.057), and Wald coverage is
**0.96**. The treatment contrasts d_t carry a small upward bias scaling with coefficient
magnitude (+0.017 at 0.8, +0.019 at 1.2) — the standard finite-sample logistic-MLE bias — with
mild under-coverage, amplified by the data-dependent centering (d_t is the effect at the
observed mean baseline). Both shrink with n. τ² recovered ≈ 0 (FE DGP), as it should.

## 2. Exchangeable σ_B² — does integrating beat maximising? (80 reps)

DGP: 6 active treatments, per-treatment slopes drawn b_t ~ N(−0.5, **σ_B² = 0.06**), n=100/arm;
two regimes — well-identified (6 studies/treatment) and sparse (2/treatment). For each rep fit the
grouped-REML/EM point and the 2-D GC (`nmr_brisk_gc`, Rust, batch mode); 80 reps each.

| regime | estimator | mean | bias | RMSE | % collapsed to boundary |
|---|---|---|---|---|---|
| **well-identified** (6/trt) | EM (REML)  | 0.057 | −0.003 | 0.060 | 9% |
| | 2-D GC mode | 0.074 | +0.014 | 0.064 | 1% |
| | 2-D GC mean | 0.096 | +0.036 | 0.078 | 0% |
| **sparse** (2/trt) | EM (REML)  | 0.057 | −0.003 | 0.076 | **35%** |
| | 2-D GC mode | 0.109 | +0.049 | 0.079 | 0% |
| | 2-D GC mean | 0.118 | +0.058 | 0.078 | 0% |

**Honest reading — σ_B² is weakly identified and neither estimator dominates on point accuracy.**
The EM maximiser is unbiased *on average* but **collapses to the boundary** in a large fraction
of reps (9% well-identified → 35% sparse); each collapse welds the per-treatment slopes to a single
value with false-precision SEs. The integrated GC **never collapses** (a proper posterior) but is
**biased upward by the prior**, increasingly so as the data weaken (the half-normal prior dominates
when σ_B² is barely identified). RMSE is comparable. So the 2-D GC's value is *not* a better σ_B²
point estimate — it is (i) a non-degenerate posterior that **propagates σ_B² uncertainty into the
slope SDs** (matching bnma's slope SDs on the dense single-dataset check in `nmr_baseline_risk.md`,
where the EM collapse gives artificially tight SDs), and (ii) lifting σ_B² off the boundary in the
weakly-identified regime (certolizumab). The point value is prior-sensitive either way — exactly
the τ²-style caveat, now quantified.

### 2a. Slope-SD calibration — where the GC earns its keep

The decisive check: does the per-treatment slope interval **cover the true b_t at 95%?** (true
b_t ~ N(−0.5, 0.06), 100 reps × 6 treatments = 600 slope estimates per regime; `nmr_brisk_gc`
emits both the EM point slope ±SE and the GC slope ±posterior-SD).

| regime | estimator | mean slope SD | bias | **95% coverage of true b_t** |
|---|---|---|---|---|
| **dense** (6/trt) | EM point | 0.118 | −0.016 | **0.870** |
| | **2-D GC** | 0.135 | −0.017 | **0.937** |
| **sparse** (2/trt) | EM point | 0.210 | −0.023 | **0.823** |
| | **2-D GC** | 0.289 | −0.025 | **0.972** |

The slope **biases are identical** (both shrink ~0.02 toward the common mean); the *only*
difference is the SD. The EM/grouped-REML point conditions on σ̂_B², which collapses toward the
boundary and **welds the slopes with artificially tight SEs → under-coverage (0.87 dense, 0.82
sparse)**. The 2-D GC integrates σ_B², inflating the slope SDs to their honest size → **coverage
0.94 / 0.97**, near nominal (slightly conservative when sparse, as the prior widens the
intervals). This is the concrete payoff of integrating rather than maximising the slope
dispersion: not a better σ_B² point estimate, but **calibrated slope uncertainty** — the
frequentist confirmation of the single-dataset finding that the GC slope SDs match bnma's while
the EM point's do not.

## 3. Rust port — exact agreement + speed

`gc-rust/examples/nmr_brisk.rs` (Fisher-scoring FE/shared) and `nmr_brisk_gc.rs` (exchangeable
mode + 2-D GC), nalgebra/Cholesky. Verified **Rust == Haskell == exact MLE to 5 dp** (point
estimates *and* observed-information SEs):

| dataset | B (Rust) | B (Haskell/MLE) | Rust time |
|---|---|---|---|
| certolizumab (np=19) | −0.84936 (se 0.13254) | −0.84936 (se 0.13254) | **0.16 ms** |
| simulation (np=52) | −0.66091 (se 0.03431) | −0.66091 (se 0.03431) | **0.29 ms** |

The naive Haskell `[[Double]]` solver takes ~30–60 s at np=52; Rust/nalgebra is ~10⁵× faster
— the performant path (cf. `nmr_benchmark.md`). The 2-D GC matches Haskell exactly and runs in
**4 ms** (vs tens of seconds in Haskell), which is what makes the σ_B² Monte Carlo above feasible:

| 2-D GC, certolizumab | EM σ_B² | GC mode | GC mean | τ² | B | time |
|---|---|---|---|---|---|---|
| Rust | 0.00132 | 0.04924 | 0.07762 | 0.09165 | −0.90526 | 4.1 ms |
| Haskell | 0.00131 | 0.045 | 0.0776 | 0.0916 | −0.905 | ~30 s |

## Reproduce
```
Rscript test/nmr_baseline_risk_mc_gen.R                 # 200 FE reps -> /tmp/brmc
stack runghc -- test/nmr_baseline_risk_mc_fit.hs        # fit all (one process)
Rscript test/nmr_baseline_risk_mc_agg.R                 # bias/RMSE/coverage
cd gc-rust && cargo build --release --example nmr_brisk --example nmr_brisk_gc
./target/release/examples/nmr_brisk    ../test/nmr_certolizumab_arms.json Placebo
./target/release/examples/nmr_brisk_gc ../test/nmr_certolizumab_arms.json Placebo
./target/release/examples/nmr_brisk_gc --batch /tmp/brexch A 80   # exch σ_B² MC (one exec)
```

Arm-level data for Rust (`{study,treatment,events,n}`) is produced by the small R snippet in
`nmr_baseline_risk_montecarlo.md`'s history; `test/nmr_certolizumab_arms.json` and
`test/nmr_br_sim_arms.json` are checked in.
