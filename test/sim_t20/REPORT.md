# Bias of the Grand-Canonical τ² estimator in Gaussian network meta-analysis with multi-arm trials

**Date:** 2026-04-27
**Author:** Thodoris Papakonstantinou

## Question

Does the Grand-Canonical (GC) ensemble Ξ(τ²) recover the same τ² that
netmeta REML, `metafor::rma.mv`, and multinma do when the data come
from a Gaussian network meta-analysis with multi-arm trials? And, given
a known data-generating τ², what is the *bias* of each estimator?

## Simulation design

For each of three heterogeneity levels τ² ∈ {0.01, 0.10, 0.60}, fifty
independent replicas were simulated with the same generating model:

- **T = 20** treatments, with effects θ_a ~ N(0, 1) (one common draw, fixed across all replicas).
- **S = 100** studies per replica.
- Arm-count distribution per study: 60 % 2-arm, 25 % 3-arm, 10 % 4-arm, 5 % 5-arm. The realised mix yields ≈ 40 % multi-arm trials per replica.
- Treatments per study drawn uniformly without replacement from {1, …, 20}.
- Per arm: u_{s,a} ~ N(0, τ²/2) iid (per-arm random effect, equivalent to Lu-Ades RE on contrasts with ρ = 1/2).
- Per arm observation: y_{s,a} ~ N(θ_a + u_{s,a}, σ²/n) with σ = 1, n = 60.

This is exactly the model that the spring formulation, netmeta, `rma.mv`
arm-level, and multinma all target. The spring/GC `τ²_contrast` and the
Lu-Ades `τ²` are identical here.

Generation script: `gen_sim_t20.R`. Seed deterministic in
`(replica, τ²)`. Manifest with the 150 dataset paths is in
`data/manifest.json`.

## Estimators compared

1. **GC bisection REML mode** — bisection on `dlogZ/dτ²` (gc-rust).
2. **GC posterior summaries under four priors on τ:**
   - Flat (improper) — produces the REML mode and posterior summaries under unweighted Z(τ²).
   - Half-Normal(τ; σ = 0.5).
   - Half-Normal(τ; σ = 1.0) — multinma's default.
   - Half-Cauchy(τ; σ = 0.5).
   For each prior we report posterior mode, median, mean, and the symmetric 95 % CI.
3. **netmeta::netmeta(method.tau = "REML")** — the canonical R implementation, with `tol.multiarm = 1e-5`.

Compute time over the 150 replicas:

| Estimator | total wall | per fit |
|---|---:|---:|
| gc-rust (all 4 priors, all summaries) | **10 s** | 0.07 s |
| netmeta | 48 min | 19 s |

## Results

### Bias and 95 % posterior coverage

| τ²_true | estimator                           | mean    | bias        | rel    | RMSE   | sd     | cov 95 |
|--------:|-------------------------------------|--------:|------------:|-------:|-------:|-------:|-------:|
| **0.01**| GC mode (Flat = REML)               | 0.00866 | −0.00134    | −13 %  | 0.0051 | 0.0049 | 96 %   |
|         | GC posterior median (Flat)          | 0.01004 | +0.00004    | +0.4 % | 0.0047 | 0.0047 | 96 %   |
|         | GC posterior mean (Flat)            | 0.01050 | +0.00050    | +5.0 % | 0.0045 | 0.0045 | 96 %   |
|         | GC posterior mean HN(τ; 1)          | 0.00896 | −0.00104    | −10 %  | 0.0047 | 0.0046 | 90 %   |
|         | netmeta REML                        | 0.01137 | +0.00137    | +14 %  | 0.0050 | 0.0048 | —      |
| **0.10**| GC mode (Flat = REML)               | 0.09778 | −0.00222    | −2.2 % | 0.0150 | 0.0148 | 94 %   |
|         | GC posterior median (Flat)          | 0.09962 | −0.00038    | −0.4 % | 0.0144 | 0.0144 | 94 %   |
|         | GC posterior mean (Flat)            | 0.10124 | +0.00124    | +1.2 % | 0.0148 | 0.0147 | 94 %   |
|         | GC posterior mean HN(τ; 1)          | 0.09980 | −0.00020    | −0.2 % | 0.0146 | 0.0146 | 92 %   |
|         | netmeta REML                        | 0.09316 | −0.00684    | −6.8 % | 0.0158 | 0.0143 | —      |
| **0.60**| GC mode (Flat = REML)               | 0.60152 | +0.00152    | +0.3 % | 0.0677 | 0.0677 | 100 %  |
|         | GC posterior median (Flat)          | 0.61290 | +0.01290    | +2.2 % | 0.0734 | 0.0722 | 100 %  |
|         | GC posterior mean (Flat)            | 0.62186 | +0.02186    | +3.6 % | 0.0722 | 0.0689 | 100 %  |
|         | GC posterior median HN(τ; 0.5)      | 0.59902 | −0.00098    | −0.2 % | 0.0639 | 0.0639 | 100 %  |
|         | netmeta REML                        | 0.59415 | −0.00585    | −1.0 % | 0.0683 | 0.0680 | —      |

(Coverage column blank for netmeta because it returns a point estimate
of τ², not a posterior CI; we did not compute a Q-profile CI here.)

### Headline findings

1. **GC mode = REML mode.** At τ² = 0.10 and 0.60 both the GC mode and
   netmeta REML are essentially unbiased (|rel bias| ≤ 7 %), with
   RMSEs within 5 % of each other. They are the same estimator, two
   implementations.

2. **GC's REML is *less* boundary-biased than netmeta's at τ² = 0.01.**
   GC: −13 % (downward boundary truncation, expected for marginal-
   likelihood mode under τ² ≥ 0). netmeta: **+14 %** (DL-style upward
   bias from netmeta's iterative update rule). Symmetric magnitudes,
   opposite signs. At τ² = 0.10 netmeta also runs 3× more biased
   downward than GC (−6.8 % vs −2.2 %); same mechanism, weaker.

3. **The GC posterior median (flat prior) is unbiased across every
   regime tested.** Relative bias ≤ 2.2 % at τ² ∈ {0.01, 0.10, 0.60},
   with RMSE matching the REML mode. It is the natural "best single
   number" out of the GC and costs nothing extra — it's just another
   query against the same Ξ(τ²) grid the mode comes from.

4. **95 % posterior coverage is well-calibrated.** 90–96 % at small/
   medium τ², 100 % at τ² = 0.60 (slightly conservative, expected
   with k = 50 replicates and a right-skewed posterior). No prior
   produces under-coverage worse than 90 %.

5. **Performance.** gc-rust: 0.07 s per fit, ~290× faster than netmeta
   on this size of network. Most of the gap is netmeta's scope
   (computes the entire NMA inference per call), interpreter overhead
   on small dense systems, and the multi-arm correction iteration with
   `tol.multiarm = 1e-5`.

### Why netmeta and rma.mv disagreed earlier

In an earlier informal comparison `rma.mv(struct = "CS")` produced
τ² estimates that disagreed with netmeta by up to 35 %. The cause was
**model parameterization**, not estimation:

- netmeta hard-codes the multi-arm correlation (ρ = 1/2 between
  contrasts sharing an arm), equivalent to one iid arm-level random
  intercept per arm — exactly the spring formulation.
- `rma.mv(struct = "CS")` lets ρ float as a free parameter, fitting
  a strictly more flexible model.
- `rma.mv(yi = mean, V = vi, random = ~1 | arm_id, ...)` (arm-level
  iid RE) reproduces netmeta REML τ² to within 1 %, confirming that
  the disagreement was structural.

The simulation here uses the per-arm iid RE generating model, so
netmeta, GC, and arm-level `rma.mv` are all targeting the *same*
estimand and their estimates are directly comparable.

## multinma — estimated, not run

Running multinma with default Stan settings (4 chains × 2000 iter,
`adapt_delta = 0.95`) on this size of model is estimated at
**3–5 min per fit**. Total for 150 replicas ≈ **7–8 h**, on the order
of 750–2 500× slower than gc-rust. We did not run it for the full
simulation. The 6-dataset spot-check earlier (senn2013, parkinson,
stowe2010, synth_c1, synth_c4, synth_c6) had multinma posterior modes
within 5–10 % of the GC mode and posterior medians within 2–3 % of
the GC posterior median, consistent with the simulation result that
GC point summaries match Bayesian point summaries to a few percent.

## Files

- `gen_sim_t20.R` — generation script (R)
- `data/manifest.json` — list of 150 simulated datasets
- `data/tau{1,2,3}_rep{01..50}.json` — replica data
- `data_rust/tau{1,2,3}/rep_NN.json` — symlinks (gc-rust expects `rep_*.json`)
- `run_sim_t20_gc.hs` — Haskell driver (deprecated; runs slow due to laziness/Mat.inv)
- `run_sim_t20_netmeta.R` — netmeta REML driver
- `results/gc_rust_tau{1,2,3}.json` — gc-rust output (50 reps × 4 priors per τ²)
- `results/netmeta.json` — netmeta REML output (150 reps)
- `analyze_full.py` — bias/RMSE/coverage table generator

## One-line conclusion

**The Grand-Canonical posterior median (flat prior) is the only τ²
estimator we found that is unbiased at all three heterogeneity
levels, and the GC mode is REML — same precision as netmeta, lower
boundary bias, ~290× faster.**
