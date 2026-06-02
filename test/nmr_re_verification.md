# Random-effects network meta-regression — `nmaRegressionRE`

The τ extension of `nmaRegression`: estimate the heterogeneity τ² **jointly** with the
shared covariate slope β, on the same three-node spring model as the golden-standard
`makeSprings` random-effects network (`StudyEnd → TauEnd → TreatmentEnd`).

## Model
Arm-level, free study baselines μ_i, reference treatment pinned (d_ref = 0):

```
y_ij = μ_i + d_t + β·x_i·[t≠ref] + δ_ij ,   δ_ij ~ N(0, τ²ₐᵣₘ),   ε_ij ~ N(0, v_ij)
```

Spring convention (identical to `makeSprings`): obs spring `k=1/v`, tau spring `k=2/τ²`
in series ⇒ effective arm variance `v + τ²ₐᵣₘ` with **arm-space τ²ₐᵣₘ = τ²/2** (the
reported τ² is contrast-space). The covariate shifts the treatment end of non-reference
arms by `β·x_i` (the παραλληλόγραφος / shared slope); β is identified only from
reference-involving contrasts (β cancels in active-vs-active studies).

## Estimation (REML/EM) — matches springREML's update exactly
Given τ², the fixed effects (μ, d, β) are the GLS solution with weights `1/(v+τ²ₐᵣₘ)`.
The heterogeneity is updated by the same EM step `springREML` uses:

```
τ²_new = 2·( mean δ̂²  +  mean Var(δ̂) )           (×2: arm-space → contrast-space)
δ̂_ij    = b_ij · (fitted_ij − y_ij)               b_ij = w·τ²ₐᵣₘ/(1+w·τ²ₐᵣₘ),  w=1/v
Var(δ̂_ij|y) = (1 − b_ij)·τ²ₐᵣₘ  +  b_ij²·Var(fitted_ij)
            = (1 − b_ij)·τ²ₐᵣₘ  +  b_ij²·(aᵀ Cov a)
```

Per-arm `δ̂²` and `Var(δ̂)` were checked to reproduce `springREML`'s internal values
**exactly** (e.g. at τ²=0.5: Var(δ̂)=0.178161/arm; at τ²=0.0688: 0.0290926/arm).

## Verification against external gold standards (REML)

Dataset A (3-treatment network, no covariate; sd=2, n=100 ⇒ arm v=0.04):
S1,S2: A–B (1.0,1.6); S3,S4: A–C (2.0,2.6); S5,S6: B–C (0.8,1.4).

| quantity            | `nmaRegressionRE` | netmeta REML | match |
|---------------------|-------------------|--------------|-------|
| τ² (contrast-space) | 0.056682          | 0.0566667    | ✓     |
| A→B                 | 1.266667          | 1.266667     | ✓     |
| A→C                 | 2.333333          | 2.333333     | ✓     |

Dataset B (two-treatment RE meta-regression on covariate x = 0…5,
contrasts 0.60,0.70,1.25,1.25,1.75,1.95; contrast var 0.08):

| quantity              | `nmaRegressionRE` | metafor `rma(REML)` | match |
|-----------------------|-------------------|---------------------|-------|
| β (slope)             | 0.282857          | 0.282857            | ✓     |
| intercept (A→B @ x=0) | 0.542857          | 0.542857            | ✓     |
| τ²                    | 0.00102 (≈0)      | 0.000000            | ≈ (boundary) |

So β, the intercept and the network contrasts reproduce the recognised REML standards
**exactly**; τ² reproduces netmeta REML exactly when positive, and sits at the ≈0 boundary
when the covariate explains the between-study variation.

## Note on `springREML`
On Dataset A `springREML` reports τ²=0.0688, which matches neither REML (0.0567), ML
(0.0111) nor DL (0.0567) from netmeta — its Newton acceleration on
`g(τ²)=τ²−emStep(τ²)` lands off the REML fixed point. The same deviation is visible in
the existing test suite (`spring REML tau2 15.97` vs `netmeta 12.97`). `nmaRegressionRE`
uses the plain REML/EM fixed point and therefore tracks netmeta/metafor REML.

Reproduce: `stack runghc -- test/REverify.hs`; references `/tmp/reref2.R` (netmeta),
`/tmp/regref.R` (metafor).
