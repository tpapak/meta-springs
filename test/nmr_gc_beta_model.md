# The β model in the Grand-Canonical (Gaussian) framework

Arm-level model (continuous), study *i*, arm of treatment *t* (reference `ref`,
`d_ref = 0`):

```
y_it = μ_i + d_t + [t≠ref]·β_t·x_i + u_it + ε_it ,   u_it ~ N(0, τ²/2),  ε_it ~ N(0, v_it)
```

Collect the free coordinates θ = (μ₁…μ_S, d₂…d_T, β-params). The grand-canonical
**partition function** at heterogeneity τ² (and, for exchangeable, slope variance σ_B²)
is the Gaussian integral over θ with a flat prior on (μ, d) (and on the slope mean):

```
Z(τ² [,σ_B²]) = ∫ exp(−E(θ)) dθ ,   E(θ) = ½ (y−Xθ)ᵀ Σ⁻¹ (y−Xθ)  +  ½ θᵀ K θ
Σ = diag(v_it + τ²/2)                      (obs + heterogeneity springs in series)
K = prior precision on the slopes         (0 except the exchangeable β-block)
```

Closed form (the GC log-evidence), with `A = XᵀΣ⁻¹X + K`, `b = XᵀΣ⁻¹y`:

```
log Z = −½ [ Σ log(v_it+τ²/2) + log|A| + (yᵀΣ⁻¹y − bᵀA⁻¹b) ]  (+ exchangeable term)
θ̂ = A⁻¹ b ,   Cov(θ) = A⁻¹
```

The τ² (and σ_B²) posterior is `∝ Z·prior`; every contrast/slope posterior is the
grid-mixture of the Gaussians `N(θ̂_param, Cov_param)` over the τ² (×σ_B²) grid.
**β is a free coordinate, integrated, never profiled.**

## The three β structures = three couplings of the slope coordinates

| model | slope columns of X | prior precision K | limit |
|---|---|---|---|
| **shared** | one column `B`; entry `x_i` on **every** non-ref arm | 0 | σ_B²→0 |
| **unrelated** | one column `β_t` per non-ref treatment; entry `x_i` on arms of `t` | 0 | σ_B²→∞ |
| **exchangeable** | `β_t` columns **plus** a hyper-mean column `B` | `(1/σ_B²)·Σ_t (e_{β_t}−e_B)(e_{β_t}−e_B)ᵀ` | 0<σ_B²<∞ |

- **shared**: a single slope common to all treatments (the παραλληλόγραφος / parallel-rule).
- **unrelated**: independent slopes, flat priors — just a wider design matrix, same integral.
- **exchangeable**: `β_t ~ N(B, σ_B²)` — a *second* Gaussian spring layer tying each slope to a
  common mean `B` with hardness `1/σ_B²`. σ_B² is a second GC variance component; its
  evidence term adds `−½(T−1)·log σ_B²` to `log Z`, and the GC grid extends to (τ², σ_B²).
  Shared and unrelated are exactly its σ_B²→0 and σ_B²→∞ limits.

So all three are one Gaussian partition function with different slope-coupling K — the
covariate enters the GC network exactly like μ and d, and the only new physics in
`exchangeable` is one extra spring layer (the slope-dispersion fugacity σ_B²).

Implemented in `Data.Meta.NMA.nmaRegressionPosteriorB` (`BetaModel = SharedB | UnrelatedB
| ExchangeableB`), returning per-treatment β, the common slope, τ², and (exchangeable) σ_B².

## Validation (5-treatment multi-arm network; `test/nmr_beta_models.hs`)

True model is *shared* β=0.30, so the 4 slopes are equal; the single dataset's shared
estimate is ≈0.235 (sampling noise).

**shared** — common B = 0.235 [0.093, 0.377]; matches netmetareg/gemtc (earlier work).

**unrelated** — independent per-treatment slopes; **matches netmetareg
`assumption="independent"` exactly** (point estimates identical to 3 dp):

| slope | spring (mean ± sd) | netmetareg \|coef\| (se) |
|---|---|---|
| β_B | 0.154 ± 0.126 | 0.154 (0.118) |
| β_C | 0.290 ± 0.138 | 0.290 (0.130) |
| β_D | 0.212 ± 0.129 | 0.212 (0.121) |
| β_E | 0.264 ± 0.089 | 0.264 (0.084) |

(spring SDs slightly wider — they integrate τ² uncertainty; netmetareg's are Wald, fixed at τ̂².)

**exchangeable** — slopes shrink toward the common mean (partial pooling); vs gemtc
`coefficient="exchangeable"`:

| quantity | spring | gemtc |
|---|---|---|
| common B | 0.230 | 0.226 |
| β_B,β_C,β_D,β_E | 0.174, 0.272, 0.213, 0.259 | 0.203, 0.246, 0.220, 0.244 |
| τ² | 0.068 | 0.059 |
| σ_B² | 0.045 | 0.004 |

B, τ², and the β_t **pattern** agree; the **degree of shrinkage differs** because σ_B²
disagrees (0.045 vs 0.004). That is honest and expected: with only 4 truly-equal slopes
σ_B² is weakly identified and **prior-dominated**, and the slope-dispersion priors were
not matched (spring used half-normal(0.5) on the slope SD; gemtc used its default
`reg.sd` prior). Matching that prior would close the gap; shared (σ_B²→0) and unrelated
(σ_B²→∞) are the validated endpoints.
