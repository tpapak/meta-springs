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

With gemtc's prior matched (`reg.sd ~ Uniform(0, om.scale)` ⇒ on the raw scale
`σ_B ~ Uniform(0, om.scale/scale)` = U(0, 0.729), `UniformSD 0.7293`) and a **log-spaced
σ_B² grid** (a coarse linear grid floors above gemtc's near-zero posterior), the two agree
across the board (both σ_B² summaries are posterior **means**):

| quantity | spring | gemtc |
|---|---|---|
| common B | 0.231 | 0.222 |
| β_B, β_C, β_D, β_E | 0.200, 0.255, 0.221, 0.249 | 0.203, 0.246, 0.220, 0.244 |
| τ² | 0.0655 | 0.0658 |
| σ_B² (mean) | 0.0239 [0.0001, 0.189] | 0.0220 [0.000, 0.175] |

The first-pass gap ("0.045 vs 0.004") was three stacked artifacts: (i) unmatched
slope-dispersion prior, (ii) a coarse σ_B² grid whose floor sat *above* gemtc's posterior
mass (σ_B² is weakly identified here — true value 0 — so the mass piles near zero), and
(iii) comparing the spring posterior *mean* to gemtc's *median²*. Fixing all three closes
it. shared (σ_B²→0) and unrelated (σ_B²→∞) remain the exactly-validated endpoints.

## Note: τ springs *are* exchangeability
Heterogeneity and exchangeable coefficients are the **same construct** in the spring/GC
picture — a Gaussian spring tying coordinates to a shared node (= a variance component):
- the **τ spring** (hardness 2/τ², StudyEnd↔TauEnd) makes the study-specific effects
  θ_i = d + δ_i exchangeable around the network mean d (variance τ²) — the standard RE model;
- the **slope spring** (hardness 1/σ_B², β_t↔B) makes the per-treatment slopes β_t
  exchangeable around B (variance σ_B²).

So `exchangeable` β is just the heterogeneity mechanism applied to the slope coordinates,
with σ_B² a *second fugacity* the GC integrates over alongside τ². shared = infinitely
stiff slope springs (σ_B²→0, β_t pinned to B); unrelated = no slope springs (σ_B²→∞).

**Folded into the tau-group machinery** (`nmaRegressionGroupedREML`,
`test/nmr_grouped_reml.hs`): σ_B² is estimated as a *second tau-group* alongside the
heterogeneity τ², in one grouped EM that returns `tau2s :: Map TauGroup TauSquare`
(`0 ↦ τ²`, `1 ↦ σ_B²`) — the same `Map TauGroup TauSquare` representation `makeSprings`
already carries. The slope-springs β_t↔B are tau edges of group 1, exactly like the
StudyEnd↔TauEnd heterogeneity edges are group 0. (The covariate **lever** β_t·x_i is a
non-±1 incidence, so the solve is the dense Gaussian one, not the spanning-tree `nma`.)
On the 5-treatment network: `tau2s = {0 ↦ 0.0545, 1 ↦ 0.0000}` — the REML point estimate
of σ_B² is **0** (the 4 true slopes are equal), so the slope-springs go rigid and β_t
collapse to B = 0.235 = the shared estimate (the σ_B²→0 limit). That is the slope analog
of REML τ²=0; gemtc's σ_B² *mode* is likewise ≈0 (its 0.022 is the right-skewed posterior
*mean*).

**Wired explicitly** (`test/nmr_exchangeable_springs.hs`): the whole exchangeable-β model
is built as ONE spring list over (μ_i, d_t, β_t, B, δ_a) where every entry is
`½·k·(Σ coef·coord − ℓ₀)²` — obs springs (k=1/v), heterogeneity τ-springs (k=2/τ², δ_a→0),
and slope-springs (k=1/σ_B², β_t→B). The τ-spring and the slope-spring are byte-for-byte
the same object. Assembling `A=Σ k·g·gᵀ`, `b=Σ k·ℓ₀·g` and solving reproduces the marginal
(δ-integrated) solve to machine precision (max|Δ| ≈ 1e-15). Diagram:
`test/nmr_exchangeable_springs.html`.
