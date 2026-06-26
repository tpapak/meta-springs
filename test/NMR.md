# Network meta-regression in meta-springs — master summary

Spring-network **network meta-regression (NMR)**: a study-level covariate's effect β on
treatment contrasts, plus heterogeneity τ², built on the `makeSprings` golden-standard
spring model and its **Grand-Canonical** (GC) extension. Implemented in **Haskell**
(`Data.Meta.NMA`) and **Rust** (`gc-rust/examples/nmr_*`), verified against
**netmeta/netmetareg**, **metafor**, **gemtc**, and **multinma**.

---

## 1. The model

Arm-level, study *i*, arm of treatment *t* (reference pinned, `d_ref = 0`):

```
y_it = μ_i + d_t + [t≠ref]·β_t·x_i + u_it + ε_it ,   u_it ~ N(0, τ²/2),  ε_it ~ N(0, v_it)
```

- **μ_i** free study baselines, **d_t** treatment contrasts, **β** the covariate slope,
  **u_it** the per-arm random effect (heterogeneity, contrast-scale τ²; arm-scale τ²/2).
- The covariate shifts the treatment end of non-reference arms by `β·x_i` (the
  παραλληλόγραφος / parallel-rule). β is identified only from reference-involving contrasts
  (it cancels in active-vs-active comparisons).

**β structures** (cf. gemtc / MetaInsight): one Gaussian partition function, three slope
couplings —

| structure | slopes | coupling |
|---|---|---|
| **shared** | one β common to all treatments | none (σ_B²→0 limit) |
| **unrelated** | independent β_t | none, flat (σ_B²→∞ limit) |
| **exchangeable** | β_t ~ N(B, σ_B²) | slope-spring β_t↔B, hardness 1/σ_B² |

**Grand-Canonical.** At heterogeneity τ² (and exchangeable σ_B²), the partition function
is the Gaussian integral over the free coordinates (μ, d, β):
`log Z = −½[ Σ log(v+τ²/2) + log|XᵀΣ⁻¹X| + RSS ]`. The τ² (and σ_B²) posterior is
`∝ Z·prior`; β's posterior is a τ²-mixture of Gaussians — **β integrated, never profiled**.

**Key unification — τ springs *are* exchangeability.** Heterogeneity (the τ-spring tying
δ_i to the network mean) and exchangeable coefficients (the slope-spring tying β_t to B)
are the *same* construct — a Gaussian spring to a shared node = a variance component the
GC integrates over. shared / unrelated are the σ_B²→0 / →∞ stiffness limits.
(`nmr_gc_beta_model.md`, `nmr_exchangeable_springs.{hs,html}`.)

---

## 2. Function inventory

**Haskell** (`Data.Meta.NMA`):

| | continuous | binary (logit) |
|---|---|---|
| FE shared β | `nmaRegression` | `nmaRegressionBin` |
| RE shared β | `nmaRegressionRE` | `nmaRegressionBinRE` |
| GC joint posterior (shared/unrel/exch) | `nmaRegressionPosterior(B)` | `nmaRegressionBinPosteriorB` |
| exchangeable as tau-group (point) | `nmaRegressionGroupedREML` | `nmaRegressionBinGroupedREML` |
| **baseline-risk** regression (shared/unrel/exch, FE+RE) | `nmaRegressionBaselineRiskMD(B)` | `nmaRegressionBaselineRiskBin(RE/B)` |

`Tau2PriorR = FlatT2 | HalfNormalTauR σ | UniformSD u`. `linearizeBinaryRegression` is the
IRLS bridge (binomial → Gaussian working response) that lets every continuous function
serve binary.

**Rust** (`gc-rust/examples/`): `nmr_fe.rs` (FE), `nmr_re_gc.rs` (RE + GC, shared/exch),
`nmr_bin.rs` (binary RE+GC), `nmr_bin_exch.rs` (exchangeable binary GC),
`nmr_post_dump.rs` (posterior densities → JSON), **`nmr_brisk.rs` (baseline-risk Fisher
scoring) and `nmr_brisk_gc.rs` (baseline-risk 2-D GC Ξ(τ²,σ_B²))**. nalgebra (Cholesky).
**Covers the full grid: FE/RE/GC × continuous/binary × shared/exchangeable, incl. baseline-risk.**

---

## 3. Verification

**vs frequentist REML** — `nmaRegressionRE` == netmeta `netmetareg` (REML) **exactly** over
200 reps (β and τ²); `nmaRegressionPosterior` mode == netmetareg; unrelated β ==
`netmetareg(assumption="independent")` exactly; β/intercept == metafor `rma(REML)`.

**vs gemtc** (exact-Bayesian) — GC posterior **mean** matches gemtc's posterior mean,
GC **mode** matches the REML point. Exchangeable (matched prior): B, β_t, τ², σ_B² all agree
(σ_B² 0.024 vs 0.022). Honest Monte-Carlo (200 reps): GC-mode/REML/spring-REML all
≈unbiased for τ²; GC-mean (flat prior) ~30% high by right-skew → use the mode.
(`nmr_gc_reml_simulation.md`.)

**multinma** — unreliable on **aggregate-only** data (β swings −1.5…+4.7, τ² 2–4× high):
its regression is identified from IPD/ML-NMR, not study-level means. Documented, not a bug.

**Rust == Haskell** — to 4–5 dp on every path (RE β 0.40375/τ² 0.10351; GC β mean 0.40375,
95% [0.2748,0.5327]; binary RE β 0.25637/τ² 0.08288; exch binary β_t identical).

**vs bnma** (baseline-risk, the MetaInsight engine) — `nmaRegressionBaselineRiskBin`
== exact MLE (R `optim`, identical point estimates *and* observed-info SEs to 4–5 dp) on
both certolizumab and a known-truth simulation; ≈ bnma to ~0.02 on a dense network (both
recover truth), with the expected mode-vs-mean gap (~0.1) on sparse certolizumab's skewed
slope posterior. RE recovers τ²≈0 on a fixed-effect DGP. **Exchangeable: a 2-D grand-canonical
Ξ(τ²,σ_B²)** (`nmaRegressionBaselineRiskBinExchGC`) integrates the slope dispersion σ_B² — the
β slope-springs "coming and going" like the τ-springs, not maximised — lifting σ_B² off the
REML boundary (sim 0.00004→0.030, ≈ bnma 0.044) and matching bnma's per-treatment slopes and
SDs. (`nmr_baseline_risk.md`.)

**Monte-Carlo verification** (`nmr_baseline_risk_montecarlo.md`) — 200 reps, FE shared: the
slope **B is unbiased (bias −0.002), 96% coverage**, observed-info SE = empirical sd, 200/200
converged; d_t carry the usual small finite-sample logistic-MLE bias. Exchangeable σ_B² (80
reps × 2 regimes, true σ_B²=0.06): σ_B² point estimation is weakly identified (EM unbiased-on-
average but collapses to the boundary 9%/35% of reps; GC never collapses but prior-biased high;
neither wins on point RMSE). **But the GC wins decisively on slope-SD calibration** — true-b_t
95% coverage **0.94/0.97 (GC) vs 0.87/0.82 (EM point)** over 600 slope estimates: the EM collapse
welds the slopes with false-precision SEs, the GC propagates σ_B² uncertainty into calibrated
slope intervals. **Rust**
`nmr_brisk` == Haskell == exact MLE to 5 dp (point + SE), 0.16 ms (cert) / 0.29 ms (sim,
np=52) vs ~30–60 s Haskell; `nmr_brisk_gc` matches the Haskell GC and runs in 4 ms (vs ~30 s).

**We solve the GC *spring* system** (`nmr_gc_spring_verify.hs`): the GC solution is the
force-balance **equilibrium** of the explicit spring network (μ,d,β,δ-per-arm),
max|net force| = 1.8e-12; δ-integration is exact (β identical); the τ² posterior is the
normalized spring **partition function Z(τ²)**.

**Baseline-risk IS the spring system too** (`nmr_baseline_risk_spring_verify.hs`): a binomial
spring network (η_i, d, B) whose `springEnergy = −logLik` and `springForce = score`, so its
force-balance equilibrium (max|net force| ≈ 3e-12) is the MLE, == the library to 6 dp. Its
new feature: a **self-referential coupled rod** — the active-arm spring grips its *own*
baseline node (`∂L/∂η_i = 1+B`) with length tied to that node (`∂L/∂B = η_i−mx`), and both
arms share one η_i node = **randomization made explicit**. (`nmr_baseline_risk.md`.)

---

## 4. Performance (T=20: 76 studies, 152 arms, np=96)

| task | Rust | Haskell | gemtc |
|---|---|---|---|
| FE shared β | 0.12 ms | 427 ms | 4140 ms |
| RE shared β | 98 ms | >3 min (impractical) | 5710 ms |
| GC joint posterior | 62 ms | (slow) | 5710 ms (MCMC) |

Rust ~35,000× faster than gemtc (FE), ~58× (RE); estimates agree, τ² matches gemtc exactly.
Haskell's naive `[[Double]]` solver is fine for small networks but impractical at np≈100 —
Rust/nalgebra is the performant path. (`nmr_benchmark.md`.)

---

## 5. Visualizations (served at `macas.lan:8755`)

- `test/nmr_posteriors.html` — spring GC vs gemtc vs netmetareg, β & τ² posteriors;
  3-trt, 5-trt multi-arm, and binary panels.
- `test/nmr_posteriors_rust.html` — **Rust** GC posterior vs Haskell spring vs gemtc
  (curves superimposed).
- `test/nmr_continuous_parallelograph.html` — continuous covariate as rods + springs;
  Pooled / Shared / **Exchangeable (σ_B² slope-spring slider)** / Unrelated.
- `test/nmr_exchangeable_springs.html` — τ-spring (heterogeneity) and slope-spring
  (exchangeable β) drawn identically: same construct.
- `test/nmr_haskell_network_asis.html` — the verbatim `makeSprings` network + β extension.

---

## 6. MetaInsight (gemtc) parity

| axis | MetaInsight | meta-springs |
|---|---|---|
| β shared / unrelated / exchangeable | ✓ | ✓ |
| RE (single τ²) / FE | ✓ | ✓ |
| continuous (MD) / binary (OR/RR) | ✓ | ✓ |
| covariate regression | ✓ | ✓ |
| baseline-risk regression | ✓ (bnma) | ✓ (shared/unrel/exch β, FE+RE, binary OR + continuous MD; Laplace) |
| 2·SD covariate scaling | ✓ (auto) | by config (prior-matchable) |

Plus meta-springs adds a **closed-form joint (β, τ²) posterior** (no MCMC) and the unifying
spring/GC picture. **Baseline-risk** regression now closed (shared β, FE+RE, binary):
Achana/bnma model `logit(p)=η_i+d_t+B·(η_i−mx)` regressing on the *latent* baseline η_i,
solved by damped Fisher scoring = exact MLE, validated == R `optim` MLE and ≈ bnma — the
actual MetaInsight engine. shared/unrelated/exchangeable slopes, binary (OR) **and continuous
(MD)**, FE+RE — the full MetaInsight baseline-risk grid, all bnma-checked
(`nmr_baseline_risk.md`). **Baseline-risk is now closed.**

---

## 7. Honest caveats

- **Binary GC is single-point Laplace** (IRLS-Gaussian at the mode), not exact-binomial MCMC;
  point estimates exact-MLE/REML, posterior *shape* approximate. τ² mildly high (PQL).
- σ_B² is weakly identified → prior-sensitive; matched to gemtc's `om.scale` bound for
  comparison. shared/unrelated (σ_B²→0/∞) are the exactly-validated endpoints.
- Validation DGP = the spring model (best case); not a misspecification stress test.
- GC mode is grid-quantized in Rust (parabolic-refined in the shared Haskell path).
- A multi-rep T=20 **coverage** study was started; netmetareg returned NA at 20 treatments
  (frequentist regression limitation), so it's pending as Rust-vs-gemtc.

---

## 8. Reproduce

```
# Haskell (stack):    stack runghc -- test/nmr_<...>.hs
# Rust:               cd gc-rust && cargo run --release --example nmr_re_gc -- data.json cov.json [exch]
# References (R):     Rscript test/nmr_*_gen.R / *_ref.R / *_netmetareg.R   (needs netmeta, metafor, gemtc, multinma)
# Spring-system check: stack runghc -- test/nmr_gc_spring_verify.hs
# Baseline-risk:      Rscript test/nmr_baseline_risk_{mle,bnma,sim}.R  (needs bnma = the MetaInsight engine)
#                     stack runghc -- test/nmr_baseline_risk.hs test/nmr_certolizumab.json Placebo
```

Detailed write-ups: `nmr_gc_beta_model.md`, `nmr_gc_reml_simulation.md`,
`nmr_joint_posterior.md`, `nmr_re_verification.md`, `nmr_benchmark.md`,
`nmr_haskell_arm_spec.md`, `nmr_baseline_risk.md`.

Detailed write-ups: `nmr_gc_beta_model.md`, `nmr_gc_reml_simulation.md`,
`nmr_joint_posterior.md`, `nmr_re_verification.md`, `nmr_benchmark.md`,
`nmr_haskell_arm_spec.md`, `nmr_baseline_risk.md`.
