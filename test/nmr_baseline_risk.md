# Baseline-risk network meta-regression (Achana/bnma model)

The one MetaInsight feature meta-springs lacked. **MetaInsight does baseline-risk
regression with `bnma` (Seo & Schmid, implementing Achana et al. 2013), NOT gemtc** —
gemtc's `mtc.model(type="regression")` only takes *external* study-level covariates.

## The model

Binary outcomes, logit link, study *i*, arm of treatment *t* (control = reference):

```
logit(p_ik) = η_i + d_t + B·(η_i − mx),   reference slope B_ref = 0
```

- **η_i** = the *latent* study baseline (control-arm log-odds), a free model parameter.
- The regressor is **η_i itself**, not the observed control proportion — this is the whole
  point: regressing on the observed baseline induces regression-to-the-mean / attenuation
  because the same control-arm data sit on both sides. Putting the model's η_i in both the
  intercept and the interaction propagates baseline uncertainty correctly (Thompson 1997,
  Achana 2013, NICE TSD3).
- **mx** = mean continuity-corrected observed control log-odds — a **fixed** data constant
  (r→0.5, n→n+1 for zero-event control arms), exactly as bnma computes it. So d_t is the
  contrast at baseline = mx.
- **B** = shared baseline-risk slope ("common" in bnma).

Because the regressor is a parameter, the predictor carries the **bilinear term B·η_i** —
the model is intrinsically nonlinear. In the spring picture the active arm's rod length
now ties to the study's *own* baseline node.

## Implementation (`Data.Meta.NMA`)

`nmaRegressionBaselineRiskBin` (FE) and `nmaRegressionBaselineRiskBinRE` (RE), returning
`BaselineRiskFit {brMx, brB, brBse, brTau2, brD, brDse}`. Solved by **damped Fisher
scoring** (Gauss–Newton) on the Jacobian

```
∂η/∂η_i = 1 + B ,   ∂η/∂d_t = 1 ,   ∂η/∂B = η_i − mx
```

which converges to the exact MLE. The bilinear term makes raw Newton steps overshoot near
`1+B≈0` (where the active arm decouples from its own baseline), so steps are backtracked on
the binomial log-likelihood. SEs use the **observed** information (Laplace curvature at the
mode); the bilinear cross-derivative `∂²η/∂B∂η_i = 1` contributes a residual-weighted term
the expected/Fisher information drops — including it matches R's `optim` Hessian exactly.
RE adds heterogeneity by PQL (τ²/2 per arm in series + BLUP/EM), the baseline-risk analogue
of `nmaRegressionBinRE`.

## Verification

Two references: an **exact MLE** by R `optim` on the identical nonlinear likelihood
(`nmr_baseline_risk_mle.R`), and the **bnma** Bayesian engine MetaInsight uses
(`nmr_baseline_risk_bnma.R`).

**1. Haskell == exact MLE (point estimates AND observed-information SEs).** On both
datasets, to 4–5 dp on every parameter.

certolizumab (12 placebo-controlled RA studies, ACR50; `bnma::certolizumab`), FE:

| param | Haskell | R optim MLE |
|---|---|---|
| B | −0.84936 (se 0.13254) | −0.84936 (se 0.13254) |
| d[CZP] | 1.88201 (se 0.10719) | 1.88201 (se 0.10719) |
| d[Adalimumab] | 2.10416 (se 0.11697) | 2.10416 (se 0.11697) |
| d[Etanercept] | 2.11166 (se 0.33259) | 2.11167 (se 0.33259) |
| d[Infliximab] | 1.65369 (se 0.10114) | 1.65369 (se 0.10114) |
| d[Rituximab] | 0.39966 (se 0.48749) | 0.39966 (se 0.48749) |
| d[Tocilizumab] | 2.15500 (se 0.14745) | 2.15500 (se 0.14745) |

**2. Known-truth simulation** (`nmr_baseline_risk_sim.R`: 48 two-arm studies, n=200/arm,
baseline spread sd 0.7; truth B=−0.60, d=[0.8, 1.2, 0.5]). On a dense, well-identified
network all three methods agree and recover truth:

| | B | d[B] | d[C] | d[D] |
|---|---|---|---|---|
| truth | −0.60 | 0.80 | 1.20 | 0.50 |
| Haskell FE | −0.66091 | 0.82794 | 1.18479 | 0.64630 |
| R optim MLE | −0.6609 | 0.828 | 1.185 | 0.646 |
| **bnma (MetaInsight)** | −0.683 | 0.827 | 1.187 | 0.655 |

Haskell == MLE to 4 dp; bnma ≈ Haskell to ~0.02 (Laplace mode ≈ posterior mean when the
posterior is near-Gaussian). Haskell **RE** recovers **τ²≈0.003** — correctly detecting the
fixed-effect DGP carries no heterogeneity.

**3. bnma on certolizumab** (converged, 4 chains): FE B=−0.943 (sd 0.085),
RE B=−0.964 (sd 0.089), τ (sd) 0.233. The Haskell/MLE FE mode is −0.849. The ~0.1 gap is
the **mode-vs-mean** difference: the slope's posterior is right-skewed on this sparse
12-study network, so bnma's posterior *mean* sits below the MLE *mode*. (Same phenomenon
NMR.md documents for the GC τ² posterior — use the mode to compare to a point estimate.)
On the dense simulation, where the posterior is near-Gaussian, the gap collapses to 0.02.

## Slope variants — shared / unrelated / exchangeable

`nmaRegressionBaselineRiskBinB studies ref model mTau` covers all three bnma slope
structures (`model = BLShared | BLUnrelated | BLExchangeable`):

- **shared** ("common"): one B for all treatments.
- **unrelated** ("independent"): an independent slope b_t per treatment, identified only
  from the studies containing t (a weak prec-1e-4 ridge keeps an under-identified slope
  finite, signalling non-identification through a large SE rather than a singular collapse).
- **exchangeable**: b_t ~ N(B, σ_B²) — a slope-spring of hardness 1/σ_B² tying each b_t to a
  common mean B, with σ_B² estimated by EM (the same construct as the τ heterogeneity
  spring). σ_B²→0 recovers shared; σ_B²→∞ recovers unrelated.

**Validation vs bnma**, simulation (DGP = shared B=−0.60; 3 actives × 16 studies):

| | shared B | b[B] | b[C] | b[D] | σ_B² |
|---|---|---|---|---|---|
| Haskell | −0.661 | −0.641 | −0.725 | −0.651 | — |
| bnma | −0.683 | −0.666 | −0.750 | −0.664 | — |
| Haskell exch | −0.661 (B) | −0.661 | −0.661 | −0.661 | 0.00004 |
| bnma exch | −0.675 (B) | −0.675 | −0.720 | −0.672 | 0.044 (sdB 0.21±0.46) |

Shared and unrelated per-treatment slopes track bnma within ~0.02–0.03 (the Laplace-mode vs
posterior-mean gap). The **exchangeable σ_B²** is the known weakly-identified variance
component: with only 3 groups the REML/EM point collapses to ≈0 (the 3 slopes are nearly
equal), while bnma's posterior *mean* is 0.044 — but its posterior SD (0.46) exceeds the
mean, i.e. consistent with ≈0. The σ_B²→0 (shared) and σ_B²→∞ (unrelated) endpoints are the
exactly-validated cases; this matches what the covariate exchangeable case already documents
(`nmr_gc_reml_simulation.md`). On sparse certolizumab, unrelated flags the two drugs that
appear in a single study (Etanercept, Rituximab) with SE = 100 — a baseline-risk slope is
not identified from one trial — while exchangeable borrows strength (σ_B²=0.0008, all
b_t ≈ −0.85). This is precisely why exchangeable is the useful middle ground.

Run: `stack runghc -- test/nmr_baseline_risk_variants.hs test/nmr_br_sim.json A` ·
`Rscript test/nmr_baseline_risk_variants_bnma.R`.

## 2-D grand canonical: σ_B² "coming and going" like τ²

The grouped-REML EM above *maximises* σ_B², so with few treatments it collapses to the
boundary (σ_B²→0, slopes welded to the shared value). The grand-canonical treatment instead
**integrates** σ_B² — the slope-springs are free, coming and going, exactly like the
τ-springs. `nmaRegressionBaselineRiskBinExchGC` does this as a 2-D ensemble:

```
Ξ(τ², σ_B²) = ∫∫ Z(τ², σ_B²) π(τ²) π(σ_B²) dτ² dσ_B²
log Z = −½[ Σ log(1/w + τ²/2) + n_T·log σ_B² + log|A| + RSS ],
        A = JᵀW_eff J + (1/σ_B²)·(slope-spring ridge)
```

Single-point Laplace: the binomial arms are linearised once at the exchangeable mode (the
bilinear Jacobian frozen), then the (τ²,σ_B²) grid integration is the exact Gaussian
partition function — the `−½ n_T log σ_B²` term is the slope-prior normalisation, and σ_B²
uses a log-spaced grid (same machinery as the covariate GC `nmaRegressionPosteriorB`).

**Result — it lifts σ_B² off the REML boundary** (half-normal priors on τ, σ_B):

| σ_B² | grouped-REML (EM) | 2-D GC mode | 2-D GC mean | bnma posterior |
|---|---|---|---|---|
| **simulation** (DGP shared) | 0.00004 (boundary) | 0.0056 | **0.030** [0.0001, 0.22] | ≈ **0.044** (sdB 0.21) |
| certolizumab | 0.0013 (boundary) | 0.045 | 0.077 [0.0003, 0.45] | sdB 0.53 (non-converged*) |

On the **dense simulation** (the fair test — slopes identified, bnma converged) the integrated
σ_B² mean (0.030) lands in bnma's ballpark (0.044), both lifted well off the 0.00004 boundary;
and the per-treatment slopes match bnma closely with **near-identical SDs**:

| b_t | 2-D GC | bnma |
|---|---|---|
| b[B] | −0.654 (sd 0.054) | −0.675 (sd 0.051) |
| b[C] | −0.691 (sd 0.068) | −0.720 (sd 0.064) |
| b[D] | −0.657 (sd 0.043) | −0.672 (sd 0.041) |

The EM point gives artificially tiny slope SDs (σ_B²→0 welds the slopes to the shared value);
the GC integration inflates them to the correct Bayesian uncertainty. The exact σ_B² is
**prior-sensitive** (weakly identified, as the covariate exchangeable case also documents);
on certolizumab bnma is itself unreliable (*Gelman–Rubin 1.02–1.10: most drugs appear in a
single study, σ_B² prior-dominated). The structural point is the one the spring picture
demands: **the β slope-springs are now treated identically to the τ-springs — both variance
components integrated, not maximised.** Run:
`stack runghc -- test/nmr_baseline_risk_exch_gc.hs test/nmr_br_sim.json A`.

## Continuous (mean-difference, identity link)

`nmaRegressionBaselineRiskMD` / `nmaRegressionBaselineRiskMDB` are the identity-link analogue
(`bnma` response="normal"): `y_ik = η_i + d_t + B·(η_i − mx) + ε`, η_i = latent control-arm
mean, mx = mean observed control mean. The regressor is still the parameter η_i, so the
bilinear `B·η_i` term remains — unlike *external*-covariate continuous NMR, which is linear —
and it is solved by the same damped Gauss–Newton (now with Gaussian weight 1/v and working
residual y−μ). All slope structures + FE/RE carry over unchanged.

**Validation**, known-truth simulation (`nmr_baseline_risk_md_sim.R`: 48 two-arm studies,
n=100/arm; truth B=−0.40, d=[2.0, 3.0, 1.0]):

| | B | d[B] | d[C] | d[D] |
|---|---|---|---|---|
| truth | −0.40 | 2.00 | 3.00 | 1.00 |
| Haskell FE | −0.37121 (se .0310) | 2.15914 (.150) | 2.86910 (.148) | 1.04644 (.150) |
| R WLS MLE | −0.3712 (se .0310) | 2.159 (.150) | 2.869 (.148) | 1.046 (.150) |
| bnma (normal) | −0.3888 | 2.145 | 2.870 | 1.060 |

Haskell == exact weighted-LS MLE to 5 dp (point estimates **and** observed-information SEs);
≈ bnma within the mode-vs-mean gap; all recover truth. Unrelated/exchangeable and RE behave as
in the binary case (here σ_B²=0.001, τ²=0.023). Run:
`stack runghc -- test/nmr_baseline_risk_md.hs test/nmr_br_md_sim.json A`.

## The honest spring representation (`nmr_baseline_risk_spring_verify.hs`)

Baseline-risk NMR **is** a spring system — and a *new kind* of one. Coordinates are the
study baselines η_i, the contrasts d_t, and the slope B; each arm is a `BinomialSpring`
whose length is the logit linear predictor. The library's own primitives make the
identification exact, not analogical:

```
springEnergy (BinomialSpring e n L) = n·log(1+e^L) − e·L   =  the negative log-likelihood
springForce  (BinomialSpring e n L) = e − n·sigmoid(L)     =  the score
```

so minimum energy = MLE and **net force = 0 is the MLE**. Verified on certolizumab (19
coords, 24 springs) and the simulation (52 coords, 96 springs):

1. **Force-balance equilibrium.** `max |net force| = 3e-12` (certolizumab), `1e-11` (sim) —
   the MLE is the equilibrium of the explicit binomial spring network.
2. **The rod is self-referential and coupled.** For an active arm the geometry is
   `∂L/∂η_i = 1 + B` (the spring's grip on its *own* baseline node depends on B, not 1) and
   `∂L/∂B = η_i − mx` (the rod *length* depends on the baseline node's position). Contrast
   external-covariate NMR, where the rod is a fixed length `β·x_i`. This coupling is the
   bilinear term; it is why the equilibrium is found by iterated relinearisation (Fisher
   scoring) rather than a single linear solve.
3. **Shared baseline node = randomization, made explicit.** Both arms of a study attach to
   *one* coordinate η_i. The treated arm's length is `η_i + d_t + B·(η_i − mx)` — the *same*
   η_i it would have had untreated (the control), which is exactly what randomization
   guarantees. The spring picture makes the model's blinding/randomization assumption
   visible as a single shared node.
4. **Equilibrium == library** to 6 dp on every d_t and B (`nmaRegressionBaselineRiskBin`).
5. **τ² posterior = spring partition function.** Linearising to working springs at the mode
   and adding τ-springs on per-arm δ, `logZ(τ²) = −½[Σ log(1/w+τ²/2) + logdet A + RSS]`
   (the spring free energy); its mode tracks the PQL point (cert 0.026 vs 0.034; sim 0.002
   vs 0.003, ≈0 for the fixed-effect DGP), δ integrated out. Same single-Laplace caveat as
   the binary GC.

This is the baseline-risk analogue of `nmr_gc_spring_verify.hs`, extended to the binary,
self-referentially-coupled case.

## Honest caveats

- **Binary baseline-risk is single-point Laplace** (Fisher-scoring MLE + observed-info
  covariance), not exact-binomial MCMC. Point estimates are the exact MLE; the posterior
  *shape* is Gaussian-approximated. This matches the existing binary-GC treatment in
  meta-springs.
- Implemented: **shared / unrelated / exchangeable slope, FE + RE, binary (OR) and
  continuous (MD)** — the full MetaInsight baseline-risk grid.
- Exchangeable **σ_B²** is weakly identified with few treatments (prior-sensitive, REML-point
  vs posterior-mean); the shared (σ_B²→0) and unrelated (σ_B²→∞) endpoints are the exactly
  validated cases. Unrelated slopes for treatments appearing in a single study are not
  identified (reported with a very large SE).
- The RE convention is meta-springs' arm-level τ²/2 (contrast-scale τ²); bnma puts the
  random effect on the contrast directly. Equivalent for 2-arm studies; τ² compares on the
  contrast scale.

## Reproduce

```
Rscript test/nmr_baseline_risk_mle.R          # exact MLE + export certolizumab JSON
Rscript test/nmr_baseline_risk_bnma.R         # bnma Bayesian reference (the MetaInsight engine)
Rscript test/nmr_baseline_risk_sim.R          # known-truth simulation (MLE + bnma)
stack runghc -- test/nmr_baseline_risk.hs test/nmr_certolizumab.json Placebo
stack runghc -- test/nmr_baseline_risk.hs test/nmr_br_sim.json A
stack runghc -- test/nmr_baseline_risk_spring_verify.hs test/nmr_certolizumab.json Placebo
```
