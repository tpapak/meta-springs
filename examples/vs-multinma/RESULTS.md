# meta-springs vs multinma: summary of results

Run on 2026-11-13 on an Apple M4 Pro, with multinma 0.9.1 and rstan 2.32.7. The full tables are in `results/*.md` and `results/*.csv`.

## 1. Time: the same posterior, 20–60× faster (`results/01_speed.md`)

All fits are random-effects NMA with τ ~ HN(1). multinma ran 4 chains × 2000 iterations on 4 cores in parallel; springs ran single-threaded.

| network | studies / trts | springs (R) | multinma | speed-up | ESS/s springs : multinma | max \|Δmean\|/SD |
|---|---|---|---|---|---|---|
| smoking | 24 / 4 | 0.03 s | 1.8 s | 60× | 133 000 : 620 | 0.16 |
| thrombolytics | 50 / 9 | 0.07 s | 3.3 s | 48× | 58 000 : 75 | 0.10 |
| atrial fibrillation | 26 / 17 | 0.08 s | 3.5 s | 46× | 53 000 : 172 | 0.34 |
| parkinsons (continuous) | 7 / 5 | 0.02 s | 0.8 s | 34× | 167 000 : 1 570 | 0.05 |
| nmadb_479969 | 95 / 21 | 0.18 s | 7.5 s | 41× | 22 000 : 147 | 0.36 |
| nmadb_501194 | 99 / 17 | 0.75 s | 15.8 s | 21× | 5 300 : 105 | 0.15 |
| nmadb_501232 | 111 / 12 | 0.20 s | 5.3 s | 27× | 20 000 : 69 | 0.16 |

* **Wall-time speed-up is 20–60×. Per effective draw it is 50–770×.** springs draws are iid; multinma's minimum bulk ESS was 70–1 600.
* **Agreement is typically within 0.1–0.2 posterior SD.** The ratio of posterior SDs is 0.9–1.05.
  * The worst cases, |z| ≈ 0.35 with SDs 0.80–0.88 of multinma's, are comparisons informed by one sparse trial (AF: LASAF, 1/90 events). The binomial Laplace step is weakest there.
* On a random sample of 12 nmadb networks we did not choose (Example 3), the median |z| is 0.01–0.07 and the max is 0.44.

## 2. Prior examination: 15 priors for the price of one fit (`results/02_priors.md`)

| | springs | multinma |
|---|---|---|
| smoking, 15 τ priors | **0.017 s** (one pass) | 19.6 s (13 refits) |
| thrombolytics, 15 τ priors | **0.067 s** | 39.8 s |
| U(0, U) prior on τ (Turner, gemtc default) | yes | not available |
| Turner log-normal priors on τ² | yes (new `ln:`) | yes |

* **τ posterior medians agree with multinma to ≈ 0.01 for every prior.** On thrombolytics, where τ ≈ 0.07, multinma's own run-to-run MCMC noise is larger than the gap (e.g. HN(5) gives 0.071 but HN(1) gives 0.083).
* **springs reproduces Turner et al. (2019) Table 4 (smoking, approach 1)** to within about 0.02–0.04 on the effects. τ matches exactly: 0.82 (0.55, 1.27) under U(0,2) and 0.73 (0.50, 1.10) vs 0.73 (0.50, 1.09) under LN(−2.75, 1.61²). Neither column takes more than 11 ms.
* The panel shows how the prior matters in practice:
  * Smoking: τ ranges from 0.67 (Turner mortality priors) to 0.81 (vague priors).
  * Thrombolytics: τ almost doubles under the Turner subjective prior (0.14 vs 0.07), but the effects barely move.

## 3. Database-wide sweep (`results/03_sweep.md`)

* **springs fitted 171 binary nmadb networks × 15 priors, 2 565 posteriors, in 16 s.**
* multinma's measured per-fit time (mean 2.2 s) extrapolates to **about 1.3 h** for the 13 priors it can express. That is single-prior runs with no ESS targeting, so it is a lower bound.
* **45% of networks** (77/171) have at least one basic comparison whose 95% CrI excludes 0 under some priors and not others. Among the 10 vague priors alone it is 34%. In total 119 of 1 062 comparisons flip.
  * **Exact recount (Example 3b, `results/03b_sweep_exact.md`).** Interval ends come from the τ²-mixture CDF, with no draws: **71/171 networks (42%)**, 111 of 1 062 comparisons.
    * With a strict margin (the interval crosses 0 by ≥ 0.10 posterior SD on both sides): 47 networks (28%). Vague priors only: 29% (14% strict).
    * Draw noise is real: two seeds give 77 and 80 networks, and 7 networks flip in and out with the seed.
    * An MCMC sweep at the ESS multinma reaches (100–1 000) would flag 49–66% of networks, inflated by Monte Carlo error that only longer runs can shrink.
* The max/min ratio of posterior-median τ across priors has a median of 1.85 and a 90th percentile of 3.2.
* Under HN(0.5), the RE-vs-FE Bayes factor favours RE (log BF > 1) in 70 networks and FE (log BF < −1) in 21, and is inconclusive in 80.

## 4. Models multinma does not offer (`results/04_models.md`)

* **RE vs FE Bayes factors / posterior model probabilities come straight from the partition function.**
  * They are validated against bayesmeta's numerical integration on Crins 2014, matching to ≤ 0.002 on the log scale for all 6 priors.
  * springs takes 7 ms for all 6 priors; bayesmeta takes 4.9 s.
  * Applied results: smoking log BF ≈ 85 (RE certain); thrombolytics P(RE) = 0.10–0.33 depending on the prior; AF P(RE) = 0.52–0.70.
  * multinma gives DIC, which needs two fits per prior and is not a posterior model probability.
* **Baseline-risk NMR** (a regression on the latent control-arm risk, as in bnma) runs on certolizumab with common, independent and exchangeable slopes, RE, in 0.07–0.26 s. Exchangeable slopes integrate σ_B² over a 2-D grid.
  * Example: CZP slope −0.88 (−1.43, −0.32), exchangeable.
  * multinma regresses only on observed covariates.

## 5. IPD + AgD: ML-NMR (`results/05_mlnmr.md`)

This refits the multinma plaque psoriasis vignette (Phillippo et al. 2020): 3 854 IPD patients (UNCOVER-1/2/3) plus FIXTURE as AgD. It uses a probit link, 5 covariates with class-common interactions, the `bernoulli2` likelihood, and 64 integration points.

| | springs | multinma | agreement |
|---|---|---|---|
| IPD-only, flat priors | MLE = `glm()` to 5e-8 (log-lik identical) | — | exact |
| FE ML-NMR | **0.72 s** | 30 s | d, β: max \|z\| 0.03; SD ratio 0.98–1.02 |
| RE ML-NMR (τ ~ HN(2.5)) | **1.5 s, with 8 τ priors** | 51 s per prior | d, β, τ: max \|z\| 0.04; τ 0.192 vs 0.188 |
| population-specific relative effects (20) | | | max \|z\| 0.04 |
| population-average predictions (24) | | | max \|z\| 0.07 |
| RE refit under HN(0.5), spot check | from the same grid | 51 s | max \|z\| 0.05 |

* **Pure Laplace was not enough here.** Placebo PASI 75 is about 5%, in the probit tail, which skews the posterior of the baselines μ. The mode (Laplace centre) was about 0.2 SD off multinma's posterior mean, even on IPD alone, where the mode equals glm's MLE exactly.
  * The importance correction (Laplace solution → multivariate-t proposal → reweighted by the exact energy) brings every quantity to |z| ≤ 0.07, at ESS about 9 000 / 16 000 (FE).
  * The same correction applies directly to the binary aggregate NMA (limitation 1 below). It has not been applied there yet.
* With only 4 studies, τ is weakly identified, so the prior matters for τ (posterior mean 0.14–0.19 across priors) but hardly at all for the effects.
  * The RE-vs-FE Bayes factor ranges from −1.6 (HN(2.5)) to +0.6 (Turner mortality prior), so it is inconclusive. That is consistent with multinma's DIC (3151.6 vs 3153.5).

## 6. Prior investigation for ML-NMR, from one fit (`results/06_mlnmr_priors.md`)

| | springs | multinma |
|---|---|---|
| FE: 11 priors on effect modifiers / effects / baselines (Normal, Cauchy, t3, sceptical, informative) | **0.9 s fit + 2.3 s for all**; a reweighting is 43 ms | 29 s per prior (≈ 5 min) |
| RE: 4 interaction priors × 8 τ priors = 32 posteriors | **2.9 s fit + 6.4 s for all** | 49 s per prior (≈ 26 min) |
| agreement with multinma refits (6 priors, incl. Cauchy and two auto-refits) | max \|z\| 0.03–0.07, SD ratio 0.96–1.05 | |

* **Why it is possible.**
  * The energy is data springs plus prior springs plus RE springs. Only the data springs are expensive, and they do not depend on the prior.
  * The fit keeps its weighted importance pool, so a new prior reweights each draw by π_new/π_old. τ priors reweight the τ² grid, and both kinds of change combine into one weight.
  * The ESS flags priors that move the posterior beyond the pool. Here those were N(0, 0.1²) on the interactions and N(0, 0.5²) on effects of size ≈ 3; they are refitted automatically in < 1 s.
  * Every reweighted posterior matches a direct refit to within the 4 000-draw Monte Carlo floor (max |z| ≤ 0.045).
* **What the priors change.**
  * Effects in the average population are prior-robust; only a strongly sceptical N(0, 0.5²) effect prior pulls IXE from 2.96 to 2.77.
  * Effect-modifier priors matter for transport. In a severe target population (25 y disease, 60% BSA, 120 kg, PsA, prior systemic), the SD of the effects shrinks from 0.24 to 0.14 as the interaction prior tightens.
  * The prior share (prior stiffness / posterior stiffness, no refit needed) flags the BSA interactions in advance: a sceptical prior holds about ⅓ of them.
  * τ is prior-driven (0.14–0.19); the effects are not.

## 7. Federated ML-NMR: no pooled data, no coordinator (`results/07_federated.md`)

Each psoriasis trial is a separate site: UNCOVER-1/2/3 hold IPD, FIXTURE holds AgD. Each site evaluates only its own data springs (`gc-rust/src/bin/mlnmr_site.rs`). Sites share only sums, through a ring secure sum with random masks, so no one sees a single site's numbers.

| step | what each site sends | result |
|---|---|---|
| 0. covariate centring | Σw·x, Σw | = pooled to 3e-13 |
| 1. force rounds (7) | log-lik, force (24), stiffness (24×24) at the current θ | equilibrium = pooled springs fit to **1.3e-11**; Laplace only: max \|z\| 0.24 vs multinma |
| 2. one energy round | log-lik at 16 000 configurations regenerated from a shared seed | exact posterior: vs multinma (pooled data) max \|z\| **0.029**, SD ratio 0.98–1.02; vs pooled springs 0.019 |
| 3. priors | nothing | each site reweights with its own prior (sceptical, Cauchy, …): vs a pooled fit under the same prior, max \|z\| ≤ 0.045 |

* **Data exchanged:** 20 213 numbers per site and no patient records. The run takes about 10 s.
* **The spring argument:** forces, stiffnesses and energies add. The summed stiffness is each site's *equivalent spring*. It is exact for linear-Gaussian models but only quadratic for binary data (hence the 0.24). One round of energy evaluations restores exactness.
* **Scope:** fixed effects only. Random effects need the same rounds at each τ² grid point; this has not been run.

## 8. The full psoriasis network: 4 IPD + 5 AgD trials (`results/08_full_network.md`)

The network has 4 IPD trials (UNCOVER-1/2/3 and IXORA-S, 4 114 patients) and 5 AgD trials (CLEAR, ERASURE, FEATURE, FIXTURE, JUNCTURE). There are 7 treatments in 3 classes, including UST (IL-12/23). One placebo arm has 0/59 responders. Two studies (CLEAR, IXORA-S) have no placebo arm.

| | springs | multinma | agreement (max \|z\|, median) |
|---|---|---|---|
| FE | **1.1 s** | 33 s | d, β 0.054 (0.020); 54 population-specific effects 0.045; 63 predictions 0.120 |
| RE (8 τ priors) | **4.1 s** | 65 s per prior | d, β, τ 0.070 (0.012); effects 0.056; predictions 0.070; τ 0.120 vs 0.117 |
| prior panel: 8 FE + 32 RE posteriors | **+1.5 s, +9.0 s** | ≈ 39 min | multinma refit check (β ~ N(0, 0.5²)) 0.074 |
| federated, 9 sites | 7 force rounds + 1 energy round, 8 s, 24 833 numbers per site | — | equilibrium = pooled to 1.5e-13; vs multinma 0.038 |

* **Effect-modifier priors, not τ priors, govern transport.** In the severe target population, the UST effect is 2.44–2.46 across the 8 τ priors, but 2.18–2.45 (SD 0.39–0.51) across the interaction priors.
* **Fixed here: random effects in studies without the reference arm.**
  * springs now follows multinma's convention: random effects are relative to the network reference, so CLEAR and IXORA-S get a δ on every arm.
  * Before the fix, springs fitted an equivalent model with a different study-baseline definition. d, β and τ were already identical, but predictions in those two populations had SDs up to 40% narrower.
* **Fixed here: two-arm studies under random effects.** A single-δ random-effect group was serialised as a scalar.

## 9. Continuous outcomes: the natural spring (`results/09_continuous.md`)

A Gaussian patient is a Hooke spring: linear force law, stiffness 1/σ². The setup is simulated with a known truth: 3 IPD trials (960 patients) and 3 AgD trials, 4 treatments, an effect-modifying covariate, τ = 0.4.

| | springs | multinma | agreement |
|---|---|---|---|
| FE | **0.32 s** | 8 s | d, β, 7 arm σ's: max \|z\| 0.033; population-specific effects 0.031 |
| RE (5 τ priors) | **0.68 s** | 10 s | max \|z\| 0.047; τ 0.571 vs 0.577 |
| federated, one round | 0.2 s; each IPD site shares XᵀX, Xᵀy, yᵀy, n per arm | — | = pooled fit to 1e-13 |

* **Exact where binary is approximate.**
  * An AgD arm is exactly the spring at its mean covariates: no aggregation bias, no integration needed.
  * A site's patients are exactly one equivalent spring (engine test `equivalent_spring_is_exact_for_gaussian_patients`). The federated analysis therefore needs one round and no energy round.
* **Engine.** Gaussian units (`Gauss::Ipd` / `Arm` / `Suff`) in `gc-rust/src/ipd.rs`, with a half-normal prior on σ (Jacobian included).
* **R.** `mlnmr(likelihood = "normal")` follows multinma's convention: one residual SD per IPD arm, with prior `prior_aux`.

## 10. Where springs work and INLA / nmaINLA cannot (`results/10_vs_inla.md`)

INLA needs one linear predictor per observation. The ML-NMR aggregate arm, p̄ = mean_q Φ(η_q), is a non-linear function of many linear predictors, so it falls outside INLA's model class. That is exactly where aggregation bias lives. Results on the full psoriasis network, FE, against multinma:

| model | INLA / nmaINLA? | vs multinma |
|---|---|---|
| A. springs ML-NMR (averaged non-linear springs) | no | max \|z\| 0.043 (effects + target population), 0.045 (9 study populations) |
| B. AgD arms at their mean covariates | INLA: yes | max \|z\| 0.52 (UST), 0.54 over study populations |
| C. arm-total NMA, no individual covariates | nmaINLA: yes | no population-specific effects at all; d differs by up to 0.8 SD (a different estimand) |

* Per-arm aggregation bias is at most 1.2 percentage points (CLEAR: UST). It still moves UST by 0.5 SD, because UST's effect rests on that AgD arm (and IXORA-S).

## 11. Path-based inconsistency: Q_path (`results/11_qpath.md`)

This implements Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou (Stat Med 2026, doi:10.1002/sim.70752) as `springs::qpath()` and `netpath_plot()`.
* **The spring reading.**
  * The hat-matrix row of comparison i–j is the force flow through the springs. Its sign orients each spring, and directed i → j routes are the paths of evidence.
  * Path estimate = sum of rest lengths; path variance = sum of compliances; paths covary through shared springs.
  * Multi-arm studies use Rücker's Laplacian reduction.
* **Validation against the paper:**
  * toy network Q = 11.111, df 2;
  * diabetes metformin vs placebo: 7 paths, Q = 22.459, df 6, p = 0.0010;
  * placebo vs sulfonylurea: 10 → 7 independent paths, same Q;
  * largest Netpath |z| 4.64 (pioglitazone vs sulfonylurea→rosiglitazone path), exactly as published;
  * network estimate = netmeta common effect (−1.11411).
  * One path through the three-arm study differs slightly (−1.006 vs −1.02); Q is unaffected.

## 12. Genetic association: genotypes + count tables + summary statistics (`results/12_genetics.md`)

The setup is simulated: one locus and six case-control cohorts (24 000 people), with population stratification (allele frequency and risk both rise with an ancestry PC). There are three data types in one spring network:
- **individual genotypes:** Bernoulli springs;
- **genotype-count tables with covariate summaries:** averaged logistic springs;
- **adjusted summary statistics:** Gaussian springs on β_g.

**Results.**

* **springs vs Stan (same likelihood):** max \|z\| 0.031 over all 7 parameters. springs takes 0.18 s; Stan takes 3 s sampling plus 15 s compile.
* **vs the gold standard** (logistic regression on all individuals): within 0.16 of its SD.
* **Crude tables** are confounded by stratification: 5.4 SD off.
* **Tables at mean covariates** (the INLA-representable model) differ from the correct model by 0.32 SD. The bias is modest at moderate risk.
* **Effect-size prior sweep** (fine-mapping W from 0.05 to 1, plus Cauchy) comes from one fit in 8 ms. The Bayes factor for association varies 33-fold; reweighted and refitted BFs agree to 0.02 log10.
* **Wakefield's ABF** is 1.0 log10 higher. That is exactly the Wald-vs-likelihood-ratio gap: z² 60.3 vs likelihood-ratio 55.9, and (z² − LR)/(2 ln 10) = 1.0.
* An earlier version understated these BFs by 0.40 log10, because the null fit kept a data-free N(0,1) β_g prior unnormalised. Fixed, and found by the Example 13 validation against Wakefield's ABF.

## 13. Real genetic data with validation: asthma, 8 countries (`results/13_asthma_real.md`)

The data are real: SNPassoc::asthma (ECRHS), 1 517 people, 312 asthma cases, SNP rs184448, adjusted for sex, age and smoking. The data-sharing scenario is designed:
- Sweden, UK and Spain share genotypes;
- Australia, Switzerland and France share genotype-count tables with per-group covariate summaries;
- Norway and Germany share adjusted summary statistics.

| validation | reference | result |
|---|---|---|
| mixed data, all 10 parameters | Stan, same likelihood | max \|z\| 0.022; springs 0.03 s vs Stan 1 s + 15 s compile |
| all genotypes, flat priors | `glm()` | equilibrium = MLE to 6e-9; log-likelihood to 2e-12 |
| summary statistics only | metafor FE meta-analysis | identical to 8 digits |
| Bayes factors, summary only | Wakefield's ABF (exact for Gaussian) | within 0.004 log10 |
| effect-size prior sweep (5 priors, one fit, 7 ms) | direct refits | Δ mean ≤ 0.003, Δ log10 BF ≤ 0.012 |

* **Estimate.** OR 1.41 per allele (gold standard with all genotypes: 1.40).
* **Prior dependence.** With a moderate signal, the Bayes factor for association ranges from 11 to 52 across the effect-size priors.

## Known limitations

1. **Binary likelihood.** Effect means sit about 0.1–0.2 SD below MCMC. This comes from the per-τ² Laplace step (the mean is pulled toward the mode). Continuous data are exact (parkinsons, Crins).
   * The importance correction built for ML-NMR closes exactly this gap. Porting it to the aggregate engine is the next step.
2. **ML-NMR is implemented and matches multinma** for binary (Examples 5–8) and continuous (Example 9) outcomes with arm-based AgD. Not yet built:
   * the ordered multinomial PASI 75/90/100 analysis;
   * survival outcomes;
   * the energy-based conflict decomposition.
3. **Speed.** Against multinma we measured 20–60× wall time and far more per effective sample.
