# meta-springs vs multinma: worked examples

Worked, reproducible comparisons: where the spring / grand-canonical (GC) engine improves on multinma in **time**, **prior
examination** and **models**, on real data with matched priors.

| script | shows | data |
|---|---|---|
| `01_speed.R` | same RE NMA, same posterior, 20–60× less wall time, 50–700× more effective draws/s | multinma: smoking, thrombolytics, atrial_fibrillation, parkinsons; three largest binary nmadb networks (95–111 studies) |
| `02_priors.R` | a 15-prior τ panel in **one** pass vs 13 Stan refits; reproduces Turner et al. 2019 Table 4 (including the U(0,2) column multinma cannot fit) | smoking, thrombolytics |
| `03_nmadb_sweep.R` | a database-wide sweep: 171 networks × 15 priors in seconds, plus how often conclusions depend on the prior; multinma timed and checked on a random sample | nmadb binary networks (`test/nmadb`) |
| `03b_sweep_exact.R` | the sweep's "how often does the prior change a conclusion" from **exact** interval ends (τ²-mixture CDF): 42% of networks, 28% with a strict margin; draw noise and what an MCMC sweep would report | as Example 3 |
| `05_mlnmr_psoriasis.R` | **IPD + AgD (ML-NMR)**: the multinma plaque psoriasis vignette (3 IPD + 1 AgD trial, probit, 5 covariates, class interactions, 64 integration points), FE and RE, 20–35× faster, and τ-prior sensitivity from one grid; IPD-only check reproduces `glm()` exactly | multinma `plaque_psoriasis_ipd/agd` |
| `06_mlnmr_priors.R` | **prior investigation for ML-NMR from one fit**: 11 priors on effect modifiers / treatment effects / baselines (Normal, Cauchy, t, informative) and 4 interaction × 8 τ priors = 32 RE posteriors, by reweighting one importance pool; each checked against a direct refit, 6 against multinma; population-specific effects in a target population | as Example 5 |
| `07_federated.R` | **federated ML-NMR, no pooled data, no coordinator**: each trial a site evaluating only its own springs (`mlnmr_site`); ring secure sum; force rounds → equilibrium = pooled to 1e-11; one energy round → exact posterior (= multinma pooled, max \|z\| 0.03); every site its own prior | as Example 5 |
| `08_full_network.R` | **the full psoriasis network** (4 IPD + 5 AgD trials, 7 treatments, 3 classes): springs vs multinma FE/RE (parameters, 54 population-specific effects, 63 predictions), 40-posterior prior panel, federated with 9 sites | multinma `plaque_psoriasis_ipd/agd`, all studies |
| `09_continuous.R` | **continuous outcomes, the natural spring** (Gaussian patients = Hooke springs): springs vs multinma (normal likelihood, identity link) FE/RE on a simulated 3 IPD + 3 AgD network with known truth; **federated in one round, exact** (each IPD site shares its equivalent spring XᵀX, Xᵀy, yᵀy, n) | simulated |
| `10_vs_inla.R` | **where springs work and INLA/nmaINLA cannot**: the ML-NMR aggregate arm (average of a non-linear inverse link over many linear predictors) is outside INLA's model class; the nearest INLA-representable model (AgD at mean covariates) is off by up to 0.5 SD (UST), nmaINLA's arm-total NMA cannot give population-specific effects | as Example 8 |
| `11_qpath.R` | **path-based inconsistency** (Q_path + Netpath plot; Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou, Stat Med 2026) in springs: `qpath()`, `netpath_plot()`; reproduces the paper's toy (Q 11.11, df 2) and diabetes (Q 22.46, df 6; largest path conflict 4.64) results | toy network; `netmeta::Senn2013` |
| `12_genetics.R` | **genetic association**: one locus, six case-control cohorts with three data types (individual genotypes; genotype-count tables with covariate summaries; adjusted summary statistics) in one spring network, under population stratification; vs Stan and vs the all-genotypes gold standard; effect-size prior sweep (fine-mapping W) with Bayes factors from one fit | simulated |
| `13_asthma_real.R` | **real genetic data, validated**: SNPassoc::asthma (ECRHS, 8 countries), genotypes + count tables + summary statistics; springs vs Stan (\|z\| ≤ 0.022), vs `glm()` (MLE to 6e-9), vs metafor (8 digits), Bayes factors vs Wakefield's ABF (0.004 log10) | SNPassoc `asthma` |
| `04_models.R` | (a) RE-vs-FE Bayes factors from the partition function, validated against bayesmeta; (b) baseline-risk NMR (latent covariate), which multinma does not offer | Crins 2014; smoking/thrombolytics/AF; certolizumab |

Results (markdown + csv) are written to `results/`. A summary is in `RESULTS.md`.

## Running

From the repository root:

```sh
cargo build --release --manifest-path gc-rust/Cargo.toml      # mtc + brisk engines
R CMD INSTALL r-package/springs
Rscript examples/vs-multinma/01_speed.R
Rscript examples/vs-multinma/02_priors.R
Rscript examples/vs-multinma/03_nmadb_sweep.R   # ~10 min, mostly the multinma sample
Rscript examples/vs-multinma/03b_sweep_exact.R    # ~45 s
Rscript examples/vs-multinma/04_models.R
Rscript examples/vs-multinma/05_mlnmr_psoriasis.R   # ~3 min, mostly multinma
Rscript examples/vs-multinma/06_mlnmr_priors.R      # ~4 min, mostly the 6 multinma refits
Rscript examples/vs-multinma/07_federated.R         # ~10 s (uses results/05_pars_fixed.csv)
Rscript examples/vs-multinma/08_full_network.R      # ~3 min first run (multinma fits cached in .cache/)
Rscript examples/vs-multinma/09_continuous.R        # ~30 s first run
Rscript examples/vs-multinma/10_vs_inla.R           # ~10 s (needs the Example 8 multinma cache)
Rscript examples/vs-multinma/11_qpath.R             # ~2 s
Rscript examples/vs-multinma/12_genetics.R          # ~30 s (Stan compile)
Rscript examples/vs-multinma/13_asthma_real.R       # ~30 s (Stan compile); needs SNPassoc, metafor, rstan
```

Requires multinma (≥ 0.9), bayesmeta and metafor.

## Matching the two models

* The likelihood is binomial-logit or normal-identity at the arm level, with study baselines and random effects shared within multi-arm trials (the standard Dias et al. model).
* multinma uses `N(0, 100²)` on baselines and effects; springs uses flat priors. These are the same for practical purposes.
* Both fit the same τ prior in each comparison. Spec strings: `hn:S`, `hc:S`, `exp:R` (on τ); `ln:MU:SIGMA` (log-normal on τ²); `unif:U` (on τ, springs only).
* multinma runs 4 chains × 2000 iterations (1000 warm-up), with the 4 chains in parallel on 4 cores. springs runs single-threaded and draws 4000 iid samples from the closed-form τ²-mixture posterior.

## Engine additions made for these examples

* `ln:MU:SIGMA`: log-normal prior on τ² (Turner 2015/2019, Rhodes 2015), and `exp:RATE`: exponential prior on τ. Both are in `gc-rust/src/gc.rs`.
* Normalised prior densities, plus `log_marginal`: log ∫ Z(τ²) π(τ²) dτ² over the shared grid. `mtc --priors` now also emits `log_ml_fe` (the τ²→0 limit), `log_bf_re_fe` per prior, and the grid itself.
* τ² posterior quantiles are now interpolated between grid nodes instead of snapping to them.
* `springs::mtc.sensitivity()` returns the τ² interval and the Bayes factor, and attaches the grid.

## IPD + AgD (ML-NMR) implementation

* Engine: `gc-rust/src/ipd.rs` (library) and `gc-rust/src/bin/mlnmr.rs` (CLI). The facade is `springs::mlnmr()` (`r-package/springs/R/mlnmr.R`). It accepts a multinma `nma_data` network unchanged, so an existing analysis is refitted by swapping `multinma::nma()` for `springs::mlnmr()`.
* Each IPD patient is a Bernoulli spring. Each AgD arm is one spring whose force law is the average of the individual force laws over its integration points (p̄ = mean_q g⁻¹(η_q)). This is the aggregation-bias correction of ML-NMR, computed deterministically.
* The equilibrium (score = 0) is found by damped Fisher/Newton relaxation. The observed-information curvature gives the Laplace covariance.
* **Importance correction (default, `correction = "is"`).**
  * The Laplace solution becomes a multivariate-t proposal, reweighted by the exact energy.
  * This removes the mode-vs-mean shift of skewed posteriors. Here it comes from rare placebo responses in the probit tail. Laplace alone put d about 0.2 SD below multinma; corrected, the gap is ≤ 0.04 SD.
  * It also corrects the partition function Z(τ²) at each grid point.
  * The ESS is reported as a diagnostic of how harmonic the springs are.
  * The draws are iid: no chains, no warm-up.
* Random effects: δ springs with stiffness τ⁻²K, plus the grand-canonical sum over a 40-point τ² grid. All listed τ priors come from one fit (`mlnmr_sensitivity()`), together with RE-vs-FE Bayes factors.
* Outputs:
  * `mlnmr_relative_effects()`: population-specific effects (each study population, or any `newdata` target population; this is transportability).
  * `mlnmr_predict()`: population-average event probabilities.
* Engine tests (`cargo test --lib ipd`):
  * finite-difference checks of score and observed information for logit/probit × binomial/binomial2;
  * binomial2 reduces to binomial when covariates don't vary;
  * force balance at the mode;
  * the importance integral equals Laplace on a Gaussian posterior.

## Prior investigation for ML-NMR (`06_mlnmr_priors.R`)

Why it is free in the spring framework:

* **The energy separates.** E(θ) = Σ data springs (every patient, every AgD integration point) + Σ prior springs (one per parameter) + random-effect springs (stiffness 1/τ²). Only the first term is expensive, and it does not depend on the prior.
* **The fit keeps a weighted pool.** The importance-corrected fit already holds draws θ_i with exact weights w_i ∝ exp(−E(θ_i))/q(θ_i). Under a new prior the posterior is the same pool with w_i · π_new(θ_i)/π_old(θ_i). No patient is ever evaluated again: a reweighting is ≈ 40 ms.
* **τ priors act on the grid.** The grand-canonical partition function Z(τ²) is prior-free, so a τ prior is a reweighting of the τ² grid, and θ-prior and τ-prior changes multiply into one weight: 4 × 8 = 32 RE posteriors cost one fit.
* **The ESS is the safety check.** When a prior moves the posterior far from the pool (here N(0, 0.1²) on the effect modifiers, or N(0, 0.5²) on effects of size ≈ 3), the ESS collapses and `mlnmr_reweight()` refits automatically (< 1 s). Nothing silently degrades.
* **Prior share from the curvature.** A prior is one more spring; its stiffness over the total posterior stiffness (≈ Var(θ | data)/prior variance) says, with no refit, which parameters any prior could move (`mlnmr_prior_share()`).
* MCMC has none of this: the chains are tied to the prior they were run under, so each prior is a new run (here 29 s FE, 53 s RE), and reweighting autocorrelated draws with ESS ≈ 1 000 is fragile.

Functions: `mlnmr(..., prior_* = <number | multinma prior object>)` (Normal at any location, Cauchy, Student-t, flat), `mlnmr_reweight()`, `mlnmr_prior_panel()`, `mlnmr_prior_share()`. Engine: `ParPrior` in `gc-rust/src/ipd.rs` (test: reweighting a pool equals refitting under the new prior), `priors` / `return_pool` in `mlnmr`.

## Known limitations

* **Binary data: small, systematic shift of effect means.** For binomial likelihoods the GC uses a Laplace approximation at each τ² grid point. The posterior mean is pulled slightly toward the conditional mode.
  * Typical gap to multinma: |Δ| ≈ 0.1–0.2 posterior SD.
  * It reaches 0.35 SD, with SDs 10–20% too narrow, for comparisons informed by a single trial with very few events (e.g. atrial_fibrillation, LASAF: 1/90 events).
  * Continuous (Gaussian) data are exact: parkinsons |z| ≤ 0.05; Crins Bayes factors match bayesmeta to 0.002.
  * A skewness / simplified-Laplace correction is the obvious next step.
* **ML-NMR scope.** Binary (logit/probit; Bernoulli IPD, binomial or two-parameter binomial AgD) and continuous (normal, identity link; arm-specific residual SDs as in multinma) outcomes, with arm-based AgD. Not yet: count/survival outcomes, contrast-based AgD, ordered multinomial (the full PASI 75/90/100 analysis), exchangeable class interactions. Integration points come from multinma's `add_integration()` (or any user-supplied set).
* Bayes factors need proper τ priors. The flat prior gives none.
