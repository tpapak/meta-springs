## Example 12 — Genetic association: genotypes + count tables + summary statistics

24,000 individuals in 6 cohorts (2 IPD, 2 genotype-count tables, 2 summary statistics); true per-allele log-OR 0.182 (OR 1.2), β_pc 0.6, β_sex 0.3; allele frequency logit-linear in the ancestry PC (stratification).

Model: logit P(case) = μ_cohort + β_g·g + β_pc·PC + β_sex·sex. Each individual is a Bernoulli spring; each genotype group of a count table is one spring whose force law is the logistic averaged over that group's covariate distribution (64 points); each summary statistic is a Gaussian spring on β_g. Priors N(0, 10²) on nuisance parameters, reference N(0, 1) on β_g.

### (1) One model for all three data types

* **springs 0.18 s** (engine; importance ESS 13826) vs **Stan 3 s** sampling + 15 s compile (min ESS 2087). All 7 parameters: max |z| springs vs Stan = 0.031.

|analysis                                                      |what                                       | estimate|    sd| bias vs gold (gold SDs)| vs correct model (its SDs)|
|:-------------------------------------------------------------|:------------------------------------------|--------:|-----:|-----------------------:|--------------------------:|
|gold standard: logistic regression on all individuals         |needs every genotype                       |    0.173| 0.022|                   0.000|                     -0.154|
|**springs: IPD + averaged count tables + summary statistics** |uses the data as available                 |    0.177| 0.023|                   0.161|                      0.000|
|Stan, same likelihood                                         |3 s (+15 s compile)                        |    0.177| 0.024|                   0.168|                      0.006|
|count tables at their mean covariates                         |one η per observation (INLA-representable) |    0.169| 0.024|                  -0.180|                     -0.324|
|count tables crude (genotype only)                            |ignores stratification                     |    0.294| 0.021|                   5.388|                      4.972|
|IVW of adjusted summaries from all 6 cohorts                  |needs every cohort to rerun its regression |    0.173| 0.022|                  -0.016|                     -0.169|

* Combining the data as available recovers the gold standard to 0.16 of its SD (some information is lost in tables and summaries: posterior SD 0.023 vs 0.022).
* Shortcuts: crude count tables are confounded by stratification (5.4 gold SDs off). Tables at their mean covariates — the model INLA can represent — remove most of the confounding but not the aggregation bias: they differ from the correct (averaged) model by -0.32 of its SDs. Here the bias is modest because risks are moderate (≈ 20–40%) and the logistic curve is nearly linear there; it grows with rarer outcomes and wider covariate spread.

### (2) The effect-size prior, applied afterwards

Bayesian fine-mapping depends on the prior SD W of the allelic effect (e.g. Wakefield's 0.1–0.2), and it is contested. From the **one** fit above, every prior is a reweighting of its 16000 stored points (8 ms for all 7 priors), giving the posterior of β_g and the Bayes factor for association (H1 vs H0: β_g = 0; H0 one extra fit, 0.18 s). Direct refits under three priors and Wakefield's approximate Bayes factor (normal priors, from the gold-standard estimate) are shown for comparison.

|prior          |  mean|    sd|       ESS| log10_BF| refit_mean| refit_sd| refit_log10_BF| refit_s| Wakefield_log10_ABF|
|:--------------|-----:|-----:|---------:|--------:|----------:|--------:|--------------:|-------:|-------------------:|
|N(0, 0.05²)    | 0.146| 0.021|  3746.653|    9.515|      0.145|    0.021|          9.534|   0.189|              10.526|
|N(0, 0.1²)     | 0.168| 0.022| 12057.801|   10.842|         NA|       NA|             NA|      NA|              11.809|
|N(0, 0.15²)    | 0.173| 0.023| 13415.633|   11.023|         NA|       NA|             NA|      NA|              11.976|
|N(0, 0.2²)     | 0.175| 0.023| 13689.623|   11.027|      0.175|    0.023|         11.028|   0.185|              11.976|
|N(0, 0.4²)     | 0.176| 0.023| 13817.032|   10.854|         NA|       NA|             NA|      NA|              11.798|
|N(0, 1)        | 0.177| 0.023| 13825.800|   10.492|         NA|       NA|             NA|      NA|              11.434|
|Cauchy(0, 0.1) | 0.172| 0.023| 13325.657|   10.796|      0.172|    0.024|         10.797|   0.190|                  NA|

* The Bayes factor for association varies 33-fold (1.5 orders of magnitude) across these priors, while the posterior of β_g barely moves except under the tightest prior: the fine-mapping prior debate, from one fit, including a non-normal (Cauchy) prior.
* Reweighted and directly refitted posteriors and Bayes factors agree (|Δ log10 BF| ≤ 0.019).
* Wakefield's ABF is a normal approximation built on the Wald statistic (z² = 60.3 here); the spring Bayes factor uses the actual likelihood, whose likelihood-ratio statistic is 55.9. The gap (1.0 in log10) is the Wald-vs-likelihood difference, (z² − LR)/(2 ln 10) = 1.0: in a logistic model with re-estimated nuisance parameters the Wald approximation overstates the evidence. (On a summary-statistics-only model, where the likelihood is Gaussian, spring BFs equal the ABF: Example 13.)
