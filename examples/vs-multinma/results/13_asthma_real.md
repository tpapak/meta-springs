## Example 13 — Real genetic data: asthma, 8 countries (validation)

SNPassoc::asthma (ECRHS), 1517 complete cases in 8 countries (312 asthma cases), SNP rs184448 coded as minor-allele count; covariates sex, age, smoking.

Data-sharing scenario on the real data: genotypes from Sweden, UK, Spain; genotype-count tables with per-group covariate summaries from Australia, Switzerland, France; adjusted summary statistics only from Norway and Germany. Model: logit P(asthma) = μ_country + β_g·g + β_male + β_age·(age−45)/10 + β_smoke; priors N(0, 10²) on nuisance parameters, N(0, 1) reference on β_g.

### Validation against established tools

| check | reference | result |
|---|---|---|
| V1 mixed data (genotypes + tables + summaries), all 10 parameters | Stan (rstan), same likelihood & priors: 1 s sampling + 15 s compile, min ESS 2052 | max \|z\| **0.022**, SD ratio 0.97–1.02; springs 0.03 s |
| V2 all six countries share genotypes, flat priors | `glm()` logistic regression | equilibrium = MLE to **5.8e-09**; log-likelihoods equal to 1.8e-12 |
| V3 all eight countries share only summary statistics | `metafor::rma(method = "FE")` | equilibrium 0.37696977 vs 0.37696977, SE 0.10474057 vs 0.10474057 |
| V3 Bayes factors, summary-only model | Wakefield's ABF (exact for a Gaussian likelihood) | max \|Δ log10 BF\| **0.004** over W = 0.1–1 |
| V4 effect-size prior sweep (5 priors, one fit, 7 ms) | direct refits | max \|Δ mean\| 0.0027, max \|Δ log10 BF\| 0.012 |

V1 parameter by parameter:

|parameter       | springs|   stan| springs_sd| stan_sd|      z| sd_ratio| stan_ess|
|:---------------|-------:|------:|----------:|-------:|------:|--------:|--------:|
|mu[Sweden]      |  -0.835| -0.837|      0.183|   0.183|  0.008|    1.001| 2052.139|
|mu[UK]          |  -0.394| -0.396|      0.224|   0.222|  0.009|    1.009| 2620.377|
|mu[Spain]       |  -2.203| -2.204|      0.226|   0.227|  0.004|    0.995| 2240.176|
|mu[Australia]   |  -0.954| -0.954|      0.243|   0.248| -0.002|    0.981| 2431.668|
|mu[Switzerland] |  -1.129| -1.130|      0.271|   0.265|  0.006|    1.023| 2589.416|
|mu[France]      |  -2.232| -2.234|      0.254|   0.253|  0.007|    1.004| 2805.444|
|beta_g          |   0.343|  0.344|      0.103|   0.102| -0.006|    1.003| 2259.663|
|beta_male       |  -0.336| -0.334|      0.176|   0.175| -0.012|    1.003| 2607.181|
|beta_age        |  -0.296| -0.294|      0.124|   0.124| -0.018|    1.003| 3326.338|
|beta_smoke      |   0.020|  0.024|      0.204|   0.209| -0.022|    0.973| 3315.682|

V3 Bayes factors (summary-only model):

|   W| springs_log10_BF| Wakefield_log10_ABF|
|---:|----------------:|-------------------:|
| 0.1|            1.199|               1.201|
| 0.2|            1.878|               1.874|
| 0.4|            2.037|               2.036|
| 1.0|            1.800|               1.800|

### Results on the real data

|analysis                                             | estimate|    sd|    OR|
|:----------------------------------------------------|--------:|-----:|-----:|
|gold standard: glm on all 8 countries' genotypes     |    0.338| 0.102| 1.402|
|**springs, data as available (mixed)**               |    0.343| 0.103| 1.409|
|Stan, same likelihood                                |    0.344| 0.102| 1.410|
|count tables at mean covariates (INLA-representable) |    0.341| 0.102| 1.407|
|count tables crude                                   |    0.332| 0.102| 1.393|
|metafor IVW, if all 8 countries published summaries  |    0.377| 0.105| 1.458|

Effect-size prior sweep on the mixed model (from one fit; refits for comparison):

|prior          |  mean|    sd|   ESS| log10_BF| refit_mean| refit_sd| refit_log10_BF|
|:--------------|-----:|-----:|-----:|--------:|----------:|--------:|--------------:|
|N(0, 0.1²)     | 0.171| 0.071|  1912|    1.051|      0.169|    0.072|          1.063|
|N(0, 0.2²)     | 0.274| 0.090|  8853|    1.616|      0.274|    0.092|          1.617|
|N(0, 0.4²)     | 0.325| 0.099| 12386|    1.714|      0.325|    0.100|          1.714|
|N(0, 1)        | 0.343| 0.103| 12658|    1.454|      0.343|    0.103|          1.454|
|Cauchy(0, 0.2) | 0.300| 0.104| 10894|    1.537|      0.300|    0.105|          1.538|

* With a moderate signal (OR ≈ 1.41 per allele), the evidence for association depends on the prior: log10 BF from 1.05 to 1.71 across these priors (BF 11 to 52).
