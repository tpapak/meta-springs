## Example 8 — The full plaque psoriasis network: 4 IPD + 5 AgD trials

4114 IPD patients in 4 trials (IXORA-S, UNCOVER-1, UNCOVER-2, UNCOVER-3) + 15 AgD arms in 5 trials (CLEAR, ERASURE, FEATURE, FIXTURE, JUNCTURE); 7 treatments; 4129 springs. PASI 75, probit, `~(durnpso + prevsys + bsa + weight + psa) * .trt`, class-common interactions (IL-17 / IL-12/23 / TNFα blockers), `bernoulli2`, 64 integration points, priors as in the multinma vignette. FEATURE's placebo arm has 0/59 responders.

### (1a) fixed effects

* **springs 1.14 s** (importance ESS 5714) vs **multinma 33 s** (4 chains × 2000, min bulk ESS 3064): 29× faster.
* d, β: max |z| 0.054 (median 0.020), SD ratio 0.97–1.03, 26 quantities
* population-specific relative effects (9 study populations × 6): max |z| 0.045 (median 0.015), SD ratio 0.96–1.04, 54 quantities
* population-average predicted PASI 75 (9 × 7): max |z| 0.120 (median 0.020), SD ratio 0.96–1.07, 63 quantities

### (1b) random effects

* **springs 4.12 s** (40-point τ² grid, all 8 τ priors) vs **multinma 65 s** (4 chains × 2000, min bulk ESS 529): 16× faster; τ springs 0.120 vs multinma 0.117.
* d, β, τ: max |z| 0.070 (median 0.012), SD ratio 0.96–1.04, 27 quantities
* population-specific relative effects (9 study populations × 6): max |z| 0.056 (median 0.026), SD ratio 0.97–1.02, 54 quantities
* population-average predicted PASI 75 (9 × 7): max |z| 0.070 (median 0.027), SD ratio 0.95–1.07, 63 quantities

Treatment effects (probit, vs placebo, network-average population):

|parameter  | springs FE| multinma FE|   z FE| springs RE| multinma RE|   z RE|
|:----------|----------:|-----------:|------:|----------:|-----------:|------:|
|d[ETN]     |      1.605|       1.605| -0.001|      1.619|       1.616|  0.029|
|d[IXE_Q2W] |      2.990|       2.989|  0.012|      3.007|       3.006|  0.008|
|d[IXE_Q4W] |      2.574|       2.573|  0.017|      2.591|       2.587|  0.030|
|d[SEC_150] |      2.246|       2.243|  0.036|      2.276|       2.277| -0.012|
|d[SEC_300] |      2.569|       2.566|  0.035|      2.600|       2.600| -0.002|
|d[UST]     |      2.128|       2.131| -0.023|      2.145|       2.159| -0.070|

### (2) Prior investigation from one fit

* FE: 8 priors in **1.5 s** after the 1.1 s fit; RE: 4 interaction priors × 8 τ priors = 32 posteriors in **9.0 s** after the 4.1 s fit. multinma would need 8 + 32 Stan fits (≈ 39 min).
* Spot check vs a multinma refit (FE, β ~ N(0, 0.5²); d, β and target-population effects): max |z| 0.074 (median 0.013), SD ratio 0.97–1.04, 32 quantities; multinma 32 s.
* τ across the 8 τ priors (vignette interaction prior): posterior mean 0.11–0.13.

FE posterior mean (SD) under each prior; `target` = effect vs placebo in the transport population (25 y disease, 60% BSA, 120 kg, PsA, prior systemic):

|prior                   |  ess|refit |d[ETN]      |d[IXE_Q2W]  |d[SEC_300]  |d[UST]      |d[target: ETN] |d[target: IXE_Q2W] |d[target: UST] |
|:-----------------------|----:|:-----|:-----------|:-----------|:-----------|:-----------|:--------------|:------------------|:--------------|
|vignette: all N(0, 10²) | 5714|FALSE |1.60 (0.08) |2.99 (0.08) |2.57 (0.09) |2.13 (0.15) |1.09 (0.26)    |2.79 (0.24)        |2.45 (0.50)    |
|reg N(0, 1)             | 4976|FALSE |1.60 (0.08) |2.98 (0.08) |2.57 (0.09) |2.11 (0.15) |1.07 (0.24)    |2.77 (0.21)        |2.34 (0.44)    |
|reg N(0, 0.5²)          | 1377|FALSE |1.59 (0.07) |2.98 (0.08) |2.56 (0.09) |2.08 (0.14) |1.03 (0.21)    |2.73 (0.19)        |2.18 (0.39)    |
|reg N(0, 0.25²)         |   60|TRUE  |1.59 (0.07) |2.97 (0.08) |2.56 (0.09) |2.06 (0.14) |1.04 (0.18)    |2.71 (0.16)        |2.00 (0.29)    |
|reg Cauchy(0, 0.5)      | 1392|FALSE |1.59 (0.07) |2.98 (0.08) |2.56 (0.09) |2.08 (0.14) |1.04 (0.21)    |2.74 (0.19)        |2.22 (0.42)    |
|reg t3(0, 0.5)          | 1501|FALSE |1.59 (0.07) |2.98 (0.08) |2.56 (0.09) |2.08 (0.14) |1.04 (0.21)    |2.73 (0.19)        |2.21 (0.41)    |
|trt N(0, 1) (sceptical) | 4958|FALSE |1.56 (0.07) |2.94 (0.08) |2.51 (0.09) |2.04 (0.15) |1.05 (0.25)    |2.75 (0.23)        |2.34 (0.49)    |
|intercept N(−1.5, 0.3²) | 4418|FALSE |1.57 (0.07) |2.97 (0.08) |2.54 (0.08) |2.13 (0.14) |1.06 (0.26)    |2.77 (0.23)        |2.44 (0.49)    |

### (3) Federated: 9 sites, no pooled data, no coordinator

|site      |data | springs| patients| numbers sent| patient records sent|
|:---------|:----|-------:|--------:|------------:|--------------------:|
|IXORA-S   |IPD  |     260|      260|        24833|                    0|
|UNCOVER-1 |IPD  |    1296|     1296|        24833|                    0|
|UNCOVER-2 |IPD  |    1219|     1219|        24833|                    0|
|UNCOVER-3 |IPD  |    1339|     1339|        24833|                    0|
|CLEAR     |AgD  |       2|      669|        24833|                    0|
|ERASURE   |AgD  |       3|      734|        24833|                    0|
|FEATURE   |AgD  |       3|      177|        24833|                    0|
|FIXTURE   |AgD  |       4|     1297|        24833|                    0|
|JUNCTURE  |AgD  |       3|      181|        24833|                    0|

* **7 force rounds** relax the joint network: equilibrium = pooled springs fit to **1.5e-13**. Quadratic equivalent springs only: vs multinma (pooled data) max |z| 0.362 (median 0.036), SD ratio 0.96–1.02, 26 quantities.
* **One energy round** (16000 shared random configurations, one log-likelihood each): vs multinma (pooled data) **max |z| 0.038 (median 0.014), SD ratio 0.98–1.05, 26 quantities**; vs pooled springs max |z| 0.051.
* Whole protocol 7.9 s (sites simulated on one machine). No patient record, covariate value or per-patient quantity leaves a site.

