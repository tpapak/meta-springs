## Example 4 — models beyond multinma

### (a) Random vs fixed effects: Bayes factors from the partition function

Validation on Crins et al. 2014 (k = 6, log OR, Gaussian likelihood — the spring partition function is exact here). bayesmeta integrates numerically, one call per prior (5.01 s total); springs gives all 6 priors in one pass (0.003 s).

|prior                             | springs_logBF| bayesmeta_logBF| springs_tau_med| bayesmeta_tau_med|   diff|
|:---------------------------------|-------------:|---------------:|---------------:|-----------------:|------:|
|HN(τ;0.5)                         |         0.128|           0.127|           0.353|             0.345|  0.000|
|HN(τ;1)                           |        -0.117|          -0.116|           0.498|             0.492| -0.001|
|HC(τ;0.5)                         |        -0.144|          -0.142|           0.359|             0.352| -0.002|
|Exp(τ;2)                          |         0.039|           0.039|           0.334|             0.323|  0.000|
|U(τ;0,2)                          |        -0.323|          -0.323|           0.605|             0.600|  0.000|
|LN(τ²;−2.56,1.74²) Turner general |         0.102|           0.102|           0.281|             0.281|  0.000|

Applied to the multinma example networks (binary: Laplace-per-τ² partition function), equal prior model odds:

|network             |prior                                      | log_BF_RE_FE|  P_RE| engine_s|
|:-------------------|:------------------------------------------|------------:|-----:|--------:|
|smoking             |HN(τ;0.5)                                  |       84.953| 1.000|    0.011|
|smoking             |HN(τ;1)                                    |       85.198| 1.000|    0.011|
|smoking             |HC(τ;0.5)                                  |       84.748| 1.000|    0.011|
|smoking             |LN(τ²;−2.56,1.74²) Turner general          |       84.531| 1.000|    0.011|
|smoking             |LN(τ²;−4.18,1.41²) Turner mortality P-vs-P |       82.255| 1.000|    0.011|
|thrombolytics       |HN(τ;0.5)                                  |       -1.519| 0.180|    0.056|
|thrombolytics       |HN(τ;1)                                    |       -2.190| 0.101|    0.056|
|thrombolytics       |HC(τ;0.5)                                  |       -1.767| 0.146|    0.056|
|thrombolytics       |LN(τ²;−2.56,1.74²) Turner general          |       -1.552| 0.175|    0.056|
|thrombolytics       |LN(τ²;−4.18,1.41²) Turner mortality P-vs-P |       -0.723| 0.327|    0.056|
|atrial_fibrillation |HN(τ;0.5)                                  |        0.624| 0.651|    0.082|
|atrial_fibrillation |HN(τ;1)                                    |        0.060| 0.515|    0.082|
|atrial_fibrillation |HC(τ;0.5)                                  |        0.302| 0.575|    0.082|
|atrial_fibrillation |LN(τ²;−2.56,1.74²) Turner general          |        0.783| 0.686|    0.082|
|atrial_fibrillation |LN(τ²;−4.18,1.41²) Turner mortality P-vs-P |        0.848| 0.700|    0.082|

multinma would report DIC for an FE and an RE fit (two Stan runs per prior) — not a posterior model probability.

### (b) Baseline-risk network meta-regression (not available in multinma)

Certolizumab network (12 studies, 7 treatments; ACR50, log OR). Treatment effects regress on the latent baseline log-odds η_i: logit p_ik = η_i + d_k + b_k(η_i − η̄). springs integrates τ² (and σ_B² for exchangeable slopes) over the grand-canonical grid; previously validated against bnma and the exact MLE (test/NMR.md).

|slopes       |effects | time_s|  tau2| sigma_B2|slope_median_range |CZP_slope            |
|:------------|:-------|------:|-----:|--------:|:------------------|:--------------------|
|common       |random  |  0.074| 0.064|       NA|-0.93 to -0.93     |-0.93 (-1.42, -0.40) |
|independent  |random  |  0.072| 0.090|       NA|-2.97 to 1.44      |-0.78 (-1.59, 0.01)  |
|exchangeable |random  |  0.083| 0.070|    0.054|-0.94 to -0.88     |-0.88 (-1.43, -0.32) |

