## Example 5 — IPD + AgD: ML-NMR on the plaque psoriasis network

3854 IPD patients (UNCOVER-1/2/3) + 4 AgD arms (FIXTURE) with 64 integration points each; probit link, `~(durnpso + prevsys + bsa + weight + psa) * .trt`, class-common interactions, two-parameter binomial AgD likelihood (`bernoulli2`). Priors as in the vignette: N(0, 10²) on intercepts, treatment effects and regression coefficients; τ ~ HN(2.5).

### (0) Exactness check: IPD-only network, flat priors, vs `glm()`

The spring equilibrium is the MLE. (Probit SEs differ from glm's by ≈1.5% because springs uses the observed information and glm the expected; for the canonical logit link the two coincide, and they agree to 1e-8.)

|link   | parameters| max_abs_diff_vs_glm_MLE| loglik_springs| loglik_glm| springs_engine_s|
|:------|----------:|-----------------------:|--------------:|----------:|----------------:|
|logit  |         26|                   0e+00|      -1549.464|  -1549.464|        0.1199053|
|probit |         26|                   5e-08|      -1550.326|  -1550.326|        0.1894947|

### (1) fixed-effects ML-NMR

* **springs: 0.72 s** (engine 0.19 s; 3858 springs, 24 parameters). multinma: **30 s** (min bulk ESS 3812).
* Parameters (d, β): max |z| = 0.03, median |z| = 0.02; SD ratio 0.98–1.02.
* Population-specific relative effects (20): max |z| = 0.04. Population-average predictions (24): max |z| = 0.06.

|parameter                           | springs_mean| multinma_mean| springs_sd| multinma_sd| multinma_ESS|      z| sd_ratio|
|:-----------------------------------|------------:|-------------:|----------:|-----------:|------------:|------:|--------:|
|d[ETN]                              |        1.553|         1.551|      0.080|       0.081|     3812.376|  0.025|    0.997|
|d[IXE_Q2W]                          |        2.956|         2.954|      0.087|       0.086|     4968.223|  0.016|    1.011|
|d[IXE_Q4W]                          |        2.542|         2.541|      0.082|       0.081|     4583.916|  0.022|    1.019|
|d[SEC_150]                          |        2.145|         2.143|      0.115|       0.113|     4381.997|  0.019|    1.018|
|d[SEC_300]                          |        2.450|         2.449|      0.119|       0.116|     5820.849|  0.008|    1.019|
|beta[durnpso]                       |        0.043|         0.043|      0.061|       0.061|     5550.486|  0.013|    1.000|
|beta[prevsys]                       |       -0.138|        -0.139|      0.160|       0.163|     5363.321|  0.006|    0.984|
|beta[bsa]                           |       -0.074|        -0.060|      0.444|       0.439|     5825.381| -0.033|    1.011|
|beta[weight]                        |        0.040|         0.040|      0.028|       0.029|     5824.197| -0.020|    0.989|
|beta[psa]                           |       -0.081|        -0.076|      0.170|       0.167|     5802.812| -0.029|    1.013|
|beta[durnpso:.trtclassTNFa blocker] |       -0.029|        -0.028|      0.074|       0.074|     5924.118| -0.021|    0.995|
|beta[durnpso:.trtclassIL blocker]   |       -0.012|        -0.011|      0.067|       0.067|     7173.356| -0.013|    1.007|
|beta[prevsys:.trtclassTNFa blocker] |        0.188|         0.192|      0.188|       0.192|     5474.990| -0.016|    0.980|
|beta[prevsys:.trtclassIL blocker]   |        0.065|         0.066|      0.175|       0.176|     6332.566| -0.006|    0.993|
|beta[bsa:.trtclassTNFa blocker]     |        0.058|         0.049|      0.516|       0.515|     5479.377|  0.017|    1.002|
|beta[bsa:.trtclassIL blocker]       |        0.296|         0.280|      0.488|       0.482|     7057.745|  0.033|    1.014|
|beta[weight:.trtclassTNFa blocker]  |       -0.167|        -0.168|      0.035|       0.035|     6008.096|  0.024|    1.002|
|beta[weight:.trtclassIL blocker]    |       -0.099|        -0.100|      0.032|       0.032|     6953.389|  0.017|    0.991|
|beta[psa:.trtclassTNFa blocker]     |       -0.053|        -0.059|      0.209|       0.204|     5423.366|  0.029|    1.022|
|beta[psa:.trtclassIL blocker]       |        0.009|         0.006|      0.186|       0.185|     6802.859|  0.020|    1.003|

### (2) random-effects ML-NMR

* **springs: 1.51 s** (engine 0.95 s; 3858 springs, 35 parameters, 11 random effects, 40-point τ² grid, 8 τ priors). multinma: **51 s** (min bulk ESS 618, single prior).
* Parameters (d, β, τ): max |z| = 0.04, median |z| = 0.02; SD ratio 0.98–1.06.
* Population-specific relative effects (20): max |z| = 0.04. Population-average predictions (24): max |z| = 0.07.

|parameter                           | springs_mean| multinma_mean| springs_sd| multinma_sd| multinma_ESS|      z| sd_ratio|
|:-----------------------------------|------------:|-------------:|----------:|-----------:|------------:|------:|--------:|
|d[ETN]                              |        1.561|         1.565|      0.155|       0.147|     2251.835| -0.025|    1.058|
|d[IXE_Q2W]                          |        2.977|         2.976|      0.162|       0.154|     2558.585|  0.007|    1.051|
|d[IXE_Q4W]                          |        2.563|         2.563|      0.159|       0.151|     2179.298|  0.004|    1.054|
|d[SEC_150]                          |        2.128|         2.132|      0.243|       0.232|     3108.732| -0.016|    1.046|
|d[SEC_300]                          |        2.434|         2.436|      0.243|       0.229|     2858.359| -0.007|    1.062|
|beta[durnpso]                       |        0.047|         0.049|      0.062|       0.062|     6553.186| -0.027|    0.996|
|beta[prevsys]                       |       -0.129|        -0.127|      0.164|       0.161|     6260.486| -0.011|    1.017|
|beta[bsa]                           |       -0.102|        -0.108|      0.454|       0.437|     6269.077|  0.013|    1.039|
|beta[weight]                        |        0.044|         0.043|      0.029|       0.029|     6672.325|  0.018|    1.014|
|beta[psa]                           |       -0.064|        -0.058|      0.173|       0.166|     5662.154| -0.038|    1.038|
|beta[durnpso:.trtclassTNFa blocker] |       -0.034|        -0.035|      0.074|       0.074|     6788.730|  0.014|    0.994|
|beta[durnpso:.trtclassIL blocker]   |       -0.015|        -0.017|      0.068|       0.069|     6785.837|  0.033|    0.984|
|beta[prevsys:.trtclassTNFa blocker] |        0.188|         0.183|      0.193|       0.185|     7072.266|  0.025|    1.044|
|beta[prevsys:.trtclassIL blocker]   |        0.053|         0.052|      0.179|       0.176|     6645.127|  0.008|    1.017|
|beta[bsa:.trtclassTNFa blocker]     |        0.086|         0.090|      0.535|       0.513|     6049.217| -0.008|    1.043|
|beta[bsa:.trtclassIL blocker]       |        0.331|         0.342|      0.497|       0.470|     6432.360| -0.025|    1.058|
|beta[weight:.trtclassTNFa blocker]  |       -0.172|        -0.171|      0.035|       0.035|     7018.035| -0.023|    1.000|
|beta[weight:.trtclassIL blocker]    |       -0.103|        -0.103|      0.032|       0.032|     7622.498| -0.019|    1.002|
|beta[psa:.trtclassTNFa blocker]     |       -0.075|        -0.076|      0.213|       0.200|     5795.068|  0.003|    1.061|
|beta[psa:.trtclassIL blocker]       |       -0.009|        -0.015|      0.190|       0.184|     6382.580|  0.031|    1.033|
|tau                                 |        0.192|         0.188|      0.131|       0.125|      617.966|  0.030|    1.052|

### (3) Population-specific relative effects (FE), vs PBO

|parameter             | springs| multinma| springs_sd| multinma_sd|     z|
|:---------------------|-------:|--------:|----------:|-----------:|-----:|
|d[FIXTURE: ETN]       |   1.662|    1.659|      0.089|       0.090| 0.035|
|d[FIXTURE: IXE_Q2W]   |   3.031|    3.028|      0.101|       0.099| 0.036|
|d[FIXTURE: IXE_Q4W]   |   2.618|    2.614|      0.097|       0.093| 0.038|
|d[FIXTURE: SEC_150]   |   2.218|    2.216|      0.118|       0.116| 0.016|
|d[FIXTURE: SEC_300]   |   2.524|    2.522|      0.122|       0.119| 0.015|
|d[UNCOVER-1: ETN]     |   1.508|    1.506|      0.086|       0.086| 0.027|
|d[UNCOVER-1: IXE_Q2W] |   2.926|    2.924|      0.089|       0.087| 0.025|
|d[UNCOVER-1: IXE_Q4W] |   2.512|    2.510|      0.084|       0.083| 0.025|
|d[UNCOVER-1: SEC_150] |   2.113|    2.112|      0.121|       0.119| 0.004|
|d[UNCOVER-1: SEC_300] |   2.419|    2.418|      0.125|       0.122| 0.003|
|d[UNCOVER-2: ETN]     |   1.509|    1.507|      0.081|       0.081| 0.033|
|d[UNCOVER-2: IXE_Q2W] |   2.925|    2.923|      0.087|       0.085| 0.024|
|d[UNCOVER-2: IXE_Q4W] |   2.512|    2.509|      0.083|       0.080| 0.025|
|d[UNCOVER-2: SEC_150] |   2.112|    2.112|      0.118|       0.115| 0.003|
|d[UNCOVER-2: SEC_300] |   2.418|    2.418|      0.122|       0.119| 0.002|
|d[UNCOVER-3: ETN]     |   1.531|    1.528|      0.080|       0.080| 0.038|
|d[UNCOVER-3: IXE_Q2W] |   2.943|    2.940|      0.088|       0.085| 0.028|
|d[UNCOVER-3: IXE_Q4W] |   2.529|    2.527|      0.083|       0.080| 0.029|
|d[UNCOVER-3: SEC_150] |   2.130|    2.129|      0.116|       0.113| 0.006|
|d[UNCOVER-3: SEC_300] |   2.435|    2.435|      0.120|       0.116| 0.005|

Population-average PASI 75 probabilities (FE):

|parameter                | springs| multinma| springs_sd| multinma_sd|      z|
|:------------------------|-------:|--------:|----------:|-----------:|------:|
|pred[FIXTURE: PBO]       |   0.041|    0.041|      0.008|       0.008|  0.015|
|pred[FIXTURE: ETN]       |   0.457|    0.456|      0.026|       0.026|  0.011|
|pred[FIXTURE: IXE_Q2W]   |   0.892|    0.891|      0.018|       0.018|  0.022|
|pred[FIXTURE: IXE_Q4W]   |   0.797|    0.796|      0.026|       0.026|  0.032|
|pred[FIXTURE: SEC_150]   |   0.669|    0.669|      0.025|       0.026|  0.027|
|pred[FIXTURE: SEC_300]   |   0.771|    0.770|      0.023|       0.023|  0.040|
|pred[UNCOVER-1: PBO]     |   0.057|    0.057|      0.009|       0.009| -0.057|
|pred[UNCOVER-1: ETN]     |   0.460|    0.460|      0.030|       0.029| -0.034|
|pred[UNCOVER-1: IXE_Q2W] |   0.900|    0.901|      0.011|       0.011| -0.026|
|pred[UNCOVER-1: IXE_Q4W] |   0.810|    0.810|      0.015|       0.015| -0.021|
|pred[UNCOVER-1: SEC_150] |   0.686|    0.687|      0.042|       0.041| -0.021|
|pred[UNCOVER-1: SEC_300] |   0.783|    0.784|      0.037|       0.035| -0.016|
|pred[UNCOVER-2: PBO]     |   0.046|    0.046|      0.008|       0.008| -0.016|
|pred[UNCOVER-2: ETN]     |   0.419|    0.420|      0.022|       0.021| -0.008|
|pred[UNCOVER-2: IXE_Q2W] |   0.882|    0.882|      0.013|       0.012|  0.004|
|pred[UNCOVER-2: IXE_Q4W] |   0.782|    0.782|      0.017|       0.016|  0.015|
|pred[UNCOVER-2: SEC_150] |   0.649|    0.649|      0.041|       0.041| -0.002|
|pred[UNCOVER-2: SEC_300] |   0.753|    0.753|      0.038|       0.035|  0.002|
|pred[UNCOVER-3: PBO]     |   0.077|    0.078|      0.011|       0.011| -0.012|
|pred[UNCOVER-3: ETN]     |   0.529|    0.529|      0.021|       0.021| -0.003|
|pred[UNCOVER-3: IXE_Q2W] |   0.928|    0.928|      0.009|       0.009|  0.010|
|pred[UNCOVER-3: IXE_Q4W] |   0.855|    0.855|      0.013|       0.013|  0.022|
|pred[UNCOVER-3: SEC_150] |   0.746|    0.746|      0.035|       0.035|  0.001|
|pred[UNCOVER-3: SEC_300] |   0.832|    0.832|      0.029|       0.028|  0.007|

### (4) Heterogeneity-prior sensitivity (RE) — 8 priors from one grid

All 8 priors come from the single RE fit above (1.51 s). One multinma RE fit costs ~51 s, so the panel would take ≈ 7 min. Spot check, multinma refit under HN(0.5): max |z| = 0.05 over d, β, τ (τ: springs 0.177 vs multinma 0.178).

|prior         | log_bf_re_fe| mean.d[ETN]| mean.d[IXE_Q2W]| mean.d[SEC_300]| mean.tau|
|:-------------|------------:|-----------:|---------------:|---------------:|--------:|
|hn:2.5        |       -1.608|       1.561|           2.977|           2.434|    0.192|
|hn:1          |       -0.714|       1.561|           2.976|           2.434|    0.188|
|hn:0.5        |       -0.091|       1.560|           2.975|           2.435|    0.177|
|hc:1          |       -0.961|       1.561|           2.976|           2.435|    0.185|
|exp:2         |       -0.124|       1.559|           2.973|           2.436|    0.164|
|unif:2        |       -1.155|       1.561|           2.977|           2.434|    0.193|
|ln:-2.56:1.74 |        0.132|       1.560|           2.975|           2.436|    0.178|
|ln:-3.95:1.34 |        0.588|       1.557|           2.970|           2.439|    0.141|

`log_bf_re_fe`: log Bayes factor for random vs fixed effects (importance-corrected partition functions), free from the same grid.

