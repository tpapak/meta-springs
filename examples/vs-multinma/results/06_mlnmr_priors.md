## Example 6 — Prior investigation for ML-NMR from one spring fit

The plaque psoriasis ML-NMR of Example 5 (probit, 5 covariates, class-common interactions, `bernoulli2`). Target population for transport: 25 y disease, 60% BSA, 120 kg, PsA, prior systemic.

**Mechanism.** The energy is data springs (every patient and every integration point: the expensive part) + one prior spring per parameter + random-effect springs (1/τ²). The fit keeps its weighted importance pool; a new prior multiplies each weight by π_new(θ)/π_old(θ), and a τ prior reweights the τ² grid. No patient is evaluated again. The effective sample size (ESS) of the new weights flags priors that move the posterior too far from the pool; those are refitted (< 1 s).

### (A) Fixed effects: 11 priors from one fit

* Base fit (vignette priors) **0.88 s**; the other 10 posteriors **2.27 s in total**, incl. target-population effects. A reweighting itself takes a median **43 ms** (pool of 16000 importance draws); the 2 priors that moved the posterior too far for the pool were refitted automatically (≈ 0.9 s each). multinma: one Stan fit per prior (≈ 29 s each here, ≈ 5 min for the panel).
* Every reweighted posterior vs a **direct springs refit** under the same prior: max |z| ≤ 0.045 over 25 quantities (d, β, target-population effects); this is the Monte Carlo floor of 4 000 resampled draws (the unchanged vignette prior shows the same gap). Refits needed (ESS < 1000): 2 of 11.

Posterior mean (SD), probit scale; `target` = population-specific effect vs placebo in the target population.

|prior                                  |  ESS|refit |d IXE Q2W   |d SEC 300   |d ETN       |β bsa×IL    |β psa×IL     |target: IXE Q2W |target: SEC 300 |target: ETN |max &#124;z&#124; vs direct refit |
|:--------------------------------------|----:|:-----|:-----------|:-----------|:-----------|:-----------|:------------|:---------------|:---------------|:-----------|:---------------------------------|
|vignette: all N(0, 10²)                | 9214|FALSE |2.96 (0.09) |2.45 (0.12) |1.55 (0.08) |0.30 (0.49) |0.01 (0.19)  |2.76 (0.24)     |2.25 (0.26)     |1.06 (0.27) |0.045                             |
|reg N(0, 2.5²)                         | 9327|FALSE |2.95 (0.09) |2.45 (0.12) |1.55 (0.08) |0.28 (0.47) |0.01 (0.18)  |2.77 (0.24)     |2.26 (0.26)     |1.06 (0.27) |0.042                             |
|reg N(0, 1)                            | 8732|FALSE |2.95 (0.09) |2.45 (0.12) |1.55 (0.08) |0.23 (0.39) |0.01 (0.18)  |2.74 (0.22)     |2.23 (0.25)     |1.04 (0.25) |0.011                             |
|reg N(0, 0.5²)                         | 5978|FALSE |2.95 (0.09) |2.44 (0.12) |1.55 (0.08) |0.17 (0.29) |0.01 (0.16)  |2.71 (0.20)     |2.21 (0.23)     |1.02 (0.22) |0.029                             |
|reg N(0, 0.25²)                        | 1871|FALSE |2.94 (0.09) |2.44 (0.12) |1.54 (0.08) |0.11 (0.19) |-0.00 (0.13) |2.69 (0.17)     |2.19 (0.20)     |1.02 (0.19) |0.035                             |
|reg N(0, 0.1²) (≈ no EM)               |   36|TRUE  |2.93 (0.09) |2.44 (0.12) |1.53 (0.08) |0.04 (0.09) |-0.01 (0.07) |2.70 (0.14)     |2.20 (0.17)     |1.08 (0.15) |0.000                             |
|reg Cauchy(0, 0.5)                     | 5118|FALSE |2.95 (0.09) |2.44 (0.12) |1.54 (0.08) |0.16 (0.28) |0.00 (0.15)  |2.71 (0.19)     |2.20 (0.22)     |1.02 (0.22) |0.029                             |
|reg t3(0, 0.5)                         | 5702|FALSE |2.95 (0.09) |2.44 (0.12) |1.55 (0.08) |0.17 (0.29) |0.00 (0.16)  |2.71 (0.20)     |2.20 (0.22)     |1.02 (0.22) |0.018                             |
|trt N(0, 1) (sceptical)                | 7562|FALSE |2.90 (0.08) |2.38 (0.11) |1.50 (0.08) |0.30 (0.47) |0.01 (0.18)  |2.73 (0.24)     |2.20 (0.25)     |1.02 (0.27) |0.035                             |
|trt N(0, 0.5²) (very sceptical)        |  103|TRUE  |2.77 (0.08) |2.20 (0.11) |1.37 (0.07) |0.33 (0.44) |0.01 (0.17)  |2.61 (0.22)     |2.03 (0.24)     |0.90 (0.25) |0.000                             |
|intercept N(−1.5, 0.3²) (placebo ≈ 7%) | 9699|FALSE |2.93 (0.08) |2.42 (0.11) |1.53 (0.07) |0.30 (0.48) |0.01 (0.18)  |2.75 (0.24)     |2.24 (0.26)     |1.04 (0.27) |0.029                             |

**Against multinma** (a Stan refit per prior; d, β and the 5 target-population effects):

|prior                           |  ESS|refit | max &#124;z&#124;|SD ratio  | quantities| multinma s|
|:-------------------------------|----:|:-----|-----------------:|:---------|----------:|----------:|
|vignette: all N(0, 10²)         | 9214|FALSE |             0.033|0.98–1.02 |         25|     30.109|
|reg N(0, 0.5²)                  | 5978|FALSE |             0.039|0.96–1.03 |         25|     28.653|
|reg N(0, 0.1²) (≈ no EM)        |   36|TRUE  |             0.054|0.98–1.02 |         25|     28.907|
|reg Cauchy(0, 0.5)              | 5118|FALSE |             0.032|0.98–1.04 |         25|     28.683|
|trt N(0, 0.5²) (very sceptical) |  103|TRUE  |             0.027|0.98–1.02 |         25|     28.610|

### (B) Random effects: 4 interaction priors × 8 τ priors = 32 posteriors

* One RE fit (40-point τ² grid, 2 000 importance draws per point) **2.85 s**; all 32 posteriors (incl. target-population effects) **6.44 s**. multinma: 32 Stan fits × ≈ 49 s ≈ **26 min**.
* Minimum ESS over the τ priors, per interaction prior: reg Cauchy(0, 0.5) 4411; reg N(0, 0.5²) 5096; reg N(0, 1) 7425; reg N(0, 10²) 7846.
* Check cell (reg N(0, 0.5²), τ ~ HN(0.5)): reweighted vs direct springs refit max |z| = 0.032; vs **multinma** max |z| = 0.074, SD ratio 0.97–1.05 over 26 quantities (τ: springs 0.175 vs multinma 0.168; multinma 49 s).

**τ** (posterior mean (SD)) — rows: τ prior; columns: interaction prior:

|tau_prior     |reg N(0, 10²) |reg N(0, 1) |reg N(0, 0.5²) |reg Cauchy(0, 0.5) |
|:-------------|:-------------|:-----------|:--------------|:------------------|
|hn:2.5        |0.19 (0.13)   |0.19 (0.13) |0.19 (0.13)    |0.19 (0.13)        |
|hn:1          |0.19 (0.12)   |0.19 (0.12) |0.19 (0.12)    |0.19 (0.12)        |
|hn:0.5        |0.18 (0.11)   |0.18 (0.11) |0.17 (0.11)    |0.17 (0.11)        |
|hc:1          |0.18 (0.12)   |0.18 (0.12) |0.18 (0.12)    |0.18 (0.12)        |
|exp:2         |0.16 (0.11)   |0.16 (0.11) |0.16 (0.11)    |0.16 (0.11)        |
|unif:2        |0.19 (0.13)   |0.19 (0.13) |0.19 (0.13)    |0.19 (0.13)        |
|ln:-2.56:1.74 |0.18 (0.09)   |0.18 (0.09) |0.18 (0.09)    |0.18 (0.09)        |
|ln:-3.95:1.34 |0.14 (0.07)   |0.14 (0.07) |0.14 (0.07)    |0.14 (0.07)        |

**IXE Q2W vs placebo in the target population** (probit; mean (SD)):

|tau_prior     |reg N(0, 10²) |reg N(0, 1) |reg N(0, 0.5²) |reg Cauchy(0, 0.5) |
|:-------------|:-------------|:-----------|:--------------|:------------------|
|hn:2.5        |2.76 (0.28)   |2.73 (0.26) |2.71 (0.24)    |2.71 (0.24)        |
|hn:1          |2.76 (0.28)   |2.74 (0.26) |2.71 (0.23)    |2.70 (0.23)        |
|hn:0.5        |2.76 (0.28)   |2.73 (0.25) |2.71 (0.23)    |2.70 (0.23)        |
|hc:1          |2.76 (0.27)   |2.73 (0.26) |2.71 (0.23)    |2.70 (0.23)        |
|exp:2         |2.76 (0.27)   |2.73 (0.25) |2.71 (0.23)    |2.71 (0.22)        |
|unif:2        |2.76 (0.28)   |2.74 (0.25) |2.71 (0.24)    |2.70 (0.23)        |
|ln:-2.56:1.74 |2.76 (0.27)   |2.73 (0.25) |2.71 (0.23)    |2.70 (0.22)        |
|ln:-3.95:1.34 |2.76 (0.26)   |2.73 (0.24) |2.70 (0.22)    |2.70 (0.21)        |

### (C) Which priors can matter: prior share of the spring stiffness

Prior share = stiffness of the prior spring / total posterior stiffness ≈ Var(θ | data, prior) / prior variance (for a t prior, its expected curvature replaces 1/variance). It needs no refit: ≈ 0 means the data alone hold the parameter; large values mean the prior holds it. The vignette's N(0, 10²) priors hold nothing (≤ 0.3%); a sceptical N(0, 0.5²) prior on the effect modifiers holds the BSA interactions by about a third.

|parameter                           | SD, all N(0, 10²)| prior share, all N(0, 10²)| SD, reg N(0, 0.5²)| prior share, reg N(0, 0.5²)|
|:-----------------------------------|-----------------:|--------------------------:|------------------:|---------------------------:|
|d[ETN]                              |            0.0802|                     0.0001|             0.0796|                      0.0001|
|d[IXE_Q2W]                          |            0.0871|                     0.0001|             0.0865|                      0.0001|
|d[IXE_Q4W]                          |            0.0825|                     0.0001|             0.0814|                      0.0001|
|d[SEC_150]                          |            0.1151|                     0.0001|             0.1137|                      0.0001|
|d[SEC_300]                          |            0.1187|                     0.0001|             0.1175|                      0.0001|
|beta[durnpso]                       |            0.0613|                     0.0000|             0.0596|                      0.0142|
|beta[prevsys]                       |            0.1603|                     0.0003|             0.1411|                      0.0796|
|beta[bsa]                           |            0.4444|                     0.0020|             0.2507|                      0.2514|
|beta[weight]                        |            0.0284|                     0.0000|             0.0284|                      0.0032|
|beta[psa]                           |            0.1695|                     0.0003|             0.1454|                      0.0845|
|beta[durnpso:.trtclassTNFa blocker] |            0.0737|                     0.0001|             0.0720|                      0.0207|
|beta[durnpso:.trtclassIL blocker]   |            0.0674|                     0.0000|             0.0656|                      0.0172|
|beta[prevsys:.trtclassTNFa blocker] |            0.1879|                     0.0004|             0.1660|                      0.1103|
|beta[prevsys:.trtclassIL blocker]   |            0.1751|                     0.0003|             0.1548|                      0.0959|
|beta[bsa:.trtclassTNFa blocker]     |            0.5156|                     0.0027|             0.3049|                      0.3718|
|beta[bsa:.trtclassIL blocker]       |            0.4884|                     0.0024|             0.2852|                      0.3253|
|beta[weight:.trtclassTNFa blocker]  |            0.0353|                     0.0000|             0.0350|                      0.0049|
|beta[weight:.trtclassIL blocker]    |            0.0318|                     0.0000|             0.0318|                      0.0040|
|beta[psa:.trtclassTNFa blocker]     |            0.2087|                     0.0004|             0.1796|                      0.1290|
|beta[psa:.trtclassIL blocker]       |            0.1860|                     0.0003|             0.1610|                      0.1037|

