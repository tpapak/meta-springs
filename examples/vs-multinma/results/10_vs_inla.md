## Example 10 — Where springs work and INLA / nmaINLA cannot: non-linear aggregation

INLA requires every observation to depend on **one** linear predictor. A non-linear link alone is fine (binomial GLMMs); aggregation with a linear link is fine (the mean of η is η at the mean, Example 9). The ML-NMR aggregate arm combines both: its likelihood depends on p̄ = mean_q Φ(η_q) over 64 integration points, a non-linear function of many linear predictors. That is exactly where aggregation bias arises, and it is outside INLA's model class.

Full psoriasis network (Example 8), fixed effects. Reference: multinma's ML-NMR (Stan).

| model | what it is | can INLA / nmaINLA fit it? |
|---|---|---|
| **A. springs ML-NMR** | IPD patients + AgD arms as averaged non-linear springs | **no** (non-linear aggregation) |
| B. AgD at mean covariates | same IPD, each AgD arm as Φ(η at its mean covariates) | INLA: yes (one η per observation) — but it ignores aggregation bias |
| C. arm-total NMA | all data as arm totals, no individual covariates | nmaINLA: yes |

### Aggregation bias, arm by arm

At the ML-NMR equilibrium: the arm's predicted PASI 75 rate under the correct averaged force law vs the rate at the arm's mean covariates (what model B uses).

|arm               | observed| ml_nmr| at_mean| bias_pp|
|:-----------------|--------:|------:|-------:|-------:|
|CLEAR: SEC_300    |    0.910|  0.911|   0.912|   0.158|
|CLEAR: UST        |    0.791|  0.790|   0.802|   1.170|
|ERASURE: PBO      |    0.045|  0.049|   0.048|  -0.116|
|ERASURE: SEC_150  |    0.716|  0.718|   0.719|   0.166|
|ERASURE: SEC_300  |    0.816|  0.809|   0.811|   0.233|
|FEATURE: PBO      |    0.000|  0.034|   0.033|  -0.085|
|FEATURE: SEC_150  |    0.695|  0.637|   0.639|   0.126|
|FEATURE: SEC_300  |    0.746|  0.755|   0.757|   0.239|
|FIXTURE: ETN      |    0.440|  0.441|   0.439|  -0.240|
|FIXTURE: PBO      |    0.049|  0.035|   0.034|  -0.073|
|FIXTURE: SEC_150  |    0.670|  0.677|   0.678|   0.124|
|FIXTURE: SEC_300  |    0.771|  0.782|   0.784|   0.183|
|JUNCTURE: PBO     |    0.033|  0.060|   0.059|  -0.102|
|JUNCTURE: SEC_150 |    0.717|  0.727|   0.730|   0.305|
|JUNCTURE: SEC_300 |    0.867|  0.825|   0.827|   0.232|

Largest gap: 1.2 percentage points (CLEAR: UST). The gaps are largest where Φ is most curved — response rates far from 50%.

### Consequences for the estimates

* **A (springs ML-NMR) vs multinma:** max |z| 0.043 over treatment effects and target-population effects; 0.045 over effects in the 9 study populations.
* **B (INLA-representable) vs multinma:** max |z| 0.52 over treatment effects and target-population effects; 0.54 over the 9 study populations — the aggregation bias propagates into the estimates.
* **C (nmaINLA's model):** no individual covariates, so no population-specific or target-population effects at all; its treatment effects are population-averaged over the trial mix and differ from the ML-NMR effects by up to 0.8 posterior SDs (a different estimand, not only a bias).

Probit scale, mean; z = (estimate − multinma) / multinma SD:

|parameter          | multinma| A: springs ML-NMR|    z A| B: AgD at mean covariates|    z B| C: arm-total NMA|    z C|
|:------------------|--------:|-----------------:|------:|-------------------------:|------:|----------------:|------:|
|d[ETN]             |    1.605|             1.605| -0.001|                     1.594| -0.134|            1.571| -0.446|
|d[IXE_Q2W]         |    2.989|             2.990|  0.012|                     2.978| -0.130|            2.951| -0.442|
|d[IXE_Q4W]         |    2.573|             2.574|  0.017|                     2.564| -0.106|            2.534| -0.480|
|d[SEC_150]         |    2.243|             2.246|  0.036|                     2.221| -0.238|            2.212| -0.344|
|d[SEC_300]         |    2.566|             2.569|  0.035|                     2.544| -0.242|            2.531| -0.383|
|d[target: ETN]     |    1.091|             1.094|  0.012|                     1.080| -0.042|               NA|     NA|
|d[target: IXE_Q2W] |    2.793|             2.800|  0.030|                     2.784| -0.036|               NA|     NA|
|d[target: IXE_Q4W] |    2.377|             2.384|  0.031|                     2.372| -0.020|               NA|     NA|
|d[target: SEC_150] |    2.046|             2.055|  0.037|                     2.028| -0.076|               NA|     NA|
|d[target: SEC_300] |    2.369|             2.380|  0.043|                     2.350| -0.079|               NA|     NA|
|d[target: UST]     |    2.445|             2.459|  0.027|                     2.369| -0.151|               NA|     NA|
|d[UST]             |    2.131|             2.128| -0.023|                     2.055| -0.518|            2.008| -0.832|

**Why springs can:** an AgD arm is a single spring whose force law is the average of its patients' non-linear force laws. Its energy, force and stiffness are computed directly (with the two-parameter binomial and its exact derivatives), so the same equilibrium / curvature / importance machinery applies — and with it the prior–likelihood separation and the federated protocol.
