## Example 9 — Continuous outcomes: the natural spring

960 IPD patients in 3 trials + 7 AgD arms in 3 trials; treatments A (control), B, C, D (one active class); `~(x1 + x2) * .trt`, class-common effect modification; true d = (B -2.0, C -3.0, D -2.5), β1 = 1.0, β2 = 0.5, γ(x1 × active) = -0.8, τ = 0.4.

### (1a) fixed effects

* **springs 0.32 s** vs **multinma 8 s**: 25× faster. Importance ESS 9643.
* d, β, σ: max |z| 0.033 (median 0.014), SD ratio 0.98–1.01, 14 quantities
* population-specific relative effects (6 study populations × 3): max |z| 0.031 (median 0.008), SD ratio 0.97–1.02, 18 quantities

|parameter                | truth| springs| multinma| springs_sd| multinma_sd|      z|
|:------------------------|-----:|-------:|--------:|----------:|-----------:|------:|
|beta[x1:.trtclassactive] |  -0.8|  -0.718|   -0.715|      0.190|       0.194| -0.016|
|d[B]                     |  -2.0|  -1.910|   -1.911|      0.248|       0.249|  0.004|
|d[C]                     |  -3.0|  -3.167|   -3.171|      0.244|       0.246|  0.017|
|d[D]                     |  -2.5|  -2.962|   -2.971|      0.251|       0.257|  0.033|

### (1b) random effects

* **springs 0.68 s** (all 5 τ priors) vs **multinma 10 s**: 15× faster. Importance ESS 5777; τ springs 0.571 vs multinma 0.577 (truth 0.4).
* d, β, σ, τ: max |z| 0.047 (median 0.017), SD ratio 0.96–1.05, 15 quantities
* population-specific relative effects (6 study populations × 3): max |z| 0.066 (median 0.021), SD ratio 0.96–1.06, 18 quantities

|parameter                | truth| springs| multinma| springs_sd| multinma_sd|      z|
|:------------------------|-----:|-------:|--------:|----------:|-----------:|------:|
|beta[x1:.trtclassactive] |  -0.8|  -0.716|   -0.715|      0.208|       0.212| -0.004|
|d[B]                     |  -2.0|  -1.881|   -1.877|      0.476|       0.468| -0.008|
|d[C]                     |  -3.0|  -3.120|   -3.141|      0.456|       0.436|  0.047|
|d[D]                     |  -2.5|  -2.964|   -2.972|      0.481|       0.503|  0.017|
|tau                      |   0.4|   0.571|    0.577|      0.397|       0.392| -0.014|

### (2) Federated in one round: the equivalent spring is exact

Each IPD site runs one local query (`mlnmr_site --mode equivalent_spring`) and shares its sufficient statistics (XᵀX, Xᵀy, yᵀy, n) — aggregate quantities, no patient record. AgD sites share their published arms. Anyone can then fit the network.

|site  |data |shared                                        | numbers| patient records|
|:-----|:----|:---------------------------------------------|-------:|---------------:|
|IPD-1 |IPD  |equivalent spring (XᵀX, Xᵀy, yᵀy, n)          |     844|               0|
|IPD-2 |IPD  |equivalent spring (XᵀX, Xᵀy, yᵀy, n)          |     844|               0|
|IPD-3 |IPD  |equivalent spring (XᵀX, Xᵀy, yᵀy, n)          |    1266|               0|
|AgD-4 |AgD  |published arm means, SEs, covariate summaries |       6|               0|
|AgD-6 |AgD  |published arm means, SEs, covariate summaries |       9|               0|
|AgD-5 |AgD  |published arm means, SEs, covariate summaries |       6|               0|

* **One round**, 0.21 s. Equilibrium = pooled fit to **8.9e-14**; posterior (same seed) = pooled to **8.1e-13** posterior SDs. No energy round and no importance correction are needed for the federation itself: the equivalent spring reproduces the patients' energy exactly (engine test `equivalent_spring_is_exact_for_gaussian_patients`).
* Contrast with binary data (Examples 7–8): there the equivalent quadratic spring is only the Laplace approximation (0.24–0.36 SD off), and one extra round of energy evaluations restores exactness.
* Residual SDs are arm-specific (as in multinma), so each site's equivalent spring is one block per arm (XᵀX, Xᵀy, yᵀy, n) and is shared individually, like richer AgD. With a common σ, the springs could also be combined by secure summation.

