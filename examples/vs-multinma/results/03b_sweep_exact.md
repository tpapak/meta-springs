## Example 3b — How often does the heterogeneity prior change a conclusion? (exact)

171 binary nmadb networks × 15 priors (the panel of Example 2). A *flip*: a basic comparison whose 95% CrI excludes 0 under some priors and includes it under others. springs sweep: 20 s per run.

Example 3 computed interval ends from 4 000 draws per prior. Here they are also computed **exactly**, from the τ²-mixture-of-Gaussians posterior (bisection on the mixture CDF): no Monte Carlo error.

|count                           | networks| % of networks| comparisons|   of|
|:-------------------------------|--------:|-------------:|-----------:|----:|
|draws, seed 1 (as in Example 3) |       77|          45.0|         119| 1062|
|draws, seed 2                   |       80|          46.8|         127| 1062|
|exact mixture                   |       71|          41.5|         111| 1062|
|exact, clear (margin 0.05 SD)   |       56|          32.7|          88| 1062|
|exact, clear (margin 0.10 SD)   |       47|          27.5|          72| 1062|
|exact, vague priors only        |       50|          29.2|          68| 1062|
|exact, vague only, clear 0.10   |       24|          14.0|          32| 1062|

* **Draw noise is real.** The two seeds flag 77 and 80 networks; 7 networks are flagged by one seed but not the other. Interval ends from 4 000 draws differ from the exact ones by a median 0.033 and up to 0.32 posterior SDs.
* **Exact count: 71 of 171 networks (42%).** Requiring the interval to cross 0 clearly (by ≥ 0.10 posterior SD on both sides) leaves 47 (28%).

### What an MCMC sweep would report

The exact interval ends perturbed by the Monte Carlo error of a 2.5%/97.5% quantile at a given effective sample size (Example 1: multinma's minimum bulk ESS was 70–1 600), 50 replicate sweeps each:

| effective draws| interval-end error (SDs)| networks flagged, mean| % of networks|range over 50 runs |
|---------------:|------------------------:|----------------------:|-------------:|:------------------|
|            4000|                    0.042|                   76.4|          44.7|73–80              |
|            1000|                    0.084|                   84.1|          49.2|80–88              |
|             300|                    0.154|                   97.0|          56.7|92–104             |
|             100|                    0.267|                  112.5|          65.8|107–118            |

Monte Carlo error adds spurious flips, and more of them at lower effective sample sizes. MCMC can only shrink this error by sampling longer; the spring posterior removes it.
