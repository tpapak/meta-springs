## Example 3 — prior sensitivity across nmadb (binary networks)

* Networks: 171 binary nmadb networks (5–145 studies, 4–29 treatments); 0 failed in springs.
* **springs: 171 networks × 15 priors = 2565 full posteriors in 16.4 s wall from R (11.6 s in the engine).**
* multinma, measured on 12 random networks (HN(τ;1)): mean 2.2 s per fit (median 1.7). Extrapolated to the same workload (13 expressible priors × 171 networks): **≈ 1.3 hours**.

### What the sweep shows

* 77 / 171 networks (45%) have at least one basic comparison whose 95% CrI excludes 0 under some priors but not others (all 15 priors).
* Restricting to the 10 vague priors only: 58 / 171 (34%).
* 119 / 1062 comparisons in total change significance status across the panel.
* Ratio max/min posterior-median τ across priors: median 1.85, 90th percentile 3.20.
* Bayes factor RE vs FE under HN(τ;0.5): log BF > 1 (favours RE) in 70, < −1 (favours FE) in 21, inconclusive in 80 networks.

### multinma sample: accuracy and time on randomly chosen networks

|network      | studies| treatments| springs_s| multinma_s| min_ESS| max_abs_z| median_abs_z| sd_ratio_min| sd_ratio_max|
|:------------|-------:|----------:|---------:|----------:|-------:|---------:|------------:|------------:|------------:|
|nmadb_501283 |      55|         17|      0.07|       3.47|  853.15|      0.14|         0.05|         0.93|         1.02|
|nmadb_480926 |       9|          8|      0.03|       1.38| 1105.23|      0.07|         0.02|         0.94|         1.01|
|nmadb_481150 |      31|         11|      0.04|       1.68|  580.39|      0.26|         0.05|         0.94|         1.02|
|nmadb_501253 |      13|         11|      0.03|       1.36| 1544.33|      0.13|         0.02|         0.97|         1.03|
|nmadb_482734 |      48|          4|      0.04|       2.33|  799.13|      0.44|         0.21|         0.93|         0.97|
|nmadb_501243 |      18|          7|      0.06|       1.80|  836.23|      0.09|         0.04|         0.90|         1.00|
|nmadb_501407 |      63|          4|      0.16|       6.58|  281.42|      0.05|         0.01|         0.87|         0.98|
|nmadb_501324 |      13|          4|      0.02|       1.20|  864.68|      0.08|         0.05|         0.96|         1.01|
|nmadb_480563 |       8|          5|      0.02|       1.63|  153.78|      0.13|         0.07|         0.85|         1.10|
|nmadb_481147 |       5|          4|      0.02|       0.60| 1057.18|      0.11|         0.03|         0.97|         1.05|
|nmadb_481588 |      26|         18|      0.04|       2.60|  242.38|      0.11|         0.07|         0.95|         1.02|
|nmadb_501209 |      20|         11|      0.03|       1.52| 1196.71|      0.23|         0.05|         0.95|         1.02|

