## Example 1 — speed at equal accuracy

multinma: 4 chains x 2000 iter (1000 warm-up), 4 cores in parallel. springs: 4000 iid draws, single thread. Prior HalfNormal(tau; 1), effects/baselines N(0,100^2) (multinma) vs flat (springs).

|                               |network                        |outcome    | studies| treatments| springs_engine_s| springs_R_s| multinma_s| speedup| multinma_min_ESS| springs_ESS_per_s| multinma_ESS_per_s| max_abs_z|sd_ratio_range |
|:------------------------------|:------------------------------|:----------|-------:|----------:|----------------:|-----------:|----------:|-------:|----------------:|-----------------:|------------------:|---------:|:--------------|
|smoking (multinma)             |smoking (multinma)             |binary     |      24|          4|             0.01|        0.03|       1.81|   60.40|          1124.27|         133333.33|             620.46|      0.16|0.99-1.02      |
|thrombolytics (multinma)       |thrombolytics (multinma)       |binary     |      50|          9|             0.03|        0.07|       3.32|   48.09|           250.35|          57971.01|              75.45|      0.10|0.90-1.03      |
|atrial_fibrillation (multinma) |atrial_fibrillation (multinma) |binary     |      26|         17|             0.04|        0.08|       3.53|   46.43|           608.00|          52631.58|             172.29|      0.34|0.80-1.04      |
|parkinsons (multinma)          |parkinsons (multinma)          |continuous |       7|          5|             0.00|        0.02|       0.81|   33.75|          1268.92|         166666.67|            1566.57|      0.05|1.01-1.04      |
|nmadb_479969                   |nmadb_479969                   |binary     |      95|         21|             0.15|        0.18|       7.45|   41.37|          1092.47|          22222.22|             146.70|      0.36|0.88-1.04      |
|nmadb_501194                   |nmadb_501194                   |binary     |      99|         17|             0.72|        0.75|      15.79|   20.97|          1657.91|           5312.08|             104.99|      0.15|0.95-1.03      |
|nmadb_501232                   |nmadb_501232                   |binary     |     111|         12|             0.17|        0.20|       5.25|   26.62|           362.74|          20304.57|              69.16|      0.16|0.93-1.05      |

`max_abs_z` = largest |springs mean − multinma mean| / multinma posterior SD over all d[·] and tau; `sd_ratio` = springs SD / multinma SD.

### Per-parameter agreement

|                                  |network                        |parameter                                    | springs_mean| multinma_mean| springs_sd| multinma_sd| z_diff|
|:---------------------------------|:------------------------------|:--------------------------------------------|------------:|-------------:|----------:|-----------:|------:|
|smoking (multinma).1              |smoking (multinma)             |d[Group counselling]                         |        1.053|         1.095|      0.422|       0.424| -0.100|
|smoking (multinma).2              |smoking (multinma)             |d[Individual counselling]                    |        0.802|         0.839|      0.230|       0.225| -0.164|
|smoking (multinma).3              |smoking (multinma)             |d[Self-help]                                 |        0.477|         0.480|      0.385|       0.386| -0.009|
|smoking (multinma).4              |smoking (multinma)             |tau                                          |        0.805|         0.811|      0.170|       0.172| -0.036|
|thrombolytics (multinma).1        |thrombolytics (multinma)       |d[ASPAC]                                     |        0.035|         0.041|      0.097|       0.095| -0.068|
|thrombolytics (multinma).2        |thrombolytics (multinma)       |d[Acc t-PA]                                  |       -0.238|        -0.249|      0.106|       0.113|  0.096|
|thrombolytics (multinma).3        |thrombolytics (multinma)       |d[PTCA]                                      |       -0.525|        -0.538|      0.131|       0.132|  0.096|
|thrombolytics (multinma).4        |thrombolytics (multinma)       |d[SK + t-PA]                                 |       -0.066|        -0.067|      0.118|       0.116|  0.009|
|thrombolytics (multinma).5        |thrombolytics (multinma)       |d[TNK]                                       |       -0.233|        -0.231|      0.169|       0.173| -0.012|
|thrombolytics (multinma).6        |thrombolytics (multinma)       |d[UK]                                        |       -0.229|        -0.228|      0.233|       0.239| -0.005|
|thrombolytics (multinma).7        |thrombolytics (multinma)       |d[r-PA]                                      |       -0.176|        -0.188|      0.126|       0.140|  0.091|
|thrombolytics (multinma).8        |thrombolytics (multinma)       |d[t-PA]                                      |       -0.017|        -0.018|      0.080|       0.084|  0.010|
|thrombolytics (multinma).9        |thrombolytics (multinma)       |tau                                          |        0.096|         0.099|      0.077|       0.076| -0.042|
|atrial_fibrillation (multinma).1  |atrial_fibrillation (multinma) |d[Acenocoumarol]                             |       -1.439|        -1.533|      0.803|       0.845|  0.111|
|atrial_fibrillation (multinma).2  |atrial_fibrillation (multinma) |d[Alternate day aspirin]                     |       -1.292|        -1.763|      1.105|       1.381|  0.341|
|atrial_fibrillation (multinma).3  |atrial_fibrillation (multinma) |d[Dipyridamole]                              |       -0.162|        -0.169|      0.418|       0.400|  0.019|
|atrial_fibrillation (multinma).4  |atrial_fibrillation (multinma) |d[Fixed dose warfarin]                       |        0.170|         0.172|      0.429|       0.447| -0.006|
|atrial_fibrillation (multinma).5  |atrial_fibrillation (multinma) |d[Fixed dose warfarin + low dose aspirin]    |       -0.270|        -0.276|      0.400|       0.401|  0.015|
|atrial_fibrillation (multinma).6  |atrial_fibrillation (multinma) |d[Fixed dose warfarin + medium dose aspirin] |        0.126|         0.122|      0.364|       0.362|  0.012|
|atrial_fibrillation (multinma).7  |atrial_fibrillation (multinma) |d[High dose aspirin]                         |       -0.231|        -0.265|      0.724|       0.783|  0.044|
|atrial_fibrillation (multinma).8  |atrial_fibrillation (multinma) |d[Indobufen]                                 |       -0.499|        -0.526|      0.482|       0.487|  0.056|
|atrial_fibrillation (multinma).9  |atrial_fibrillation (multinma) |d[Low adjusted dose anti-coagulant]          |       -1.024|        -1.051|      0.351|       0.362|  0.073|
|atrial_fibrillation (multinma).10 |atrial_fibrillation (multinma) |d[Low dose aspirin]                          |       -0.138|        -0.142|      0.205|       0.203|  0.018|
|atrial_fibrillation (multinma).11 |atrial_fibrillation (multinma) |d[Low dose aspirin + copidogrel]             |       -0.223|        -0.253|      0.405|       0.405|  0.073|
|atrial_fibrillation (multinma).12 |atrial_fibrillation (multinma) |d[Low dose aspirin + dipyridamole]           |       -0.468|        -0.484|      0.437|       0.437|  0.038|
|atrial_fibrillation (multinma).13 |atrial_fibrillation (multinma) |d[Medium dose aspirin]                       |       -0.365|        -0.373|      0.214|       0.221|  0.040|
|atrial_fibrillation (multinma).14 |atrial_fibrillation (multinma) |d[Standard adjusted dose anti-coagulant]     |       -0.744|        -0.763|      0.194|       0.197|  0.097|
|atrial_fibrillation (multinma).15 |atrial_fibrillation (multinma) |d[Triflusal]                                 |       -0.141|        -0.099|      0.640|       0.639| -0.065|
|atrial_fibrillation (multinma).16 |atrial_fibrillation (multinma) |d[Ximelagatran]                              |       -0.826|        -0.848|      0.331|       0.319|  0.070|
|atrial_fibrillation (multinma).17 |atrial_fibrillation (multinma) |tau                                          |        0.282|         0.277|      0.125|       0.134|  0.039|
|parkinsons (multinma).1           |parkinsons (multinma)          |d[2]                                         |       -1.849|        -1.825|      0.463|       0.444| -0.053|
|parkinsons (multinma).2           |parkinsons (multinma)          |d[3]                                         |       -0.487|        -0.482|      0.584|       0.578| -0.009|
|parkinsons (multinma).3           |parkinsons (multinma)          |d[4]                                         |       -0.524|        -0.518|      0.575|       0.567| -0.010|
|parkinsons (multinma).4           |parkinsons (multinma)          |d[5]                                         |       -0.815|        -0.812|      0.678|       0.667| -0.003|
|parkinsons (multinma).5           |parkinsons (multinma)          |tau                                          |        0.319|         0.316|      0.276|       0.270|  0.012|
|nmadb_479969.1                    |nmadb_479969                   |d[10]                                        |        0.340|         0.346|      0.520|       0.546| -0.011|
|nmadb_479969.2                    |nmadb_479969                   |d[11]                                        |       -0.568|        -0.589|      0.843|       0.834|  0.025|
|nmadb_479969.3                    |nmadb_479969                   |d[12]                                        |       -0.199|        -0.200|      0.320|       0.328|  0.004|
|nmadb_479969.4                    |nmadb_479969                   |d[13]                                        |        0.888|         0.901|      0.295|       0.302| -0.042|
|nmadb_479969.5                    |nmadb_479969                   |d[14]                                        |        0.090|         0.087|      0.503|       0.504|  0.005|
|nmadb_479969.6                    |nmadb_479969                   |d[15]                                        |        1.265|         1.292|      0.372|       0.386| -0.071|
|nmadb_479969.7                    |nmadb_479969                   |d[16]                                        |        0.212|         0.207|      0.370|       0.382|  0.014|
|nmadb_479969.8                    |nmadb_479969                   |d[17]                                        |        1.025|         1.020|      0.907|       0.940|  0.005|
|nmadb_479969.9                    |nmadb_479969                   |d[18]                                        |        0.300|         0.308|      0.623|       0.645| -0.013|
|nmadb_479969.10                   |nmadb_479969                   |d[19]                                        |        1.939|         2.091|      0.984|       1.039| -0.146|
|nmadb_479969.11                   |nmadb_479969                   |d[2]                                         |        1.201|         1.256|      0.242|       0.248| -0.222|
|nmadb_479969.12                   |nmadb_479969                   |d[20]                                        |        0.562|         0.570|      0.811|       0.837| -0.011|
|nmadb_479969.13                   |nmadb_479969                   |d[21]                                        |       -0.349|        -0.362|      0.486|       0.510|  0.024|
|nmadb_479969.14                   |nmadb_479969                   |d[3]                                         |        0.446|         0.453|      0.283|       0.291| -0.023|
|nmadb_479969.15                   |nmadb_479969                   |d[4]                                         |        0.754|         0.784|      0.268|       0.275| -0.108|
|nmadb_479969.16                   |nmadb_479969                   |d[5]                                         |        0.235|         0.241|      0.378|       0.388| -0.015|
|nmadb_479969.17                   |nmadb_479969                   |d[6]                                         |        1.537|         1.596|      0.768|       0.792| -0.075|
|nmadb_479969.18                   |nmadb_479969                   |d[7]                                         |        0.384|         0.408|      0.471|       0.476| -0.050|
|nmadb_479969.19                   |nmadb_479969                   |d[8]                                         |        2.293|         2.920|      1.541|       1.757| -0.357|
|nmadb_479969.20                   |nmadb_479969                   |d[9]                                         |        0.147|         0.138|      0.535|       0.554|  0.016|
|nmadb_479969.21                   |nmadb_479969                   |tau                                          |        0.611|         0.618|      0.085|       0.082| -0.085|
|nmadb_501194.1                    |nmadb_501194                   |d[10]                                        |       -0.570|        -0.587|      0.371|       0.380|  0.044|
|nmadb_501194.2                    |nmadb_501194                   |d[11]                                        |       -0.289|        -0.290|      0.412|       0.410|  0.003|
|nmadb_501194.3                    |nmadb_501194                   |d[12]                                        |       -0.806|        -0.844|      0.311|       0.308|  0.121|
|nmadb_501194.4                    |nmadb_501194                   |d[13]                                        |       -0.178|        -0.199|      0.526|       0.549|  0.039|
|nmadb_501194.5                    |nmadb_501194                   |d[14]                                        |       -0.621|        -0.660|      0.833|       0.845|  0.046|
|nmadb_501194.6                    |nmadb_501194                   |d[15]                                        |       -0.079|        -0.095|      0.675|       0.690|  0.023|
|nmadb_501194.7                    |nmadb_501194                   |d[16]                                        |       -0.924|        -0.950|      0.529|       0.544|  0.047|
|nmadb_501194.8                    |nmadb_501194                   |d[17]                                        |       -1.645|        -1.733|      0.798|       0.816|  0.107|
|nmadb_501194.9                    |nmadb_501194                   |d[2]                                         |        0.173|         0.172|      0.364|       0.372|  0.002|
|nmadb_501194.10                   |nmadb_501194                   |d[3]                                         |       -0.771|        -0.860|      0.588|       0.608|  0.147|
|nmadb_501194.11                   |nmadb_501194                   |d[4]                                         |       -0.588|        -0.622|      0.268|       0.281|  0.120|
|nmadb_501194.12                   |nmadb_501194                   |d[5]                                         |       -0.614|        -0.637|      0.316|       0.327|  0.071|
|nmadb_501194.13                   |nmadb_501194                   |d[6]                                         |       -0.402|        -0.428|      0.242|       0.241|  0.109|
|nmadb_501194.14                   |nmadb_501194                   |d[7]                                         |       -0.057|        -0.040|      0.583|       0.614| -0.026|
|nmadb_501194.15                   |nmadb_501194                   |d[8]                                         |       -0.237|        -0.254|      0.612|       0.633|  0.026|
|nmadb_501194.16                   |nmadb_501194                   |d[9]                                         |       -0.512|        -0.550|      0.339|       0.330|  0.115|
|nmadb_501194.17                   |nmadb_501194                   |tau                                          |        0.801|         0.810|      0.095|       0.099| -0.088|
|nmadb_501232.1                    |nmadb_501232                   |d[10]                                        |        0.076|         0.077|      0.089|       0.090| -0.012|
|nmadb_501232.2                    |nmadb_501232                   |d[11]                                        |        0.318|         0.320|      0.098|       0.099| -0.015|
|nmadb_501232.3                    |nmadb_501232                   |d[12]                                        |       -0.010|        -0.009|      0.115|       0.113| -0.010|
|nmadb_501232.4                    |nmadb_501232                   |d[2]                                         |        0.221|         0.222|      0.075|       0.075| -0.014|
|nmadb_501232.5                    |nmadb_501232                   |d[3]                                         |        0.099|         0.099|      0.088|       0.085|  0.000|
|nmadb_501232.6                    |nmadb_501232                   |d[4]                                         |        0.277|         0.277|      0.082|       0.081|  0.001|
|nmadb_501232.7                    |nmadb_501232                   |d[5]                                         |        0.017|         0.015|      0.068|       0.069|  0.024|
|nmadb_501232.8                    |nmadb_501232                   |d[6]                                         |       -0.015|        -0.020|      0.122|       0.120|  0.041|
|nmadb_501232.9                    |nmadb_501232                   |d[7]                                         |        0.007|         0.008|      0.147|       0.145| -0.008|
|nmadb_501232.10                   |nmadb_501232                   |d[8]                                         |        0.244|         0.245|      0.070|       0.069| -0.018|
|nmadb_501232.11                   |nmadb_501232                   |d[9]                                         |       -0.393|        -0.392|      0.126|       0.125| -0.008|
|nmadb_501232.12                   |nmadb_501232                   |tau                                          |        0.117|         0.109|      0.049|       0.053|  0.156|
