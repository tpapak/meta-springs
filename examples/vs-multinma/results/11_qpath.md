## Example 11 — Path-based inconsistency (Q_path) in springs

Tahmasebi, Davies, Papakonstantinou, Rücker & Nikolakopoulou (Stat Med 2026). Spring reading: each direct comparison is a spring (rest length = pooled direct estimate, stiffness = 1/variance); the hat-matrix row of comparison i-j is the force flow through the springs when i and j are pulled apart; its sign orients the springs, and every directed i → j route is a path of evidence. Q_path tests whether the paths agree with the equilibrium.

|comparison   | paths| independent|       Q| df|      p|paper                                 |
|:------------|-----:|-----------:|-------:|--:|------:|:-------------------------------------|
|1 vs 3       |     3|           3| 11.1111|  2| 0.0039|Q 11.11, df 2                         |
|2 vs 3       |     3|           3| 11.1111|  2| 0.0039|Q 11.11, df 2                         |
|metf vs plac |     7|           7| 22.4590|  6| 0.0010|7 paths, Q 22.45, df 6, p 0.001       |
|plac vs sulf |    10|           7| 22.4590|  6| 0.0010|10 -> 7 paths, Q 22.45, df 6, p 0.001 |

Network estimate metformin vs placebo: springs -1.11411, netmeta common effect -1.11411.

Paths for metformin vs placebo:

|path                         | estimate| variance|independent | row_score|
|:----------------------------|--------:|--------:|:-----------|---------:|
|metf -> acar -> plac         |  -1.0059|   0.1707|TRUE        |    4.0074|
|metf -> piog -> plac         |  -1.4600|   0.0233|TRUE        |   12.6479|
|metf -> piog -> rosi -> plac |  -1.2083|   0.0435|TRUE        |    6.9049|
|metf -> plac                 |  -1.1520|   0.0061|TRUE        |    8.2882|
|metf -> rosi -> plac         |  -1.0752|   0.0289|TRUE        |    6.8499|
|metf -> sulf -> acar -> plac |  -0.7759|   0.0579|TRUE        |    8.5546|
|metf -> sulf -> rosi -> plac |  -0.3183|   0.0374|TRUE        |   18.2154|

Netpath plot: `results/11_netpath_metf_plac.png`.

Note: path estimates match the paper's Table 1 except the acarbose path through the three-arm study (−1.006 vs −1.02; variance 0.171 vs 0.15), likely a different multi-arm variance reduction; Q_path matches.
