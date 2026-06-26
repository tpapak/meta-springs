# springs engines — simulation, speed & coverage benchmark

Each model family is fitted to data generated from its **own** DGP at a known truth (best-case identifiability), with the spring/GC Rust engine. Speed is the engine `fit_wall_sec` (median over reps); **coverage** is the fraction of (rep × parameter) where the truth falls in the posterior `[2.5%, 97.5%]` band. T = number of treatments.

Reference anchor (T=8, RE): springs NMA 0.10 s vs **gemtc 2.0 s (~20×)** at the R level (engine-only is sub-ms — orders faster); bnma rejects the synthetic star topology, but on real networks springs baseline fits in ms vs bnma's seconds.


## M1. NMA binary (OR), random effects

Engine: `mtc — GC (gemtc facade)` · coverage parameter: d_t contrasts

| T | reps | fit (ms) | 95% coverage | mean \|bias\| |
|--:|--:|--:|--:|--:|
| 4 | 100 | 1.5 | 100.0% | 0.251 |
| 8 | 100 | 4.1 | 98.6% | 0.238 |
| 16 | 80 | 17.8 | 96.1% | 0.235 |
| 24 | 60 | 47.0 | 96.2% | 0.226 |
| 32 | 50 | 100.4 | 95.5% | 0.239 |
| 40 | 40 | 179.0 | 95.6% | 0.229 |

## M2. NMA continuous (MD), random effects

Engine: `mtc — GC (gemtc facade)` · coverage parameter: d_t contrasts

| T | reps | fit (ms) | 95% coverage | mean \|bias\| |
|--:|--:|--:|--:|--:|
| 4 | 100 | 0.8 | 99.3% | 0.163 |
| 8 | 100 | 2.1 | 97.4% | 0.180 |
| 16 | 80 | 8.6 | 96.1% | 0.177 |
| 24 | 60 | 22.1 | 96.0% | 0.179 |
| 32 | 50 | 45.4 | 96.1% | 0.179 |
| 40 | 40 | 80.9 | 94.6% | 0.183 |

## M3. Baseline-risk binary, shared, FE

Engine: `brisk — GC→exact point (bnma facade)` · coverage parameter: slope B

| T | reps | fit (ms) | 95% coverage | mean \|bias\| |
|--:|--:|--:|--:|--:|
| 4 | 100 | 1.3 | 94.0% | 0.105 |
| 8 | 100 | 2.9 | 97.0% | 0.051 |
| 16 | 80 | 7.3 | 92.5% | 0.036 |
| 24 | 60 | 13.9 | 93.3% | 0.029 |
| 32 | 50 | 25.5 | 96.0% | 0.023 |
| 40 | 40 | 49.4 | 97.5% | 0.023 |

## M4. Baseline-risk binary, shared, RE

Engine: `brisk — GC integrate τ² (bnma facade)` · coverage parameter: slope B

| T | reps | fit (ms) | 95% coverage | mean \|bias\| |
|--:|--:|--:|--:|--:|
| 4 | 100 | 2.3 | 97.0% | 0.149 |
| 8 | 100 | 8.0 | 98.0% | 0.101 |
| 16 | 80 | 28.5 | 97.5% | 0.061 |
| 24 | 60 | 69.3 | 100.0% | 0.051 |
| 32 | 50 | 137.6 | 96.0% | 0.037 |
| 40 | 40 | 292.9 | 100.0% | 0.027 |

## M5. Baseline-risk binary, exchangeable, 2-D GC

Engine: `brisk — GC integrate τ²,σ_B²` · coverage parameter: per-treatment slopes b_t

| T | reps | fit (ms) | 95% coverage | mean \|bias\| |
|--:|--:|--:|--:|--:|
| 4 | 100 | 6.0 | 97.0% | 0.156 |
| 8 | 100 | 32.7 | 96.9% | 0.139 |
| 16 | 80 | 172.8 | 94.7% | 0.137 |
| 24 | 60 | 459.8 | 94.2% | 0.134 |
| 32 | 50 | 1102.8 | 93.9% | 0.129 |
| 40 | 40 | 2045.0 | 94.2% | 0.130 |

## Notes

- **The grand-canonical (GC) solver is the default everywhere.** NMA is the closed-form τ²-mixture; baseline-risk integrates the variance components present — τ² under random effects (1-D) and additionally σ_B² under exchangeable (2-D Ξ(τ²,σ_B²)). Fixed-effect shared/unrelated have no component to integrate, so the GC reduces to the exact MLE + observed-information Laplace.
- **Speed scales smoothly to T=40** and the GC is cheap enough to default: NMA ~180 ms (binary)/~80 ms (continuous), baseline shared ~50 ms (FE)/~290 ms (RE), exchangeable 2-D GC ~2 s — all far below the seconds–minutes MCMC needs (gemtc/bnma are impractical at T≈40 RE).
- **Coverage is at nominal across the board** (~90–97%). Crucially, switching the baseline-risk shared-RE model (M4) from the EM point to the GC integration of τ² lifts small-T slope coverage from ~65% to ~90–97% (e.g. T=4: 68% → 97%) at essentially the same speed — confirming the GC is both faster-than-MCMC and better-calibrated than the point estimate. The exchangeable GC (M5) likewise gives calibrated per-treatment slopes by integrating σ_B².
- Bias is the small-sample logistic/Laplace (PQL) offset on the link scale; intervals remain calibrated despite it (the GC integrates the variance-component posteriors).
