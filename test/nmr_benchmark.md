# NMR verification + performance benchmark: springs (Rust / Haskell) vs gemtc

## Methods
- **Rust** `gc-rust/examples/nmr_fe.rs` (FE) and `nmr_re_gc.rs` (RE + GC joint posterior),
  nalgebra (Cholesky) — compiled.
- **Haskell** `Data.Meta.NMA` — `nmaRegression` (FE), `nmaRegressionRE` (REML/EM),
  `nmaRegressionPosterior` (GC); dense list-based (`[[Double]]`) elimination.
- **gemtc** — Bayesian MCMC (JAGS), shared regressor.

## Verification — Rust == Haskell == gemtc
On the 3-treatment rep with heterogeneity (`/tmp/nmrgc/rep_001`, β truth 0.30, τ²=0.10):

| quantity | Rust | Haskell | gemtc |
|---|---|---|---|
| RE β | 0.40375 | 0.40375 | ~0.40 |
| RE τ² | 0.10351 | 0.10351 | — |
| GC β mean | 0.40375 | 0.4038 | — |
| GC β sd | 0.06945 | 0.0695 | — |
| GC β 95% | [0.2659, 0.5416] | [0.2659, 0.5416] | — |
| GC τ² mean | 0.14150 | 0.1415 | — |

Rust RE+GC reproduce Haskell **to 5 dp** (same WLS / EM / Gaussian-marginal). On T=20,
Rust RE τ²=**0.00423** = gemtc τ²=**0.00423** exactly; β 0.30483 (Rust) vs 0.3037 (gemtc).

## Benchmark — wall-clock per fit

**FE shared β, T=20** (76 studies, 152 arms, np=96):

| method | β̂ | time | vs gemtc |
|---|---|---|---|
| **Rust** | 0.30483 | **0.12 ms** | ~35,000× |
| Haskell `nmaRegression` | 0.30483 | 427 ms | ~10× |
| gemtc | 0.3042 | 4140 ms | 1× |

**RE shared β, T=20** (np=96):

| method | time | note |
|---|---|---|
| **Rust** `nmr_re_gc` RE | **98 ms** | β 0.30483, τ² 0.00423 |
| gemtc (random) | 5710 ms | β 0.3037, τ² 0.00423 — Rust **~58× faster** |
| Haskell `nmaRegressionRE` | **>3 min (didn't finish)** | naive O(np⁴) inverse × EM impractical at np=96 |

**GC joint posterior, T=20**:

| method | time | output |
|---|---|---|
| **Rust** `nmr_re_gc` GC | **62 ms** | full (β, τ²) posterior, closed form |
| gemtc | 5710 ms | full posterior via MCMC |

**RE shared β, 3-trt / 20-study** (τ²=0.10):

| method | per-fit |
|---|---|
| Rust `nmr_re_gc` RE | 0.50 ms |
| Haskell `nmaRegressionRE` | 316 ms |
| gemtc | 1350 ms |

## Takeaways (honest)
- **Rust is the performant path.** RE+GC ported to Rust reproduce Haskell exactly and run
  in tens of ms even at np=96, where the **Haskell RE/GC are impractical** (its naive
  `[[Double]]` ≈O(np⁴) inverse, repeated over EM iterations / a τ² grid). Rust RE is
  ~58× faster than gemtc on T=20; Rust GC gives the closed-form posterior in 62 ms vs
  gemtc's 5.7 s MCMC.
- **Estimates agree**: Rust = Haskell to 5 dp; τ² matches gemtc exactly on T=20; β matches
  (small gemtc difference = MCMC + covariate centering).
- Rust now covers FE, RE, and the GC joint posterior (shared β; exchangeable via the
  `exch` flag, same Gaussian machinery). Binary would wrap these in the IRLS linearisation
  (`linearizeBinaryRegression`), as in Haskell.
- gemtc is exact-Bayesian (full posterior); the spring GC is the closed-form competitor.
  Generators/timers: `test/nmr_t20_gen.R`, `test/nmr_t20_bench.hs`, `gc-rust/examples/nmr_*`.
