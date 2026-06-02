# NMR verification + performance benchmark: springs (Rust / Haskell) vs gemtc

## Methods compared
- **Rust** `gc-rust/examples/nmr_fe.rs` — FE shared-β WLS via nalgebra LU (compiled).
- **Haskell** `Data.Meta.NMA.nmaRegression` (FE) / `nmaRegressionRE` (RE) — dense
  list-based (`[[Double]]`) Gaussian elimination.
- **gemtc** — Bayesian MCMC (JAGS), shared regressor.

## Benchmark A — T=20 network, fixed-effect shared β
76 studies, 152 arms, np=96 (`test/nmr_t20_gen.R`; truth β=0.30).

| method | β̂ | wall-clock / fit | speedup vs gemtc |
|---|---|---|---|
| **Rust** (nalgebra) | 0.30483 | **0.12 ms** | **~35,000×** |
| Haskell (`nmaRegression`) | 0.30483 | 427 ms | ~10× |
| gemtc (MCMC, 20k iter) | 0.3042 | 4140 ms | 1× |

**Verification:** Rust and Haskell agree to 5 dp (0.30483 — same WLS); gemtc 0.3042
(MCMC + covariate centering). Rust ≈ **3600× faster than Haskell**, ~35,000× vs gemtc.
The Haskell gap is its naive `[[Double]]` solver (≈O(np⁴) with list-indexing overhead);
fine for small networks, slow at np≈100 — Rust/nalgebra is the performant path.

## Benchmark B — random-effects shared β (3-trt, 20 studies)
Reusing the 200-rep continuous simulation (truth β=0.30, τ²=0.10).

| method | per-fit | note |
|---|---|---|
| Haskell `nmaRegressionRE` (REML/EM) | 316 ms | 200 fits, deterministic |
| gemtc (random, 20k iter) | 1350 ms | 15 fits, MCMC |

springs ~4× faster (the EM does many naive `matInv` steps; a Rust RE port would be far
faster). Estimates verified earlier: `nmaRegressionRE` == netmeta `netmetareg` exactly,
β/τ² match gemtc (see `nmr_gc_reml_simulation.md`).

## Honest notes
- The Rust NMR is FE shared-β only (the WLS core); RE / exchangeable / GC are Haskell-only
  so far. Porting the RE/GC to Rust would carry the 3600× speedup to those too.
- gemtc is exact-Bayesian (full posterior); the springs FE/REML are point/Laplace — the
  speed gap is partly the price of MCMC's exact posterior. For the posterior, the spring GC
  (`nmaRegressionPosteriorB`) is the closed-form competitor (no MCMC).
- Timing is wall-clock on this machine; gemtc excludes JAGS compile (warm).
