# gc-rust — Binomial Grand Canonical NMA (Rust port)

A LAPACK-backed Rust port of the binomial Grand Canonical pipeline from
this repo's Haskell library (`src/Data/Meta/RandomEffects.hs`). The Rust
binary reproduces the Haskell library's τ² posterior and per-contrast
Laplace mixture summaries on the same data, runs roughly **100× faster**,
and resolves the rare-events ill-conditioning that pure-Haskell `Mat.inv`
hits at large τ² (where one T=50 replicate stalled the Haskell solver
for 18 minutes; in `gc-rust` the same replicate runs in 4.5 s).

## Build

```
cargo build --release
```

(From the `gc-rust/` directory, or `cargo build --release --manifest-path gc-rust/Cargo.toml` from the repo root.)

The release profile uses `lto = "thin"` and `codegen-units = 1` for
consistent fit-to-fit timing.

## Run — single replicate (publication-style report)

```
./gc-rust/target/release/gc-rust \
  --input  test/diabetes.json \
  --output /tmp/diabetes_rust.json
```

Single-rep mode emits the full payload: τ² posterior summary, the entire
log-spaced (τ², weight) grid, and per-contrast point-by-grid-point
mixture data. Use this when you want to overlay GC posteriors against a
Bayesian comparator (multinma, bayesmeta) on a single analysis.

Schema is bit-compatible with the Haskell `proto_diabetes_gc.hs` output,
so the existing Python compare scripts in `test/` work unchanged.

## Run — multi-replicate (bias study)

```
./gc-rust/target/release/gc-rust \
  --multi-rep /tmp/t30_reps_bin \
  --output    /tmp/t30_reps_bin_gc_rust.json
```

Multi-rep mode iterates over `rep_*.json` files (sorted by name) in the
given directory. Schema is the lite version that matches
`proto_t30_reps_gc.hs` / `proto_t50_rare_gc.hs`: top-level
`{total_wall_sec, n_reps, results}`, per-rep `{rep, wall_sec, priors}`,
per-prior `{label, tau2, effects: [{to, mean, sd}]}`. The heavy
`tau2_grid` and effect `points` arrays are dropped to keep multi-rep
files small.

## CLI flags

- `--input <PATH>` — arm-level binary NMA JSON (one record per arm,
  fields: `study`, `treatment`, `events`, `n`). `treatment` accepts
  strings or integers; integers are stringified to match the Haskell
  `show TreatmentId` convention (so `1` and `"1"` round-trip
  identically).
- `--multi-rep <DIR>` — directory of `rep_*.json` files. Mutually
  exclusive with `--input`.
- `--output <PATH>` — output JSON path. Required.
- `--n-grid <INT>` — adaptive grid size (default 100). Applies to both
  modes.

## Output schemas

### Single-rep
Top-level keys: `dataset`, `fit_wall_sec`, `ref`, `priors`. Each prior
has `label`, `tau2` (mode/median/mean/CI), `tau2_grid` (full grid),
`effects` with `points` (per-grid-point mixture data).

### Multi-rep
Top-level: `total_wall_sec`, `n_reps`, `results: [...]`. Each rep:
`rep`, `wall_sec`, `priors: [...]`. Each prior: `label`, `tau2`
(mode/median/mean/CI), `effects: [{to, mean, sd}]`.

## Priors (hardcoded, in this order)

1. `Flat (REML)` — improper, gives the REML τ² mode and posterior
   summaries under the unweighted Z(τ²)
2. `HalfNormal(τ; σ=0.5)`
3. `HalfNormal(τ; σ=1)`
4. `HalfCauchy(τ; σ=0.5)`

Reweighting under any prior is essentially free given the cached log-Z
grid; this is the architectural advantage over multinma (which must
re-fit per prior).

## Verified working numbers (2026-04-27, MacBook)

| Mode | Dataset | wall | speedup vs Haskell |
|---|---|---:|---:|
| single-rep | `test/diabetes.json` (k=22, T=6, 4 three-arm trials) | **10 ms** | ~77× |
| multi-rep | `/tmp/t30_reps_bin/` (30 reps, T=30, k=60) | **4.4 s** | ~133× |
| multi-rep | `/tmp/t50_rare_bin/` (5 reps, T=50, k=100, ~25% zero-event arms) | **15.9 s** | ~106× |

The T=50 rare-events run includes the historical "rep 4" that stalled
the Haskell solver for 18 minutes; in `gc-rust` it converges in 4.5 s
with τ² posterior summaries within 8e-3 of the Haskell baseline on every
field.

## What's implemented

- `solver::newton_solve_reduced_bin` — reduced-Schur Newton on the
  `(k+T−1)`-dim system; LAPACK-backed LU via nalgebra.
- `solver::eval_xi_at` — Laplace `logZ(τ²)` per grid point with the
  exact eStar formula `Σ[n·log1p_exp(l) − e·l] + Σ u²/(2τ²_arm)`.
- Armijo line-search damping on the Newton step (kicks in only after
  ~30 plain-Newton iters so well-conditioned networks stay
  bit-identical to the un-damped reference).
- `gc::find_reml_mode` — bisection on the sign of `dlogZ/dτ²` (central
  finite difference); 1e-6 relative tolerance.
- `gc::adaptive_grid_taus` — `[max(1e-6, mode/30), max(2.0, mode·10)]`
  log-spaced grid (the upper bound is tighter than Haskell's ×30/5.0
  because LAPACK gives accurate solves there too, but the very high
  τ² regime contributes negligible posterior mass under any reasonable
  prior).
- `gc::posterior_under` — log-grid posterior summary
  (mode/median/mean/CI), with the +log τ² Jacobian for log-spaced
  grids.
- Three priors + Flat/REML.

## What's not implemented (intentional MVP scope)

- Continuous outcomes / GaussianSpring path.
- Full network variance pass — `gc-rust` uses the local Schur identity
  for ref-vs-target contrasts only; multi-step contrast variances would
  need the full inverse Hessian.
- Multi-contrast (k-step) credible regions.

## Validation

See [`VALIDATION.md`](VALIDATION.md) for the side-by-side comparison
against the Haskell reference outputs (single-rep diabetes + multi-rep
T=30 + multi-rep T=50). Worst-case numbers from the most recent run:

- Single-rep diabetes: max |Δ| over τ² posterior summaries = 1.9e-3,
  over per-contrast effect means/sds = 3.7e-6.
- Multi-rep T=30: τ² mean max |Δ| = 7e-4; effect mean (connected,
  sd<50) max |Δ| = 8e-4.
- Multi-rep T=50: τ² mean max |Δ| = 3.5e-3; rep-4 specifically (the
  Haskell stall) within 8e-3 of Haskell on every τ² field.

Residual differences trace to the REML-anchor difference between the
two pipelines (Haskell `springREMLBin` EM/Newton vs Rust dlogZ-bisection),
which shifts the `[mode/30, mode·10]` grid bounds slightly. Posteriors
under any single fixed grid agree to <1e-6.

## Common pitfalls

- The integer-vs-string treatment ID handling: ensure your input file
  uses one consistently per dataset (mixing within a single file is not
  supported).
- The reference treatment is chosen alphabetically-first (matches the
  Haskell library); to compare against multinma, you'll need to rebase
  contrasts by point-wise subtraction (see
  `test/proto_diabetes_html.py` for an example).
- Multi-rep mode does NOT write any per-fit grid files — only the lite
  summary. If you need full grid data per replicate, run single-rep
  mode in a loop yourself.
