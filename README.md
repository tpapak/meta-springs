# meta-springs

Haskell package for performing network meta-analysis (NMA) by solving the
equivalent system of springs. A spring network whose equilibrium reproduces
the NMA treatment effects, with between-study heterogeneity (τ²) carried by
"tau springs".

The library implements:

- **REML / SSPI** — REML heterogeneity estimation and study-specific
  prediction intervals (`springREML*`), including per-group τ²
  (τ-grouping: `setTauGroups`, `springREMLBinGrouped`).
- **BinomialSpring** — exact binomial likelihood for binary NMA, instead of
  Gaussian inverse-variance springs.
- **Grand Canonical ensemble** — deterministic full posteriors of τ² and the
  treatment effects via the spring partition function, without MCMC
  (`springGrandCanonical*`).
- **Contribution matrices** — `Data.Meta.Contribution`.

## Build & test

```
stack build
stack test
```

or, interactively in ghci:

```
stack ghci --ghci-options -itest meta-springs:meta-springs-test --ghci-options -isrc
```

then hit `:r` after each change.

Executables:
 - `metabin` / `metacont`: e.g. `stack exec metacont test/sim50.json minNP 4.1`
   (methods: minNP rootNP REML)

## Rust port (binomial Grand Canonical)

A LAPACK-backed Rust port of the binomial GC pipeline lives at
[`gc-rust/`](gc-rust/). It reproduces the Haskell library's τ² posterior
and per-contrast Laplace mixture summaries on the same data, ~100× faster,
and resolves the rare-events ill-conditioning that pure-Haskell `Mat.inv`
hits at large τ². See [`gc-rust/README.md`](gc-rust/README.md) for build
instructions and [`gc-rust/VALIDATION.md`](gc-rust/VALIDATION.md) for the
side-by-side comparison against the Haskell reference.

## Papers

Two write-ups in [`paper/`](paper/), both backed by this codebase:

- `sspi-nma.tex` — *Study-Specific Prediction Intervals and REML
  Heterogeneity Estimation for Network Meta-Analysis via the Spring Network
  Framework.*
- `grand_canonical_xi.tex` — *The Grand Canonical Spring Ensemble:
  Deterministic Full Posteriors for Network Meta-Analysis.*
