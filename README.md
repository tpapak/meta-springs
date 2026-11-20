# meta-springs

Deterministic Bayesian evidence synthesis with spring networks, without MCMC.

The negative log-likelihood is treated as the internal energy of a spring
network, and the prior as an external potential. The likelihood is
factorised, not the posterior: it is computed once and stored, so changing
the prior does not need a new fit.

## Features

- **Exact likelihood springs.** Each IPD participant or AgD arm is a spring
  carrying its exact likelihood: linear (Hooke) for Gaussian outcomes,
  non-linear for binomial ones (logit / probit).
- **Stored likelihood, replaceable prior.** The internal energy is stored on a
  heterogeneity (τ²) grid and as importance-weighted points corrected to the
  exact likelihood. A new prior on heterogeneity, treatment effects or effect
  modifiers is a reweighting of the stored points, with an effective sample
  size check (refit when it is too low).
- **Prior sensitivity panels.** Many priors from one fit, without Monte Carlo
  error. Example: 15 τ priors over 171 published networks in seconds.
- **Bayes factors** from the partition function: fixed vs random effects, and
  effect-size priors (validated against bayesmeta and Wakefield's ABF).
- **ML-NMR without MCMC.** Multilevel network meta-regression (Phillippo et
  al. 2020) for mixed IPD + AgD networks. An AgD arm is a spring whose force
  law averages its participants' force laws over the reported covariates.
  Fitted by equilibrium, curvature and an importance correction on the exact
  energy. It accepts multinma `nma_data` networks (`springs::mlnmr()` in place
  of `multinma::nma()`). On the plaque psoriasis network, posterior means are
  within 0.12 posterior SDs of multinma's, 16–30× faster.
- **Federated ML-NMR.** Sites (IPD or AgD) evaluate only their own springs and
  exchange secure sums, with no coordinator and no agreed prior. Force rounds
  recover the pooled equilibrium, and one energy round gives the exact
  posterior. Each site then applies its own prior (`mlnmr_site`).
- **Network meta-regression** with shared or exchangeable slopes, and
  baseline-risk regression (validated against gemtc, bnma and multinma).
- **gemtc-compatible R interface** (`mtc.network`, `mtc.model`, `mtc.run`,
  `relative.effect.table`, `rank.probability`, …), so existing gemtc code can
  switch namespace.
- **Frequentist spring NMA:** REML heterogeneity, study-specific
  prediction intervals, grouped τ², contribution matrices, and path-based
  inconsistency (`qpath()`, Netpath plot).

## Layout

| path | contents |
|---|---|
| `gc-rust/` | Rust engine (LAPACK): binaries `mtc` (aggregate NMA), `brisk` (baseline risk), `mlnmr` (ML-NMR), `mlnmr_site` (one federated site) |
| `r-package/springs/` | R package over the Rust engine |
| `src/`, `app/`, `test/` | Haskell reference library and tests |
| `examples/vs-multinma/` | reproducible benchmarks against multinma, bayesmeta, INLA and Stan; results in `results/`, summary in `RESULTS.md` |
| `blog/` | interactive series on NMA as a spring network (Markdown sources in `blog/content/`; build and serve with `bash blog/dev.sh`, see [`blog/README.md`](blog/README.md)) |

## Build

Rust engine and R package:

```sh
cargo build --release --manifest-path gc-rust/Cargo.toml
R CMD INSTALL r-package/springs
```

The R package looks for the engine binaries in this order: the R option
(`springs.engine`, `springs.mlnmr`, `springs.brisk`), the environment variable
(`SPRINGS_MTC_BIN`, `SPRINGS_MLNMR_BIN`, `SPRINGS_BRISK_BIN`),
`gc-rust/target/release/` relative to the package, then the `PATH`.

Haskell library:

```sh
stack build
stack test
```

The examples run from the repository root, e.g.
`Rscript examples/vs-multinma/05_mlnmr_psoriasis.R`; see
[`examples/vs-multinma/README.md`](examples/vs-multinma/README.md).
