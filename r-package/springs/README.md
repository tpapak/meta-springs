# springs — a gemtc-compatible NMA facade over the spring/GC engine

`springs` is a drop-in replacement for the slice of [gemtc](https://cran.r-project.org/package=gemtc)
that [MetaInsight](https://github.com/CRSU-Apps/MetaInsight) uses for Bayesian
network meta-analysis. It returns objects shaped like gemtc's, so consumers work
by swapping the `gemtc::` namespace for `springs::`.

Under the hood it shells out to the verified **spring / Grand-Canonical** Rust
engine (`gc-rust`), which forms a **closed-form joint posterior** (no MCMC) of the
basic treatment contrasts and the heterogeneity SD, then samples it to reproduce
all-pairwise league tables and rank probabilities the way gemtc derives them from
chains.

## Build the engine

```sh
cd gc-rust && cargo build --release --bin mtc
```

The package finds the binary via `options(springs.engine=)`, `$SPRINGS_MTC_BIN`,
or the default `gc-rust/target/release/mtc`.

## Use (identical call shape to gemtc)

```r
library(springs)          # or: source("r-package/springs/R/springs.R")
data(smoking, package = "gemtc")

net <- mtc.network(smoking$data.ab)
mod <- mtc.model(net, likelihood = "binom", link = "logit", linearModel = "random")
res <- mtc.run(mod, n.iter = 8000)

summary(res)$summaries$statistics   # d.A.B, d.A.C, ... and sd.d
relative.effect.table(res)          # all-pairwise [A, B, ] = B relative to A
rank.probability(res, preferredDirection = 1)
```

## Heterogeneity prior

By default `springs` reproduces gemtc's default heterogeneity prior,
`τ ~ Uniform(0, om.scale)` (gemtc's `mtc.hy.prior("std.dev", "dunif", 0, "om.scale")`),
including the data-derived `om.scale = guess.scale()` (largest within-study relative
MLE effect on the link scale). This gives **credible-interval parity** with gemtc,
including on small networks where τ is weakly identified and the intervals are wide.
Override with `hy.prior = "flat" | "hn:SIGMA" | "hc:SIGMA" | "unif:UPPER"`.

## Validated

Against real gemtc fits: smoking (binary, OR), parkinson (continuous, MD) and a
small 5-study MD network — basic effects, SDs, the heterogeneity SD `sd.d`, SUCRA,
**and the all-pairwise credible intervals** agree with gemtc (point estimates to
~0.01–0.02; CIs to ~0.1–0.4 on the weakly-identified network).

## Implemented vs gemtc

| gemtc API | springs | notes |
|---|---|---|
| `mtc.network` / `mtc.model` / `mtc.run` | ✓ | consistency model |
| `relative.effect` (t1) / `relative.effect.table` | ✓ | gemtc orientation |
| `summary` → `$summaries$statistics`/`$quantiles` | ✓ | + `sd.d` (random only) |
| `rank.probability` | ✓ | SUCRA-ready |
| binary **OR** (logit) / continuous **MD** (identity) | ✓ | |
| default τ prior `dunif(0, om.scale)` | ✓ | CI parity with gemtc |
| **RR** (log link) | ✗ | engine is logit/identity only (TODO) |
| `$DIC` | partial | reports `data points`; deviance NA (TODO) |
| `mtc.nodesplit` / `mtc.deviance` / UME / `blobbogram` | ✗ | separate features (TODO) |

Fixed-effect models report effects (exact) with no `sd.d`, as in gemtc.
