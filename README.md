# meta-analysis
Haskell package for performing network meta-analysis
by solving the equivalent system of springs.

View tests and run them by ```stack test```
or better with ghci
```stack ghci --ghci-options  -itest meta-analysis:meta-analysis-test --ghci-options -isrc```
then hit ```:r``` after each change

Executables:
 -metacont: ```stack exec metacont test/sim50.json minNP 4.1```
 methods: minNP rootNP REML

## Rust port (binomial Grand Canonical)

A LAPACK-backed Rust port of the binomial GC pipeline lives at
[`gc-rust/`](gc-rust/). It reproduces the Haskell library's τ² posterior
and per-contrast Laplace mixture summaries on the same data, ~100× faster,
and resolves the rare-events ill-conditioning that pure-Haskell `Mat.inv`
hits at large τ². See [`gc-rust/README.md`](gc-rust/README.md) for build
instructions and [`gc-rust/VALIDATION.md`](gc-rust/VALIDATION.md) for the
side-by-side comparison against the Haskell reference.
