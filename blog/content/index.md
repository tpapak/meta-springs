---
site_title: meta-springs
tagline: Network meta-analysis as a mechanical spring network — deterministic effects, heterogeneity posteriors, and meta-regression, with no MCMC.
---

This blog is the **companion to the meta-springs software**. The source code,
build instructions and validation examples are in the
[**meta-springs GitHub repository**](https://github.com/tpapak/meta-springs).

**meta-springs** represents network meta-analysis as a *spring network*.
Treatments and studies are nodes; observation springs encode the data and
heterogeneity springs encode between-study variation. The series connects
that picture to effect estimation, heterogeneity posteriors and meta-regression,
using interactive examples with expandable study-data tables.

The software includes a Haskell implementation, a Rust engine and an R interface.
Gaussian conditional calculations are analytic; heterogeneity is integrated
numerically. Binary and nonlinear regression paths use approximations described
on the relevant pages.

## Software features and maturity {#software-features .section-h}

The statuses below summarize the implementation and checks documented in this
repository; they are not release or production-readiness guarantees.
**Benchmarked** means comparisons are recorded for specific datasets;
**experimental** means the feature needs further validation or integration.

| Feature | Maturity | Current scope |
|---|---|---|
| Fixed-effect and random-effects NMA | **Implemented; benchmarked** | Gaussian mean differences and binary log-odds models, including multi-arm studies. |
| Spring-REML heterogeneity estimation | **Implemented; benchmarked** | Gaussian comparisons against established estimators; binary variants use approximations. |
| Grand canonical heterogeneity and effect posteriors | **Implemented; benchmarked** | Deterministic variance-grid integration with Gaussian conditional calculations or binomial Laplace approximations. Prior sensitivity uses grid reweighting. |
| Rust engine | **Implemented; cross-checked** | Recorded comparisons with the Haskell binomial engine; see the [validation report](https://github.com/tpapak/meta-springs/blob/HEAD/gc-rust/VALIDATION.md). |
| R interface, league tables and rankings | **Implemented; partial API compatibility** | Supports a subset of `gemtc` workflows for mean differences and odds ratios. See the [supported API](https://github.com/tpapak/meta-springs/blob/HEAD/r-package/springs/README.md). |
| Grouped heterogeneity | **Experimental** | Haskell routines support grouped variance components; practical integration cost grows with the number of groups. |
| Meta-regression | **Experimental; example comparisons available** | Supplied covariates and shared, unrelated or exchangeable slopes; baseline-risk regression is available through the Rust `brisk` CLI. Nonlinear posterior calculations use a local approximation. |
| Study-specific prediction intervals | **Experimental; covariance verification needed** | The blog describes full joint-covariance intervals. The existing scalar SSPI helper uses a different formula and needs reconciliation before claiming that implementation matches the construction. |
| Risk-ratio likelihood, inconsistency models and full `gemtc` diagnostics | **Not supported in the current R interface** | Log-link RR models, node splitting and unrelated mean effects are not implemented there; DIC/deviance support is incomplete. |

Start with [Part 1](posts/part1.html) for the spring construction,
[Part 2](posts/part2.html) for posterior integration, or
[Part 3](posts/part3.html) for meta-regression.

## Previous work {#paper-previous-work .section-h}

- [Meta-analysis as a system of springs [--- Papakonstantinou, Nikolakopoulou,
  Egger & Salanti (2021)]{style="font-weight:400;color:var(--muted)"}](https://pubmed.ncbi.nlm.nih.gov/33264498/){.paper-title}
  [The foundational construction: the NMA model as
  a mechanical spring network whose equilibrium yields the consistency
  estimates. (Previous work.)]{.paper-meta}
