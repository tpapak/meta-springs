---
title: Grand canonical: the Bayesian solver
animation: xi-animation.html
hero_caption: The grand canonical sum, live: drag the chain length $N$ to grow/shrink the elementary $\tau$-springs and watch the treatment nodes re-equilibrate and the $\tau^2$ posterior bars update — Gaussian and binomial likelihoods on the same running network (toggle the likelihood).
blurb: Chain length = heterogeneity; $\Xi = \sum Z$ gives the $\tau^2$ posterior and a mixture-of-Gaussians for every effect; any prior for free; multiple $\tau^2$ posteriors; exchangeability.
---

Part 1 left us with a single $\hat\tau^2$ (spring-REML) and the
random-effects estimate at that value. A Bayesian wants the whole
posterior --- of $\tau^2$ and of every contrast. meta-springs gets it
deterministically, by turning the $\tau$-spring into a **grand
canonical ensemble**. Part 2 continues on the
**same 4-treatment running example**.

## FAQ

### What's genuinely new versus GLS effects + profile-likelihood τ² + quadrature?

Each ingredient has a pedigree — deliberately; it's why the answers match
netmeta/metafor/bayesmeta. The contribution is the **unification**: one mechanical
object giving the consistency effects, a deterministic full posterior of $\tau^2$ and
every contrast for **both** Gaussian and exact-binomial NMA, grouped heterogeneity
and FE-as-a-grid-point from the grand-canonical view, and SSPIs and meta-regression
from the *same* compliances — fast (11–39× vs multinma here), reproducible to machine
precision, with no convergence diagnostics, shipped as a gemtc-shaped drop-in.

### How was it validated?

Effects and REML $\tau^2$ against netmeta and metafor (exact on curated sets; within
10% across 149/163 non-boundary nmadb networks); full posteriors against multinma
(exact-binomial MCMC) and gemtc; the pairwise analytic posterior against bayesmeta
($\tau^2$ median to 3 s.f.). Reproduce commands and datasets are in the
[repository](https://github.com/tpapak/meta-springs).
