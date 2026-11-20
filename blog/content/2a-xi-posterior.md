---
title: The grand canonical $\Xi$ gives the posterior
animation: xi-animation.html
hero_title: The main animation · the Ξ(τ²) ensemble
hero_caption: Drag the chain length $N$ to grow/shrink the elementary $\tau$-springs, watch the treatment nodes re-equilibrate and the $\tau^2$ posterior bars update — Gaussian and binomial likelihoods on the same running network (toggle the likelihood).
nav_label: $\Xi$ gives the posterior
blurb: Chain length = heterogeneity; $\Xi = \sum_N Z$; the $\tau^2$ posterior and a mixture-of-Gaussians for every effect; any prior for free; agreement &amp; speed vs MCMC.
---

## Chain length is heterogeneity {#chain}

Discretise each $\tau$-spring into a chain of $N$ *elementary*
springs of hardness $k_\tau$ in series. A chain of length $N$ has
arm variance $\tau^2(N)/2=N/k_\tau$, so the conventional contrast
variance is $\tau^2(N)=2N/k_\tau$. $N=0$ is a rigid bond (the
fixed-effect model), and longer chains mean more heterogeneity. The
number of elementary springs is a particle number; summing over it is a
grand canonical partition function:

$$ \Xi \;=\; \sum_{N=0}^{N_{\max}} Z\bigl(\tau^2(N)\bigr)
   \;=\; \sum_{N} p\bigl(y \mid \tau^2(N)\bigr). $$

Each $Z(\tau^2)$ is one network solve: for Gaussian outcomes it is the
exact profile likelihood (a closed form in $\det$ and the equilibrium
energy); for binomial outcomes it is a Laplace integral at the Newton
mode. Either way the expensive part is computed once per grid point.

## From $\Xi$ to the posterior {#posterior}

Normalising $\Xi$ over the grid *is* the posterior of $\tau^2$,
and at each grid point the network solve also hands back a Gaussian
conditional for every contrast. So the marginal effect posterior is a
$\Xi$-weighted mixture of Gaussians:

$$ p(\tau^2_k\mid y) \propto Z_k\,\pi(\tau^2_k), \qquad
   p(d_j\mid y) = \sum_k p(\tau^2_k\mid y)\,\mathcal N\!\bigl(d_j;\mu_{j,k},\sigma^2_{j,k}\bigr). $$

That mixture is exactly what the animation assembles as you drag
$N$: each chain length contributes one Gaussian, weighted by
$Z_k$.

## Any prior, and the fixed-effect model, for free {#priors}

Because the costly grid $\{(\tau^2_k, \ln Z_k, \mu_{j,k}, \sigma^2_{j,k})\}$ is prior-independent, *any* prior on
$\tau$ is a one-line reweighting $w_k \propto \exp(\ln Z_k)\,\pi(\tau^2_k)$ --- flat, half-Normal,
half-Cauchy, Uniform --- with no re-fit. The fixed-effect model is not a
separate run either: it is the single grid point $N=0$. The grid is
a *sufficient intermediate*: medians, credible intervals, $P(d_j>0)$,
ranks and SUCRA, and full prior sensitivity all fall out of it as
arithmetic.

## Does it agree with MCMC --- and how fast? {#speed}

On the same exact likelihoods, the deterministic grand-canonical solver
matches MCMC and is much faster (each sweep returns four
prior-conditional posteriors; `multinma` times are a single half-Normal
fit):

  Dataset                                  meta-springs $\Xi$   multinma (MCMC)   Speedup
  ---------------------------------------- ------------------------- ----------------- ---------
  Dias diabetes ($T{=}6$, $k{=}22$)   0.85 s                    9.3 s             11×
  Synthetic $T{=}50$ rare events         1.7 s/rep                 66 s/rep          39×
  Synthetic $T{=}100$                     4.2 s/rep                 122 s/rep         29×

Agreement: posterior contrast means within $0.02$ log-odds of
`multinma`, $\tau^2$ medians and 95% CrIs within Monte-Carlo error;
against analytic bayesmeta the $\tau^2$ median matches to three
significant figures. On synthetic binary networks with a known
$\tau^2$, meta-springs has roughly half the $\tau^2$ RMSE of
`multinma` and 15% lower effect RMSE; on Gaussian networks the two are
tied (the Gaussian solve is exact).

## Code

::::::: codecompare
:::: codeblock
::: code-label
meta-springs (R) --- full posterior + prior sweep, ONE solve
:::

``` code
re <- mtc.run(mtc.model(net, likelihood = "binom",
                        link = "logit", linearModel = "random"))
summary(re)$summaries$quantiles        # full posterior quantiles, incl. sd.d
rank.probability(re); sucra(re)        # ranks / SUCRA from the same grid
# every prior on tau is a reweighting of the one GC sweep -- no refit:
mtc.sensitivity(re, priors = c("flat", "hn:1", "hc:0.5", "unif:2"))
```
::::

:::: codeblock
::: code-label
multinma (R) --- one MCMC run per prior
:::

``` code
re   <- nma(net, trt_effects="random", prior_het=half_normal(1), ...)
# a different prior on tau means a new MCMC fit:
re_hc <- nma(net, trt_effects="random", prior_het=half_cauchy(0.5), ...)
posterior_ranks(re); # ranks from the chains
```
::::
:::::::

## FAQ

### The τ prior is applied "for free" after one sweep — must it be conjugate?

No — conjugacy is irrelevant, because nothing is marginalised analytically. The
expensive object is a grid $\{\tau^2_k,\ \ln Z_k,\ (\mu_{j,k},\sigma^2_{j,k})\}$,
where $Z_k = p(y\mid\tau^2_k)$ is the profile likelihood at that $\tau^2$ (exact for
Gaussian, Laplace-at-the-mode for binomial). A prior enters only as a **pointwise
weight**, $p(\tau^2_k\mid y)\propto Z_k\,\pi(\tau^2_k)$ — literally numerical
quadrature on the $\tau^2$ axis. So any $\pi$ you can *evaluate* on the grid works
(half-Normal, half-Cauchy, uniform, log-Normal, an empirical density…) with no
re-fit, and improper priors are fine as long as the reweighted grid is normalisable.
Conjugacy would only matter if we were integrating in closed form; we aren't. See
[Part 2a](2a-xi-posterior.html).

### Is this just a Laplace approximation to dodge MCMC?

Not a single global Laplace — it's **deterministic quadrature over $\tau^2$ wrapped
around a per-$\tau^2$ solve**, arranged so that Laplace only ever touches the *easy*
dimension. (1) The heterogeneity $\tau^2$ — the boundary-heavy dimension where MCMC
actually struggles ($\tau\to0$) — is handled by **1-D quadrature on the grid**, not by
Laplace and not by sampling. (2) Conditional on a fixed $\tau^2$: for **Gaussian**
outcomes the model is linear-Gaussian, so $Z(\tau^2)=p(y\mid\tau^2)$ is a **closed
form** (a log-det plus the equilibrium energy) — no Laplace at all; for **binomial**
outcomes the conditional model is a GLM, so $Z(\tau^2)$ uses **Laplace at the
Newton/IRLS mode**, and even there only the volume factor $\ln\det A_{\text{eff}}$ is
Gaussian — the energy term keeps the *exact* binomial log-likelihood at the mode. So
Laplace is confined to the conditionally-log-concave effect block, where the mode is a
good expansion point; the skewed, mass-near-zero $\tau^2$ posterior is never
Laplaced — it is integrated. A naïve global Laplace on $(d,\tau^2)$ would do the
opposite, Gaussianising $\tau^2$ exactly where that fails. Empirically this matches
exact-binomial multinma to $\sim0.02$ log-odds and reproduces bayesmeta; a 1-loop
correction shifts summaries only 1–3%. Documented caveat: a small Armijo gap at
$\tau^2\le10^{-2}$ where the $\tau$-block is near-singular, not affecting medians or
credible intervals on any benchmark.

### How is the τ² posterior different from bayesmeta's quadrature?

For the pairwise Gaussian case it's the same idea (integrate over $\tau^2$), and we
reproduce bayesmeta's $\tau^2$ median to three significant figures. What's new is the
generalisation: it extends to **networks**, to the **exact-binomial** likelihood
(Laplace at the Newton mode, not a normal/OR approximation), and — via the
grand-canonical framing — to **grouped/multiple $\tau^2$** and to the fixed-effect
model as a single grid point ($N=0$).

### Speed and reproducibility?

One deterministic sweep on a $\sim$100-point adaptive grid (log-spaced around the
REML mode), each point an LU solve on a reduced Schur system, single-core. On the
benchmarks it runs 11× (Dias diabetes) to 39× (T=50 rare-event) faster than 4-chain
multinma, is bit-for-bit reproducible, and returns four prior-conditional posteriors
per sweep. No $\hat R$, no divergent transitions.
