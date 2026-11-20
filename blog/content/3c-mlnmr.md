---
title: ML-NMR: patient covariates, study rods and averaged springs
animation: mlnmr-springs.html
hero_title: An IPD study and an aggregate study · tilted study and treatment rods
hero_caption: Each study row now has height, the patient covariate $x$. Study rods tilt by the prognostic slope $\beta_1$, treatment rods by the interaction $\beta_2$. Switch the regression terms, compare the averaged aggregate spring with the at-mean spring, and move the reported covariate SD of Study 2.
nav_label: ML-NMR: study rods and averaged springs
blurb: Patient-level covariates give each study a height; study rods tilt by $\beta_1$, treatment rods by $\beta_2$; an aggregate arm is one spring with extent, and averaging its force law removes aggregation bias.
---

In [3a](3a-covariate-axis.html) every study had **one** covariate value. Its
arms all sat at one height, so the prognostic term was absorbed into the study
baseline and only the treatment slopes appeared. In [3b](3b-ipd.html) both
trials shared their patients, and every patient was a spring at their own
height. Usually only some trials do. Multilevel network meta-regression
(ML-NMR; Phillippo et al. 2020, implemented in multinma) combines individual
participant data (IPD) from some trials with published covariate summaries
from the others. The animation keeps 3b's Study 1 patients unchanged and
replaces Study 2 by a trial of the same size and population that reports only
its arm totals and its covariate mean and SD. Sections 1–4 recap the rods and levers of 3b in this
setting; section 5 is new.

## 1. The model in multinma's notation {#model}

For a patient in study $j$ on treatment $k$ with covariate $x$,

$$ \operatorname{logit} p = \mu_j + \beta_1 (x-\bar x) + d_k + \beta_{2,k}(x-\bar x),
\qquad d_A=\beta_{2,A}=0. $$

| Symbol | Meaning | multinma output |
|---|---|---|
| $\mu_j$ | study baseline | `mu[j]` |
| $d_k$ | treatment effect at $\bar x$ | `d[k]` |
| $\beta_1$ | prognostic slope: changes response on **every** arm | `beta[x]` |
| $\beta_{2,k}$ | interaction: changes the **treatment effect** | `beta[x:.trtk]` |

The animation shares one interaction across the active treatments,
$\beta_{2,B}=\beta_{2,C}=\beta_2$.

## 2. Study rods appear {#rods}

Put each patient at the height of their own $x$.

* **Treatment rods.** As in 3a, treatment $k$ is a rigid rod at $d_k$, tilted
  by $\beta_{2,k}$ about the pivot $\bar x$. It is glued through every study:
  all studies see the same position and tilt (consistency). The reference $A$
  is pinned and vertical.
* **Study rods.** The study spindle of 3a opens into a rod over the study's
  covariate range, tilted by $\beta_1$. Study rods are glued in **tilt only**:
  each keeps its own baseline $\mu_j$, but all tilt by the same $\beta_1$, so
  they stay parallel.
* **Springs.** A patient is a spring from the study rod to the treatment rod at
  the patient's height. Its horizontal length is the patient's log-odds $\eta$
  (the study rod sits at $-\mu_j$, as in [1b](1b-binomial-spring.html)).

This is why 3a had no study rods: with one $x$ per study the rod has no length,
and tilting it is the same as moving $\mu_j$. $\beta_1$ is identified only when
$x$ varies **within** a study.

## 3. What the tilt does: levers {#levers}

The rods are rigid but their displacement is not fixed. Tilting a rod by
$\Delta$ moves its contact at height $x$ by $(x-\bar x)\Delta$. The rod stores
no energy; it transmits force:

* A spring pulling with force $F_i$ turns the tilt with force
  $(x_i-\bar x)F_i$ and resists it with stiffness $(x_i-\bar x)^2 k_i$.
* The balance $\sum_i (x_i-\bar x)F_i=0$ is the score equation for $\beta_1$
  (all arms) or $\beta_2$ (active arms); $\sum_i (x_i-\bar x)^2k_i$ is its
  Fisher information.
* A patient at $\bar x$ sits at the pivot and says nothing about a slope. If
  every patient sat at one height, tilt and position could not be separated.
  The heights are the entries of the design matrix.

The link function changes the springs, not the rods, so the same rods serve
Gaussian, binomial and survival likelihoods.

## 4. Binary patients have no rest length {#binary}

A single Bernoulli observation has no finite best value of $\eta$: a responder
pushes $\eta$ up with force $1-p$, a non-responder pushes it down with force
$p$ ([1b](1b-binomial-spring.html) with $n=1$). In the animation responders are
green and non-responders grey. The equilibrium exists because the two push
against each other through the rods. Without covariates every patient in an arm
shares $\eta$, the pushes balance at $\operatorname{logit}(r/n)$, and the
patients add up exactly to the arm's binomial spring. With covariates they sit
at different heights and stay separate.

If all responders sat at one end of the covariate range and all non-responders
at the other, every levered push would turn the tilt the same way and the slope
would run off to infinity: complete separation. A prior on the slope (a spring
to ground) stops it.

## 5. Aggregate arms: one spring with extent {#agd}

Study 2 reports only arm totals $r$ of $n$ and the covariate mean and SD. Its
arm is still **one** spring, but the spring has extent: it sits at every height
of the arm's covariate distribution. Draw integration points $x_q$ from that
distribution; average the probability, then form one binomial energy:

$$ \eta_q=\mu_j+\beta_1(x_q-\bar x)+d_k+\beta_{2,k}(x_q-\bar x),\qquad
\bar p=\frac1Q\sum_q \operatorname{logit}^{-1}(\eta_q),\qquad
E=-\log\operatorname{Bin}(r\mid n,\bar p). $$

Replacing the sheath by one spring at the mean height,
$\operatorname{logit}^{-1}(\bar\eta)$, is the **at-mean-covariate** model.
The difference is the aggregation bias, to second order

$$ \bar p-\operatorname{logit}^{-1}(\bar\eta)\approx
\tfrac12\,\sigma''(\bar\eta)\,(\beta_1+\beta_{2,k})^2\operatorname{Var}(x). $$

It vanishes with no covariate spread, with an identity link, and at $p=1/2$;
it grows toward the tails. The averaged arm also feels the tilt through all
its heights: its effective lever is the $\sigma'$-weighted mean of $x_q$, not
the plain mean.

### Try the picture

1. Start in **$\beta_1$ + interaction $\beta_2$** with the **averaged
   spring**: the estimates in the panel are close to the simulation truth.
2. Switch to **at mean covariate**. $d_C$ shifts, and the anchored C vs B
   comparison changes sign. The panel also shows, at the current fit, the C
   arm's averaged response next to its response at the mean covariate.
3. Move the **reported SD** to 0: the sheath collapses to one spring and the two
   models agree. Increase it and they separate.
4. Select **No covariate**: all rods are vertical, Study 1's patients collapse
   to two binomial springs, and the C vs B comparison ignores that the two
   studies enrolled different populations.

## 6. What is implemented {#code}

The engine (`gc-rust/src/ipd.rs`, `gc-rust/src/bin/mlnmr.rs`) builds these
springs, finds the equilibrium by Newton's method on the exact energy, and
corrects the Gaussian curvature by importance weights against the exact
likelihood. The R wrapper takes a multinma network:

```r
library(multinma)   # set_ipd(), set_agd_arm(), combine_network(), add_integration()
fit <- springs::mlnmr(net,
         regression = ~(durnpso + prevsys + bsa + weight + psa) * .trt,
         class_interactions = "common", link = "probit", likelihood = "bernoulli2")
```

On the plaque psoriasis network (four IPD trials with 4,114 patients, five
aggregate trials) it reproduces multinma's posteriors to within 0.12 posterior
standard deviations, in 1–4 seconds instead of 33–65.

### This page's example against multinma

The animation's simulated data (500 IPD patients in Study 1; arm totals and a
covariate mean and SD in Study 2) fitted with multinma (Stan, 4 × 2,000 draws,
64 integration points, logit link, shared interaction, $\bar x = 0.5$) and with
`springs::mlnmr` on the same network:

| parameter | multinma mean (SD) | springs mean (SD) | z |
|---|---|---|---|
| $\mu_1$ | −1.228 (0.170) | −1.234 (0.167) | −0.04 |
| $d_B$ | 1.616 (0.237) | 1.621 (0.238) | 0.02 |
| $d_C$ | 1.829 (0.317) | 1.824 (0.319) | −0.02 |
| $\beta_1$ | 0.769 (0.179) | 0.764 (0.182) | −0.03 |
| $\beta_2$ | 0.602 (0.261) | 0.604 (0.263) | 0.01 |

$z$ = (springs − multinma) / multinma SD. multinma took 8.6 s, springs 0.17 s.
The animation shows the equilibrium itself (the maximum-likelihood point,
using 15 integration points), which is close to these posterior means.

## FAQ

### Why can't the at-mean model just be fitted by a standard GLM tool?

It can, and that is the problem: it has one linear predictor per observation,
the form INLA and ordinary GLM software require. The averaged spring depends on
a non-linear average over many linear predictors, which those tools cannot
express. That average is exactly where aggregation bias is removed.

### Where does $\beta_1$ go when only aggregate data are available?

With only aggregate studies, $\beta_1$ is informed only through the averaging
across different covariate distributions, which is weak. IPD from at least
some studies is what pins the slopes; the aggregate studies then borrow them.
