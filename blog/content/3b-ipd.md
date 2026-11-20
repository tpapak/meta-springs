---
title: IPD: every patient is a spring
animation: ipd-springs.html
hero_title: Two trials sharing their patients · binary or continuous outcomes
hero_caption: Each patient is one spring at the height of their covariate. Switch between binary and continuous outcomes and between regression terms, sum the patients into arm springs, and stretch the within-study covariate spread to see the slope estimates sharpen.
nav_label: IPD: every patient a spring
blurb: With individual participant data each patient is a spring. Without covariates the patients of an arm add up exactly to the arm spring; with a patient covariate they sit at different heights, and the within-study spread is the lever that identifies the slopes. The equilibrium is the glm() fit.
---

In [3a](3a-covariate-axis.html) a covariate was a property of a **study**: one
value per trial, and the slope was learned from differences between trials.
Individual participant data (IPD) record the covariate and the outcome for
every **patient**. The spring network takes them as they are: one spring per
patient.

## 1. A patient is a spring {#patient}

Patient $i$ in study $j$ on treatment $k$ has linear predictor
$\eta_i=\mu_j+d_k$ (covariates come in section 3). As in Part 1, the spring runs
from the study node to the treatment node, and its energy is the patient's
negative log-likelihood:

* **Continuous outcome**, $y_i\sim\mathcal N(\eta_i,\sigma^2)$:
  $E_i=(y_i-\eta_i)^2/2\sigma^2$. A Hooke spring with rest length $y_i$ and
  stiffness $1/\sigma^2$.
* **Binary outcome**, $y_i\in\{0,1\}$, $\operatorname{logit}p_i=\eta_i$:
  $E_i=-y_i\log p_i-(1-y_i)\log(1-p_i)$. The force is $y_i-p_i$ and the
  stiffness $p_i(1-p_i)$: the binomial spring of
  [1b](1b-binomial-spring.html) with $n=1$. A single patient has **no rest
  length**: a responder pushes $\eta$ up, a non-responder pushes it down, and
  neither push ever reaches zero.

## 2. Without covariates, patients add up to the arm spring {#collapse}

If every patient in an arm has the same $\eta$, their springs act in parallel
and add:

$$ \sum_{i\in\text{arm}}\frac{(y_i-\eta)^2}{2\sigma^2}
=\frac{n(\bar y-\eta)^2}{2\sigma^2}+\text{const},\qquad
\sum_{i\in\text{arm}}(y_i-p)=r-np. $$

The $n$ Gaussian patients are one spring of stiffness $n/\sigma^2$ at the arm
mean $\bar y$: the arm spring of [1a](1a-pairwise-fe.html) with
$v=\sigma^2/n$. The $n$ binary patients pull exactly like the arm's binomial
spring with $r$ events. The arm totals are sufficient statistics, so IPD and
arm totals give **the same equilibrium**. In the animation, select
**Summed into arm springs**: the panel compares the two fits, and they differ
only by rounding ($10^{-15}$).

So IPD changes nothing until patients differ in something that matters for
their outcome.

## 3. Patient covariates give the patients heights {#heights}

With a patient covariate $x$, in multinma's notation,

$$ \eta_i=\mu_j+\beta_1(x_i-\bar x)+d_k+\beta_{2,k}(x_i-\bar x),
\qquad d_A=\beta_{2,A}=0. $$

Put each patient at the height of their own $x$:

* **Study rods.** The study node of 3a opens into a rod, tilted by the
  prognostic slope $\beta_1$. Each study keeps its own position $\mu_j$; all
  study rods share the tilt.
* **Treatment rods.** As in 3a, treatment $k$ is a rod at $d_k$ tilted by the
  interaction $\beta_{2,k}$, glued through every study that has it. The
  reference $A$ is pinned and vertical. The animation shares one interaction
  between B and C, $\beta_{2,B}=\beta_{2,C}=\beta_2$.
* **Springs.** Patient $i$ is a spring from the study rod to their treatment
  rod at height $x_i$. Its horizontal length is $\eta_i$.

Now the patients of one arm sit at different heights, see different $\eta$,
and no longer collapse into one spring.

## 4. Levers: the within-study spread identifies the slopes {#levers}

Tilting a rod by $\Delta$ moves its contact at height $x$ by
$(x-\bar x)\Delta$. A spring pulling with force $F_i$ at height $x_i$ turns
the rod with torque $(x_i-\bar x)F_i$, and resists the tilt with stiffness
$(x_i-\bar x)^2k_i$. At equilibrium

$$ \sum_i (x_i-\bar x)\,F_i=0, $$

which is the score equation for the slope, and $\sum_i (x_i-\bar x)^2k_i$ is
its Fisher information. The heights are the lever arms:

* A patient at the pivot $\bar x$ says nothing about a slope. Patients far
  from it say the most.
* The slope's standard error falls in proportion to the covariate's spread
  **within** the studies. Halve the spread and the standard error roughly
  doubles.
* Summing an arm into one spring puts all its patients at one height: no
  lever, no slope. That is why the **Summed into arm springs** view switches
  the covariate terms off.

This is the main gain of IPD over the study-level regression of 3a. There the
slope came from comparing trials with different average $x$, which is
confounded with anything else that differs between the trials (ecological or
aggregation bias). Here each study rod has its own position $\mu_j$, so
between-study differences in baseline are absorbed; $\beta_2$ is learned by
comparing randomised arms at the same $x$. In this two-study network each
active treatment appears in only one study, so $d_B$ and $d_C$ also absorb any
between-study difference in effect and $\beta_2$ comes from within-trial
comparisons alone. In larger networks a one-stage model can mix within- and
between-study information on an interaction unless the covariate is centred
within each study (Riley et al. 2020, *Statistics in Medicine*).

### Try the picture

1. Start with **Binary outcome** and **$\beta_1$ + interaction $\beta_2$**.
   The estimates sit near the simulation truth; note the standard errors of
   $\beta_1$ and $\beta_2$.
2. Select **Summed into arm springs**. Each arm becomes one spring labelled
   with its totals, the rods stand vertical, and the panel shows that the
   patient fit and the arm fit without covariates are the same.
3. Back to **Patients as springs**. Drag the **within-study covariate
   spread** down to $\times 0.25$: the patients bunch up around each study's
   mean, the levers shorten, and the slope standard errors grow three to four
   times. Drag it up to $\times 2$ and they fall, but by less than half:
   patients at extreme heights have $p$ near 0 or 1, and their springs go soft
   (stiffness $p(1-p)$).
4. Switch to **Continuous outcome**. Each patient is now a Hooke spring whose
   rest length is their outcome (the dot). Orange springs are compressed and
   push the rods apart; blue ones are stretched and pull them together. The
   standard errors are much smaller: a continuous outcome with residual SD 0.5
   carries more information per patient than a yes/no answer. Here every
   spring has the same stiffness, so the slope standard errors are exactly
   inversely proportional to the spread.

## 5. The equilibrium is the glm() fit {#glm}

With no priors, the equilibrium is the maximum-likelihood estimate and the
stiffness matrix at equilibrium is the observed information. For the logit
link (the canonical link) the observed and expected information coincide, so
the spring network reproduces `glm()` exactly, standard errors included; for a
continuous outcome it reproduces `lm()`. The animation solves the network by
Newton's method in the browser, and its panel matches R to every digit shown.

The patients in the animation (spread $\times 1$) are in
[ipd-two-studies.csv](../data/ipd-two-studies.csv). With the `springs` R
package from the [meta-springs repository](https://github.com/tpapak/meta-springs)
and multinma installed:

```r
library(multinma)
d <- read.csv("ipd-two-studies.csv")
d$xc <- d$x - 0.5                                        # centre at x-bar = 0.5
d$trtclass <- ifelse(d$trt == "A", "control", "active")  # one interaction for B and C
net <- set_ipd(d, study = study, trt = trt, r = y_binary, trt_class = trtclass)
fit <- springs::mlnmr(net, regression = ~ xc + xc:.trt, class_interactions = "common",
                      link = "logit", likelihood = "bernoulli", center = FALSE,
                      prior_intercept = Inf, prior_trt = Inf, prior_reg = Inf)

d$act <- as.numeric(d$trt != "A")
g <- glm(y_binary ~ 0 + study + trt + xc + xc:act, family = binomial, data = d)
cbind(springs = fit$mode, glm = coef(g))
```

| parameter | springs | `glm()` | SE |
|---|---|---|---|
| $\mu_1$ | −1.2277 | −1.2277 | 0.165 |
| $\mu_2$ | −0.6564 | −0.6564 | 0.184 |
| $d_B$ | 1.6146 | 1.6146 | 0.231 |
| $d_C$ | 1.4537 | 1.4537 | 0.291 |
| $\beta_1$ | 0.6388 | 0.6388 | 0.139 |
| $\beta_2$ | 0.7196 | 0.7196 | 0.212 |

The largest difference is $7\times10^{-8}$, the solver's convergence
tolerance. On real data, the three UNCOVER psoriasis trials in multinma
(26 parameters, five covariates and their interactions with treatment) give the
same agreement: identical to `glm()` for the logit link and within
$5\times10^{-8}$ for the probit.

The `prior_*` arguments set to `Inf` mean flat priors. Finite values add
springs to ground, and `springs::mlnmr` then returns the posterior rather than
the maximum-likelihood point, without MCMC.

Usually only some trials share their patients. In
[3c](3c-mlnmr.html) Study 2 keeps only its arm totals and its covariate mean
and SD, and its arms become springs averaged over that distribution.

## FAQ

### Is this one-stage or two-stage IPD meta-analysis?

One-stage: every patient's spring is in one network, solved once. A two-stage
analysis would first fit each trial and then pool the trial estimates as
contrast springs. Without covariates and with Gaussian outcomes the two give
the same treatment effects (section 2). With covariates or binary outcomes they
can differ, and the one-stage network keeps the exact patient likelihood.

### Do I need IPD from every trial?

No. Trials without IPD enter through their published arm totals and covariate
summaries; that is multilevel network meta-regression, in [3c](3c-mlnmr.html).
