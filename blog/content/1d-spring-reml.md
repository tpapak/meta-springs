---
title: Spring-REML: heterogeneity as a mean-square length
animation: spring-reml.html
hero_title: The main animation · the mean-square length
hero_caption: Effective stiffness is statistical precision. At the EM fixed point, contrast heterogeneity variance equals twice the sum of average squared displacement and average inverse stiffness. Move $\tau^2$ to recalculate both terms; press <b>Solve REML</b> to find the fixed point.
nav_label: Spring-REML: mean-square length
blurb: Statistical precision is effective stiffness: it supplies the inverse-stiffness term in the REML update and determines the width of study-specific prediction intervals.
---


## Notation

Throughout this page, $i$ indexes studies and $t$ treatments. The arm random
effect $\zeta_{it}$ has mean zero and **arm-level variance** $\tau^2/2$.
We reserve $\tau^2$ for conventional **contrast-level heterogeneity**:
a difference of two independent arm deviations has variance $\tau^2$,
as derived in [1c](1c-random-effects.html).

We use $q$ for the vector of model coordinates, $\theta_{i,ab}$ for a true
study-specific treatment contrast, $V$ for a conditional variance, and
$\kappa$ for its reciprocal precision. Subscripts $\zeta$ and $\theta$
distinguish arm deviations from study contrasts.

## The law of total variance

The [law of total variance](https://en.wikipedia.org/wiki/Law_of_total_variance)
separates variability of conditional means from average conditional uncertainty:

$$ \frac{\tau^2}{2}=\Var(\zeta_{it})
=\Var_Y\!\left(\E[\zeta_{it}\mid Y]\right)
+\E_Y\!\left[\Var(\zeta_{it}\mid Y)\right]. $$

Here the outer expectation and variance range over possible datasets $Y$,
with the model parameters held fixed. For the observed dataset $y$, the
related **conditional second-moment identity** is

$$ \underbrace{\langle\zeta_{it}^2\rangle_y}_{\E[\zeta_{it}^2\mid y]}
=\underbrace{\langle\zeta_{it}\rangle_y^2}_{\hat\zeta_{it}^{\,2}}
+\underbrace{\Var(\zeta_{it}\mid y)}_{V_{\zeta,it}}. $$

Angle brackets denote conditional expectation:
$\langle f(\zeta)\rangle_y=\E[f(\zeta)\mid y]$.
For the REML calculation below, $\hat\zeta_{it}$ is the BLUP and
$V_{\zeta,it}$ includes uncertainty from estimating the fixed effects.
Equivalently, these are Gaussian conditional moments after integrating the
fixed effects with a flat prior, at a fixed value of $\tau^2$. We suppress
that variance parameter in the conditioning notation for readability.

## The physical connection: precision is effective stiffness

For a Gaussian coordinate, the density has the form

$$ p(\zeta_{it}\mid y)\propto
\exp\!\left[-\frac{\kappa_{\zeta,it}}2
(\zeta_{it}-\hat\zeta_{it})^2\right],
\qquad
\boxed{\kappa_{\zeta,it}=\frac1{V_{\zeta,it}}.} $$

In units where $k_BT=1$, **conditional precision is effective stiffness**.
It measures how strongly the network constrains a displacement from the fitted
value. Greater stiffness means greater precision and smaller fluctuations.
The variance entering the REML update is therefore the inverse stiffness,
$V_{\zeta,it}=1/\kappa_{\zeta,it}$.

For the full identified Gaussian system, let $A$ be the precision matrix,
so $\operatorname{Cov}(q\mid y)=A^{-1}$. Any linear combination $u=c^Tq$ has

$$ V_u=c^TA^{-1}c,\qquad \kappa_u=\frac1{V_u}. $$

This accounts for the response of the coupled network. For a single coordinate,
its marginal precision is $1/(A^{-1})_{aa}$, generally different from $A_{aa}$.
As a simple special case, with a known baseline and treatment mean and arm
sampling variance $v_{it}$,

$$ \Var(\zeta_{it}\mid y,\text{fixed effects})
=\left(\frac2{\tau^2}+\frac1{v_{it}}\right)^{-1}. $$

When those fixed effects are estimated, use the full covariance calculation.

## From the decomposition to the REML update

With $N=\sum_i a_i$ arms, expectation-maximization (EM) updates the variance by

$$ \tau_{\mathrm{new}}^2
=\frac2N\sum_{i,t}
\left[\hat\zeta_{it}^{\,2}+V_{\zeta,it}\right]_{\tau^2=\tau_{\mathrm{old}}^2}. $$

Recompute the BLUPs and their uncertainty, update $\tau^2$, and repeat.
The factor of two converts arm variance to contrast variance.
Including fixed-effect estimation uncertainty gives the REML update.
At its fixed point,

$$ \boxed{\frac{\hat\tau^2}{2}
=\underbrace{\frac1N\sum_{i,t}\hat\zeta_{it}^{\,2}}_{\text{average squared displacement}}
+\underbrace{\frac1N\sum_{i,t}\frac1{\kappa_{\zeta,it}}}_{\text{average inverse stiffness}}.} $$

The conditional identity supplies the decomposition; EM supplies the estimation
procedure. Equality with the current variance holds at the fixed point, rather
than at every trial value. Move $\tau^2$ in the animation to recalculate the two
terms, displayed with a factor of two; **Solve REML** iterates until their
sum agrees with the contrast variance estimate.

## Study-specific prediction interval {#sspi}

The true effect of treatment b relative to a in study i is

$$ \theta_{i,ab}=(d_b-d_a)+(\zeta_{ib}-\zeta_{ia}). $$

Let $c_{i,ab}$ select this combination from $q$. Using the same joint Gaussian
system at $\hat\tau^2$ gives its predicted value and uncertainty:

$$ \tilde\theta_{i,ab}=c_{i,ab}^T\hat q,\qquad
V_{\theta,i,ab}=\Var(\theta_{i,ab}\mid y,\hat\tau^2)
=c_{i,ab}^TA^{-1}c_{i,ab}
=\frac1{\kappa_{\theta,i,ab}}. $$

The covariance must retain the treatment effects, arm deviations and their
cross-covariances, including dependence in multi-arm studies. If auxiliary
coordinates were eliminated, their covariance must be reconstructed.
An approximate 95% interval is

$$ \boxed{\tilde\theta_{i,ab}\;\pm\;1.96\sqrt{V_{\theta,i,ab}}
=\tilde\theta_{i,ab}\;\pm\;\frac{1.96}{\sqrt{\kappa_{\theta,i,ab}}}.} $$

**The SSPI width is determined by the precision of the study-specific effect.**
In the spring network, this precision is the effective stiffness
$\kappa_{\theta,i,ab}$ of the whole contrast, after accounting for uncertainty
in the other coordinates. Its standard error is $1/\sqrt{\kappa_{\theta,i,ab}}$,
so the 95% interval has half-width $1.96/\sqrt{\kappa_{\theta,i,ab}}$.
A stiffer, more precise contrast has a narrower interval; four times the
precision halves its width. The interval is centred on the predicted study
effect $\tilde\theta_{i,ab}$.

This contrast precision differs from $\kappa_{\zeta,it}$, which describes one
arm deviation. Both express the same statistical–mechanical connection:
**precision is effective stiffness**.

### Connection to van Aert et al.

This construction **generalizes to NMA the pairwise study-specific prediction
interval approach described by van Aert, Schmid, Svensson and Jackson (2021)**,
[*Study specific prediction intervals for random-effects meta-analysis: A tutorial*](https://doi.org/10.1002/jrsm.1490).

For the pairwise limit, $\tau^2$ retains its conventional contrast-level
meaning. Write $\sigma_i^2$ for the sampling variance of $y_i$. With
$B_i=\tau^2/(\tau^2+\sigma_i^2)$, the predicted true effect is

$$ \tilde\theta_i=B_i y_i+(1-B_i)\hat\mu. $$

Its conditional variance is the tutorial's recommended Raudenbush formula
(Equation 10):

$$ V_{\theta,i}
=\frac{\tau^2\sigma_i^2}{\tau^2+\sigma_i^2}
+(1-B_i)^2\Var(\hat\mu),
\qquad \Var(\hat\mu)=\left[\sum_j\frac1{\sigma_j^2+\tau^2}\right]^{-1}. $$

The second term propagates uncertainty in the pooled mean. NMA replaces that
mean with network treatment contrasts and propagates their joint covariance.

These intervals describe true effects **in observed studies**. A new-study
interval also includes a fresh heterogeneity deviation. Plugging in the estimated
heterogeneity does not fully propagate its uncertainty, so 95% coverage is not
guaranteed. Binary outcomes using a local Gaussian approximation introduce an
additional approximation. [Part 2](part2.html) discusses integration over
heterogeneity.

## FAQ

### Is spring-REML exactly REML, or an approximation?

For the linear Gaussian model, the EM update targets the **restricted
likelihood**, provided the full prediction uncertainty is included. Numerical
convergence must still be checked. For binomial models, the Gaussian
linearisation introduces an approximation, so agreement with Gaussian REML
does not establish accuracy for sparse binary networks.
