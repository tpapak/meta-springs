---
title: Binomial outcomes: the nonlinear observation spring
animation: binomial-spring.html
hero_title: The same arm, two likelihoods
hero_caption: Drag either treatment node (or the &#8467; slider): the <b style="color:#1a56c4">Gaussian</b> restoring force keeps growing while the <b style="color:#c0392b">binomial</b> force saturates, and the force plot tracks both live. Press <b>Relax (IRLS)</b> to watch the nonlinear spring find equilibrium by repeated linearisation.
nav_label: Binomial: the nonlinear spring
blurb: For binary outcomes the observation spring carries the exact binomial energy; its restoring force saturates; Newton/IRLS relaxes it to an equivalent Gaussian arm.
---

## From probability to energy to force

The spring analogy begins with the likelihood. For a Gaussian observation,
the negative log-likelihood is a quadratic energy, and its restoring force
is linear — Hooke's law. We can apply the same construction to a binomial
observation: write its likelihood as an exponential, identify the energy,
and differentiate to obtain the force.

For one arm, let $\ell$ be the model coordinate. Define energy as
$E(\ell)=-\log L(\ell)$, ignoring terms independent of $\ell$, and the force
in the direction of increasing $\ell$ as $F(\ell)=-\mathrm dE/\mathrm d\ell$.
The **local stiffness** is the energy curvature,

$$ \kappa(\ell)=\frac{\mathrm d^2E}{\mathrm d\ell^2}
=-\frac{\mathrm dF}{\mathrm d\ell}. $$

### Gaussian observation: a linear spring

With observed arm mean $y$ and known sampling variance $v$,

$$ L_G(\ell;y)\propto\exp\!\left[-\frac{(y-\ell)^2}{2v}\right],
\qquad E_G(\ell)=\frac{(y-\ell)^2}{2v}. $$

Differentiating gives

$$ F_G(\ell)=-\frac{\mathrm dE_G}{\mathrm d\ell}
=\frac{y-\ell}{v}. $$

The force is proportional to displacement from the rest length $y$, with
constant stiffness $\kappa_G=1/v$, so $F_G(\ell)=\kappa_G(y-\ell)$.

### Binomial observation: a nonlinear spring

For $e$ events among $n$ observations, the binomial probability is

$$ \Pr(e\mid n,p)=\binom ne p^e(1-p)^{n-e}. $$

Use log-odds as the coordinate: $\ell=\operatorname{logit}(p)$, so
$p=\sigma(\ell)=1/(1+\exp(-\ell))$. Viewed as a likelihood for $\ell$,

$$ L_B(\ell;e)\propto
\exp\!\left[e\ell-n\log(1+\exp(\ell))\right]. $$

The binomial coefficient is independent of $\ell$, so it contributes only
an additive constant to the energy. Thus

$$ E_B(\ell)=n\log(1+\exp(\ell))-e\ell,
\qquad \boxed{F_B(\ell)=-\frac{\mathrm dE_B}{\mathrm d\ell}=e-n\sigma(\ell).} $$

The force is **observed minus expected events**: a positive force increases
the predicted probability, and a negative force decreases it. The energy
gradient has the opposite sign, $E_B'(\ell)=n\sigma(\ell)-e$.
Because $\sigma$ is bounded, the force saturates between $e-n$ and $e$.
For $0 \lt e \lt n$, it vanishes at the empirical log-odds
$\ell^*=\operatorname{logit}(e/n)$. If $e=0$ or $e=n$, an isolated arm has
no finite likelihood-maximizing rest length.

The binomial spring's stiffness varies with its coordinate:

$$ \kappa_B(\ell)=n\sigma(\ell)\bigl(1-\sigma(\ell)\bigr)=np(1-p). $$

This is the binomial Fisher information for the log-odds. It is largest at
$p=1/2$ and tends to zero in either tail. The animation labels this same
local stiffness $w(\ell)$, the usual IRLS weight.

In the network notation, arm $j$ of study $i$ has coordinate
$\ell_{ij}=x^t_j-x^\tau_{ij}$ and energy

$$ E_{ij}(\ell_{ij})=n_{ij}\log(1+\exp(\ell_{ij}))-e_{ij}\ell_{ij}. $$

## Solving the nonlinear network

The network no longer solves in one shot. Newton / IRLS relaxes it:
linearise each nonlinear spring at its current length into a local
Hookean spring of stiffness $\kappa_{ij}=\kappa_B(\hat\ell_{ij})=w_{ij}$,
re-solve the now-linear network, and repeat to
convergence. At the solution, the local quadratic approximation has variance
$v_{ij}=1/\kappa_{ij}=1/w_{ij}$. This is a local curvature equivalence; the full likelihood
remains binomial. The [next page](1c-random-effects.html) introduces the
heterogeneity springs used alongside these observation springs.

## Beyond the binomial: exponential-family springs

The same construction extends to every regular **exponential-family
likelihood**. In its natural coordinate $\ell$, with any dispersion parameter
held fixed, write

$$ p(y\mid\ell)=h(y)\exp\!\left[\ell T(y)-A(\ell)\right]. $$

Taking the negative logarithm and differentiating gives

$$ E(\ell)=A(\ell)-\ell T(y),\qquad
F(\ell)=T(y)-A'(\ell),\qquad
\kappa(\ell)=A''(\ell). $$

Here $T(y)$ is the sufficient statistic and $A$ is the log-normalising
function: its first derivative is the expected statistic and its second
derivative is its variance. Thus the force is again **observed minus
expected**, and the stiffness is the **Fisher information in the natural
coordinate** ([exponential-family identities](https://www.stat.umn.edu/geyer/5421/notes/expfam.html)).
For example, a Poisson count with log-rate $\ell$ gives
$F=y-\exp(\ell)$ and $\kappa=\exp(\ell)$. With several natural parameters,
the derivatives become a gradient and a stiffness matrix; with a
noncanonical link, the derivatives must also account for the change of
coordinate. The spring construction therefore extends beyond binary data,
although each likelihood still needs its own implementation. **Currently,
the Haskell and Rust solvers implement Gaussian and binomial observation
likelihoods; Rust also has a binomial log-link path for risk ratios.**
Poisson and Gamma observation likelihoods are not currently implemented.

## FAQ

### Binomial data — exact likelihood or normal approximation? (multinma does exact binomial)

Exact binomial energy. The observation spring carries
$n_{ij}\log(1+e^{\ell})-e_{ij}\ell$, and the per-$\tau^2$ solve is a damped-Newton /
IRLS relaxation to the joint mode; only the volume factor (the log-det of the
Hessian) is Gaussian. It is not a normal approximation of the likelihood or a
fixed-variance log-OR. Against multinma's exact-binomial MCMC, contrast posterior
means agree within $\sim0.02$ log-odds and $\tau^2$ medians within Monte-Carlo error;
on synthetic $T{=}6$ binary networks with known $\tau^2$, spring-$\Xi$ has about half
the $\tau^2$ RMSE.
