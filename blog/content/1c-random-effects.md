---
title: Random effects: a $\tau$-spring per arm, at a given $\tau$
animation: re-tau-springs.html
hero_title: The main animation · random-effects $\tau$-springs
hero_caption: Drag $\tau^2$ on the same 4-treatment network: each arm grows a $\tau$-chain to its $\hat\zeta$ node (the equilibrium $\tau$-spring length), over-precise studies are down-weighted, and the treatment columns slide from the fixed-effect estimate ($\tau^2=0$) toward the random-effects fit. Raising $\tau^2$ is literal mechanical shrinkage.
nav_label: Random effects: a &tau;-spring per arm
blurb: A soft $\tau$-spring in series with each observation spring; each arm gets a length $\zeta_{ij}\sim\mathcal N(0,\tau^2/2)$; the effective stiffness $\keff = 1/(v+\tau^2/2)$; raising $\tau^2$ as literal mechanical shrinkage.
---

## Conventional heterogeneity, represented by arm springs

We use **$\tau^2$ for the between-study variance of a treatment contrast**,
as in the usual Lu–Ades NMA model. Give each arm an independent deviation

$$ \zeta_{ij}\sim\mathcal N(0,\tau^2/2). $$

For treatment b relative to a in study i, the true study-specific effect is

$$ \boxed{\theta_{i,ab}=(d_b-d_a)+(\zeta_{ib}-\zeta_{ia}).} $$

Here $d_b-d_a$ is the population mean contrast; the difference of arm
deviations is its study-specific departure. The study baseline cancels.
Consequently,

$$ \Var(\theta_{i,ab}\mid d,\tau^2)
=\Var(\zeta_{ib})+\Var(\zeta_{ia})
=\frac{\tau^2}{2}+\frac{\tau^2}{2}=\tau^2. $$

For two contrasts sharing reference arm a,

$$ \operatorname{Cov}(\theta_{i,ab},\theta_{i,ac}\mid d,\tau^2)
=\Var(\zeta_{ia})=\frac{\tau^2}{2}. $$

Thus their correlation is $1/2$. For all contrasts against one reference
in a multi-arm study, the heterogeneity covariance is exactly
$\tau^2(I+J)/2$, where $I$ is the identity and $J$ the all-ones matrix.
**Independent arm variances of $\tau^2/2$ reproduce the standard Lu–Ades
contrast model**, including its multi-arm dependence. With equivalent
baseline treatment and matching priors, this is a reparameterisation:
it gives the same contrast likelihood and inference. See also the
[exchangeability page](2c-exchangeability.html).

## Effective stiffness

Each arm deviation is the length of a **$\tau$-spring**, with prior
stiffness $k_\tau=2/\tau^2$ and mean-square length
$\langle\zeta_{ij}^2\rangle=\tau^2/2$. The name refers to the shared
heterogeneity parameter; an individual spring's variance is half of it.

At a given $\tau^2$, the Gaussian network is still linear. The observation
spring (variance $v_{ij}$) and heterogeneity spring (variance $\tau^2/2$)
combine in series, giving

$$ \boxed{\;\keff_{ij}=\frac{1}{v_{ij}+\tau^2/2}\;} $$

where $v_{ij}=1/w_{ij}$ and the arm weight $w_{ij}$ --- the observation-spring
hardness --- is

$$ w_{ij} = \begin{cases}
   \dfrac{n_{ij}}{s_{ij}^2}, & \text{Gaussian } \bigl(v_{ij}=s_{ij}^2/n_{ij}\bigr),\\[8pt]
   n_{ij}\,\sigma(\hat\ell_{ij})\bigl(1-\sigma(\hat\ell_{ij})\bigr), & \text{binomial (Fisher information)}.
   \end{cases} $$

so equivalently $\keff_{ij} = \dfrac{w_{ij}}{1 + w_{ij}\,\tau^2/2}$. Each $\keff_{ij}$
is one edge of a weighted graph-Laplacian $A(\tau^2)$ on the study and treatment
nodes (a non-reference arm adds $+\keff_{ij}$ to the study--study and
treatment--treatment diagonals and $-\keff_{ij}$ to the study--treatment
off-diagonals, the right-hand side carrying $\keff_{ij}\,y_{ij}$). Solving the
single linear system

$$ A(\tau^2)\,x \;=\; b(\tau^2), \qquad x = (\mu_1,\dots,\mu_S,\;d_2,\dots,d_T) $$

places every node; the treatment effects $d_t$ and the **equilibrium spring lengths
$\hat\zeta_{ij}$** (each arm's $\tau$-node minus its study node) are read straight off
the solution.

Raising $\tau^2$ softens every $\tau$-spring (drops every $\keff_{ij}$), letting the
$\hat\zeta$ nodes slide out and down-weighting over-precise studies --- the familiar
random-effects shrinkage, now literal mechanical relaxation, exactly as the
animation shows. The one missing ingredient is $\tau^2$ itself; the
[next page](1d-spring-reml.html) estimates it as twice the mean-square
arm-spring length. For binomial observations, these stiffnesses describe each
local IRLS step, as explained in [1b](1b-binomial-spring.html).
