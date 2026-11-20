---
title: Pairwise &rarr; fixed-effect NMA
animation: fe-spring-network.html
hero_title: The running example · fixed-effect spring network
hero_caption: The 4-treatment continuous network that runs through the whole series. Springs start at their <b>natural length</b> — the raw within-study effects; press <b>Link studies</b> and the shared treatment nodes glue into one column each, the springs stretch, and the network settles into the inverse-variance estimate.
nav_label: Pairwise &rarr; fixed-effect NMA
blurb: Pairwise meta-analysis as two springs; fixed-effect NMA as a bipartite spring network.
---

## Pairwise meta-analysis is two springs {#pairwise}

Take one study comparing treatment $B$ to $A$ with observed
effect $y_i$ and variance $v_i$. Put a node on a 1-D effect axis
and connect it to the pooled effect $d$ by a spring of natural
length $y_i$ and hardness $k_i = 1/v_i$.  **The equilibrium position of
$d$ minimises the total elastic energy**

$$ E(d) = \sum_i \tfrac{1}{2}\,k_i\,(d - y_i)^2 \;\Rightarrow\;
   \hat d = \frac{\sum_i k_i\,y_i}{\sum_i k_i}, $$

which is exactly the inverse-variance fixed-effect estimate: a stiffer
spring (more precise study) pulls the pooled effect harder.
Meta-analysis *is* a force balance.

## Fixed-effect NMA is a bipartite spring network {#fe-nma}

A network has $T$ treatments and $S$ studies. Give every
treatment a node $d_t$ on the effect axis (reference pinned at
$d_{\text{ref}}=0$) and every study a baseline node $\mu_i$. Each arm
$(i,t)$ is an **observation spring** from the study to the treatment
node, natural length $y_{it}$, hardness $1/v_{it}$. The network is
bipartite --- study nodes on one side, treatment nodes on the other,
springs across --- and its equilibrium \$\\arg\\min\_{\\mu,d}
\\sum\_{it} \\tfrac{1}{2v\_{it}}(\\mu_i + d_t - y\_{it})\^2\$ is the
generalised-least-squares / consistency NMA estimate. Multi-arm trials
are just studies whose baseline node carries several springs --- this is
the network in the animation above; press **Link studies** there to
watch it settle.

## Code

::::::: codecompare
:::: codeblock
::: code-label
meta-springs (R --- gemtc-shaped facade)
:::

``` code
library(springs)                       # drop-in for the gemtc slice
data(smoking, package = "gemtc")
net <- mtc.network(smoking$data.ab)
fe  <- mtc.run(mtc.model(net, likelihood = "binom",
                         link = "logit", linearModel = "fixed"))
summary(fe)$summaries$statistics       # d.A.B, d.A.C, ...  (closed form, no MCMC)
```
::::

:::: codeblock
::: code-label
multinma (R)
:::

``` code
library(multinma)
net <- set_agd_arm(smoking, study = studyn, trt = trtn,
                   r = events, n = sampleSize)
fe  <- nma(net, trt_effects = "fixed",
           prior_intercept = normal(0, 100),
           prior_trt       = normal(0, 100))   # MCMC
summary(fe)
```
::::
:::::::

