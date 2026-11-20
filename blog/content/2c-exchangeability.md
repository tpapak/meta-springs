---
title: Why the $\tau$-springs are exchangeable
animation: exchangeability.html
hero_title: The main animation · exchangeable arms, shared anchor
hero_caption: The four-arm study $S_3$ from the running network: four i.i.d. $\tau$-spring lengths $\zeta_{ij}$ on one shared study anchor. Press <b>Permute arms</b> — the joint law and the $(I+J)/2$ contrast correlation are unchanged. The inset shows two elementary springs in a chain swapping to an identical configuration.
nav_label: Why the $\tau$-springs are exchangeable
blurb: One i.i.d. $\tau$-spring length $\zeta_{ij}$ per arm on a shared study anchor; contrasts $\delta=\zeta-\zeta$ give the Lu&ndash;Ades $(I+J)/2$ correlation for free; indistinguishable elementary springs.
---

meta-springs gives each arm one i.i.d.\ $\tau$-spring length,
$\zeta_{ij}\sim\mathcal N(0,\tau^2/2)$, all sharing a study's baseline node. That single
choice hands you the entire multi-arm covariance for free. A treatment contrast is a
**difference of arm lengths**,

$$ \delta_{jk} = \zeta_{ij}-\zeta_{ik}
   \;\Longrightarrow\;
   \Var(\delta_{jk}) = \tau^2, \qquad
   \operatorname{corr}(\delta_{jk},\delta_{jl}) = \tfrac12, $$

the Lu--Ades $(I+J)/2$ structure --- never written as a matrix, just inherited from
the one shared anchor the two lengths are measured against. (This is exactly why we
keep $\zeta$ for the arm-level lengths and $\delta$ for contrasts.)

The arm lengths $\zeta_{ij}$ are **exchangeable**, and that is precisely why the grand
canonical picture is legitimate: the elementary springs making up a $\tau$-chain are
indistinguishable bosons, so a configuration depends only on the chain length $N$,
not on which springs. Exchangeability of the $\zeta$'s is what makes "count the
particles" ($\Xi=\sum_N Z$) the right sum --- the sum that Part 2 is built on.

Permuting the arms of a study (the animation's **Permute arms** button) leaves the
set of $\tau$-springs and the shared-anchor structure invariant --- the
contrast-correlation panel stays $(I+J)/2$ throughout. And swapping two elementary
springs inside one $\tau$-chain gives an identical configuration: indistinguishability
is what turns the sum over configurations into a sum over chain lengths alone.

## FAQ

### Is the per-arm (i.i.d.) random-effects parameterisation the same model as contrast-level Lu–Ades?

Yes, exactly — not an approximation. Each arm carries one length
$\zeta_{ij}\sim\mathcal N(0,\tau^2/2)$ sharing the study's baseline node; a contrast is a
difference $\delta_{jk}=\zeta_{ij}-\zeta_{ik}$, which reproduces the Lu–Ades $(I+J)/2$
covariance $\tau^2(I+J)/2$, with $\tau^2$ already on the conventional
contrast scale. With equivalent baseline treatment and matching priors, the
contrast likelihood and posterior agree. The full study-specific effect adds
the mean contrast: $\theta_{i,ab}=(d_b-d_a)+(\zeta_{ib}-\zeta_{ia})$;
see [1c](1c-random-effects.html).
