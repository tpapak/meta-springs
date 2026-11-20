---
title: Multiple $\tau^2$ posteriors: multi-species sums
animation: multi-species-tau.html
hero_title: The main animation · two heterogeneity species
hero_caption: The running network with its arms split into two exchangeable groups — $\{A,B\}$ and $\{C,D\}$ — each with its own $\tau^2$. Drag the two sliders and watch each group's marginal posterior $p(\tau^2_g\mid y)$ update independently.
nav_label: Multiple $\tau^2$ posteriors
blurb: One boson species per exchangeable group; marginalising the multi-species sum gives a separate heterogeneity posterior per group.
---

Nothing forces a single heterogeneity for the whole network. If the arms
split into $G$ groups --- say by treatment class --- each group gets its own chain-length variable $N_g$
and its own $\tau^2_g$. Each arm in group g has variance $\tau_g^2/2$.
A contrast within that group therefore has heterogeneity variance $\tau_g^2$;
a contrast between groups g and h has variance $(\tau_g^2+\tau_h^2)/2$.
Setting every $\tau_g^2=\tau^2$ recovers the usual common-heterogeneity model. Summing over *every* combination of chain lengths
is a **multi-species grand canonical sum**, one boson species per group:

$$ \Xi \;=\; \sum_{N_1}\cdots\sum_{N_G} \; Z\bigl(\tau^2_1(N_1),\dots,\tau^2_G(N_G)\bigr), $$

and marginalising $\Xi$ over all species but one gives the posterior
of that group\'s $\tau^2_g$ --- a separate heterogeneity posterior per
group, from the same machinery. In the animation the two species are
$\{A,B\}$ and $\{C,D\}$; each arm\'s effective spring uses its own
group\'s $\tau^2/2$. Each panel shows a conditional posterior slice, computed
by sweeping the partition function over its $\tau^2$ with the other
held fixed.

## Which groupings are legitimate {#which}

The springs set the rule: because the $\tau$-springs are attached
per arm and are exchangeable *within* a group (the [next
page](2c-exchangeability.html)), any partition of the arms into
exchangeable blocks is a valid species decomposition --- a single shared
$\tau^2$ ($G=1$), class-specific $\tau^2$ --- while orderings that would break
exchangeability inside a block are not. The cost is a grid per species,
so in practice $G$ is small; the two- and three-species cases
cover the usual "does heterogeneity differ across classes?" question.
When the lattice is too dense to enumerate, the outer sum can be
replaced by MCMC on the discrete chain-length vector, with the
deterministic inner Laplace supplying exact conditional effect
posteriors.

## FAQ

### Multiple heterogeneity variances (comparison- or class-specific τ²)?

A **multi-species** grand-canonical sum: partition the arms into exchangeable groups,
give each its own chain-length variable, sum the lattice, and marginalise over the
other species to get each group's $\tau^2$ posterior. The legitimate partitions are
exactly those that keep the arm $\tau$-springs exchangeable within a group. See
[Part 2b](2b-multiple-tau.html).
