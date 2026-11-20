---
title: From pairwise to NMA
animation: fe-spring-network.html
hero_caption: The running example: a 4-treatment continuous network ($A,B,C,D$; four studies, incl. a three-arm $S_2$ and a four-arm $S_3$). Press <b>Link studies</b> and the observation springs pull the shared treatment nodes into the inverse-variance fixed-effect estimate. This same network carries through Parts&nbsp;1&ndash;3.
blurb: Pairwise meta-analysis as two springs &rarr; fixed-effect NMA as a bipartite network &rarr; the binomial nonlinear spring &rarr; $\tau$-springs for random effects &rarr; spring-REML.
---

**meta-springs** does network meta-analysis (NMA) by treating the whole
model as a mechanical *spring network* and reading inferences off its
equilibrium --- building on [*Meta-analysis as a system of springs*](https://pubmed.ncbi.nlm.nih.gov/33264498/)
(Papakonstantinou, Nikolakopoulou, Egger & Salanti, 2021).
Part 1 builds that network from the ground up.
Each page adds one layer and comes with its own interactive animation;
every animation and every code example runs on the **same running
example** --- one synthetic 4-treatment continuous network.
