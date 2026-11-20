---
title: Network meta-regression
animation: nmr-covariate-4trt.html
hero_caption: The four-treatment network with a fixed study covariate. Switch slope structures and vary $\tau^2$ to see adjusted effects and residual heterogeneity. The slope is fitted automatically.
blurb: From effect nodes to effect lines: covariate geometry, shared and exchangeable slopes, Gaussian partition functions, and the nonlinear baseline-risk solver.
---

Parts 1 and 2 gave treatment effects and uncertainty from a spring network.
Meta-regression asks how those effects change with a study characteristic.

Start with a **supplied covariate**: adding slope coordinates preserves the
quadratic Gaussian energy. Then couple those slopes to share information
across treatments. Finally, let the covariate be the **latent study baseline**:
that makes the model nonlinear and introduces a local approximation in the
current solver. Page 3b moves to **patient-level covariates** (ML-NMR): study
rows gain height, study rods tilt by the prognostic slope, and an aggregate
arm becomes one spring averaged over its covariate distribution.

The walkthrough connects the animation to the Haskell and Rust source, and
includes commands for a Gaussian example and a separate binary baseline-risk
example. It distinguishes analytic conditional calculations, numerical
integration, and the approximations used for nonlinear regression.
