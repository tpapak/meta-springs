---
title: Meta-regression: from a covariate axis to the solver
animation: nmr-covariate-4trt.html
hero_title: A covariate shifts the treatment effect
hero_caption: Study rows sit at the height of their covariate, so each treatment column is a rigid rod tilted by its slope. Switch between no covariate, shared slope and unrelated slopes; move $\tau^2$ to refit the Gaussian network. The four study covariates are fixed inputs, centred at $\bar x=0.35$. The displayed slope interval is conditional on the selected $\tau^2$.
nav_label: Covariates, slopes and the solver
blurb: Fixed covariates extend the quadratic energy; exchangeable slopes add springs; latent baseline risk makes the model nonlinear. A runnable example connects the picture to the code.
---

A treatment effect may change with a study characteristic. Meta-regression
asks **how the effect changes with that covariate, and how much variation
remains after accounting for it**. The spring construction extends naturally:
add slope coordinates to the equilibrium problem, then read their uncertainty
from the same stiffness matrix.

There are two distinct cases. A supplied study covariate is a fixed input.
A study's *latent baseline risk* is an unknown parameter that must be fitted
alongside the effects. Start with the fixed input: it makes the mechanics clear.

## 1. From an effect node to an effect line {#axis}

Let $x_i^c=x_i-\bar x$. For continuous arm means, write

$$ y_{it}=\mu_i+d_t+\beta_{2,t} x_i^c+u_{it}+\epsilon_{it},
\qquad d_A=\beta_{2,A}=0, $$

with sampling variance $\Var(\epsilon_{it})=v_{it}$ and independent arm
heterogeneity $\Var(u_{it})=\tau^2/2$. Here $\tau^2$ is on the **contrast
scale**: subtracting two arms gives heterogeneity variance $\tau^2$.

Notation follows multinma (Phillippo et al. 2020): $\mu_i$ study baselines,
$d_t$ treatment effects, $\beta_1$ prognostic coefficients and $\beta_{2,t}$
treatment–covariate interactions (multinma's `beta[x:.trtB]` for treatment B). With one
covariate value per study the prognostic term $\beta_1 x_i$ is constant within
the study and is absorbed into $\mu_i$, so only $\beta_{2,t}$ appears here.
Patient-level covariates, where $\beta_1$ becomes a tilted study rod, are in
[3b](3b-mlnmr.html).

The treatment node becomes the line $d_t+\beta_{2,t} x^c$. Its intercept is the
effect relative to A at the centring value; its slope is the change per unit
of the covariate. The effect of C relative to B is therefore

$$ \Delta_{BC}(x)=(d_C-d_B)+(\beta_{2,C}-\beta_{2,B})(x-\bar x). $$

That subtraction matters. With a shared slope for all non-reference
treatments, B versus A and C versus A both change with $x$, but C versus B
does not: their slopes cancel. The reference treatment has slope zero.

### Try the picture

1. Select **No covariate**: every treatment rod is vertical, one effect per
   treatment.
2. Select **Shared slope**: the rods of B, C and D tilt together by one slope
   about the pivot $\bar x$; each study's arms land on the rods at that
   study's height.
3. Select **Unrelated slopes** to let each rod tilt by its own slope.
4. Increase $\tau^2$: the solver gives the residual arm deviations more room
   and recalculates the effects, slopes and conditional uncertainty.

The four-treatment network is the same illustrative network used earlier.
Here its supplied $x_i$ values are $0.4,-0.8,0.6,1.2$. They are held fixed;
this animation does **not** estimate latent baseline risk. The study rows are
placed at the height of their covariate, so the treatment line
$d_t+\beta_{2,t} x^c$ is drawn as a straight rod: its position at height $x$ is the
effect at that covariate value, and its slope is the tilt. The rod is rigid but
its displacement is not fixed: changing the slope by $\Delta$ moves the contact
at height $x_i$ by $x_i^c\Delta$. So the horizontal gap between the untilted
position (dashed line) and the rod at a study's height is $\beta_{2,t} x_i^c$:
the slope converts height into effect, and the covariate acts as a lever on the
slope coordinate. A rod stores no energy, so the tilt does not introduce another
freely varying residual.

## 2. The energy and the actual solve {#solver}

Integrating the Gaussian arm deviations gives effective variance
$v_{it}+\tau^2/2$. At fixed $\tau^2$, the energy is

$$ E(\theta)=\frac12\sum_{i,t}
\frac{(y_{it}-\mu_i-d_t-\beta_{2,t} x_i^c)^2}{v_{it}+\tau^2/2}
+\frac12\theta^T K\theta. $$

Here $\theta$ collects the baselines, effects and slopes. $K$ contains any
Gaussian slope coupling. If $X$ contains the coefficients of those parameters
and $W$ the inverse effective variances, force balance is

$$ A\hat\theta=b,\qquad A=X^T W X+K,\qquad b=X^T W y. $$

Each non-reference arm adds its covariate value to a slope column of $X$.
This is precisely what Haskell's `nmaRegression` and Rust's `nmr_fe` example
assemble. They store the study coordinate with the opposite sign
($\mu_i=-\phi_{S_i}$); the statistical model is the same. The covariate
coefficients need not be $\pm1$, so these routines use a dense design-matrix
solve rather than the ordinary edge-incidence Laplacian alone.

For this linear Gaussian model, at fixed variance components and with an
identified design, the conditional posterior has mean $\hat\theta$ and
covariance $A^{-1}$. The partition function includes both the energy at
equilibrium and the fluctuation volume:

$$ \log Z(\tau^2)=-\frac12\left[
\sum_{i,t}\log(v_{it}+\tau^2/2)+\log\det A
+y^T W y-b^T\hat\theta\right]+C. $$

$C$ collects terms independent of $\tau^2$ for this fixed model. For
exchangeable slopes, also retain their variance-dependent prior normalisation.
The determinant comes from **integrating** the effect and slope coordinates.
This is the integrated Gaussian likelihood, rather than just the likelihood
at its optimum.

The REML route iterates the heterogeneity estimate using residual stretches
and their uncertainty. The grand canonical route evaluates a grid and weights
its cells by $Z$, the variance prior and the integration measure. It gives a
mixture of conditional Gaussians for the slopes as well as the effects.
Gaussian integration within a cell is analytic; the finite grid is numerical.
Residual heterogeneity need not decrease in every fitted dataset.

## 3. Sharing slopes is another spring construction {#slopes}

| Structure | Statistical model | Mechanical interpretation |
|---|---|---|
| No covariate | $\beta_{2,t}=0$ | Remove the slope coordinates. |
| Shared | $\beta_{2,t}=B$ for $t\ne A$ | Use one slope coordinate. |
| Unrelated | Separate $\beta_{2,t}$ | Fit separate slope coordinates. |
| Exchangeable | $\beta_{2,t}\sim N(B,\sigma_B^2)$ for $t\ne A$ | Tie each slope to a common mean with stiffness $1/\sigma_B^2$. |

Exchangeability adds the energy
$\sum_{t\ne A}(\beta_{2,t}-B)^2/(2\sigma_B^2)$. Small $\sigma_B^2$ pulls the
slopes together. Large $\sigma_B^2$ weakens that coupling. The GC calculation
can integrate both residual heterogeneity $\tau^2$ and slope dispersion
$\sigma_B^2$, connecting directly to [multiple variance components](2b-multiple-tau.html).
The animation currently shows the first three structures.

A slope needs covariate variation and a design that separates it from the
treatment effects. Merely having different covariate values somewhere in the
network does not guarantee that every treatment's slope is identified.

## 4. When the covariate is itself unknown {#baseline}

For binary baseline-risk regression, the covariate is the latent reference-arm
log-odds $\eta_i$. The implemented predictor is

$$ \operatorname{logit}(p_{it})=\eta_i+d_t+\beta_{2,t}(\eta_i-m_x),
\qquad d_A=\beta_{2,A}=0. $$

$m_x$ is a fixed centring constant calculated from the reference arms.
Unlike a supplied covariate, $\eta_i$ is inferred from the data. The product
$\beta_{2,t}\eta_i$ makes this model nonlinear, even for the continuous-outcome
version with an identity link.

Both implementations linearise the predictor using derivatives
$1+\beta_{2,t}$, $1$, and $\eta_i-m_x$ with respect to the baseline, effect and
slope. They solve the resulting weighted system and damp the step when needed.
The baseline and slope therefore move together during fitting.

### What the posterior calculation currently approximates

- **Gaussian outcomes, supplied covariate:** analytic conditional integration
  followed by numerical variance-grid integration.
- **Binary outcomes, supplied covariate:** Haskell's
  `nmaRegressionBinPosteriorB` freezes a working Gaussian response at a shared-slope
  binomial fit, then integrates that approximation over the variance grid.
- **Latent baseline risk:** the Haskell exchangeable GC routine and Rust's
  `brisk --method gc` likewise freeze a local linearisation before the grid.
  Rust also applies a bilinear curvature correction, with a fallback when that
  corrected stiffness is not positive definite.

Thus these regression GC paths do not refit the nonlinear model at every grid
cell. **No MCMC does not mean an exact nonlinear posterior.** In `brisk`,
variance-grid weights are deterministic; the exported effect and slope draws
are random draws from the resulting approximate mixture.

## 5. Run the model behind the picture {#code}

The solver is in the [meta-springs repository](https://github.com/tpapak/meta-springs);
it needs Rust (`cargo`). Clone it, then build and run the Gaussian
fixed-effect example on this page's data:

```bash
git clone https://github.com/tpapak/meta-springs.git
cd meta-springs
cargo build --release --manifest-path gc-rust/Cargo.toml --example nmr_fe
gc-rust/target/release/examples/nmr_fe \
  blog/data/nmr-four-treatment.json blog/data/nmr-four-treatment-covariates.json
```

It prints:

```
beta = -1.08069
  A->B = 0.87176
  A->C = 1.56743
  A->D = 3.26861
```

The downloadable [arm data](../data/nmr-four-treatment.json) and
[centred covariates](../data/nmr-four-treatment-covariates.json) reproduce the
animation's shared-slope fit at $\tau^2=0$. The command prints the slope and
the effects relative to A. Covariates are centred in the input file: the
example solver does not centre them automatically.

For the nonlinear baseline-risk model, use the separate bundled binary
simulation, with A present in every study (from the same `meta-springs`
directory):

```bash
cargo build --release --manifest-path gc-rust/Cargo.toml --bin brisk
gc-rust/target/release/brisk \
  --input test/nmr_br_sim_arms.json \
  --output /tmp/meta-springs-baseline-draws.json \
  --ref A --regressor common --effects random --method gc \
  --samples 4000 --seed 1
```

Change `common` to `independent` or `exchangeable` for the other slope
structures. This CLI takes arm outcomes, not an external covariate file.
Its current GC variance priors are half-normal with scale 0.5 on the
heterogeneity SD and, when exchangeable, the slope SD; they are hard-coded.

The output includes `mx`, treatment order, `d_draws`, `b_draws`, and `tau2`
(the GC mean of the contrast-scale variance). Paired effect and slope draws
let you evaluate $d_t+\beta_{2,t}(\eta-m_x)$ at a specified baseline log-odds
$\eta$. The `d_hat` and `b_hat` fields are the initial fitted point estimates,
not the GC mixture means. The baseline-risk example uses a different dataset
because the four-study animation includes a C–D study without reference A.

## Source map {#source}

In the [repository](https://github.com/tpapak/meta-springs):

- [`src/Data/Meta/NMA.hs`](https://github.com/tpapak/meta-springs/blob/master/src/Data/Meta/NMA.hs): `nmaRegression`, `nmaRegressionRE`,
  `nmaRegressionPosteriorB`, `nmaRegressionBinPosteriorB`,
  `baselineRiskCoreB`, and `nmaRegressionBaselineRiskBinExchGC`.
- [`gc-rust/examples/nmr_fe.rs`](https://github.com/tpapak/meta-springs/blob/master/gc-rust/examples/nmr_fe.rs): the minimal Gaussian external-covariate solve.
- [`gc-rust/examples/nmr_bin.rs`](https://github.com/tpapak/meta-springs/blob/master/gc-rust/examples/nmr_bin.rs): a binary external-covariate working-response example.
- [`gc-rust/src/bin/brisk.rs`](https://github.com/tpapak/meta-springs/blob/master/gc-rust/src/bin/brisk.rs): the baseline-risk CLI, slope coupling, variance grid
  and mixture draws.

The central step is now visible: **enlarge
the coordinates to include slopes; construct the energy; solve for equilibrium
and fluctuations; integrate the variance components.** Whether that inner
energy is quadratic determines which calculations are analytic.

## FAQ

### Priors on the basic parameters d, not just on τ?

A prior on a $d$ (or on $\beta_{2,t}$ in meta-regression) is a **spring to ground**: a
spring from that node to its prior mean with stiffness = prior precision (Gaussian
prior). Non-Gaussian priors are non-linear springs, handled by the same Newton
relaxation. Flat priors = no ground spring.
