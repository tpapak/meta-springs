# Network meta-regression in the spring framework

Branch `network-meta-regression`. Goal: extend the spring NMA so a study-level
covariate modifies treatment effects, à la MetaInsight's `covariate_model`
(`gemtc` regression). Start with a **binary covariate** and the **shared
coefficient** model — the simplest case and the one in the toy.

Toy / spec demo: [`test/nmr_binary_covariate_springs.html`](nmr_binary_covariate_springs.html)
(self-contained, no build — open in a browser).

## Model — real nodes, a load-bearing covariate spring

Every covariate level is its own sub-network of **genuine, independent
treatment nodes** (one node per treatment per layer). The free coordinates are:

    coords = [ θ_B, θ_C,   θ_B1, θ_C1,   β ]
               x=0 layer    x=1 layer    shared β node

- `θ_t`  = effect of treatment `t` vs reference in the `x=0` layer.
- `θ_t1` = effect of treatment `t` vs reference in the `x=1` layer (a *free* node).
- `β`    = the shared covariate coefficient, held by its own node.
- Reference `A` is pinned at `0` in both layers.

Rendered in the full **arm-level grammar** of `paper/xi_animation.html`
(standard NMA convention `y_ij = μ_i + d_j + β·x_i + δ_ij`): every node sits on
one effect axis and every spring is horizontal.

- **Treatment columns** — the covariate is **centred** (`x → x − x̄`), so each
  treatment's **base node** `d_j` (solid, capital letter) is the effect at the
  *mean* covariate and its two **covariate nodes** (dashed, lowercase `b₀`,`b₁`)
  sit **symmetrically** at `d_j ∓ β/2`. β is a **length** on the axis — the
  spring joining the two levels through the base node — and all such springs
  share one **β node** (β = the network-mean covariate slope, exactly as a
  treatment effect is the network-mean of its evidence). Centering keeps β
  unchanged but decorrelates it from the intercept; the reference is pinned at 0
  and has no covariate node.
- **β node wired to the studies that load it** — dashed wires run from the
  shared β node to the spindles of studies that compare a treatment *against the
  reference* (the only ones that identify the shared β). In an active-vs-active
  study the common `β·x` shift is absorbed by `μ_i`, so β cancels and the study
  gets no wire (S5, S6 in the toy). This is `∂E/∂β = Σ k·(x−x̄)·resid` made
  visible: only reference-involving arms contribute.
- **Study spindle `S_i`** at the centroid `μ̂_i`; **τ-chain** from the spindle
  to each arm's **BLUP node** `δ_ij = (τ²/(v+τ²))·(y − μ_i − d_j − β·x_i)`; and
  an **observation spring** from the BLUP node to the arm's treatment column
  (the covariate-adjusted one for x=1 arms). Raising τ² slides the BLUP nodes
  out along the chains and relaxes the obs springs — τ² is now *residual*
  heterogeneity after the regression.

The fixed parameters are solved jointly from the arm-level normal equations
`[d_1..d_{T-1}, μ_1..μ_S, β]`, exactly as the Ξ animation solves `[d, μ]` — the
covariate just adds the β column.

Two kinds of spring:

1. **Study springs** — each study `i` is a coil *inside its own covariate layer*
   connecting the nodes it compares, natural length `y_i`, precision
   `1/(v_i + τ²)`.
2. **Covariate springs** — for each active treatment, a spring enforcing
   `(θ_t1 − θ_t) − β = 0`, **anchored at the β node** with stiffness `k_β`.
   These are load-bearing: the β node feels the combined pull of every `x=1`
   study and settles at the regression coefficient. (An earlier draft drew β
   between a node and its *derived* copy `θ_t + β` — cosmetic, carrying no load.
   Here the upper nodes are free and the coupling does the work.)

Total energy minimised at equilibrium:

    E = Σ_i (1/(v_i+τ²))·(contrast_i − y_i)²        (study springs)
      + k_β · Σ_{t∈{B,C}} ((θ_t1 − θ_t) − β)²        (covariate springs)

The study-spring part is exactly weighted least squares on the NMA design with
the covariate column appended; the spring equilibrium **is** the WLS /
random-effects estimate.

## Three regimes = one coupling stiffness

- **Unrelated** (`k_β = 0`): coupling cut → the two layers are independent NMAs,
  each treatment keeps its own `β_t = θ_t1 − θ_t`.
- **Shared** (`k_β` large): all gaps tied through the single β node → one β
  (this is gemtc's `shared` regressor; the toy uses `k_β = 80`, so the per-tree
  gaps match to ~0.01 — exact equality needs `k_β → ∞`).
- **Pooled** (gap clamped to 0, β ≡ 0): layers collapse to one network; the
  study coils strain (weighted SS jumps ~0.12 → ~16 in the toy), showing why a
  covariate is needed when effects vary with `x`.

Because A is pinned in both layers it has no covariate spring, so β cancels for
active-vs-active comparisons (S5/S6) and is identified through
reference-involving contrasts + the between-layer gap.

## Coefficient variants (mirroring gemtc)

- **shared** — one β node for all treatments (toy: stiff coupling).
- **unrelated** — coupling cut; a free β_t per treatment vs reference (toy mode).
- **exchangeable** — β_t drawn from a common distribution; in spring terms the
  covariate springs attach to the β node through their *own* τ-springs
  (a second heterogeneity tier shrinking the per-treatment covariate effects
  toward a common β).

## Implementation path (Haskell / Rust)

1. Carry a per-study covariate on `BinaryStudy`/`ContinuousStudy`
   (`src/Data/Meta/Studies.hs`) — a labelled `Double` (binary = 0/1, but the
   math is identical for continuous covariates).
2. In `Data.Meta.NMA`, append covariate column(s) to the spring design so the
   solved system has the extra `β` coordinate(s); reuse the existing reduced
   Schur/Newton solve — only the design matrix grows.
3. REML (`springREML*`) and the Grand Canonical posterior carry through
   unchanged: β is just additional fixed coordinates; τ² is now *residual*
   heterogeneity after regression.
4. Continuous covariate, then `unrelated` / `exchangeable` coefficients.
