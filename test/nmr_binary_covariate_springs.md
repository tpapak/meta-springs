# Network meta-regression in the spring framework

Branch `network-meta-regression`. Goal: extend the spring NMA so a study-level
covariate modifies treatment effects, à la MetaInsight's `covariate_model`
(`gemtc` regression). Start with a **binary covariate** and the **shared
coefficient** model — the simplest case and the one in the toy.

Toy / spec demo: [`test/nmr_binary_covariate_springs.html`](nmr_binary_covariate_springs.html)
(self-contained, no build — open in a browser).

## Model (shared coefficient)

Treatments `A` (reference), `B`, `C`. Generalised coordinates of the system:

    coords = [ d_B, d_C, β ]

- `d_t` = effect of treatment `t` vs reference at covariate `x = 0`.
- `β`   = the regression coefficient (a single **shared** value across treatments).

Position of treatment `t` at covariate level `x`:

    pos(A, x) = 0                     (reference pinned in every covariate layer)
    pos(B, x) = d_B + β·x
    pos(C, x) = d_C + β·x

A study comparing arm `p` vs `q` with covariate `x_i` has expected contrast
`pos(p,x_i) − pos(q,x_i)`, observed `y_i`, precision `1/(v_i + τ²)`. The fit
minimises the spring energy

    E = Σ_i  (1/(v_i+τ²)) · ( [pos(p,x_i) − pos(q,x_i)] − y_i )²

which is exactly weighted least squares with the covariate column appended to
the NMA design matrix. The spring equilibrium **is** the WLS / random-effects
estimate.

## The spring picture

- Two covariate layers (`x=0`, `x=1`). Each study is a coil **inside its own
  layer**, connecting the treatment nodes it compares, with natural length `y_i`.
- The covariate enters as a **shared "β spring"** that couples each active
  treatment's `x=0` node to its `x=1` node. The shared assumption forces all of
  these couplings to have the **same length β** — a rigid translation of the
  active nodes between layers.
- The reference `A` is pinned at `0` in both layers, so its coupling is vertical
  (length 0 regardless of β): the covariate shifts effects *relative to the
  reference*. This is also why, under the shared model, β cancels for
  active-vs-active comparisons (S5/S6 in the toy) and is identified mainly
  through reference-involving contrasts and the between-layer gap.

Toggling **Ignore covariate** clamps β≡0 (one pooled network for both groups):
the study coils strain badly (weighted SS jumps ~0.13 → ~19 in the toy),
showing why a covariate is needed when effects vary with `x`.

## Coefficient variants (future, mirroring gemtc)

- **shared** — one β for all treatments (this toy).
- **unrelated** — a separate β_t per treatment vs reference (one β spring length
  per active node, all free).
- **exchangeable** — β_t drawn from a common distribution; in spring terms a
  *second tier* of τ-springs on the β's (hierarchical shrinkage of the
  per-treatment covariate effects).

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
