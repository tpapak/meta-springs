# Exact arm/spring representation in the Haskell `Data.Meta.NMA`

Ground truth for matching the visualizations **exactly** (not just equivalently).
Source: `src/Data/Meta/NMA.hs` (`makeSprings`, `Spring`, `SpringVertex`), `Data.Meta.Effects`.

## Nodes (`SpringVertex`)
- `StudyEnd sid` — one node per **study** (the study baseline; call its potential `φ^S_i`).
- `TreatmentEnd tid` — one node per **treatment**, *including the reference* (`φ^T_t`).
- `TauEnd (TauId sid tid)` — one node per **arm**, **only when τ²>0** (the BLUP node).
- Gauge: one vertex is `pinned` (default vertex `1`); its potential is fixed (0).

## Springs (per arm), `Spring k naturalLength springLength`
Energy `½·k·(l − nl)²`, force `−k·(l − nl)`, where `l` = edge length = **potential
difference** of its endpoints. For an arm of treatment `t` in study `i`:

- **Fixed effect (τ²=0 / `Nothing`)** — *one* spring per arm on edge `StudyEnd i → TreatmentEnd t`:
  - `k = 1 / variance(eff)`   (inverse-variance hardness)
  - `nl = expectation(relatedRelative armEffect)` = the arm's **absolute** effect
    (continuous: the **arm mean**; binary: the **arm log-odds**). `relatedRelative (Mean p v) = MD p v`, `expectation (MD p v)=p` — the point estimate is kept.
  - ⇒ at equilibrium the edge length satisfies `φ^T_t − φ^S_i = nl_{it}` (the observed arm effect).
    So the model convention is **`armEffect_{it} = φ^T_t − φ^S_i`** (study potential **subtracted**).
- **Random effects (τ²>0)** — *two* springs per arm, in series via the BLUP node:
  - `StudyEnd i → TauEnd (i,t)`: `Spring (2/τ²) 0 0` (tau spring; hardness `2/τ²`, natural length 0).
  - `TauEnd (i,t) → TreatmentEnd t`: `Spring (1/v) nl nl` (the observation spring as above).

Treatment **contrasts** are potential differences: `d_{ts} = φ^T_t − φ^T_s` (μ_i cancels);
`networkEstimates` are these contrasts. Solve = spring/graph equilibrium (spanning tree,
`(DX)⁻¹DL`), gauge-fixed at the pinned vertex.

Binary: `makeBinomialSprings` uses `BinomialSpring e n l` on the same edges — exact binomial
energy `n·log(1+e^l) − e·l`, hardness `n·p(1−p)`, working-response natural length — solved by IRLS.

## Exact ⇄ what the HTML did (equivalent, NOT identical)
| | Haskell (ground truth) | earlier HTML | identical? |
|---|---|---|---|
| reference treatment | has its own `TreatmentEnd` node; gauge by `pinned` | A had **no node**, pinned at effect 0 | **no** |
| convention | `effect = φ^T_t − φ^S_i` (study **subtracted**) | `y = μ_i + d_t` (study **added**) | **no** (sign/gauge) |
| spring natural length | **absolute** arm effect (mean / log-odds) | contrast vs A | **no** |
| node per arm | StudyEnd→TreatmentEnd (FE); +TauEnd (RE) | study node→treatment node (FE) | topology yes |
| contrasts `d_t − d_s` | identical | identical | **yes** (output equal) |

So the previous views were *equivalent* (same contrasts) but used a different gauge/sign and
relative (not absolute) natural lengths. To be **identical**, a view must: give **every**
treatment a node, **pin** the reference, use **absolute arm effects** as natural lengths with
`k=1/v`, and the convention `φ^T_t − φ^S_i = arm effect`.

## Verified against the running Haskell
`springNMA studies Nothing (Just 0) meanArm` (fixed effect) on
S1:(A=0.40,B=1.80) S2:(A=−0.80,C=1.20) S3:(B=0.90,C=2.10), equal variances, returns
`A→B=1.2, A→C=2.2, B→C=1.0`. Solving the documented model by hand (inverse-variance WLS on the
arm contrasts with A pinned) gives the identical **1.2 / 2.2 / 1.0** — and it is the *network*
estimate (A→B = 1.2, not the direct 1.4), i.e. the full spring equilibrium. ⇒ the spec above is
the exact model; any view built on it produces **identical** contrasts, not merely equivalent ones.

## The covariate extension (must be added to BOTH identically)
The Haskell has **no covariate yet**. To stay identical, the NMR term enters the *same* way in
the HTML and (when implemented) in `makeSprings`: the treatment-end of an arm in study `i` sits at
`φ^T_t + β·x_i` for non-reference `t` (reference unshifted), i.e. the arm spring becomes
`(φ^T_t + β·x_i) − φ^S_i = arm effect`. β is one shared coordinate (the παραλληλόγραφος);
`unrelated` → per-treatment `β_t`. This is the planned `makeSprings` change (extra β column /
covariate-shifted treatment end) so the implementation and the picture remain the same model.
