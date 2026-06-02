# NMR spring framework — verification results

Cross-checks of the spring network-meta-regression against established packages.
Scripts: `test/nmr_*_verify.R`, `test/nmr_*_simulation*.R` (raw logs gitignored).
The spring estimate is the **equilibrium = weighted least squares = the NMR MLE**
(the HTML relaxations converge to it; the simulations solve it directly).

## Point checks (real / fixed data)

| Example | covariate | spring β | gemtc β | match |
|---|---|---|---|---|
| Star toy (3 trt, Gaussian) | binary | 1.023 | 0.993 (SD 0.326) | ✓ |
| Bigger looped (5 trt, 14 studies, multi-arm) | binary | 0.463 | 0.443 | ✓ (≤0.006 on effects) |
| **Certolizumab** (real, binomial, 12 trials) | continuous (disease duration) | 0.138 /yr | 0.135 /yr | ✓ |

gemtc reports `B` per **2·SD** (it standardises the covariate); divide by `scale` for per-unit.

## Monte-Carlo

- **Binary covariate, 120 reps** (4-trt networks with loops), spring vs gemtc:
  β cor **0.9993**, RMSD 0.028; d.A.B cor 0.99999. Tiny residual = MCMC noise.
- **Continuous covariate, 30 reps** (well-identified, A-heavy), spring vs gemtc:
  β **cor 1.0000, RMSD 0.001, lm slope 0.998** — exact, no shrinkage once the
  covariate is correctly mapped to studies.

### Two harness bugs found & fixed (worth recording)
1. *Relaxation in the sim loop* didn't converge in the iteration budget for
   strongly-coupled random networks → use the exact equilibrium (WLS) for the
   correctness test; keep relaxation for the HTML demos (where it does converge,
   gradient ~1e-14).
2. *`split(x, study)` orders groups lexicographically* (S1, S10, S11, …, S2), so
   the per-study covariate vector handed to gemtc/multinma was scrambled →
   garbage slopes, while the spring (raw arm-level `x`) was unaffected. Fixed by
   building the covariate in `S1..Sn` order.

## multinma — not yet correctly specified

`multinma` (0.8.1) ran, but `regression = ~x` with `class_interactions="common"`
on an AgD-only network gave inconsistent slopes across seeds (ratios ×14.6,
×17.4, ×−10.3 vs the MLE — sign-flipping), i.e. it is **not** estimating the
shared aggregate-level effect modifier under this specification. multinma's NMR
API targets IPD + AgD (ML-NMR); the AgD-only shared-EM spec needs the correct
form (prognostic vs effect-modifier terms, centering/standardisation). **Open** —
gemtc (exact match above) is the established-package validation in the meantime.
