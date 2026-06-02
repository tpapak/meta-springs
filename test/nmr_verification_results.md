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

## multinma — different estimand for the covariate term (resolved)

Following multinma (0.8.1)'s vignette pattern — `set_agd_arm(..., sample_size, trt_class)`,
`nma(..., regression = ~x, class_interactions = "common")`, covariate centred,
8000 iters with `adapt_delta=0.999, max_treedepth=15` — the fit **converges**
(Rhat 1.00) and its **treatment effects match** (e.g. d[B]=2.56 = 0.6+0.30·6.63
at the centred covariate ✓). But the **effect-modifier coefficient β[x] is
unidentified from aggregate-only data**: posterior ≈ prior (sd ≈ 7.5, 95% CI
[−14.5, +15.3]), even though the MLE pins β=0.257 tightly.

This is multinma's design, not a bug or a mis-spec: `regression = ~x` is the
**ML-NMR effect modifier**, identified from **individual-level (IPD)** covariate
variation. From between-study aggregate variation alone it is *not* identified
(that route is ecological/aggregation bias, which multinma is built to avoid).
**Classical aggregate meta-regression** (gemtc, netmeta) *does* use between-study
variation — and that is exactly what the spring framework implements. Hence:
- spring ≡ gemtc/netmeta (classical aggregate NMR) — exact match (above);
- multinma confirms the **treatment effects** but targets a **different
  estimand** for the covariate term, so it is not a comparator for the
  aggregate β. gemtc is the established-package validation.
