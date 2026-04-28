//! Grand Canonical NMA pipeline: τ² mode finder, adaptive log-spaced grid,
//! and prior-driven posterior summaries.
//!
//! Mirrors `springGrandCanonicalBinAdaptive` and `evalXiBinAt` from
//! `src/Data/Meta/RandomEffects.hs:1591..1739` for the binomial path and
//! `springGrandCanonicalAdaptive` (lines 1308-1465) for the continuous path.
//!
//! The mode finder, grid construction and posterior reduction are shared
//! between both likelihoods — the only thing that varies is the
//! `evalAt(τ²) -> GridPoint` callback. We therefore expose generic helpers
//! parameterised by a closure plus thin wrappers for each likelihood.

use crate::data::{Dataset, Mode};
use crate::solver::{eval_xi_at, GridPoint, Topology};
use crate::solver_cont::{cont_arms, eval_xi_cont_at, ContArm};

#[derive(Debug, Clone, Copy)]
pub enum Tau2Prior {
    Flat,
    HalfNormalTau(f64),
    HalfCauchyTau(f64),
}

impl Tau2Prior {
    pub fn label(&self) -> String {
        match self {
            Tau2Prior::Flat => "Flat (REML)".into(),
            Tau2Prior::HalfNormalTau(s) => {
                if (*s - 0.5).abs() < 1e-12 {
                    "HalfNormal(τ; σ=0.5)".into()
                } else if (*s - 1.0).abs() < 1e-12 {
                    "HalfNormal(τ; σ=1)".into()
                } else {
                    format!("HalfNormal(τ; σ={})", s)
                }
            }
            Tau2Prior::HalfCauchyTau(s) => {
                if (*s - 0.5).abs() < 1e-12 {
                    "HalfCauchy(τ; σ=0.5)".into()
                } else if (*s - 1.0).abs() < 1e-12 {
                    "HalfCauchy(τ; σ=1)".into()
                } else {
                    format!("HalfCauchy(τ; σ={})", s)
                }
            }
        }
    }

    /// log π(τ²). Drops constants. Mirrors `logTau2Prior`.
    pub fn log_prior(&self, t2: f64) -> f64 {
        if t2 <= 0.0 {
            return -1e30;
        }
        match self {
            Tau2Prior::Flat => 0.0,
            Tau2Prior::HalfNormalTau(s) => -0.5 * t2.ln() - 0.5 * t2 / (s * s),
            Tau2Prior::HalfCauchyTau(s) => {
                -0.5 * t2.ln() - (1.0 + t2 / (s * s)).ln()
            }
        }
    }
}

/// Approximate dlogZ/dτ² by central finite difference. h ≈ 1e-3 · τ².
fn d_log_z<F: Fn(f64) -> f64>(eval_lz: &F, t2: f64) -> f64 {
    let h = (1e-3 * t2).max(1e-9);
    let lo = eval_lz((t2 - h).max(1e-10));
    let hi = eval_lz(t2 + h);
    (hi - lo) / (2.0 * h)
}

/// Find the REML mode of log Z(τ²) by bisection on the sign of dlogZ/dτ².
/// Bracket from τ²=1 by halving/doubling until the sign flips, then bisect.
///
/// Generic over the log-evidence callback so the same routine drives both
/// the binomial and the continuous paths.
pub fn find_reml_mode_with<F: Fn(f64) -> f64>(eval_lz: F) -> f64 {
    let t = 1.0_f64;
    let d0 = d_log_z(&eval_lz, t);
    let mut lo;
    let mut hi;
    if d0 > 0.0 {
        lo = t;
        let mut t2 = t * 2.0;
        let mut d2 = d_log_z(&eval_lz, t2);
        let mut steps = 0;
        while d2 > 0.0 && steps < 30 {
            t2 *= 2.0;
            d2 = d_log_z(&eval_lz, t2);
            steps += 1;
        }
        hi = t2;
    } else {
        hi = t;
        let mut t2 = t * 0.5;
        let mut d2 = d_log_z(&eval_lz, t2);
        let mut steps = 0;
        while d2 < 0.0 && steps < 40 {
            t2 *= 0.5;
            d2 = d_log_z(&eval_lz, t2);
            steps += 1;
            if t2 < 1e-9 {
                break;
            }
        }
        lo = t2.max(1e-9);
    }

    for _ in 0..80 {
        let mid = (lo * hi).sqrt();
        let dm = d_log_z(&eval_lz, mid);
        if dm > 0.0 {
            lo = mid;
        } else {
            hi = mid;
        }
        if (hi - lo) / mid < 1e-6 {
            return mid;
        }
    }
    (lo * hi).sqrt()
}

/// Binomial-path REML mode finder (kept as a thin wrapper for backwards
/// compatibility with callers using the binomial-only API).
pub fn find_reml_mode(topo: &Topology) -> f64 {
    find_reml_mode_with(|t2| eval_xi_at(topo, t2).log_z)
}

/// Adaptive grid bounds. The two likelihoods use slightly different bounds
/// in the Haskell reference and we faithfully port both.
#[derive(Debug, Clone, Copy)]
pub enum GridBounds {
    /// Binomial: `[max(τ̂/30, 1e-6), max(τ̂·30, 5)]` — matches
    /// `springGrandCanonicalBinAdaptive` in `RandomEffects.hs`.
    Binomial,
    /// Gaussian: `[max(τ̂/1000, 1e-4), max(τ̂·4, 5)]` — matches the
    /// `springGrandCanonicalAdaptive` definition in `RandomEffects.hs`
    /// lines 1453-1454. The continuous path uses much wider lower bounds
    /// because its log Z is finite at τ²→0 (no rare-events floor).
    Continuous,
}

impl GridBounds {
    fn lo_hi(&self, mode: f64) -> (f64, f64) {
        match self {
            GridBounds::Binomial => {
                let lo = (mode / 30.0).max(1e-6);
                let hi = (mode * 30.0).max(5.0);
                (lo, hi)
            }
            GridBounds::Continuous => {
                let lo = (mode / 1000.0).max(1e-4);
                let hi = (mode * 4.0).max(5.0);
                (lo, hi)
            }
        }
    }
}

/// Build the adaptive log-spaced τ² grid using the binomial bounds.
/// Kept for backwards compatibility; new code should call
/// [`adaptive_grid_taus_with`] with an explicit [`GridBounds`].
pub fn adaptive_grid_taus(reml_mode: f64, n_pts: usize) -> Vec<f64> {
    adaptive_grid_taus_with(reml_mode, n_pts, GridBounds::Binomial)
}

/// Build the adaptive log-spaced τ² grid for an explicit bound choice.
pub fn adaptive_grid_taus_with(
    reml_mode: f64,
    n_pts: usize,
    bounds: GridBounds,
) -> Vec<f64> {
    let t_hat = reml_mode.max(1e-5);
    let (lo, hi) = bounds.lo_hi(t_hat);
    (0..n_pts)
        .map(|i| {
            let f = i as f64 / (n_pts as f64 - 1.0);
            lo * (hi / lo).powf(f)
        })
        .collect()
}

/// Run the GC pipeline against an arbitrary likelihood by passing in the
/// per-grid-point evaluator. Returns the REML mode and a τ²-sorted grid.
pub fn run_adaptive_with<F: Fn(f64) -> GridPoint>(
    eval_at: F,
    n_pts: usize,
    bounds: GridBounds,
) -> (f64, Vec<GridPoint>) {
    let mode = find_reml_mode_with(|t2| eval_at(t2).log_z);
    let mut taus = adaptive_grid_taus_with(mode, n_pts, bounds);
    taus.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let grid: Vec<GridPoint> = taus.iter().map(|&t| eval_at(t)).collect();
    (mode, grid)
}

/// Run the binomial GC pipeline. Mirrors the existing public API used by the
/// binary tests; remains bit-identical to the pre-refactor implementation.
pub fn run_adaptive(ds: &Dataset, n_pts: usize) -> (Topology, f64, Vec<GridPoint>) {
    let topo = Topology::build(ds);
    let (mode, grid) =
        run_adaptive_with(|t2| eval_xi_at(&topo, t2), n_pts, GridBounds::Binomial);
    (topo, mode, grid)
}

/// Run the continuous GC pipeline.
pub fn run_adaptive_cont(
    ds: &Dataset,
    n_pts: usize,
) -> (Topology, Vec<ContArm>, f64, Vec<GridPoint>) {
    let topo = Topology::build(ds);
    let arms = cont_arms(ds);
    let (mode, grid) = run_adaptive_with(
        |t2| eval_xi_cont_at(&topo, &arms, t2),
        n_pts,
        GridBounds::Continuous,
    );
    (topo, arms, mode, grid)
}

/// Convenience entry point that picks the right backend by `Dataset::mode`.
/// The returned tuple shape `(topology, mode, grid)` matches the binomial
/// run; continuous callers that need the per-arm `ContArm` payload should
/// call `run_adaptive_cont` directly.
pub fn run_adaptive_auto(
    ds: &Dataset,
    n_pts: usize,
) -> (Topology, f64, Vec<GridPoint>) {
    match ds.mode {
        Mode::Binary => run_adaptive(ds, n_pts),
        Mode::Continuous => {
            let (topo, _arms, mode, grid) = run_adaptive_cont(ds, n_pts);
            (topo, mode, grid)
        }
    }
}

#[derive(Debug, Clone)]
pub struct PosteriorSummary {
    pub mode: f64,
    pub median: f64,
    pub mean: f64,
    pub ci_lo: f64,
    pub ci_hi: f64,
    /// Normalised weights, one per grid point (in input order).
    pub weights: Vec<f64>,
}

/// Compute posterior summary under a τ² prior given a log-spaced grid of
/// (τ², logZ).  Mirrors `posteriorUnder True` from the Haskell:
///
///   log_post(τ²) = logZ(τ²) + log π(τ²) + log(τ²)   (Jacobian for log-grid)
pub fn posterior_under(
    grid: &[(f64, f64)], // (tau2, logZ)
    prior: Tau2Prior,
) -> PosteriorSummary {
    // Filter τ² > 0 (defensive).
    let pts: Vec<(f64, f64)> = grid.iter().copied().filter(|(t, _)| *t > 0.0).collect();

    // log p(τ² | y) ∝ logZ + log_prior — the density on the natural Lebesgue
    // scale of τ². The MODE should maximise this — no Jacobian shift.
    let log_density: Vec<f64> = pts
        .iter()
        .map(|(t, lz)| lz + prior.log_prior(*t))
        .collect();

    // Trapezoid weights for log-spaced quadrature need the Jacobian
    // dτ²/dlog τ² = τ² so the integral ∫ p(τ²) dτ² is correct.  These
    // weights are used for CDF-based summaries (median, CI) and mean.
    // They are NOT used to find the mode: including +log τ² would otherwise
    // shift the argmax to larger τ² (matters most when τ² > 1).
    let lps: Vec<f64> = pts
        .iter()
        .zip(log_density.iter())
        .map(|((t, _), &ld)| ld + t.max(1e-300).ln())
        .collect();
    let m = lps.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let ws_un: Vec<f64> = lps.iter().map(|lp| (lp - m).exp()).collect();
    let tot: f64 = ws_un.iter().sum();
    let weights: Vec<f64> = ws_un.iter().map(|w| w / tot).collect();

    // Mode = argmax of the density.
    let mode_idx = log_density
        .iter()
        .enumerate()
        .fold((0usize, f64::NEG_INFINITY), |acc, (i, &v)| {
            if v > acc.1 { (i, v) } else { acc }
        })
        .0;
    let mode = pts[mode_idx].0;

    // Mean.
    let mean: f64 = pts.iter().zip(weights.iter()).map(|((t, _), w)| t * w).sum();

    // Sort (τ², w) by τ², then weighted CDF.
    let mut pairs: Vec<(f64, f64)> =
        pts.iter().map(|(t, _)| *t).zip(weights.iter().copied()).collect();
    pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    let mut cums = vec![0.0_f64; pairs.len()];
    let mut acc = 0.0;
    for (i, (_, w)) in pairs.iter().enumerate() {
        acc += w;
        cums[i] = acc;
    }
    let w_quantile = |q: f64| -> f64 {
        // index of first cum >= q (matches Haskell `length (takeWhile (<q) cums)`).
        let mut ix = 0usize;
        while ix < cums.len() && cums[ix] < q {
            ix += 1;
        }
        if ix >= pairs.len() {
            pairs[pairs.len() - 1].0
        } else {
            pairs[ix].0
        }
    };

    PosteriorSummary {
        mode,
        median: w_quantile(0.5),
        mean,
        ci_lo: w_quantile(0.025),
        ci_hi: w_quantile(0.975),
        weights,
    }
}

/// Posterior weights with the same convention as `posterior_under` but
/// returned in the SAME order as the input grid (not sorted). Useful for
/// computing per-contrast mixture moments.
pub fn grid_weights(grid: &[(f64, f64)], prior: Tau2Prior) -> Vec<f64> {
    let lps: Vec<f64> = grid
        .iter()
        .map(|(t, lz)| {
            if *t > 0.0 {
                lz + prior.log_prior(*t) + t.max(1e-300).ln()
            } else {
                f64::NEG_INFINITY
            }
        })
        .collect();
    let m = lps.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let ws_un: Vec<f64> = lps.iter().map(|lp| (lp - m).exp()).collect();
    let tot: f64 = ws_un.iter().sum();
    ws_un.iter().map(|w| w / tot).collect()
}
