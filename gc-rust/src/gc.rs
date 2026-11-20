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
    /// τ (the heterogeneity SD) ~ Uniform(0, upper). This is gemtc's default
    /// `mtc.hy.prior("std.dev", "dunif", 0, om.scale)`; `upper` = om.scale.
    UniformSD(f64),
    /// τ² ~ LogNormal(μ, σ²) on the VARIANCE — the Turner et al. (2012/2015)
    /// and Rhodes et al. (2015) empirical predictive priors. Fields (μ, σ).
    LogNormalTau2(f64, f64),
    /// τ ~ Exponential(rate) on the SD.
    ExpTau(f64),
}

impl Tau2Prior {
    /// Parse a prior spec: `flat | hn:S | hc:S | unif:U | exp:R` (on τ) or
    /// `ln:MU:SIGMA` (log-normal on τ²).
    pub fn parse(spec: &str) -> Result<Tau2Prior, String> {
        let s = spec.trim().to_lowercase();
        let num = |x: &str| x.parse::<f64>().map_err(|e| format!("prior '{spec}': {e}"));
        if s == "flat" {
            return Ok(Tau2Prior::Flat);
        }
        let (kind, rest) = s.split_once(':').ok_or_else(|| format!("bad prior '{spec}'"))?;
        Ok(match kind {
            "hn" => Tau2Prior::HalfNormalTau(num(rest)?),
            "hc" => Tau2Prior::HalfCauchyTau(num(rest)?),
            "unif" => Tau2Prior::UniformSD(num(rest)?),
            "exp" => Tau2Prior::ExpTau(num(rest)?),
            "ln" => {
                let (m, sd) = rest.split_once(':').ok_or_else(|| format!("ln prior needs ln:MU:SIGMA, got '{spec}'"))?;
                Tau2Prior::LogNormalTau2(num(m)?, num(sd)?)
            }
            _ => return Err(format!("unknown prior '{spec}'")),
        })
    }

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
            Tau2Prior::UniformSD(u) => format!("Uniform(τ; 0, {})", u),
            Tau2Prior::LogNormalTau2(m, sd) => format!("LogNormal(τ²; {}, {}²)", m, sd),
            Tau2Prior::ExpTau(r) => format!("Exponential(τ; rate={})", r),
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
            // Uniform on τ = √τ² over [0, u]: density on τ² is ∝ τ²^(-1/2)
            // (the Jacobian dτ/dτ²) truncated at τ² = u². Constant dropped.
            Tau2Prior::UniformSD(u) => {
                if t2 > u * u { -1e30 } else { -0.5 * t2.ln() }
            }
            Tau2Prior::LogNormalTau2(m, sd) => {
                let z = (t2.ln() - m) / sd;
                -t2.ln() - 0.5 * z * z
            }
            // p(τ) = r e^{-rτ}; on τ²: × 1/(2τ).
            Tau2Prior::ExpTau(r) => -0.5 * t2.ln() - r * t2.sqrt(),
        }
    }

    /// NORMALISED log density of τ² (all constants kept), or `None` for the
    /// improper flat prior. Needed for marginal likelihoods / Bayes factors;
    /// differs from [`log_prior`] only by a τ²-independent constant.
    pub fn log_prior_norm(&self, t2: f64) -> Option<f64> {
        use std::f64::consts::PI;
        if t2 <= 0.0 {
            return Some(f64::NEG_INFINITY);
        }
        let ln_tau = 0.5 * t2.ln();
        match self {
            Tau2Prior::Flat => None,
            // p(τ²) = p(τ) / (2τ)
            Tau2Prior::HalfNormalTau(s) => Some(
                (2.0 / (s * (2.0 * PI).sqrt())).ln() - 0.5 * t2 / (s * s) - (2.0f64).ln() - ln_tau,
            ),
            Tau2Prior::HalfCauchyTau(s) => Some(
                (2.0 / (PI * s)).ln() - (1.0 + t2 / (s * s)).ln() - (2.0f64).ln() - ln_tau,
            ),
            Tau2Prior::UniformSD(u) => Some(if t2 > u * u {
                f64::NEG_INFINITY
            } else {
                -(u.ln()) - (2.0f64).ln() - ln_tau
            }),
            Tau2Prior::LogNormalTau2(m, sd) => {
                let z = (t2.ln() - m) / sd;
                Some(-t2.ln() - sd.ln() - 0.5 * (2.0 * PI).ln() - 0.5 * z * z)
            }
            Tau2Prior::ExpTau(r) => Some(r.ln() - r * t2.sqrt() - (2.0f64).ln() - ln_tau),
        }
    }

    /// Prior CDF P(τ² ≤ t2) — used to account for the prior mass below the
    /// grid's lower bound (where Z(τ²) ≈ Z(0)). `None` for the flat prior.
    pub fn cdf(&self, t2: f64) -> Option<f64> {
        if t2 <= 0.0 {
            return Some(0.0);
        }
        let tau = t2.sqrt();
        match self {
            Tau2Prior::Flat => None,
            Tau2Prior::HalfNormalTau(s) => Some(erf(tau / (s * std::f64::consts::SQRT_2))),
            Tau2Prior::HalfCauchyTau(s) => Some(2.0 / std::f64::consts::PI * (tau / s).atan()),
            Tau2Prior::UniformSD(u) => Some((tau / u).min(1.0)),
            Tau2Prior::LogNormalTau2(m, sd) => {
                Some(0.5 * (1.0 + erf((t2.ln() - m) / (sd * std::f64::consts::SQRT_2))))
            }
            Tau2Prior::ExpTau(r) => Some(1.0 - (-r * tau).exp()),
        }
    }
}

/// erf via Abramowitz–Stegun 7.1.26 refined (|err| < 1.2e-7) — ample for the
/// sub-grid prior-mass correction.
fn erf(x: f64) -> f64 {
    // Numerical Recipes erfc Chebyshev approximation (fractional err < 1.2e-7).
    let z = x.abs();
    let t = 1.0 / (1.0 + 0.5 * z);
    let r = t * (-z * z - 1.26551223
        + t * (1.00002368
        + t * (0.37409196
        + t * (0.09678418
        + t * (-0.18628806
        + t * (0.27886807
        + t * (-1.13520398
        + t * (1.48851587
        + t * (-0.82215223 + t * 0.17087277)))))))))
        .exp();
    if x >= 0.0 { 1.0 - r } else { r - 1.0 }
}

/// log marginal likelihood  log ∫ Z(τ²) π(τ²) dτ²  on a log-spaced τ² grid
/// (trapezoid in log τ²), with the prior mass below the grid's lower edge
/// assigned Z(τ²_lo) (Z is flat as τ²→0). `None` for an improper prior.
/// Constants common to all τ² in log Z cancel in Bayes factors.
pub fn log_marginal(grid: &[(f64, f64)], prior: Tau2Prior) -> Option<f64> {
    let mut pts: Vec<(f64, f64)> = grid.iter().copied().filter(|(t, _)| *t > 0.0).collect();
    pts.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
    if pts.len() < 2 {
        return None;
    }
    // integrand in u = log τ²:  Z(τ²) π(τ²) τ²
    let mut lf = Vec::with_capacity(pts.len());
    for (t, lz) in &pts {
        lf.push(lz + prior.log_prior_norm(*t)? + t.ln());
    }
    let m = lf.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let mut acc = 0.0;
    for i in 1..pts.len() {
        let du = pts[i].0.ln() - pts[i - 1].0.ln();
        acc += 0.5 * du * ((lf[i] - m).exp() + (lf[i - 1] - m).exp());
    }
    // sub-grid mass: P(τ² < τ²_lo) · Z(τ²_lo)
    let below = prior.cdf(pts[0].0)?;
    acc += below * (pts[0].1 - m).exp();
    Some(m + acc.ln())
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

/// Like [`adaptive_grid_taus_with`] but guarantees the upper τ² reaches at
/// least `min_hi`. Needed when a bounded prior (e.g. Uniform on τ over
/// `[0, u]`, gemtc's default) places posterior mass at large τ² that the
/// REML-mode-centred default bound would otherwise truncate.
pub fn adaptive_grid_taus_min_hi(
    reml_mode: f64,
    n_pts: usize,
    bounds: GridBounds,
    min_hi: f64,
) -> Vec<f64> {
    let t_hat = reml_mode.max(1e-5);
    let (lo, hi0) = bounds.lo_hi(t_hat);
    let hi = hi0.max(min_hi);
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
    run_adaptive_with_min_hi(eval_at, n_pts, bounds, 0.0)
}

/// As [`run_adaptive_with`] but with a minimum upper τ² bound (see
/// [`adaptive_grid_taus_min_hi`]).
pub fn run_adaptive_with_min_hi<F: Fn(f64) -> GridPoint>(
    eval_at: F,
    n_pts: usize,
    bounds: GridBounds,
    min_hi: f64,
) -> (f64, Vec<GridPoint>) {
    let mode = find_reml_mode_with(|t2| eval_at(t2).log_z);
    let mut taus = adaptive_grid_taus_min_hi(mode, n_pts, bounds, min_hi);
    taus.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let grid: Vec<GridPoint> = taus.iter().map(|&t| eval_at(t)).collect();
    (mode, grid)
}

/// Convenience: run the right backend by `Dataset::mode`, with a minimum upper
/// τ² grid bound. Mirrors [`run_adaptive_auto`].
pub fn run_adaptive_auto_min_hi(
    ds: &Dataset,
    n_pts: usize,
    min_hi: f64,
) -> (Topology, f64, Vec<GridPoint>) {
    match ds.mode {
        Mode::Binary => {
            let topo = Topology::build(ds);
            let (mode, grid) = run_adaptive_with_min_hi(
                |t2| eval_xi_at(&topo, t2),
                n_pts,
                GridBounds::Binomial,
                min_hi,
            );
            (topo, mode, grid)
        }
        Mode::Continuous => {
            let topo = Topology::build(ds);
            let arms = cont_arms(ds);
            let (mode, grid) = run_adaptive_with_min_hi(
                |t2| eval_xi_cont_at(&topo, &arms, t2),
                n_pts,
                GridBounds::Continuous,
                min_hi,
            );
            (topo, mode, grid)
        }
    }
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
    // Quantiles: midpoint-rule CDF (node i carries mass w_i centred on it),
    // linearly interpolated in log τ² between nodes — avoids snapping every
    // quantile to a grid node.
    let mids: Vec<f64> = cums.iter().zip(pairs.iter()).map(|(c, (_, w))| c - 0.5 * w).collect();
    let w_quantile = |q: f64| -> f64 {
        let n = pairs.len();
        if q <= mids[0] {
            return pairs[0].0;
        }
        if q >= mids[n - 1] {
            return pairs[n - 1].0;
        }
        let mut ix = 1usize;
        while ix < n - 1 && mids[ix] < q {
            ix += 1;
        }
        let (c0, c1) = (mids[ix - 1], mids[ix]);
        let f = if c1 > c0 { (q - c0) / (c1 - c0) } else { 0.5 };
        let (l0, l1) = (pairs[ix - 1].0.ln(), pairs[ix].0.ln());
        (l0 + f * (l1 - l0)).exp()
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
