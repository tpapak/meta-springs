//! ML-NMR (IPD + AgD) spring engine.
//!
//! Reads a fully-built design (the R facade `springs::mlnmr()` builds it from
//! the regression formula), relaxes the spring network to equilibrium, and
//! returns the posterior:
//!
//! * fixed effects: the equilibrium + curvature (Laplace), optionally
//!   importance-corrected against the exact energy (`correction: "is"`, default);
//! * random effects: the grand-canonical mixture over a τ² grid; each grid
//!   point's partition function is importance-corrected the same way, and every
//!   τ prior in `tau_priors` is a reweighting of the SAME grid.
//!
//! Usage:  mlnmr --input design.json --output post.json
//!
//! Input JSON:
//! ```text
//! { "link": "logit"|"probit", "agd_likelihood": "binomial"|"binomial2",
//!   "par_names": [..P..], "prior_prec": [..P..],   // N(0,1/prec); 0 = flat
//!   "priors": [{"dist":"normal"|"t"|"flat","loc":..,"scale":..,"df":..}, ..P..],
//!                                              // optional, overrides prior_prec
//!   "return_pool": false,                      // true ⇒ also the weighted IS pool
//!   "re_groups": [[col, ...], ...],            // empty ⇒ fixed effects
//!   "tau_priors": ["hn:2.5", ...],             // first = primary (draws)
//!   "units": [{"r":.., "n":.., "q":Q, "x":[Q*P row-major]}, ...],
//!   "samples": 4000, "seed": 1, "n_grid": 40,
//!   "correction": "is"|"laplace", "is_per_point": 400 }
//! ```

use clap::Parser;
use nalgebra::{Cholesky, DMatrix, DVector};
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;
use std::time::Instant;

use gc_rust::gc::{grid_weights, log_marginal, posterior_under, Tau2Prior};
use gc_rust::ipd::{fit_mode, gc_grid, is_pool, log_grid, AgdLik, Gauss, GcPoint, Link, ParPrior, Problem, Rng, Unit};

const NU: f64 = 8.0; // proposal degrees of freedom (multivariate t)

#[derive(Parser, Debug)]
#[command(name = "mlnmr", about = "IPD+AgD ML-NMR via the spring / grand-canonical engine")]
struct Args {
    #[arg(long)]
    input: String,
    #[arg(long)]
    output: String,
}

#[derive(Deserialize)]
struct UnitIn {
    /// "binomial" (default) | "ipd_normal" | "arm_normal" | "suff_normal"
    #[serde(default)]
    kind: String,
    #[serde(default)]
    r: f64,
    #[serde(default = "d_onef")]
    n: f64,
    #[serde(default = "d_one_usize")]
    q: usize,
    #[serde(default)]
    x: Vec<f64>,
    #[serde(default)]
    y: f64,
    #[serde(default)]
    se: f64,
    #[serde(default)]
    sig: usize,
    #[serde(default)]
    xtx: Vec<f64>,
    #[serde(default)]
    xty: Vec<f64>,
    #[serde(default)]
    yty: f64,
}
fn d_onef() -> f64 { 1.0 }
fn d_one_usize() -> usize { 1 }

fn to_unit(u: UnitIn, p: usize) -> Unit {
    let g = match u.kind.as_str() {
        "" | "binomial" => None,
        "ipd_normal" => Some(Box::new(Gauss::Ipd { y: u.y, sig: u.sig })),
        "arm_normal" => Some(Box::new(Gauss::Arm { y: u.y, se: u.se })),
        "suff_normal" => {
            assert_eq!(u.xtx.len(), p * p, "suff xtx size");
            assert_eq!(u.xty.len(), p, "suff xty size");
            Some(Box::new(Gauss::Suff { xtx: u.xtx, xty: u.xty, yty: u.yty, n: u.n, sig: u.sig }))
        }
        k => panic!("unknown unit kind '{k}'"),
    };
    let x = if u.x.is_empty() { vec![0.0; p] } else { u.x };
    assert_eq!(x.len(), u.q * p, "unit design size mismatch");
    Unit { r: u.r, n: u.n, q: u.q, x, g }
}

#[derive(Deserialize)]
struct PriorIn {
    dist: String,
    #[serde(default)]
    loc: f64,
    #[serde(default = "d_one")]
    scale: f64,
    #[serde(default = "d_one")]
    df: f64,
}
fn d_one() -> f64 { 1.0 }

impl PriorIn {
    fn to_prior(&self) -> ParPrior {
        match self.dist.as_str() {
            "flat" => ParPrior::Flat,
            "normal" if self.scale.is_finite() => ParPrior::Normal { loc: self.loc, scale: self.scale },
            "normal" => ParPrior::Flat,
            "t" | "student_t" if self.scale.is_finite() => ParPrior::StudentT { df: self.df, loc: self.loc, scale: self.scale },
            "cauchy" if self.scale.is_finite() => ParPrior::StudentT { df: 1.0, loc: self.loc, scale: self.scale },
            "t" | "student_t" | "cauchy" => ParPrior::Flat,
            "halfnormal_log" => ParPrior::HalfNormalLog { scale: self.scale },
            d => panic!("unsupported prior '{d}' (normal | t | cauchy | flat)"),
        }
    }
}

/// The weighted importance pool behind every summary: re-scoring it under a
/// new prior is a reweighting (`springs::mlnmr_reweight`).
#[derive(Serialize)]
struct PoolOut {
    names: Vec<String>,
    /// values (fixed parameters, then tau for RE)
    values: Vec<Vec<f64>>,
    /// τ² grid index of each value (RE), empty for FE
    grid: Vec<usize>,
    /// importance weight, normalised within its grid point (FE: overall)
    w: Vec<f64>,
    /// per τ prior: normalised grid-point mixture weights (RE)
    prior_grid_w: Vec<Vec<f64>>,
}

#[derive(Deserialize)]
struct Input {
    link: String,
    #[serde(default = "d_lik")]
    agd_likelihood: String,
    par_names: Vec<String>,
    #[serde(default)]
    prior_prec: Vec<f64>,
    #[serde(default)]
    priors: Vec<PriorIn>,
    #[serde(default)]
    return_pool: bool,
    /// starting configuration (length P; default zeros)
    #[serde(default)]
    init: Vec<f64>,
    #[serde(default)]
    re_groups: Vec<Vec<usize>>,
    #[serde(default = "d_priors")]
    tau_priors: Vec<String>,
    units: Vec<UnitIn>,
    #[serde(default = "d_samples")]
    samples: usize,
    #[serde(default = "d_seed")]
    seed: u64,
    #[serde(default = "d_grid")]
    n_grid: usize,
    #[serde(default = "d_corr")]
    correction: String,
    #[serde(default = "d_isp")]
    is_per_point: usize,
}
fn d_lik() -> String { "binomial".into() }
fn d_priors() -> Vec<String> { vec!["hn:1".into()] }
fn d_samples() -> usize { 4000 }
fn d_seed() -> u64 { 1 }
fn d_grid() -> usize { 40 }
fn d_corr() -> String { "is".into() }
fn d_isp() -> usize { 400 }

#[derive(Serialize)]
struct ParSummary {
    name: String,
    mean: f64,
    sd: f64,
    q025: f64,
    q50: f64,
    q975: f64,
}

#[derive(Serialize)]
struct PriorOut {
    prior: String,
    tau: Option<ParSummary>,
    /// log Bayes factor RE vs FE (proper priors only)
    log_bf_re_fe: Option<f64>,
    /// Kish effective sample size of the (importance × grid) weights
    ess: f64,
    pars: Vec<ParSummary>,
}

#[derive(Serialize)]
struct Output {
    model: String,
    link: String,
    agd_likelihood: String,
    correction: String,
    n_units: usize,
    n_par: usize,
    n_re: usize,
    fixed_names: Vec<String>,
    /// equilibrium (posterior mode) of the fixed parameters (FE model)
    mode: Vec<f64>,
    /// FE only: Laplace covariance of the fixed parameters
    cov: Option<Vec<Vec<f64>>>,
    log_lik_mode: f64,
    iterations: usize,
    converged: bool,
    observed_information: bool,
    /// FE: log ∫exp(F) importance-corrected minus Laplace (≈0 ⇔ harmonic springs)
    log_z_correction: Option<f64>,
    /// FE: log ∫exp(F) dθ, the (importance-corrected) marginal likelihood with the
    /// UNNORMALISED prior densities of the engine (add their normalising constants
    /// to compare models)
    log_marginal_unnorm: Option<f64>,
    draw_names: Vec<String>,
    draws: Vec<Vec<f64>>,
    priors: Vec<PriorOut>,
    grid_tau2: Vec<f64>,
    grid_log_z: Vec<f64>,
    grid_log_z_laplace: Vec<f64>,
    grid_ess: Vec<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pool: Option<PoolOut>,
    fit_wall_sec: f64,
}

fn block(m: &DMatrix<f64>, idx: &[usize]) -> DMatrix<f64> {
    DMatrix::from_fn(idx.len(), idx.len(), |i, j| m[(idx[i], idx[j])])
}

/// Weighted summaries of columns; `w` normalised.
fn wsummary(names: &[String], vals: &[Vec<f64>], w: &[f64]) -> Vec<ParSummary> {
    names
        .iter()
        .enumerate()
        .map(|(j, nm)| {
            let mean: f64 = vals.iter().zip(w).map(|(v, wi)| v[j] * wi).sum();
            let var: f64 = vals.iter().zip(w).map(|(v, wi)| (v[j] - mean).powi(2) * wi).sum();
            let mut pr: Vec<(f64, f64)> = vals.iter().zip(w).map(|(v, wi)| (v[j], *wi)).filter(|p| p.1 > 0.0).collect();
            pr.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
            let q = |p: f64| {
                // midpoint-rule weighted CDF, linear interpolation
                let mut acc = 0.0;
                let mut prev = (pr[0].0, 0.0);
                for &(x, wi) in &pr {
                    let mid = acc + 0.5 * wi;
                    if mid >= p {
                        let f = if mid > prev.1 { (p - prev.1) / (mid - prev.1) } else { 1.0 };
                        return prev.0 + f.clamp(0.0, 1.0) * (x - prev.0);
                    }
                    prev = (x, mid);
                    acc += wi;
                }
                pr[pr.len() - 1].0
            };
            ParSummary { name: nm.clone(), mean, sd: var.max(0.0).sqrt(), q025: q(0.025), q50: q(0.5), q975: q(0.975) }
        })
        .collect()
}

/// Systematic resampling of `s` indices from normalised weights.
fn resample(w: &[f64], s: usize, rng: &mut Rng) -> Vec<usize> {
    let u0 = rng.unif() / s as f64;
    let mut out = Vec::with_capacity(s);
    let (mut acc, mut i) = (w[0], 0usize);
    for k in 0..s {
        let u = u0 + k as f64 / s as f64;
        while acc < u && i + 1 < w.len() {
            i += 1;
            acc += w[i];
        }
        out.push(i);
    }
    // shuffle so consecutive draws are not ordered by grid point
    for k in (1..out.len()).rev() {
        let j = (rng.unif() * (k + 1) as f64) as usize;
        out.swap(k, j.min(k));
    }
    out
}

fn normalise_log(lw: &[f64]) -> (Vec<f64>, f64) {
    let m = lw.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let w: Vec<f64> = lw.iter().map(|x| (x - m).exp()).collect();
    let s: f64 = w.iter().sum();
    let w: Vec<f64> = w.iter().map(|x| x / s).collect();
    let ess = 1.0 / w.iter().map(|x| x * x).sum::<f64>();
    (w, ess)
}

fn main() {
    let args = Args::parse();
    let t0 = Instant::now();
    let inp: Input = serde_json::from_str(&fs::read_to_string(&args.input).expect("read input"))
        .expect("parse input JSON");
    let p = inp.par_names.len();
    let link = match inp.link.as_str() {
        "logit" => Link::Logit,
        "probit" => Link::Probit,
        "identity" => Link::Logit, // Gaussian units ignore the link
        l => panic!("unsupported link '{l}' (logit | probit)"),
    };
    let agd_lik = match inp.agd_likelihood.as_str() {
        "binomial" | "bernoulli" => AgdLik::Binomial,
        "binomial2" | "bernoulli2" => AgdLik::Binomial2,
        "normal" => AgdLik::Binomial, // Gaussian units carry their own likelihood
        l => panic!("unsupported AgD likelihood '{l}' (binomial | binomial2 | normal)"),
    };
    let use_is = match inp.correction.as_str() {
        "is" => true,
        "laplace" | "none" => false,
        c => panic!("unknown correction '{c}' (is | laplace)"),
    };
    let units: Vec<Unit> = inp
        .units
        .into_iter()
        .map(|u| to_unit(u, p))
        .collect();
    let priors: Vec<ParPrior> = if !inp.priors.is_empty() {
        assert_eq!(inp.priors.len(), p, "priors length != number of parameters");
        inp.priors.iter().map(|x| x.to_prior()).collect()
    } else if !inp.prior_prec.is_empty() {
        assert_eq!(inp.prior_prec.len(), p, "prior_prec length != number of parameters");
        inp.prior_prec.iter().map(|&x| ParPrior::from_prec(x)).collect()
    } else {
        vec![ParPrior::Flat; p]
    };
    let prob = Problem { p, link, agd_lik, units, priors, re_groups: inp.re_groups };
    let is_re = prob.is_re_col();
    let fixed: Vec<usize> = (0..p).filter(|&i| !is_re[i]).collect();
    let fixed_names: Vec<String> = fixed.iter().map(|&i| inp.par_names[i].clone()).collect();
    let nf = fixed.len();
    let mut rng = Rng::seeded(inp.seed);
    let s = inp.samples;

    // FE equilibrium (δ ≡ 0: RE columns dropped) — the FE model itself, the RE warm
    // start, and the FE partition function for the Bayes factor.
    let fe_prob = drop_columns(&prob, &fixed);
    let init_full = if inp.init.len() == p { inp.init.clone() } else { vec![0.0; p] };
    let fe_init: Vec<f64> = fixed.iter().map(|&i| init_full[i]).collect();
    let fe = fit_mode(&fe_prob, None, &fe_init, 500);
    let fe_pool = if use_is { Some(is_pool(&fe_prob, None, &fe, if prob.re_groups.is_empty() { 4 * s } else { 4000 }, NU, inp.seed ^ 0xFE)) } else { None };
    let log_int_fe = match &fe_pool {
        Some(pl) => pl.log_int,
        None => fe.log_post + 0.5 * nf as f64 * (2.0 * std::f64::consts::PI).ln() - 0.5 * fe.log_det_h,
    };

    // ------------------------------------------------------------- fixed effects
    if prob.re_groups.is_empty() {
        let (draws, ess, corr): (Vec<Vec<f64>>, f64, Option<f64>) = match &fe_pool {
            Some(pl) => {
                let (w, ess) = normalise_log(&pl.log_w);
                let idx = resample(&w, s, &mut rng);
                (idx.iter().map(|&i| pl.theta[i].clone()).collect(), ess, Some(pl.log_int - pl.log_int_laplace))
            }
            None => {
                let l = Cholesky::new(fe.cov.clone()).expect("cov").l();
                let mu = DVector::from_column_slice(&fe.theta);
                ((0..s).map(|_| (&mu + &l * DVector::from_fn(nf, |_, _| rng.normal())).iter().copied().collect()).collect(), s as f64, None)
            }
        };
        let pars = match &fe_pool {
            Some(pl) => wsummary(&fixed_names, &pl.theta, &normalise_log(&pl.log_w).0),
            None => wsummary(&fixed_names, &draws, &vec![1.0 / s as f64; s]),
        };
        let fixed_names_pool = fixed_names.clone();
        let out = Output {
            model: "fixed".into(), link: inp.link, agd_likelihood: inp.agd_likelihood,
            correction: inp.correction, n_units: prob.units.len(), n_par: p, n_re: 0,
            fixed_names: fixed_names.clone(), mode: fe.theta.clone(),
            cov: Some((0..nf).map(|i| (0..nf).map(|j| fe.cov[(i, j)]).collect()).collect()),
            log_lik_mode: fe.log_lik, iterations: fe.iters, converged: fe.converged,
            observed_information: fe.used_observed, log_z_correction: corr,
            log_marginal_unnorm: Some(log_int_fe),
            draw_names: fixed_names, draws,
            priors: vec![PriorOut { prior: "none".into(), tau: None, log_bf_re_fe: None, ess, pars }],
            grid_tau2: vec![], grid_log_z: vec![], grid_log_z_laplace: vec![], grid_ess: vec![],
            pool: match (&fe_pool, inp.return_pool) {
                (Some(pl), true) => Some(PoolOut { names: fixed_names_pool.clone(), values: pl.theta.clone(), grid: vec![],
                                                   w: normalise_log(&pl.log_w).0, prior_grid_w: vec![] }),
                _ => None,
            },
            fit_wall_sec: t0.elapsed().as_secs_f64(),
        };
        fs::write(&args.output, serde_json::to_string(&out).unwrap()).expect("write output");
        eprintln!("mlnmr: FE, {} units, {} params, {} iters, ESS {:.0}, {:.3}s", out.n_units, p, out.iterations, ess, out.fit_wall_sec);
        return;
    }

    // ------------------------------------------------------------ random effects
    let priors: Vec<(String, Tau2Prior)> = inp
        .tau_priors
        .iter()
        .map(|sp| (sp.clone(), Tau2Prior::parse(sp).unwrap_or_else(|e| panic!("{e}"))))
        .collect();
    let mut init = vec![0.0; p];
    for (k, &i) in fixed.iter().enumerate() {
        init[i] = fe.theta[k];
    }
    // coarse pass to locate the τ² mass under ALL priors, then a fine grid
    let coarse = gc_grid(&prob, &log_grid(1e-6, 25.0, 25), &init);
    let ctl: Vec<(f64, f64)> = coarse.iter().map(|g| (g.tau2, g.log_z)).collect();
    let mut keep = vec![false; ctl.len()];
    for (_, pr) in &priors {
        let lw: Vec<f64> = ctl.iter().map(|(t, lz)| lz + pr.log_prior(*t) + t.ln()).collect();
        let m = lw.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
        for (k, w) in lw.iter().enumerate() {
            if *w > m - 16.0 {
                keep[k] = true;
            }
        }
    }
    let ks: Vec<usize> = (0..ctl.len()).filter(|&k| keep[k]).collect();
    let lo = ctl[ks[0].saturating_sub(1)].0;
    let hi = ctl[(ks[ks.len() - 1] + 1).min(ctl.len() - 1)].0;
    let mid_init = coarse[ks[ks.len() / 2]].fit.theta.clone();
    let grid: Vec<GcPoint> = gc_grid(&prob, &log_grid(lo, hi, inp.n_grid), &mid_init);
    let iters: usize = coarse.iter().chain(grid.iter()).map(|g| g.fit.iters).sum();
    let converged = grid.iter().all(|g| g.fit.converged);
    let observed = grid.iter().all(|g| g.fit.used_observed);
    let m_re = prob.n_re() as f64;
    let half_p_log2pi = 0.5 * p as f64 * (2.0 * std::f64::consts::PI).ln();

    // importance pools per grid point (parallel), corrected partition function
    let pools: Vec<Option<gc_rust::ipd::IsPool>> = grid
        .par_iter()
        .enumerate()
        .map(|(gi, g)| if use_is {
            Some(is_pool(&prob, Some(g.tau2), &g.fit, inp.is_per_point, NU, inp.seed.wrapping_add(1000 + gi as u64)))
        } else { None })
        .collect();
    let log_z: Vec<f64> = grid
        .iter()
        .zip(&pools)
        .map(|(g, pl)| match pl {
            Some(pl) => pl.log_int - half_p_log2pi - 0.5 * m_re * g.tau2.ln(),
            None => g.log_z,
        })
        .collect();
    let tlz: Vec<(f64, f64)> = grid.iter().zip(&log_z).map(|(g, lz)| (g.tau2, *lz)).collect();

    // per grid point: weighted sample of (fixed params, tau)
    let mut names = fixed_names.clone();
    names.push("tau".into());
    let mut cells: Vec<(usize, Vec<f64>, f64)> = Vec::new(); // (grid, values, within-point weight)
    for (gi, g) in grid.iter().enumerate() {
        match &pools[gi] {
            Some(pl) => {
                let (w, _) = normalise_log(&pl.log_w);
                for (th, wi) in pl.theta.iter().zip(w) {
                    let mut v: Vec<f64> = fixed.iter().map(|&i| th[i]).collect();
                    v.push(g.tau2.sqrt());
                    cells.push((gi, v, wi));
                }
            }
            None => {
                // Laplace: Gaussian draws at this grid point
                let l = Cholesky::new(block(&g.fit.cov, &fixed)).expect("cov").l();
                let mu = DVector::from_iterator(nf, fixed.iter().map(|&i| g.fit.theta[i]));
                let k = inp.is_per_point;
                for _ in 0..k {
                    let mut v: Vec<f64> = (&mu + &l * DVector::from_fn(nf, |_, _| rng.normal())).iter().copied().collect();
                    v.push(g.tau2.sqrt());
                    cells.push((gi, v, 1.0 / k as f64));
                }
            }
        }
    }
    let vals: Vec<Vec<f64>> = cells.iter().map(|c| c.1.clone()).collect();

    let log_int_re_laplace_fe = log_int_fe - half_p_log2pi + 0.5 * m_re * (2.0 * std::f64::consts::PI).ln();
    let mut prior_out = Vec::new();
    let mut prior_grid_w = Vec::new();
    let mut draws: Vec<Vec<f64>> = Vec::new();
    for (pi, (label, pr)) in priors.iter().enumerate() {
        let wg = grid_weights(&tlz, *pr);
        prior_grid_w.push(wg.clone());
        let w: Vec<f64> = cells.iter().map(|c| wg[c.0] * c.2).collect();
        let ess = 1.0 / w.iter().map(|x| x * x).sum::<f64>();
        let mut pars = wsummary(&names, &vals, &w);
        let tau_sum = pars.pop();
        // τ quantiles from the deterministic grid mixture
        let ps = posterior_under(&tlz, *pr);
        let tau = tau_sum.map(|t| ParSummary { q025: ps.ci_lo.sqrt(), q50: ps.median.sqrt(), q975: ps.ci_hi.sqrt(), ..t });
        // BF: RE marginal = ∫Z π, with Z_RE(τ²) = ∫exp(F_τ) / [(2π)^{m/2} τ^m |K|^{-1/2}];
        // FE marginal = ∫exp(F_FE). log_z omits (2π)^{p/2}; restore consistently.
        let log_bf = log_marginal(&tlz, *pr)
            .map(|l| l + 0.5 * re_logdet_k(&prob) - log_int_re_laplace_fe);
        if pi == 0 {
            let idx = resample(&w, s, &mut rng);
            draws = idx.iter().map(|&i| vals[i].clone()).collect();
        }
        prior_out.push(PriorOut { prior: label.clone(), tau, log_bf_re_fe: log_bf, ess, pars });
    }
    let out = Output {
        model: "random".into(), link: inp.link, agd_likelihood: inp.agd_likelihood,
        correction: inp.correction, n_units: prob.units.len(), n_par: p, n_re: prob.n_re(),
        fixed_names, mode: fe.theta.clone(), cov: None,
        log_lik_mode: fe.log_lik, iterations: iters, converged, observed_information: observed,
        log_z_correction: None, log_marginal_unnorm: None, draw_names: names.clone(), draws, priors: prior_out,
        grid_tau2: tlz.iter().map(|g| g.0).collect(), grid_log_z: log_z.clone(),
        grid_log_z_laplace: grid.iter().map(|g| g.log_z).collect(),
        grid_ess: pools.iter().map(|pl| pl.as_ref().map(|x| x.ess).unwrap_or(f64::NAN)).collect(),
        pool: if inp.return_pool {
            Some(PoolOut { names: names.clone(), values: vals, grid: cells.iter().map(|c| c.0).collect(),
                           w: cells.iter().map(|c| c.2).collect(), prior_grid_w })
        } else { None },
        fit_wall_sec: t0.elapsed().as_secs_f64(),
    };
    fs::write(&args.output, serde_json::to_string(&out).unwrap()).expect("write output");
    eprintln!("mlnmr: RE, {} units, {} params ({} RE), {} grid points, {:.3}s",
              out.n_units, p, out.n_re, out.grid_tau2.len(), out.fit_wall_sec);
}

/// The same network with only the columns `keep` (used for the FE model: δ ≡ 0).
fn drop_columns(prob: &Problem, keep: &[usize]) -> Problem {
    let p = prob.p;
    let units = prob
        .units
        .iter()
        .map(|u| {
            let mut x = Vec::with_capacity(u.q * keep.len());
            for k in 0..u.q {
                for &j in keep {
                    x.push(u.x[k * p + j]);
                }
            }
            let pos = |c: usize| keep.iter().position(|&k| k == c).expect("σ column dropped");
            let g = u.g.as_ref().map(|g| Box::new(match &**g {
                Gauss::Ipd { y, sig } => Gauss::Ipd { y: *y, sig: pos(*sig) },
                Gauss::Arm { y, se } => Gauss::Arm { y: *y, se: *se },
                Gauss::Suff { xtx, xty, yty, n, sig } => Gauss::Suff {
                    xtx: keep.iter().flat_map(|&i| keep.iter().map(move |&j| xtx[i * p + j])).collect(),
                    xty: keep.iter().map(|&i| xty[i]).collect(), yty: *yty, n: *n, sig: pos(*sig) },
            }));
            Unit { r: u.r, n: u.n, q: u.q, x, g }
        })
        .collect();
    Problem {
        p: keep.len(),
        link: prob.link,
        agd_lik: prob.agd_lik,
        units,
        priors: keep.iter().map(|&j| prob.priors[j]).collect(),
        re_groups: vec![],
    }
}

/// log det K of the RE structure (τ² = 1): Σ_groups log det 2(I − 11ᵀ/(m+1)).
fn re_logdet_k(prob: &Problem) -> f64 {
    prob.re_groups
        .iter()
        .map(|g| {
            let m = g.len() as f64;
            // eigenvalues of 2(I − 11ᵀ/(m+1)): 2 (×(m−1)) and 2/(m+1)
            (m - 1.0) * 2f64.ln() + (2.0 / (m + 1.0)).ln()
        })
        .sum()
}
