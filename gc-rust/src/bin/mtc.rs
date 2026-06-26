//! gemtc-facade sampler.
//!
//! Runs the Grand-Canonical consistency NMA (binomial or continuous, picked by
//! the data) and emits **posterior draws of the basic parameters** d.ref.t
//! together with sd.d = sqrt(tau2) — the same raw material an MCMC engine such
//! as `gemtc` produces. The R package `springs` reads these draws and builds a
//! gemtc-compatible `mtc.result`, so MetaInsight's Bayesian path works by
//! swapping `gemtc::` for `springs::`.
//!
//! The GC posterior is a tau2-mixture of Gaussians: at each grid tau2 the
//! effect vector is jointly Gaussian with mean `GridPoint::effects` and
//! covariance `GridPoint::eff_cov`. A draw = (pick a grid tau2 ~ its posterior
//! weight) then (draw d ~ N(mean, cov)). This reproduces all-pairwise contrasts
//! and rank probabilities exactly the way gemtc derives them from samples.
//!
//! Usage:
//!   mtc --input arms.json --output draws.json [--prior hn:1] [--samples 4000]
//!       [--seed 1] [--n-grid 100]

use clap::Parser;
use nalgebra::{Cholesky, DMatrix, DVector};
use serde::Serialize;
use std::fs;
use std::time::Instant;

use gc_rust::data::{read_dataset, Mode};
use gc_rust::gc::{grid_weights, posterior_under, run_adaptive_auto_min_hi, Tau2Prior};

#[derive(Parser, Debug)]
#[command(name = "mtc", about = "GC NMA gemtc-facade posterior sampler")]
struct Args {
    /// Arm-level JSON: records {study, treatment, events, n} (binary) or
    /// {study, treatment, mean, sd, n} (continuous).
    #[arg(long)]
    input: String,
    /// Output draws JSON.
    #[arg(long)]
    output: String,
    /// tau2 prior: `flat`, `hn:SIGMA` (half-normal on tau), `hc:SIGMA`
    /// (half-cauchy on tau). Default `hn:1` (matches gemtc's vague default on
    /// the smoking benchmark).
    #[arg(long, default_value = "hn:1")]
    prior: String,
    /// Comma-separated prior list for a one-pass SENSITIVITY analysis, e.g.
    /// `flat,hn:0.5,hn:1,hc:0.5,unif:2`. The τ² grid (the expensive part) is
    /// computed ONCE; each prior is a cheap reweighting. Emits per-prior effect
    /// + τ² summaries instead of the single-prior draw matrix.
    #[arg(long)]
    priors: Option<String>,
    /// Binary link: `logit` (OR, default) or `log` (RR). `log` uses the explicit
    /// study-baseline + log-RR-contrast IRLS path (p=exp η, η≤0), not the logit GC.
    #[arg(long, default_value = "logit")]
    link: String,
    /// `random` (RE, integrate τ²) or `fixed` (FE). Only consulted by the log-link
    /// (RR) path; the logit path infers RE/FE from the τ² prior.
    #[arg(long, default_value = "random")]
    effects: String,
    /// Number of posterior draws.
    #[arg(long, default_value_t = 4000)]
    samples: usize,
    /// RNG seed (reproducible, like MetaInsight's JAGS seeds).
    #[arg(long, default_value_t = 1)]
    seed: u64,
    /// Adaptive tau2 grid size.
    #[arg(long, default_value_t = 100)]
    n_grid: usize,
}

fn parse_prior(s: &str) -> Tau2Prior {
    let s = s.trim().to_lowercase();
    if s == "flat" {
        return Tau2Prior::Flat;
    }
    if let Some(rest) = s.strip_prefix("hn:") {
        return Tau2Prior::HalfNormalTau(rest.parse().expect("prior sigma"));
    }
    if let Some(rest) = s.strip_prefix("hc:") {
        return Tau2Prior::HalfCauchyTau(rest.parse().expect("prior sigma"));
    }
    // Uniform on the SD τ over [0, upper] — gemtc's default dunif(0, om.scale).
    if let Some(rest) = s.strip_prefix("unif:") {
        return Tau2Prior::UniformSD(rest.parse().expect("prior upper"));
    }
    panic!("unknown --prior '{s}' (use flat | hn:SIGMA | hc:SIGMA | unif:UPPER)");
}

/// splitmix64 -> uniforms; tiny, deterministic, no external dependency.
struct Rng {
    state: u64,
}
impl Rng {
    fn new(seed: u64) -> Self {
        // Avoid a zero state degenerating.
        Rng { state: seed.wrapping_mul(0x9E3779B97F4A7C15).wrapping_add(1) }
    }
    fn next_u64(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = self.state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }
    /// Uniform in (0, 1).
    fn unif(&mut self) -> f64 {
        // 53-bit mantissa; shift into (0,1), nudged off 0.
        let u = (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64;
        if u <= 0.0 { f64::MIN_POSITIVE } else { u }
    }
    /// Standard normal via Box-Muller.
    fn normal(&mut self) -> f64 {
        let u1 = self.unif();
        let u2 = self.unif();
        (-2.0 * u1.ln()).sqrt() * (std::f64::consts::TAU * u2).cos()
    }
}

#[derive(Serialize)]
struct Tau2Out {
    mode: f64,
    median: f64,
    mean: f64,
    ci_lo: f64,
    ci_hi: f64,
}

// ── RR (log-link) NMA: explicit study-baseline + log-RR-contrast IRLS + GC ──
// Model: log(p_ik) = μ_i + d_t  (d_ref = 0), p = exp(η) with η ≤ 0. Non-canonical
// link ⇒ a direct constrained IRLS (feasible init μ_i = log control prop, line
// search on the binomial −logL which is +∞ when any p ≥ 1). RE integrates τ² over
// a grid via the working-Gaussian partition function (same GC as brisk.rs).
#[derive(serde::Deserialize)]
struct RrArm { study: i64, treatment: String, events: f64, n: f64 }

fn fit_log_nma(input: &str, samples: usize, seed: u64, n_grid: usize, random: bool) -> DrawsReport {
    use std::collections::{BTreeMap, BTreeSet};
    let t0 = Instant::now();
    let arms_in: Vec<RrArm> =
        serde_json::from_str(&std::fs::read_to_string(input).expect("read")).expect("parse arms");
    let studies: Vec<i64> = arms_in.iter().map(|a| a.study).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String> = arms_in.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff = treats[0].clone();
    let nonref: Vec<String> = treats.iter().filter(|t| **t != reff).cloned().collect();
    let ns = studies.len(); let nt = nonref.len(); let np = ns + nt;
    let sidx: BTreeMap<i64, usize> = studies.iter().enumerate().map(|(i, s)| (*s, i)).collect();
    let tidx: BTreeMap<&str, usize> = nonref.iter().enumerate().map(|(i, t)| (t.as_str(), i)).collect();
    let dcol = |t: usize| ns + t;
    // arm: (study idx, Some(treat idx)|None for ref, events, n)
    let arms: Vec<(usize, Option<usize>, f64, f64)> = arms_in.iter().map(|a|
        (sidx[&a.study], if a.treatment == reff { None } else { Some(tidx[a.treatment.as_str()]) }, a.events, a.n)).collect();

    // feasible init: μ_i = log(study event rate), clamped < 0; d_t = 0.
    let mut srate = vec![(0.5f64, 1.0f64); ns];   // (events, n) accumulators (cc)
    for &(si, _, e, n) in &arms { srate[si].0 += e; srate[si].1 += n; }
    let mut theta = DVector::<f64>::zeros(np);
    for i in 0..ns { theta[i] = (srate[i].0 / srate[i].1).min(0.95).ln().min(-1e-3); }

    let pieces = |th: &DVector<f64>, si: usize, ot: Option<usize>, e: f64, n: f64|
      -> (Vec<(usize, f64)>, f64, f64) {
        let eta = (th[si] + ot.map_or(0.0, |t| th[dcol(t)])).min(-1e-9);
        let p = eta.exp();
        let w = (n * p / (1.0 - p)).max(1e-9);   // W = (dμ/dη)²/Var = n p/(1−p)
        let dmu = (n * p).max(1e-9);             // dμ/dη = n p (≠ W for the non-canonical log link)
        let jac = match ot { None => vec![(si, 1.0)], Some(t) => vec![(si, 1.0), (dcol(t), 1.0)] };
        let jt: f64 = jac.iter().map(|(i, v)| v * th[*i]).sum();
        (jac, jt + (e - n * p) / dmu, w)         // working response z = η + (e−np)/(dμ/dη)
    };
    let nll = |th: &DVector<f64>| -> f64 {     // −logL; +∞ if any arm infeasible (p ≥ 1)
        let mut s = 0.0;
        for &(si, ot, e, n) in &arms {
            let eta = th[si] + ot.map_or(0.0, |t| th[dcol(t)]);
            if eta >= 0.0 { return f64::INFINITY; }
            // log(1−p) = ln(−expm1(η))
            s += -(e * eta) - (n - e) * (-(eta.exp_m1())).ln();
        }
        s
    };
    let w_eff = |w: f64, t2: f64| if t2 > 0.0 { 1.0 / (1.0 / w + t2 / 2.0) } else { w };
    let build = |th: &DVector<f64>, t2: f64| -> (DMatrix<f64>, DVector<f64>) {
        let mut a = DMatrix::<f64>::zeros(np, np); let mut b = DVector::<f64>::zeros(np);
        for &(si, ot, e, n) in &arms {
            let (jac, z, w) = pieces(th, si, ot, e, n); let we = w_eff(w, t2);
            for &(i, vi) in &jac { b[i] += we * vi * z; for &(j, vj) in &jac { a[(i, j)] += we * vi * vj; } }
        }
        (a, b)
    };

    // Relax the RR spring network to equilibrium at a given τ² (IRLS / Fisher
    // scoring; line search on −logL keeps every p < 1). Re-solved per τ² grid
    // point — NOT frozen — so the partition function uses the true per-τ²
    // linearisation (matches the logit GC; the frozen variant biased the means).
    let solve_at = |t2c: f64, warm: &DVector<f64>| -> DVector<f64> {
        let mut th = warm.clone();
        for _ in 0..200 {
            let (a, b) = build(&th, t2c);
            let sol = match a.lu().solve(&b) { Some(s) => s, None => break };
            let dir = &sol - &th; let f0 = nll(&th);
            let mut al = 1.0; let mut next = th.clone();
            for _ in 0..50 { let cand = &th + &dir * al; if nll(&cand).is_finite() && nll(&cand) <= f0 + 1e-9 { next = cand; break; } al *= 0.5; }
            let step = (&next - &th).amax(); th = next;
            if step < 1e-10 { break; }
        }
        th
    };
    // log Z(τ²) and the per-τ² mode + covariance (the working-Gaussian partition fn).
    let logz_at = |th: &DVector<f64>, t2c: f64| -> (f64, DMatrix<f64>, f64) {
        let mut a = DMatrix::<f64>::zeros(np, np); let mut b = DVector::<f64>::zeros(np);
        let (mut slv, mut zwz) = (0.0, 0.0);
        for &(si, ot, e, n) in &arms {
            let (jac, z, w) = pieces(th, si, ot, e, n); let we = w_eff(w, t2c);
            slv += if t2c > 0.0 { (1.0 / w + t2c / 2.0).ln() } else { (1.0 / w).ln() };
            zwz += we * z * z;
            for &(i, vi) in &jac { b[i] += we * vi * z; for &(j, vj) in &jac { a[(i, j)] += we * vi * vj; } }
        }
        let chol = Cholesky::new(a).expect("RR stiffness not PD");
        let logdet = 2.0 * chol.l().diagonal().iter().map(|x| x.ln()).sum::<f64>();
        let bx: f64 = b.iter().zip(th.iter()).map(|(bi, ti)| bi * ti).sum();
        (-0.5 * (slv + logdet + (zwz - bx)), chol.inverse(), 0.0)
    };

    theta = solve_at(if random { 0.05 } else { 0.0 }, &theta);
    // REML τ² mode by coarse search, then an adaptive grid around it.
    let t2hat = if random {
        let cand = [0.005, 0.02, 0.05, 0.1, 0.2, 0.4, 0.8, 1.6, 3.2];
        cand.iter().map(|&tc| (tc, logz_at(&solve_at(tc, &theta), tc).0))
            .fold((0.05, f64::NEG_INFINITY), |b, (tc, lz)| if lz > b.1 { (tc, lz) } else { b }).0
    } else { 0.0 };
    let t2_mode = t2hat;
    let ts: Vec<f64> = if random {
        let (lo, hi) = ((t2hat / 8.0).max(1e-4), (t2hat * 8.0).max(0.05));
        (0..n_grid.max(2)).map(|i| lo * (hi / lo).powf(i as f64 / (n_grid.max(2) as f64 - 1.0))).collect()
    } else { vec![0.0] };
    // per cell: (logpost, mean, L of cov, τ²)
    let mut cells: Vec<(f64, DVector<f64>, DMatrix<f64>, f64)> = Vec::with_capacity(ts.len());
    let mut mxlp = -1e300;
    let mut warm = theta.clone();
    for &t2c in &ts {
        let th = solve_at(t2c, &warm); warm = th.clone();
        let (lz, cov, _) = logz_at(&th, t2c);
        // half-normal(0.5) prior on τ, + Jacobian for the log-spaced grid (τ²)
        let lp = lz + if t2c > 0.0 { -t2c / (2.0 * 0.25) - 0.5 * t2c.ln() + t2c.ln() } else { 0.0 };
        let lc = Cholesky::new((&cov + cov.transpose()) * 0.5).map(|c| c.l()).unwrap_or_else(|| {
            let mut d = DMatrix::<f64>::zeros(np, np); for k in 0..np { d[(k, k)] = cov[(k, k)].max(0.0).sqrt(); } d });
        if lp > mxlp { mxlp = lp; }
        cells.push((lp, th, lc, t2c));
    }
    let mut wsum = 0.0; let mut cumw = Vec::with_capacity(cells.len());
    for c in &cells { wsum += (c.0 - mxlp).exp(); cumw.push(wsum); }
    let mut gtau = 0.0; for c in &cells { gtau += (c.0 - mxlp).exp() / wsum * c.3; }

    let treatments = { let mut v = vec![reff.clone()]; v.extend(nonref.iter().cloned()); v };
    let ord_dcol: Vec<Option<usize>> = treatments.iter().map(|t| if *t == reff { None } else { Some(dcol(tidx[t.as_str()])) }).collect();
    let mut rng = Rng::new(seed);
    let mut draws = Vec::with_capacity(samples); let mut sd_d = Vec::with_capacity(samples);
    for _ in 0..samples {
        let u = rng.unif() * wsum;
        let mut ci = cumw.partition_point(|&x| x < u); if ci >= cells.len() { ci = cells.len() - 1; }
        let (_, mean, lc, t2c) = &cells[ci];
        sd_d.push(t2c.max(0.0).sqrt());
        let z = DVector::<f64>::from_fn(np, |_, _| rng.normal());
        let th = mean + lc * z;
        draws.push(ord_dcol[1..].iter().map(|c| c.map_or(0.0, |c| th[c])).collect());
    }
    let ref_t = treatments[0].clone();
    let param_names: Vec<String> = treatments[1..].iter().map(|t| format!("d.{ref_t}.{t}")).collect();
    let post_tau = |q: f64| { // crude τ² quantile from cells
        let mut pairs: Vec<(f64, f64)> = cells.iter().map(|c| (c.3, (c.0 - mxlp).exp() / wsum)).collect();
        pairs.sort_by(|a, b| a.0.partial_cmp(&b.0).unwrap());
        let mut acc = 0.0; for (t, w) in &pairs { acc += w; if acc >= q { return *t; } } pairs.last().map_or(0.0, |p| p.0)
    };
    DrawsReport {
        dataset: input.to_string(), mode: "binary".into(), ref_treatment: ref_t, treatments,
        param_names, prior: "log-link (RR)".into(), n_samples: samples, seed,
        reml_mode_iterative: t2_mode,
        tau2: Tau2Out { mode: t2_mode, median: post_tau(0.5), mean: gtau, ci_lo: post_tau(0.025), ci_hi: post_tau(0.975) },
        fit_wall_sec: t0.elapsed().as_secs_f64(), draws, sd_d,
    }
}

#[derive(Serialize)]
struct EffSummary { to: String, mean: f64, sd: f64, lo: f64, hi: f64 }

#[derive(Serialize)]
struct PriorSummary {
    label: String,
    tau2: Tau2Out,
    sd_d_mean: f64,
    effects: Vec<EffSummary>,
}

#[derive(Serialize)]
struct SensitivityReport {
    dataset: String,
    mode: String,
    #[serde(rename = "ref")]
    ref_treatment: String,
    treatments: Vec<String>,
    param_names: Vec<String>,
    n_samples: usize,
    seed: u64,
    fit_wall_sec: f64,
    /// one entry per requested prior, all from a single τ² grid pass.
    priors: Vec<PriorSummary>,
}

#[derive(Serialize)]
struct DrawsReport {
    dataset: String,
    mode: String, // "binary" | "continuous"
    #[serde(rename = "ref")]
    ref_treatment: String,
    treatments: Vec<String>,
    /// Basic-parameter names in column order, e.g. "d.A.B".
    param_names: Vec<String>,
    prior: String,
    n_samples: usize,
    seed: u64,
    reml_mode_iterative: f64,
    tau2: Tau2Out,
    fit_wall_sec: f64,
    /// `n_samples` rows, each `treatments.len()-1` basic effects d.ref.t.
    draws: Vec<Vec<f64>>,
    /// `n_samples` heterogeneity SD draws (sqrt of the grid tau2 picked).
    sd_d: Vec<f64>,
}

fn main() {
    let args = Args::parse();
    let t0 = Instant::now();

    // RR (log link): explicit study-baseline + log-RR IRLS path, not the logit GC.
    if args.link == "log" {
        let report = fit_log_nma(&args.input, args.samples, args.seed, args.n_grid,
                                 args.effects == "random");
        fs::write(&args.output, serde_json::to_string(&report).expect("serialise")).expect("write");
        eprintln!("mtc: {} (binary, log/RR), {} treatments, {} draws in {:.3}s -> {}",
            report.dataset, report.treatments.len(), report.n_samples, report.fit_wall_sec, args.output);
        return;
    }

    let ds = read_dataset(&args.input).unwrap_or_else(|e| panic!("read dataset: {e}"));
    let mode_str = match ds.mode {
        Mode::Binary => "binary",
        Mode::Continuous => "continuous",
    };
    // Priors to evaluate: the single default, or a comma-separated sensitivity list.
    let prior_specs: Vec<(String, Tau2Prior)> = match &args.priors {
        Some(list) => list.split(',').map(|s| (s.trim().to_string(), parse_prior(s.trim()))).collect(),
        None => vec![(args.prior.clone(), parse_prior(&args.prior))],
    };
    let prior = prior_specs[0].1; // single-prior path
    // A bounded SD prior (Uniform on τ over [0, u]) places mass up to τ² = u²; make the
    // grid reach the LARGEST such bound across all priors so no posterior tail is truncated.
    let min_hi = prior_specs.iter()
        .map(|(_, p)| if let Tau2Prior::UniformSD(u) = p { u * u } else { 0.0 })
        .fold(0.0, f64::max);
    let (topo, reml_mode, grid) = run_adaptive_auto_min_hi(&ds, args.n_grid, min_hi);
    let treatments = topo.treatment_order.clone();
    let ref_t = treatments[0].clone();
    let n_eff = treatments.len() - 1; // basic params d.ref.t for t = 1..n_t

    // tau2 posterior weights (grid order) and summary.
    let grid_tlz: Vec<(f64, f64)> = grid.iter().map(|g| (g.tau2, g.log_z)).collect();
    let weights = grid_weights(&grid_tlz, prior);
    let post = posterior_under(&grid_tlz, prior);

    // Cumulative weights for inverse-CDF grid sampling.
    let mut cum = vec![0.0f64; weights.len()];
    let mut acc = 0.0;
    for (i, w) in weights.iter().enumerate() {
        acc += w;
        cum[i] = acc;
    }

    // Pre-factor each grid point's covariance (Cholesky L, with jitter retry).
    // Index by grid point; None if degenerate (n_eff == 0 handled separately).
    let chols: Vec<Option<DMatrix<f64>>> = grid
        .iter()
        .map(|g| {
            if n_eff == 0 {
                return None;
            }
            let mut m = DMatrix::<f64>::zeros(n_eff, n_eff);
            for a in 0..n_eff {
                for b in 0..n_eff {
                    m[(a, b)] = g.eff_cov[a + 1][b + 1];
                }
            }
            // Symmetrise to kill rounding asymmetry, then Cholesky with jitter.
            let mt = m.transpose();
            m = (m + mt) * 0.5;
            let mut jit = 0.0f64;
            loop {
                let mut trial = m.clone();
                if jit > 0.0 {
                    for d in 0..n_eff {
                        trial[(d, d)] += jit;
                    }
                }
                if let Some(c) = Cholesky::new(trial) {
                    break Some(c.l());
                }
                jit = if jit == 0.0 { 1e-12 } else { jit * 10.0 };
                if jit > 1e-3 {
                    // Give up on the factorisation; fall back to diagonal sd.
                    let mut l = DMatrix::<f64>::zeros(n_eff, n_eff);
                    for d in 0..n_eff {
                        l[(d, d)] = m[(d, d)].max(0.0).sqrt();
                    }
                    break Some(l);
                }
            }
        })
        .collect();

    // ---- multi-prior sensitivity: reuse the ONE grid + chols, reweight per prior ----
    if args.priors.is_some() {
        let param_names: Vec<String> = treatments[1..].iter().map(|t| format!("d.{ref_t}.{t}")).collect();
        let mut out_priors = Vec::with_capacity(prior_specs.len());
        for (pi, (label, p)) in prior_specs.iter().enumerate() {
            let w = grid_weights(&grid_tlz, *p);
            let post_p = posterior_under(&grid_tlz, *p);
            let mut cum = vec![0.0f64; w.len()];
            let mut acc = 0.0;
            for (i, wi) in w.iter().enumerate() { acc += wi; cum[i] = acc; }
            let mut rng = Rng::new(args.seed.wrapping_add(pi as u64).wrapping_mul(0x9E3779B97F4A7C15));
            let mut cols: Vec<Vec<f64>> = vec![Vec::with_capacity(args.samples); n_eff];
            let mut sdsum = 0.0;
            for _ in 0..args.samples {
                let u = rng.unif();
                let mut gi = 0usize;
                while gi < cum.len() && cum[gi] < u { gi += 1; }
                if gi >= grid.len() { gi = grid.len() - 1; }
                let gp = &grid[gi];
                sdsum += gp.tau2.max(0.0).sqrt();
                if n_eff > 0 {
                    let z = DVector::<f64>::from_fn(n_eff, |_, _| rng.normal());
                    let shift = chols[gi].as_ref().unwrap() * z;
                    for a in 0..n_eff { cols[a].push(gp.effects[a + 1] + shift[a]); }
                }
            }
            let q = |v: &mut [f64], pr: f64| -> f64 {
                v.sort_by(|a, b| a.partial_cmp(b).unwrap());
                v[((pr * (v.len() as f64 - 1.0)).round() as usize).min(v.len().saturating_sub(1))]
            };
            let effects = (0..n_eff).map(|a| {
                let v = &mut cols[a];
                let mean = v.iter().sum::<f64>() / v.len() as f64;
                let sd = (v.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / v.len() as f64).sqrt();
                EffSummary { to: treatments[a + 1].clone(), mean, sd, lo: q(v, 0.025), hi: q(v, 0.975) }
            }).collect();
            out_priors.push(PriorSummary {
                label: label.clone(),
                tau2: Tau2Out { mode: post_p.mode, median: post_p.median, mean: post_p.mean, ci_lo: post_p.ci_lo, ci_hi: post_p.ci_hi },
                sd_d_mean: sdsum / args.samples as f64,
                effects,
            });
        }
        let report = SensitivityReport {
            dataset: args.input.clone(), mode: mode_str.to_string(), ref_treatment: ref_t.clone(),
            treatments: treatments.clone(), param_names, n_samples: args.samples, seed: args.seed,
            fit_wall_sec: t0.elapsed().as_secs_f64(), priors: out_priors,
        };
        fs::write(&args.output, serde_json::to_string(&report).expect("serialise")).expect("write output");
        eprintln!("mtc: {} ({mode_str}), {} priors in ONE grid pass, {:.3}s -> {}",
            report.dataset, report.priors.len(), report.fit_wall_sec, args.output);
        return;
    }

    let mut rng = Rng::new(args.seed);
    let mut draws: Vec<Vec<f64>> = Vec::with_capacity(args.samples);
    let mut sd_d: Vec<f64> = Vec::with_capacity(args.samples);

    for _ in 0..args.samples {
        // Pick grid index by inverse CDF.
        let u = rng.unif();
        let mut gi = 0usize;
        while gi < cum.len() && cum[gi] < u {
            gi += 1;
        }
        if gi >= grid.len() {
            gi = grid.len() - 1;
        }
        let gp = &grid[gi];
        sd_d.push(gp.tau2.max(0.0).sqrt());

        // d = mean + L z.
        let mut row = vec![0.0f64; n_eff];
        if n_eff > 0 {
            let z = DVector::<f64>::from_fn(n_eff, |_, _| rng.normal());
            let l = chols[gi].as_ref().unwrap();
            let shift = l * z;
            for a in 0..n_eff {
                // effects[0] is the ref (0.0); basic params are effects[1..].
                row[a] = gp.effects[a + 1] + shift[a];
            }
        }
        draws.push(row);
    }

    let param_names: Vec<String> =
        treatments[1..].iter().map(|t| format!("d.{ref_t}.{t}")).collect();

    let report = DrawsReport {
        dataset: args.input.clone(),
        mode: mode_str.to_string(),
        ref_treatment: ref_t,
        treatments,
        param_names,
        prior: args.prior.clone(),
        n_samples: args.samples,
        seed: args.seed,
        reml_mode_iterative: reml_mode,
        tau2: Tau2Out {
            mode: post.mode,
            median: post.median,
            mean: post.mean,
            ci_lo: post.ci_lo,
            ci_hi: post.ci_hi,
        },
        fit_wall_sec: t0.elapsed().as_secs_f64(),
        draws,
        sd_d,
    };

    fs::write(&args.output, serde_json::to_string(&report).expect("serialise"))
        .expect("write output");
    eprintln!(
        "mtc: {} ({mode_str}), {} treatments, {} draws in {:.3}s -> {}",
        report.dataset,
        report.treatments.len(),
        report.n_samples,
        report.fit_wall_sec,
        args.output
    );
}
