//! Baseline-risk meta-regression posterior sampler (Achana/bnma model) — the
//! engine behind the `springs` bnma-compatible facade. A faithful port of the
//! validated Haskell `baselineRiskCoreB` / `baselineRiskCoreMD`.
//!
//!   logit(p_ik) = η_i + d_t + b_t·(η_i − mx)      (binary, OR)
//!   y_ik        = η_i + d_t + b_t·(η_i − mx) + ε   (continuous, MD)
//!
//! η_i free per-study baselines, d_t the contrasts at baseline = mx, b_t the
//! baseline-risk slopes. Slope structures: shared/common (one B), unrelated/
//! independent (b_t per treatment, weak 1e-4 ridge), exchangeable (b_t ~ N(B,
//! σ_B²), slope-spring 1/σ_B²). Heterogeneity τ²/2 per arm.
//!
//! Mechanics: the spring network is RELAXED to its force-balance equilibrium —
//! spring energy = −logL, spring force = score, so net force → 0 is the MLE
//! (the damped Newton/IRLS iteration below is that relaxation; "Fisher scoring"
//! is just its statistical name). The posterior is then the grand-canonical
//! partition function: the GC (`--method gc`, default) integrates the variance-
//! component ensemble (τ², and σ_B² for exchangeable) over a grid, with the
//! observed-information Hessian as the harmonic stiffness — the true curvature
//! of the energy well, not the Gauss–Newton approximation. EM gives the point.
//!
//! Usage: brisk --input arms.json --ref REF [--regressor common|independent|
//!   exchangeable] [--effects fixed|random] [--samples N] [--seed S]

use clap::Parser;
use nalgebra::{Cholesky, DMatrix, DVector};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct Arm { study: String, treatment: String, events: Option<f64>, n: f64, mean: Option<f64>, sd: Option<f64> }

fn sigmoid(x: f64) -> f64 { if x >= 0.0 { 1.0 / (1.0 + (-x).exp()) } else { let e = x.exp(); e / (1.0 + e) } }

#[derive(Parser, Debug)]
#[command(name = "brisk", about = "Baseline-risk NMA posterior sampler (bnma facade)")]
struct Args {
    #[arg(long)] input: String,
    #[arg(long)] output: String,
    #[arg(long = "ref")] reff: String,
    /// common | independent | exchangeable
    #[arg(long, default_value = "common")] regressor: String,
    /// fixed | random
    #[arg(long, default_value = "fixed")] effects: String,
    /// em | gc  (gc = 2-D grand-canonical Ξ(τ²,σ_B²), exchangeable only)
    #[arg(long, default_value = "em")] method: String,
    #[arg(long, default_value_t = 4000)] samples: usize,
    #[arg(long, default_value_t = 1)] seed: u64,
}

// half-normal(σ) log-density on a variance component v (with the v-Jacobian).
fn lphn(s: f64, v: f64) -> f64 { if v <= 0.0 { -1e300 } else { -v / (2.0 * s * s) - 0.5 * v.ln() } }

#[derive(Clone, Copy, PartialEq)]
enum Reg { Common, Independent, Exchangeable }

struct Rng { state: u64 }
impl Rng {
    fn new(seed: u64) -> Self { Rng { state: seed.wrapping_mul(0x9E3779B97F4A7C15).wrapping_add(1) } }
    fn next_u64(&mut self) -> u64 {
        self.state = self.state.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = self.state;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }
    fn unif(&mut self) -> f64 { let u = (self.next_u64() >> 11) as f64 / (1u64 << 53) as f64; if u <= 0.0 { f64::MIN_POSITIVE } else { u } }
    fn normal(&mut self) -> f64 { let (u1, u2) = (self.unif(), self.unif()); (-2.0 * u1.ln()).sqrt() * (std::f64::consts::TAU * u2).cos() }
}

#[derive(Serialize)]
struct Report {
    dataset: String, response: String,
    #[serde(rename = "ref")] ref_treatment: String,
    treatments: Vec<String>, regressor: String, effects: String,
    mx: f64, tau2: f64, sigma_b2: Option<f64>,
    n_samples: usize, seed: u64, fit_wall_sec: f64,
    d_hat: Vec<f64>, b_hat: Vec<f64>,
    d_draws: Vec<Vec<f64>>, b_draws: Vec<Vec<f64>>,
}

fn main() {
    let args = Args::parse();
    let t0 = Instant::now();
    let arms_in: Vec<Arm> = serde_json::from_str(&std::fs::read_to_string(&args.input).expect("read input")).expect("parse arms");
    let binary = arms_in.iter().all(|a| a.events.is_some());
    let reg = match args.regressor.as_str() {
        "common" => Reg::Common, "independent" => Reg::Independent, "exchangeable" => Reg::Exchangeable,
        other => panic!("unknown --regressor '{other}'"),
    };
    // Grand-canonical integration of the variance components present:
    //   σ_B²  — only exchangeable has a slope dispersion to integrate.
    //   τ²    — integrate under random effects; exchangeable always integrates it
    //           (the 2-D Ξ(τ²,σ_B²) by definition).
    // FE common/independent have no variance component, so GC falls through to the
    // exact point-Laplace path (observed-information SEs).
    let gc = args.method == "gc";
    let integ_sig = gc && reg == Reg::Exchangeable;
    let integ_tau = gc && (args.effects == "random" || reg == Reg::Exchangeable);
    let random = args.effects == "random" || integ_tau;

    let studies: Vec<String> = arms_in.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String> = arms_in.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    assert!(treats.contains(&args.reff), "reference '{}' not present", args.reff);
    let nonref: Vec<String> = treats.iter().filter(|t| **t != args.reff).cloned().collect();
    let ns = studies.len(); let nt = nonref.len();
    let slope_base = ns + nt;
    let common_col = slope_base + nt;
    let np = match reg { Reg::Common => ns + nt + 1, Reg::Independent => ns + 2 * nt, Reg::Exchangeable => ns + 2 * nt + 1 };
    let sidx: BTreeMap<&str, usize> = studies.iter().enumerate().map(|(i, s)| (s.as_str(), i)).collect();
    let tidx: BTreeMap<&str, usize> = nonref.iter().enumerate().map(|(i, t)| (t.as_str(), i)).collect();
    let dcol = |t: usize| ns + t;
    let slope_col = |t: usize| match reg { Reg::Common => slope_base, _ => slope_base + t };

    // arm: (study, Some(treat idx)|None, y, wd)  -- binary wd=n,y=events; cont wd=v,y=mean
    let arms: Vec<(usize, Option<usize>, f64, f64)> = arms_in.iter().map(|a| {
        let ot = if a.treatment == args.reff { None } else { Some(tidx[a.treatment.as_str()]) };
        if binary { (sidx[a.study.as_str()], ot, a.events.unwrap(), a.n) }
        else { (sidx[a.study.as_str()], ot, a.mean.unwrap(), (a.sd.unwrap().powi(2) / a.n).max(1e-12)) }
    }).collect();

    // mx + baseline starts
    let mut base0 = vec![0.0f64; ns];
    for a in &arms_in {
        if a.treatment == args.reff {
            let si = sidx[a.study.as_str()];
            base0[si] = if binary {
                let (mut r, mut n) = (a.events.unwrap(), a.n);
                if r == 0.0 { r = 0.5; n = a.n + 1.0; }
                let p = r / n; (p / (1.0 - p)).ln()
            } else { a.mean.unwrap() };
        }
    }
    let mx: f64 = base0.iter().sum::<f64>() / ns as f64;
    let n_arm = arms.len() as f64;

    // per-arm linear algebra pieces at theta: (sparse jac, working response z, weight w)
    let pieces = |th: &DVector<f64>, si: usize, ot: Option<usize>, y: f64, wd: f64|
      -> (Vec<(usize, f64)>, f64, f64) {
        let bt = ot.map_or(0.0, |t| th[slope_col(t)]);
        let eta = match ot { None => th[si], Some(t) => th[si] + th[dcol(t)] + bt * (th[si] - mx) };
        let (w, resid) = if binary {
            let p = sigmoid(eta); ((wd * p * (1.0 - p)).max(1e-9), y - wd * p)
        } else { (1.0 / wd, y - eta) };
        let jac = match ot { None => vec![(si, 1.0)], Some(t) => vec![(si, 1.0 + bt), (dcol(t), 1.0), (slope_col(t), th[si] - mx)] };
        let jtheta: f64 = jac.iter().map(|(i, v)| v * th[*i]).sum();
        (jac, jtheta + resid / w, w)
    };
    let w_eff = |w: f64, t2: f64| if t2 > 0.0 { 1.0 / (1.0 / w + t2 / 2.0) } else { w };

    // A = JᵀW_eff J (+ ridge), b = JᵀW_eff z
    let build = |th: &DVector<f64>, t2: f64, sig: f64| -> (DMatrix<f64>, DVector<f64>) {
        let mut a = DMatrix::<f64>::zeros(np, np);
        let mut b = DVector::<f64>::zeros(np);
        for &(si, ot, y, wd) in &arms {
            let (jac, z, w) = pieces(th, si, ot, y, wd);
            let we = w_eff(w, t2);
            for &(i, vi) in &jac { b[i] += we * vi * z; for &(j, vj) in &jac { a[(i, j)] += we * vi * vj; } }
        }
        let inv_sig = 1.0 / sig;
        match reg {
            Reg::Exchangeable => for t in 0..nt {
                let (bt, bb) = (slope_col(t), common_col);
                a[(bt, bt)] += inv_sig; a[(bb, bb)] += inv_sig; a[(bt, bb)] -= inv_sig; a[(bb, bt)] -= inv_sig;
            },
            Reg::Independent => for t in 0..nt { let c = slope_col(t); a[(c, c)] += 1e-4; },
            Reg::Common => {}
        }
        (a, b)
    };
    let inv = |m: &DMatrix<f64>| m.clone().try_inverse().expect("singular matrix in baseline-risk solve");

    let loglik = |th: &DVector<f64>| -> f64 {
        let mut s = 0.0;
        for &(si, ot, y, wd) in &arms {
            let eta = match ot { None => th[si], Some(t) => th[si] + th[dcol(t)] + th[slope_col(t)] * (th[si] - mx) };
            if binary { let p = sigmoid(eta).clamp(1e-12, 1.0 - 1e-12); s += y * p.ln() + (wd - y) * (1.0 - p).ln(); }
            else { let r = y - eta; s -= 0.5 * r * r / wd; }
        }
        s
    };
    let pen = |th: &DVector<f64>, sig: f64| -> f64 {
        let mut s = loglik(th);
        if reg == Reg::Exchangeable {
            let mut q = 0.0; for t in 0..nt { let d = th[slope_col(t)] - th[common_col]; q += d * d; }
            s -= 0.5 / sig * q;
        }
        s
    };

    // EM updates
    let em_tau = |th: &DVector<f64>, cov: &DMatrix<f64>, t2: f64| -> f64 {
        let t2arm = t2 / 2.0; let (mut s1, mut s2) = (0.0, 0.0);
        for &(si, ot, y, wd) in &arms {
            let (jac, z, w) = pieces(th, si, ot, y, wd);
            let resid = z - jac.iter().map(|(i, v)| v * th[*i]).sum::<f64>();
            let b = w * t2arm / (1.0 + w * t2arm);
            // quadForm jac cov
            let mut qf = 0.0; for &(i, vi) in &jac { for &(j, vj) in &jac { qf += vi * cov[(i, j)] * vj; } }
            s1 += (b * resid).powi(2); s2 += (1.0 - b) * t2arm + b * b * qf;
        }
        (2.0 * (s1 / n_arm + s2 / n_arm)).max(1e-8)
    };
    let em_sig = |th: &DVector<f64>, cov: &DMatrix<f64>| -> f64 {
        let mut s = 0.0;
        for t in 0..nt {
            let (bt, bb) = (slope_col(t), common_col);
            let m = th[bt] - th[bb];
            s += m * m + cov[(bt, bt)] + cov[(bb, bb)] - 2.0 * cov[(bt, bb)];
        }
        (s / nt as f64).max(1e-8)
    };

    // relax the spring network to force-balance equilibrium (damped Newton/IRLS =
    // the relaxation dynamics; net force → 0 ⇔ MLE), updating (θ, τ², σ_B²)
    let mut theta = DVector::<f64>::zeros(np);
    for i in 0..ns { theta[i] = base0[i]; }
    let mut t2 = if random { 0.1 } else { 0.0 };
    let mut sig = 0.1;
    let tol_pen = if binary { 1e-6 } else { 1e-9 };
    for _ in 0..200 {
        let (a, b) = build(&theta, t2, sig);
        let newton = inv(&a) * &b;
        let dir = &newton - &theta;
        let pen_cur = pen(&theta, sig);
        let mut alpha = 1.0; let mut next = theta.clone();
        for _ in 0..40 {
            let cand = &theta + &dir * alpha;
            let pt = pen(&cand, sig);
            if pt.is_finite() && pt >= pen_cur - tol_pen { next = cand; break; }
            alpha *= 0.5;
        }
        let (a2, _) = build(&next, t2, sig);
        let cov2 = inv(&a2);
        let t2n = if random { em_tau(&next, &cov2, t2) } else { 0.0 };
        let sign = if reg == Reg::Exchangeable { em_sig(&next, &cov2) } else { sig };
        let conv = (0..np).map(|i| (theta[i] - next[i]).abs()).fold(0.0, f64::max) < 1e-9
            && (t2 - t2n).abs() < 1e-9 && (sig - sign).abs() < 1e-9;
        theta = next; t2 = t2n; sig = sign;
        if conv { break; }
    }

    let mut treat_order = vec![args.reff.clone()];
    treat_order.extend(nonref.iter().cloned());
    let ord_dcol: Vec<Option<usize>> = treat_order.iter().map(|t| if *t == args.reff { None } else { Some(dcol(tidx[t.as_str()])) }).collect();
    let ord_bcol: Vec<Option<usize>> = treat_order.iter().map(|t| if *t == args.reff { None } else { Some(slope_col(tidx[t.as_str()])) }).collect();
    let d_hat: Vec<f64> = ord_dcol.iter().map(|c| c.map_or(0.0, |c| theta[c])).collect();
    let b_hat: Vec<f64> = ord_bcol.iter().map(|c| c.map_or(0.0, |c| theta[c])).collect();
    let take = |th: &DVector<f64>, ord: &[Option<usize>]| -> Vec<f64> { ord.iter().map(|c| c.map_or(0.0, |c| th[c])).collect() };

    let do_gc = integ_tau || integ_sig;     // GC grid only meaningful with a variance component
    let mut rng = Rng::new(args.seed);
    let mut d_draws = Vec::with_capacity(args.samples);
    let mut b_draws = Vec::with_capacity(args.samples);
    let (mut tau2_out, mut sig_out) = (t2, if reg == Reg::Exchangeable { Some(sig) } else { None });

    if do_gc {
        // Grand-canonical integration of the present variance components: freeze the
        // (binomial/Gaussian) linearisation at the mode, then integrate the exact Gaussian
        // partition function over a grid — τ² (heterogeneity, if RE) and σ_B² (slope dispersion,
        // exchangeable). Draws = (pick a cell ∝ Ξ) then N(mean_cell, cov_cell). The slope-springs
        // (and τ-springs) "come and go" rather than being pinned at a point — this gives
        // calibrated intervals where the point-Laplace SE is too tight (weakly-identified τ²/σ_B²).
        let jacs: Vec<(Vec<(usize, f64)>, f64, f64)> =
            arms.iter().map(|&(si, ot, y, wd)| pieces(&theta, si, ot, y, wd)).collect();
        // Observed-information correction: the bilinear b_t·η_i term contributes
        // ∂²(−logL)/∂b_t∂η_i = residual at (si, slope_col t). Using the true energy
        // curvature (Hessian) — not the working JᵀWJ — as the harmonic stiffness makes
        // the partition function the exact mechanics of the spring network, not a
        // Gauss–Newton stiffness. (Empty for purely linear models / canonical NMA.)
        let cross: Vec<(usize, usize, f64)> = arms.iter().filter_map(|&(si, ot, y, wd)| ot.map(|t| {
            let eta = theta[si] + theta[dcol(t)] + theta[slope_col(t)] * (theta[si] - mx);
            let r = if binary { y - wd * sigmoid(eta) } else { (y - eta) / wd };
            (si, slope_col(t), r)
        })).collect();
        // τ² axis: linear [0, 12·τ̂²] when integrating, else the mode (single point).
        let ts: Vec<f64> = if integ_tau {
            let ng = 26usize; let hi = (t2 * 12.0).max(0.02);
            (1..=ng).map(|i| hi * (i as f64 - 0.5) / ng as f64).collect()
        } else { vec![t2] };
        // σ_B² axis: log [1e-4, 1] when integrating (exchangeable), else a single dummy cell.
        let ss: Vec<f64> = if integ_sig {
            let nsg = 26usize; let (slo, shi) = (1.0f64 / 1e4, 1.0f64);
            (1..=nsg).map(|j| slo * (shi / slo).powf((j as f64 - 0.5) / nsg as f64)).collect()
        } else { vec![1.0] };
        // per cell: (logpost, mean, Cholesky-L of cov, τ², σ_B²)
        let mut cells: Vec<(f64, DVector<f64>, DMatrix<f64>, f64, f64)> = Vec::with_capacity(ts.len() * ss.len());
        let mut mxlp = -1e300;
        for &t2c in &ts { for &s2c in &ss {
            let mut a = DMatrix::<f64>::zeros(np, np); let mut b = DVector::<f64>::zeros(np);
            let (mut slv, mut zwz) = (0.0, 0.0);
            for (jac, z, w) in &jacs {
                let we = 1.0 / (1.0 / w + t2c / 2.0);
                slv += (1.0 / w + t2c / 2.0).ln(); zwz += we * z * z;
                for &(i, vi) in jac { b[i] += we * vi * z; for &(j, vj) in jac { a[(i, j)] += we * vi * vj; } }
            }
            // ridge by slope structure: exchangeable integrates σ_B² (1/σ²), independent a fixed
            // 1e-4 keep-finite, common none.
            match reg {
                Reg::Exchangeable => { let invs = 1.0 / s2c;
                    for t in 0..nt { let (bt, bb) = (slope_col(t), common_col);
                        a[(bt, bt)] += invs; a[(bb, bb)] += invs; a[(bt, bb)] -= invs; a[(bb, bt)] -= invs; } },
                Reg::Independent => for t in 0..nt { let c = slope_col(t); a[(c, c)] += 1e-4; },
                Reg::Common => {}
            }
            // Working equilibrium (mode) from the Gauss–Newton stiffness A = JᵀW_eff J + ridge.
            let chol_a = match Cholesky::new(a.clone()) { Some(c) => c, None => continue };
            let mean = chol_a.solve(&b);
            let bx: f64 = b.iter().zip(mean.iter()).map(|(bi, ti)| bi * ti).sum();
            // Harmonic stiffness = observed-information Hessian H = A − cross. This sets the
            // fluctuation covariance and the partition-function determinant (the true curvature
            // of the energy well). PD-guard: fall back to A where the bilinear term breaks
            // positive-definiteness (η near the decoupling point 1+b≈0).
            let mut h = a;
            for &(si, c, r) in &cross { h[(si, c)] -= r; h[(c, si)] -= r; }
            let (logdet, cov) = match Cholesky::new(h) {
                Some(ch) => (2.0 * ch.l().diagonal().iter().map(|x| x.ln()).sum::<f64>(), ch.inverse()),
                None => (2.0 * chol_a.l().diagonal().iter().map(|x| x.ln()).sum::<f64>(), chol_a.inverse()),
            };
            let lz = -0.5 * (slv + logdet + (zwz - bx));
            // half-normal(0.5) priors; σ_B² adds the slope-prior norm −½n_T·logσ² and log-σ Jacobian.
            let lp = lz
                + if integ_tau { lphn(0.5, t2c) } else { 0.0 }
                + if integ_sig { -0.5 * nt as f64 * s2c.ln() + lphn(0.5, s2c) + s2c.ln() } else { 0.0 };
            let lc = Cholesky::new((&cov + cov.transpose()) * 0.5).map(|c| c.l()).unwrap_or_else(|| {
                let mut d = DMatrix::<f64>::zeros(np, np);
                for k in 0..np { d[(k, k)] = cov[(k, k)].max(0.0).sqrt(); } d });
            if lp > mxlp { mxlp = lp; }
            cells.push((lp, mean, lc, t2c, s2c));
        }}
        let mut wsum = 0.0; let mut cumw = Vec::with_capacity(cells.len());
        for c in &cells { wsum += (c.0 - mxlp).exp(); cumw.push(wsum); }
        let (mut gtau, mut gsig) = (0.0, 0.0);
        for c in &cells { let p = (c.0 - mxlp).exp() / wsum; gtau += p * c.3; gsig += p * c.4; }
        if integ_tau { tau2_out = gtau; }
        if integ_sig { sig_out = Some(gsig); }
        for _ in 0..args.samples {
            let u = rng.unif() * wsum;
            let mut ci = cumw.partition_point(|&x| x < u);
            if ci >= cells.len() { ci = cells.len() - 1; }
            let (_, mean, lc, _, _) = &cells[ci];
            let z = DVector::<f64>::from_fn(np, |_, _| rng.normal());
            let th = mean + lc * z;
            d_draws.push(take(&th, &ord_dcol)); b_draws.push(take(&th, &ord_bcol));
        }
    } else {
        // observed-info Laplace draws: iFisher = A(mode) − bilinear cross at (si, slope_col t).
        let (mut iobs, _) = build(&theta, t2, sig);
        for &(si, ot, y, wd) in &arms {
            if let Some(t) = ot {
                let eta = theta[si] + theta[dcol(t)] + theta[slope_col(t)] * (theta[si] - mx);
                let r = if binary { y - wd * sigmoid(eta) } else { (y - eta) / wd };
                let c = slope_col(t); iobs[(si, c)] -= r; iobs[(c, si)] -= r;
            }
        }
        let cov_obs = inv(&iobs);
        let cs = (&cov_obs + cov_obs.transpose()) * 0.5;
        let l = Cholesky::new(cs).map(|c| c.l()).unwrap_or_else(|| {
            let mut d = DMatrix::<f64>::zeros(np, np);
            for i in 0..np { d[(i, i)] = cov_obs[(i, i)].max(0.0).sqrt(); } d });
        for _ in 0..args.samples {
            let z = DVector::<f64>::from_fn(np, |_, _| rng.normal());
            let th = &theta + &l * z;
            d_draws.push(take(&th, &ord_dcol)); b_draws.push(take(&th, &ord_bcol));
        }
    }

    let report = Report {
        dataset: args.input.clone(),
        response: if binary { "binomial".into() } else { "normal".into() },
        ref_treatment: args.reff.clone(),
        treatments: treat_order,
        regressor: args.regressor.clone(),
        effects: args.effects.clone(),
        mx, tau2: tau2_out, sigma_b2: sig_out,
        n_samples: args.samples, seed: args.seed, fit_wall_sec: t0.elapsed().as_secs_f64(),
        d_hat, b_hat, d_draws, b_draws,
    };
    std::fs::write(&args.output, serde_json::to_string(&report).expect("serialise")).expect("write output");
    eprintln!("brisk: {} ({}, {}, {}), mx={:.4} tau2={:.4}{}, {} draws in {:.3}s",
        report.dataset, report.response, report.regressor, report.effects, report.mx, report.tau2,
        report.sigma_b2.map_or(String::new(), |s| format!(" sigB2={s:.4}")), report.n_samples, report.fit_wall_sec);
}
