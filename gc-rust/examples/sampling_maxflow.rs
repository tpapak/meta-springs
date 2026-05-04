//! Sampling-based max-flow: find the mode of `p(y) ∝ exp(β · F(y))`
//! where `y_e` are per-edge resistances and `F(y)` is the feasible
//! flow value at that resistance configuration.
//!
//! Each iteration: propose multiplicative perturbation to `y`, solve
//! one electrical flow (PCG), accept/reject via Metropolis. Anneal
//! temperature β toward 0 (high coldness) so the chain concentrates
//! near the mode = max-flow optimum.
//!
//! The hope: where deterministic MWU plateaus at a fixed point, MCMC
//! escapes by occasionally accepting energy-increasing moves and can
//! find a better basin. As β → ∞ the chain freezes at the optimum
//! (in principle).
//!
//! Single-commodity for now. K-commodity extension is straightforward.
//! Compared against OR-Tools.

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::CsrLap;
use nalgebra::{Cholesky, DMatrix};

struct Xs256 { s: [u64; 4] }
impl Xs256 {
    fn new(seed: u64) -> Self {
        let mut z = seed.wrapping_add(0x9E3779B97F4A7C15);
        let mut sm = || {
            z = z.wrapping_add(0x9E3779B97F4A7C15);
            let mut x = z;
            x = (x ^ (x >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
            x = (x ^ (x >> 27)).wrapping_mul(0x94D049BB133111EB);
            x ^ (x >> 31)
        };
        Self { s: [sm(), sm(), sm(), sm()] }
    }
    fn next_u64(&mut self) -> u64 {
        let result = self.s[0].wrapping_add(self.s[3]).rotate_left(23).wrapping_add(self.s[0]);
        let t = self.s[1] << 17;
        self.s[2] ^= self.s[0]; self.s[3] ^= self.s[1];
        self.s[1] ^= self.s[2]; self.s[0] ^= self.s[3];
        self.s[2] ^= t; self.s[3] = self.s[3].rotate_left(45);
        result
    }
    fn gen_range(&mut self, lo: u64, hi: u64) -> u64 { lo + self.next_u64() % (hi - lo) }
    fn unit(&mut self) -> f64 { (self.next_u64() as f64 / u64::MAX as f64).clamp(0.0, 1.0) }
    /// Standard normal via Box-Muller.
    fn normal(&mut self) -> f64 {
        let u1 = self.unit().max(1e-15);
        let u2 = self.unit();
        (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos()
    }
}

#[derive(Clone)]
struct Graph {
    n: usize,
    edges: Vec<(usize, usize, i64)>,
    source: usize,
    sink: usize,
}

fn gen_graph(n: usize, target_e: usize, seed: u64) -> Graph {
    let mut rng = Xs256::new(seed);
    let mut edges = Vec::with_capacity(target_e);
    let mut seen = std::collections::HashSet::with_capacity(target_e);
    let mut perm: Vec<usize> = (0..n).collect();
    for i in (1..n).rev() {
        let j = rng.gen_range(0, (i as u64) + 1) as usize;
        perm.swap(i, j);
    }
    for k in 1..n {
        let u = perm[k];
        let parent = perm[rng.gen_range(0, k as u64) as usize];
        let cap = (rng.gen_range(1, 101)) as i64;
        let (a, b) = if u < parent { (u, parent) } else { (parent, u) };
        if seen.insert((a, b)) { edges.push((a, b, cap)); }
    }
    while edges.len() < target_e.min(n * (n - 1) / 2) {
        let u = rng.gen_range(0, n as u64) as usize;
        let v = rng.gen_range(0, n as u64) as usize;
        if u == v { continue; }
        let (a, b) = if u < v { (u, v) } else { (v, u) };
        if seen.insert((a, b)) {
            let cap = (rng.gen_range(1, 101)) as i64;
            edges.push((a, b, cap));
        }
    }
    Graph { n, edges, source: perm[0], sink: perm[n - 1] }
}

fn write_dimacs(g: &Graph, path: &Path) -> std::io::Result<()> {
    let mut f = std::fs::File::create(path)?;
    writeln!(f, "p max {} {}", g.n, g.edges.len() * 2)?;
    writeln!(f, "n {} s", g.source + 1)?;
    writeln!(f, "n {} t", g.sink + 1)?;
    for &(u, v, c) in &g.edges {
        writeln!(f, "a {} {} {}", u + 1, v + 1, c)?;
        writeln!(f, "a {} {} {}", v + 1, u + 1, c)?;
    }
    Ok(())
}

fn run_ortools(driver: &Path, file: &Path) -> Option<(i64, u64)> {
    let out = Command::new("python3").arg(driver).arg(file).output().ok()?;
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
}

fn pcg_pinned(csr: &CsrLap, b: &[f64], pin: usize, tol: f64, max_iter: usize) -> (Vec<f64>, usize) {
    let n = csr.n();
    let mut x = vec![0.0_f64; n];
    let mut r = b.to_vec();
    r[pin] = 0.0;
    let diag = csr.diag(0.0);
    let m_inv: Vec<f64> = (0..n).map(|i| if i != pin && diag[i] > 0.0 { 1.0 / diag[i] } else { 0.0 }).collect();
    let mut z: Vec<f64> = r.iter().zip(m_inv.iter()).map(|(ri, mi)| ri * mi).collect();
    let mut p = z.clone();
    let mut ap = vec![0.0_f64; n];
    let mut p_pin_zero = vec![0.0_f64; n];
    let mut rz_old: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
    let b_norm: f64 = b.iter().map(|v| v * v).sum::<f64>().sqrt().max(1.0);
    for it in 0..max_iter {
        p_pin_zero.copy_from_slice(&p);
        p_pin_zero[pin] = 0.0;
        csr.apply(0.0, &p_pin_zero, &mut ap);
        ap[pin] = 0.0;
        let p_ap: f64 = p.iter().zip(ap.iter()).map(|(a, b)| a * b).sum();
        if p_ap.abs() < 1e-30 { return (x, it); }
        let alpha = rz_old / p_ap;
        for i in 0..n { x[i] += alpha * p[i]; r[i] -= alpha * ap[i]; }
        x[pin] = 0.0; r[pin] = 0.0;
        let r_norm: f64 = r.iter().map(|v| v * v).sum::<f64>().sqrt();
        if r_norm / b_norm < tol { return (x, it + 1); }
        for i in 0..n { z[i] = r[i] * m_inv[i]; }
        let rz_new: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
        let beta = rz_new / rz_old;
        for i in 0..n { p[i] = z[i] + beta * p[i]; }
        p[pin] = 0.0;
        rz_old = rz_new;
    }
    (x, max_iter)
}

/// Evaluate (F_feasible, log_xi) at given resistances y. log_xi is the
/// log of the **grand canonical partition function** at y:
///
///   Ξ(y) ≈ exp(F_feas(y)) · (1/√det(L_y + εI)) · prior(y)
///   log Ξ(y) = F_feas(y) − (1/2) log det(L_y + εI) + log prior(y)
///
/// The −(1/2) log det term is the entropic / volume contribution of the
/// flow-distribution Gaussian centred at the modal flow at this y. Higher
/// det = stiffer system = smaller volume = lower Ξ. So Ξ rewards both
/// **high feasible flow** AND **soft / large-volume** configurations.
///
/// One Cholesky factorisation per evaluation (replaces PCG; gives both the
/// modal flow and the log det in O(n³) for the dense case here).
fn evaluate_xi(g: &Graph, y: &[f64], caps: &[f64], eps_reg: f64) -> (f64, f64) {
    let n = g.n;
    let mut a = DMatrix::<f64>::zeros(n, n);
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        let g_e = 1.0 / y[i].max(1e-12);
        a[(u, u)] += g_e;
        a[(v, v)] += g_e;
        a[(u, v)] -= g_e;
        a[(v, u)] -= g_e;
    }
    // Add ε I + pin the sink (set diag huge to enforce φ_sink ≈ 0, simpler than
    // removing a row/col for this small dense case).
    for i in 0..n { a[(i, i)] += eps_reg; }
    let pin_penalty = 1e6;
    a[(g.sink, g.sink)] += pin_penalty;

    let chol = match Cholesky::new(a.clone()) {
        Some(c) => c,
        None => return (0.0, f64::NEG_INFINITY),
    };

    // Modal flow: solve a φ = e_s.
    let mut b = nalgebra::DVector::<f64>::zeros(n);
    b[g.source] = 1.0;
    let phi = chol.solve(&b);

    // Compute max congestion.
    let mut max_cong = 0.0_f64;
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        let f = (phi[u] - phi[v]) / y[i].max(1e-12);
        let cong = f.abs() / caps[i];
        if cong > max_cong { max_cong = cong; }
    }
    let f_feas = if max_cong > 0.0 { 1.0 / max_cong } else { 0.0 };

    // log det via Cholesky factor: 2 · sum log(L[i,i]).
    let l = chol.l();
    let log_det: f64 = (0..n).map(|i| l[(i, i)].ln()).sum::<f64>() * 2.0;

    let log_xi = f_feas - 0.5 * log_det;
    (f_feas, log_xi)
}

#[derive(Clone)]
struct SaResult {
    f_best: f64,
    accepted: usize,
    proposed: usize,
    total_pcg: usize,
    time_ms: f64,
}

fn simulated_annealing(g: &Graph, n_iters: usize, beta_start: f64, beta_end: f64, step_size: f64, seed: u64) -> SaResult {
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut rng = Xs256::new(seed ^ 0xC0FFEE);

    // Initialize y_e = 1/c_e^2 (favor high-cap edges).
    let mut y: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c)).collect();
    let eps_reg = 1e-6;
    let (mut f_curr, mut log_xi_curr) = evaluate_xi(g, &y, &caps, eps_reg);
    let mut f_best = f_curr;
    let mut accepted = 0_usize;
    let mut proposed = 0_usize;
    let t0 = Instant::now();

    for k in 0..n_iters {
        let frac = k as f64 / n_iters as f64;
        let beta = beta_start * (beta_end / beta_start).powf(frac);

        // Propose: multiplicative log-normal perturbation per edge.
        let mut y_prop = y.clone();
        for i in 0..m {
            y_prop[i] *= (step_size * rng.normal()).exp();
            y_prop[i] = y_prop[i].max(1e-12).min(1e12);
        }

        let (f_prop, log_xi_prop) = evaluate_xi(g, &y_prop, &caps, eps_reg);
        proposed += 1;

        // Metropolis on log Ξ (the partition function score, not just F).
        let log_alpha = beta * (log_xi_prop - log_xi_curr);
        let accept = if log_alpha >= 0.0 { true } else { rng.unit().ln() < log_alpha };

        if accept {
            y = y_prop;
            log_xi_curr = log_xi_prop;
            f_curr = f_prop;
            accepted += 1;
        }
        if f_curr > f_best { f_best = f_curr; }
    }
    SaResult {
        f_best,
        accepted,
        proposed,
        total_pcg: 0,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(50);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(200);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(5000);
    let beta_start: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-start=")?.parse().ok()).unwrap_or(0.001);
    let beta_end: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-end=")?.parse().ok()).unwrap_or(10.0);
    let step: f64 = args.iter().find_map(|a| a.strip_prefix("--step=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();

    println!("# n={n}, m={m}, T={n_iters}, β=[{beta_start}, {beta_end}], step={step}, seed={seed}");
    println!("# source={}, sink={}", g.source, g.sink);

    let res = simulated_annealing(&g, n_iters, beta_start, beta_end, step, seed);
    println!("F_best (sampling) = {:.4}", res.f_best);
    println!("accepted: {}/{} ({:.1}%), total PCG iters: {}, time: {:.1} ms",
        res.accepted, res.proposed, 100.0 * res.accepted as f64 / res.proposed as f64, res.total_pcg, res.time_ms);

    // OR-Tools ground truth.
    let tmp = std::env::temp_dir().join(format!("samp_{seed}.max"));
    write_dimacs(&g, &tmp).unwrap();
    if let Some((f_or, us_or)) = run_ortools(driver, &tmp) {
        let f_or = f_or as f64;
        let rel = (res.f_best - f_or).abs() / f_or.max(1.0);
        println!();
        println!("OR-Tools (single-commodity ground truth) F* = {:.0} ({:.3} ms)", f_or, us_or as f64 / 1000.0);
        println!("rel err vs OR-Tools = {:.4}%", rel * 100.0);
    }
    let _ = std::fs::remove_file(&tmp);
}
