//! Spring-based stochastic augmenting flow — the proper "sample Ξ"
//! version that stays in the spring framework and reaches F*.
//!
//! Each iteration:
//!   1. Compute residual capacities from accumulated flow `f`.
//!   2. **Spring solve**: PCG on the Laplacian with conductance =
//!      f(residual_capacity), pinned sink, unit demand at source.
//!      This is the actual electrical-flow / spring-equilibrium step.
//!   3. Get the direction d_e from the potentials.
//!   4. **Sample** the augmenting step size α — random fraction of the
//!      max feasible step. Stochastic counterpart of cap-scaling's
//!      always-take-α_max.
//!   5. Augment: f += α · d. Update accumulated flow value F.
//!   6. Stop when residual graph has no augmenting direction.
//!
//! By Ford-Fulkerson logic, the sequence of augmentations converges to
//! F* (deterministically in cap-scaling, in expectation here). Every
//! step uses a Laplacian solve = spring inner. This is what I should
//! have built when you said "sample Ξ in flow space" — instead I went
//! to the springs-free cycle-basis formulation.

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::CsrLap;

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

#[derive(Clone)]
struct AugResult {
    f_best: f64,
    augmentations: usize,
    pcg_iters: usize,
    time_ms: f64,
}

/// Spring-based stochastic augmenting flow: Ford-Fulkerson with
/// electrical-flow oracle and randomised step sizes.
fn spring_sample_augment(
    g: &Graph,
    n_iters: usize,
    alpha_lo: f64,
    alpha_hi: f64,
    seed: u64,
) -> AugResult {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f = vec![0.0_f64; m]; // accumulated signed flow per edge
    let mut f_total = 0.0_f64;
    let mut f_best = 0.0_f64;

    let mut b = vec![0.0_f64; n];
    b[g.source] = 1.0;

    let mut total_pcg = 0_usize;
    let mut augmentations = 0_usize;
    let mut rng = Xs256::new(seed ^ 0xCAFE_F00D);
    let t0 = Instant::now();

    for _it in 0..n_iters {
        // Conductance per edge = residual capacity in absolute terms.
        // High residual = "easy" to push more current = high conductance.
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| {
                let resid = (caps[i] - f[i].abs()).max(1e-12);
                (u as u32, v as u32, resid)
            })
            .collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);

        // **Spring solve**: PCG on the Laplacian.
        let (phi, pcg_it) = pcg_pinned(&csr, &b, g.sink, 1e-10, 4 * n);
        total_pcg += pcg_it;

        // Direction d_e = (φ_u − φ_v) · conductance (= the electrical current).
        let mut d = vec![0.0_f64; m];
        let mut div_s = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let resid = (caps[i] - f[i].abs()).max(1e-12);
            d[i] = (phi[u] - phi[v]) * resid;
            if u == g.source { div_s += d[i]; }
            if v == g.source { div_s -= d[i]; }
        }
        // Disconnected residual graph → terminate.
        if (div_s - 1.0).abs() > 1e-3 { break; }

        // Max feasible α: largest scaling such that |f + α·d| ≤ c per edge.
        let mut alpha_max = f64::INFINITY;
        for i in 0..m {
            let d_e = d[i]; let f_e = f[i]; let c_e = caps[i];
            if d_e.abs() < 1e-15 { continue; }
            let bound = if d_e > 0.0 { (c_e - f_e) / d_e } else { (-c_e - f_e) / d_e };
            if bound > 0.0 && bound < alpha_max { alpha_max = bound; }
        }
        if !alpha_max.is_finite() || alpha_max < 1e-9 { break; }

        // **Sample** the step: random fraction of α_max in [alpha_lo, alpha_hi].
        let frac = alpha_lo + (alpha_hi - alpha_lo) * rng.unit();
        let alpha = alpha_max * frac;

        // Augment.
        for i in 0..m { f[i] += alpha * d[i]; }
        f_total += alpha * div_s;
        augmentations += 1;
        if f_total > f_best { f_best = f_total; }
    }

    AugResult {
        f_best,
        augmentations,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(200);
    let alpha_lo: f64 = args.iter().find_map(|a| a.strip_prefix("--lo=")?.parse().ok()).unwrap_or(0.5);
    let alpha_hi: f64 = args.iter().find_map(|a| a.strip_prefix("--hi=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, T={n_iters}, α∈[{alpha_lo}, {alpha_hi}], seed={seed}", g.edges.len());

    let res = spring_sample_augment(&g, n_iters, alpha_lo, alpha_hi, seed);
    println!("F_best (spring sample augment) = {:.4}", res.f_best);
    println!("augmentations: {}, PCG iters total: {}, time: {:.1} ms",
        res.augmentations, res.pcg_iters, res.time_ms);

    let tmp = std::env::temp_dir().join(format!("ssa_{seed}.max"));
    write_dimacs(&g, &tmp).unwrap();
    if let Some((f_or, us_or)) = run_ortools(driver, &tmp) {
        let f_or = f_or as f64;
        let rel = (res.f_best - f_or).abs() / f_or.max(1.0);
        println!();
        println!("OR-Tools F* = {:.0} ({:.3} ms)", f_or, us_or as f64 / 1000.0);
        println!("rel err = {:.4}%, pass(≤1%) = {}", rel * 100.0, rel <= 0.01);
    }
    let _ = std::fs::remove_file(&tmp);
}
