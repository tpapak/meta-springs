//! Newton-IPM for single-commodity max-flow with spring/Laplacian inner.
//!
//! Mądry-style IPM in primal/dual form:
//!
//!   Variables:
//!     - per-edge resistance r_e   (state)
//!     - flow target F             (annealed up)
//!
//!   Per Newton step:
//!     1. Build weighted Laplacian L = B^T diag(1/r_e) B with current r.
//!     2. **Spring solve** (PCG on L, pinned sink): L · φ = F · (e_s − e_t).
//!     3. Edge currents:  f_e = (φ_u − φ_v) / r_e.
//!     4. Compute capacity slack g_e = c_e² − f_e².
//!     5. Newton-update r_e ← g_e / 2  (this is the barrier Hessian's
//!        natural step in the log-barrier formulation).
//!     6. If max_cong < 1 − tol: grow F by a factor; else stop.
//!
//! Each iteration uses our `lap_solver::pcg_solve` (the actual spring
//! primitive). This is a genuinely spring-based deterministic
//! max-flow algorithm — different from cycle-basis GD which never
//! solves a Laplacian.
//!
//! Validation: K=1 vs OR-Tools push-relabel, exact match expected
//! within IPM tolerance.

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

/// PCG with one vertex pinned to 0.
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
struct IpmResult {
    f_best: f64,
    newton_iters: usize,
    pcg_iters: usize,
    time_ms: f64,
}

/// Newton-IPM with capacity-scaling outer. Each Newton step solves
/// **one Laplacian system via PCG** (the spring solve).
fn spring_ipm_maxflow(g: &Graph, n_outer: usize, n_inner: usize) -> IpmResult {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // Initial resistances: r_e = 1/c_e². With this, electrical flow at unit
    // demand favours high-capacity edges proportionally.
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();
    let mut f_value = 1.0_f64; // grow F over outer iters
    let mut f_best = 0.0_f64;

    // Demand vector with sink pinned (sink row will be ignored by PCG).
    let mut b = vec![0.0_f64; n];

    let mut total_pcg = 0_usize;
    let mut total_newton = 0_usize;
    let t0 = Instant::now();

    for _outer in 0..n_outer {
        // Try to push F up. Run inner Newton iterations at this F.
        b[g.source] = f_value;
        b[g.sink] = 0.0; // pinned; PCG ignores this row

        let mut converged = false;
        for _newton in 0..n_inner {
            total_newton += 1;
            // Build weighted Laplacian with conductance 1/r_e.
            let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i].max(1e-15)))
                .collect();
            let csr = CsrLap::from_canonical_weights(&weighted, n);

            // **Spring solve**: PCG on Laplacian, pinned sink.
            let (phi, pcg_it) = pcg_pinned(&csr, &b, g.sink, 1e-10, 4 * n);
            total_pcg += pcg_it;

            // Edge currents.
            let mut max_cong = 0.0_f64;
            let mut div_s = 0.0_f64;
            let mut f_edge = vec![0.0_f64; m];
            for (i, &(u, v, _)) in g.edges.iter().enumerate() {
                let f_e = (phi[u] - phi[v]) / r[i].max(1e-15);
                f_edge[i] = f_e;
                if u == g.source { div_s += f_e; }
                if v == g.source { div_s -= f_e; }
                let cong = f_e.abs() / caps[i];
                if cong > max_cong { max_cong = cong; }
            }
            // Sanity: divergence at source should equal demand f_value.
            if (div_s - f_value).abs() > 1e-3 * f_value.max(1.0) {
                // Numerical drift; abort this outer iter and shrink F.
                break;
            }

            // Newton-update r_e: r_e ← (c_e² − f_e²) / 2.
            // Comes from the log-barrier Hessian — drives f away from saturation.
            // Damped: blend 50% of new value with 50% old to stabilise.
            let mut max_change = 0.0_f64;
            for i in 0..m {
                let g_e = (caps[i] * caps[i] - f_edge[i] * f_edge[i]).max(1e-12);
                let r_new = g_e / 2.0;
                let damp = 0.05;
                let r_old = r[i];
                r[i] = damp * r_new + (1.0 - damp) * r_old;
                let rel_change = ((r[i] - r_old) / r_old.max(1e-15)).abs();
                if rel_change > max_change { max_change = rel_change; }
            }

            if max_cong < 1.0 - 1e-3 && max_change < 1e-4 {
                converged = true;
                break;
            }
            if max_cong > 1.0 - 1e-6 { break; }
        }

        // Check feasibility of current configuration: re-solve to get final f.
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i].max(1e-15)))
            .collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);
        let (phi, pcg_it) = pcg_pinned(&csr, &b, g.sink, 1e-10, 4 * n);
        total_pcg += pcg_it;
        let mut max_cong = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let f_e = (phi[u] - phi[v]) / r[i].max(1e-15);
            let cong = f_e.abs() / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        if max_cong <= 1.0 + 1e-3 {
            if f_value > f_best { f_best = f_value; }
            // Push F harder.
            f_value *= if converged { 1.5 } else { 1.1 };
        } else {
            // Backed off; tighten.
            f_value *= 0.95;
            if f_value <= f_best { break; }
        }
    }
    IpmResult {
        f_best,
        newton_iters: total_newton,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let outer: usize = args.iter().find_map(|a| a.strip_prefix("--outer=")?.parse().ok()).unwrap_or(40);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(50);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    println!("# n={n}, m={m}, outer={outer}, inner={inner}, seed={seed}");
    println!("# source={}, sink={}", g.source, g.sink);

    let res = spring_ipm_maxflow(&g, outer, inner);
    println!("F_best (spring IPM) = {:.4}", res.f_best);
    println!("Newton iters = {}, PCG iters total = {}, time = {:.1} ms",
        res.newton_iters, res.pcg_iters, res.time_ms);

    let tmp = std::env::temp_dir().join(format!("ipm_{seed}.max"));
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
