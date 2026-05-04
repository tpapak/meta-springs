//! Multi-commodity max-throughput flow prototype, spring/Laplacian-based.
//!
//! Setup: K commodities, each (s_i, t_i) with shared edge capacities.
//! Goal: maximise Σ_i (value of commodity i's flow), subject to
//!     Σ_i |f_i,e|  ≤  c_e   for every edge.
//!
//! Algorithm (Garg-Könemann-style MWU with electrical-flow inner):
//!
//!   y_e ← δ / c_e                          (small initial dual weight)
//!   F_i ← 0 for each commodity
//!   while not converged:
//!     for each commodity i:
//!       solve electrical flow with conductance 1/y_e, unit demand s_i → t_i
//!       compute direction d_i (divergence-free, value 1)
//!       α_i = max scaling such that Σ_i' |f_i' + (i==i') · α_i · d_i,e| ≤ c_e
//!         (so we don't break feasibility on shared edges)
//!       f_i ← f_i + α_i · d_i
//!       F_i ← F_i + α_i · div_s(d_i)
//!     update y_e ← y_e · (1 + ε · congestion_e²)
//!     where congestion_e = Σ_i |f_i,e| / c_e
//!   return total throughput Σ_i F_i and per-commodity F_i
//!
//! Validation: K=1 must match OR-Tools single max-flow exactly. K>1
//! has no easy ground truth — we check feasibility, conservation,
//! and that the algorithm self-consistently converges.

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
    /// (source, sink) per commodity. Vertex 0 is reserved for pinning.
    commodities: Vec<(usize, usize)>,
}

fn gen_graph(n: usize, target_e: usize, k: usize, seed: u64) -> Graph {
    assert!(n >= 2 + 2 * k, "need enough vertices for K commodities + pin");
    let mut rng = Xs256::new(seed);
    let mut edges = Vec::with_capacity(target_e);
    let mut seen = std::collections::HashSet::with_capacity(target_e);
    // Random spanning tree on vertices 0..n.
    let mut perm: Vec<usize> = (0..n).collect();
    for i in (1..n).rev() {
        let j = rng.gen_range(0, (i as u64) + 1) as usize;
        perm.swap(i, j);
    }
    for kk in 1..n {
        let u = perm[kk];
        let parent = perm[rng.gen_range(0, kk as u64) as usize];
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
    // Pick K commodities. Sources & sinks all in 1..n (vertex 0 reserved
    // for pinning). All terminals distinct from each other and from 0.
    let mut commodities: Vec<(usize, usize)> = Vec::with_capacity(k);
    let mut used = std::collections::HashSet::new();
    used.insert(0);
    for _ in 0..k {
        loop {
            let s = rng.gen_range(1, n as u64) as usize;
            let t = rng.gen_range(1, n as u64) as usize;
            if s != t && !used.contains(&s) && !used.contains(&t) {
                used.insert(s); used.insert(t);
                commodities.push((s, t));
                break;
            }
        }
    }
    Graph { n, edges, commodities }
}

fn write_dimacs_single(g: &Graph, ci: usize, path: &Path) -> std::io::Result<()> {
    let (s, t) = g.commodities[ci];
    let mut f = std::fs::File::create(path)?;
    writeln!(f, "p max {} {}", g.n, g.edges.len() * 2)?;
    writeln!(f, "n {} s", s + 1)?;
    writeln!(f, "n {} t", t + 1)?;
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

/// PCG with vertex pinning, no preconditioner beyond Jacobi.
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
struct McResult {
    /// Per-commodity flow values F_i.
    f_per: Vec<f64>,
    /// Total throughput Σ_i F_i.
    total: f64,
    /// Final per-edge per-commodity flow f[i][e].
    f_edge: Vec<Vec<f64>>,
    iters: usize,
    pcg_iters_total: usize,
    time_ms: f64,
}

/// Garg-Könemann-style MWU max-throughput multi-commodity flow with
/// electrical-flow inner solver.
fn mc_maxflow(g: &Graph, max_iters: usize, eps: f64) -> McResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    // Per-commodity per-edge flow.
    let mut f_per: Vec<f64> = vec![0.0; k];
    let mut f_edge: Vec<Vec<f64>> = vec![vec![0.0; m]; k];
    // Edge dual weights (Garg-Könemann y_e). Start tiny.
    let delta = 1e-3 / caps.iter().cloned().fold(0.0_f64, f64::max);
    let mut y: Vec<f64> = caps.iter().map(|c| delta / c).collect();

    let pcg_tol = 1e-10;
    let max_pcg = (4 * n).max(2000);
    let mut total_pcg = 0_usize;
    let mut iters_run = 0_usize;

    let t0 = Instant::now();

    for outer in 0..max_iters {
        iters_run = outer + 1;
        // Build CSR with conductance = 1/y_e (capacity-aware "easy" routes have low resistance).
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / y[i]))
            .collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);

        // For each commodity, solve electrical flow s_i → t_i, scale to feasibility,
        // augment that commodity's flow. Pin THIS commodity's sink at φ = 0 so
        // conservation at internal vertices is exact (b[t_i] is absorbed by the pin).
        let mut total_pushed = 0.0;
        for ci in 0..k {
            let (s_i, t_i) = g.commodities[ci];
            let mut b = vec![0.0_f64; n];
            b[s_i] = 1.0;
            // Pin t_i: drop b[t_i] entry (force φ[t_i] = 0); divergence at t_i
            // becomes -1 by global conservation, no leakage anywhere else.
            let (phi, pcg_it) = pcg_pinned(&csr, &b, t_i, pcg_tol, max_pcg);
            total_pcg += pcg_it;

            // Direction d on each edge: f_e = (φ_u − φ_v) · conductance
            let mut d = vec![0.0_f64; m];
            let mut div_s = 0.0_f64;
            for (i, &(u, v, _)) in g.edges.iter().enumerate() {
                d[i] = (phi[u] - phi[v]) / y[i];
                if u == s_i { div_s += d[i]; }
                if v == s_i { div_s -= d[i]; }
            }
            if (div_s - 1.0).abs() > 1e-3 { continue; } // disconnected residual; skip

            // α: max scaling such that |f_per[ci' for all ci'] augmented| respects cap.
            // Since this commodity is the only one being augmented, the relevant
            // bound per edge:
            //   Σ_{j≠ci} |f_edge[j][e]| + |f_edge[ci][e] + α·d[e]|  ≤  c_e
            // Define existing_other[e] = Σ_{j≠ci} |f_edge[j][e]|
            //        existing_self[e]  = f_edge[ci][e]
            //        slack[e] = c_e − existing_other[e]
            //        require |existing_self[e] + α·d[e]| ≤ slack[e]
            // → if d[e] > 0: α ≤ (slack[e] − existing_self[e]) / d[e]
            // → if d[e] < 0: α ≤ (−slack[e] − existing_self[e]) / d[e]
            let mut existing_other: Vec<f64> = vec![0.0; m];
            for j in 0..k {
                if j == ci { continue; }
                for i in 0..m {
                    existing_other[i] += f_edge[j][i].abs();
                }
            }
            // Compute α: the largest scaling such that |f_self + α·d_e| ≤ slack
            // for every edge. The previous version skipped edges with `bound ≤ 0`,
            // which silently allowed α to grow past what those edges permit —
            // the algorithm then pushed infeasible flow.
            let mut alpha = f64::INFINITY;
            let mut feasible_pre = true;
            for i in 0..m {
                let d_e = d[i];
                let slack = caps[i] - existing_other[i];
                let f_self = f_edge[ci][i];
                if slack < -1e-9 {
                    // Numerical infeasibility from drift; bail this commodity.
                    feasible_pre = false;
                    break;
                }
                if d_e.abs() < 1e-15 {
                    // Edge unchanged by this direction; require current flow already feasible.
                    if f_self.abs() > slack.max(0.0) + 1e-9 {
                        feasible_pre = false;
                        break;
                    }
                    continue;
                }
                let upper = if d_e > 0.0 {
                    (slack.max(0.0) - f_self) / d_e
                } else {
                    (-slack.max(0.0) - f_self) / d_e
                };
                // upper may be ≤ 0 if this edge is already binding — that
                // means α cannot increase, set to 0.
                let upper = upper.max(0.0);
                if upper < alpha { alpha = upper; }
            }
            if !feasible_pre || !alpha.is_finite() || alpha < 1e-12 { continue; }

            // Damp the step so a single greedy commodity can't saturate
            // everything before MWU has a chance to redistribute. GK-style
            // bounded step: push at most a small fraction of feasibility.
            let damp = 0.1;
            let alpha = alpha * damp;

            for i in 0..m { f_edge[ci][i] += alpha * d[i]; }
            f_per[ci] += alpha * div_s;
            total_pushed += alpha;
        }

        // Update edge weights based on TOTAL congestion across commodities.
        let mut max_cong = 0.0_f64;
        for i in 0..m {
            let total_load: f64 = (0..k).map(|j| f_edge[j][i].abs()).sum();
            let cong = total_load / caps[i];
            y[i] *= 1.0 + eps * cong * cong;
            if cong > max_cong { max_cong = cong; }
        }

        // Stop when one outer iter pushed essentially nothing
        if total_pushed < 1e-9 { break; }
    }

    let total: f64 = f_per.iter().sum();
    McResult {
        f_per,
        total,
        f_edge,
        iters: iters_run,
        pcg_iters_total: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

/// Verify result: each commodity's flow is conservation-respecting and
/// per-edge total absolute flow is within tolerance of capacity.
fn verify(g: &Graph, res: &McResult) -> (bool, String) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // 1. Per-commodity conservation at internal vertices
    for ci in 0..k {
        let (s_i, t_i) = g.commodities[ci];
        let mut div = vec![0.0_f64; g.n];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            div[u] += res.f_edge[ci][i];
            div[v] -= res.f_edge[ci][i];
        }
        for i in 0..g.n {
            if i == s_i || i == t_i { continue; }
            if div[i].abs() > 1e-4 {
                return (false, format!("commodity {ci} non-zero divergence {} at internal {i}", div[i]));
            }
        }
        // Source divergence ≈ F_i, sink divergence ≈ −F_i
        if (div[s_i] - res.f_per[ci]).abs() > 1e-3 {
            return (false, format!("commodity {ci}: div[source] = {} but F = {}", div[s_i], res.f_per[ci]));
        }
    }

    // 2. Per-edge feasibility: total absolute flow ≤ capacity
    for i in 0..m {
        let total_load: f64 = (0..k).map(|j| res.f_edge[j][i].abs()).sum();
        if total_load > caps[i] * 1.001 {
            return (false, format!("edge {i} overloaded: load {} cap {}", total_load, caps[i]));
        }
    }

    (true, "OK".into())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(50);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(200);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(2);
    let max_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(2000);
    let eps: f64 = args.iter().find_map(|a| a.strip_prefix("--eps=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T_max={max_iters}, eps={eps}, seed={seed}", g.edges.len());
    println!("# commodities (s, t): {:?}", g.commodities);

    let res = mc_maxflow(&g, max_iters, eps);
    let (ok, msg) = verify(&g, &res);
    println!("# verification: {} ({})", if ok { "PASS" } else { "FAIL" }, msg);
    println!("# total throughput = {:.4}", res.total);
    println!("# per-commodity F_i = {:?}", res.f_per.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("# outer iters = {}, total PCG iters = {}, time = {:.1} ms", res.iters, res.pcg_iters_total, res.time_ms);

    if k == 1 {
        // Validate against OR-Tools single max-flow.
        let tmp = std::env::temp_dir().join(format!("mc1_{seed}.max"));
        write_dimacs_single(&g, 0, &tmp).unwrap();
        if let Some((f_or, us_or)) = run_ortools(driver, &tmp) {
            let f_or = f_or as f64;
            let rel_err = (res.total - f_or).abs() / f_or.max(1.0);
            println!("# OR-Tools (single-commodity ground truth) F = {:.0} ({:.3} ms)", f_or, us_or as f64 / 1000.0);
            println!("# rel err vs OR-Tools = {:.5}%  pass = {}", rel_err * 100.0, rel_err <= 0.01);
        }
        let _ = std::fs::remove_file(&tmp);
    }
}
