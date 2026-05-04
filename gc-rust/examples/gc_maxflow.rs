//! Correctness experiment: GC-style outer loop over the flow value `F`,
//! with `lap_solver::pcg_solve` as the inner spring-equilibrium solver,
//! verified against OR-Tools' deterministic max-flow.
//!
//! Algorithm:
//!
//!   F_lo = 0
//!   F_hi = sum of source-incident capacities  (trivial upper bound)
//!   while (F_hi − F_lo) / max(F_hi, 1) > tol:
//!       F = (F_lo + F_hi) / 2
//!       feasible = irls_test_feasibility(F):
//!           reset resistances r_e = 1/c_e²
//!           sum_flow = 0
//!           for k = 1..T_inner:
//!               solve electrical flow at value F with current r
//!               sum_flow += f^k
//!               r_e ← r_e · (1 + η · (load_e / max_cong_k)²)
//!               renormalise r by geometric mean
//!           f_avg = sum_flow / T_inner
//!           max_cong_avg = max |f_avg_e| / c_e
//!           return max_cong_avg ≤ 1 + slack
//!       if feasible: F_lo = F
//!       else:        F_hi = F
//!
//! Final F_lo is reported as F_GC. Compared against OR-Tools' exact F*.
//!
//! Run:
//!   cargo run --release --example gc_maxflow

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::{pcg_solve, CsrLap, Jacobi};

// ─────────────────────────────────────────────────────────────────────
// Tiny xoshiro256++ PRNG — reproducible, deterministic
// ─────────────────────────────────────────────────────────────────────

struct Xs256 {
    s: [u64; 4],
}

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
        let result = self.s[0]
            .wrapping_add(self.s[3])
            .rotate_left(23)
            .wrapping_add(self.s[0]);
        let t = self.s[1] << 17;
        self.s[2] ^= self.s[0];
        self.s[3] ^= self.s[1];
        self.s[1] ^= self.s[2];
        self.s[0] ^= self.s[3];
        self.s[2] ^= t;
        self.s[3] = self.s[3].rotate_left(45);
        result
    }
    fn gen_range(&mut self, lo: u64, hi: u64) -> u64 {
        lo + self.next_u64() % (hi - lo)
    }
}

// ─────────────────────────────────────────────────────────────────────
// Graph generation: connected random graph, integer capacities
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct Graph {
    n: usize,
    /// One entry per undirected edge: `(u, v, cap)` with `u < v`.
    edges: Vec<(usize, usize, i64)>,
    source: usize,
    sink: usize,
}

fn gen_graph(n: usize, target_e: usize, seed: u64) -> Graph {
    let mut rng = Xs256::new(seed);
    let mut edges: Vec<(usize, usize, i64)> = Vec::with_capacity(target_e);
    let mut seen: std::collections::HashSet<(usize, usize)> =
        std::collections::HashSet::with_capacity(target_e);

    // Random spanning tree to guarantee connectivity.
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
        if seen.insert((a, b)) {
            edges.push((a, b, cap));
        }
    }
    // Top up with random edges.
    let max_edges = n * (n - 1) / 2;
    let target_e = target_e.min(max_edges);
    while edges.len() < target_e {
        let u = rng.gen_range(0, n as u64) as usize;
        let v = rng.gen_range(0, n as u64) as usize;
        if u == v {
            continue;
        }
        let (a, b) = if u < v { (u, v) } else { (v, u) };
        if seen.insert((a, b)) {
            let cap = (rng.gen_range(1, 101)) as i64;
            edges.push((a, b, cap));
        }
    }
    Graph { n, edges, source: perm[0], sink: perm[n - 1] }
}

// ─────────────────────────────────────────────────────────────────────
// DIMACS .max writer + OR-Tools subprocess
// ─────────────────────────────────────────────────────────────────────

fn write_dimacs(g: &Graph, path: &Path) -> std::io::Result<()> {
    let mut f = std::fs::File::create(path)?;
    let m_directed = g.edges.len() * 2;
    writeln!(f, "c gc_maxflow experiment graph")?;
    writeln!(f, "p max {} {}", g.n, m_directed)?;
    // 1-indexed source/sink in DIMACS.
    writeln!(f, "n {} s", g.source + 1)?;
    writeln!(f, "n {} t", g.sink + 1)?;
    for &(u, v, c) in &g.edges {
        writeln!(f, "a {} {} {}", u + 1, v + 1, c)?;
        writeln!(f, "a {} {} {}", v + 1, u + 1, c)?;
    }
    Ok(())
}

fn run_ortools(driver: &Path, graph_file: &Path) -> Option<(i64, u64)> {
    let out = Command::new("python3")
        .arg(driver)
        .arg(graph_file)
        .output()
        .ok()?;
    if !out.status.success() {
        eprintln!("ortools stderr: {}", String::from_utf8_lossy(&out.stderr));
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    let flow: i64 = it.next()?.parse().ok()?;
    let us: u64 = it.next()?.parse().ok()?;
    Some((flow, us))
}

// ─────────────────────────────────────────────────────────────────────
// IRLS feasibility test at fixed F using averaged flow
// ─────────────────────────────────────────────────────────────────────

/// Build a CSR Laplacian from the undirected edges with weights = 1/r_e.
fn build_csr(edges: &[(usize, usize, i64)], r: &[f64], n: usize) -> CsrLap {
    let weighted: Vec<(u32, u32, f64)> = edges
        .iter()
        .enumerate()
        .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i]))
        .collect();
    CsrLap::from_canonical_weights(&weighted, n)
}

/// Feasibility test by *witness*: run T_inner IRLS iterations; if **any
/// single iteration** produces a flow of value F with max congestion ≤
/// 1 + slack, F is achievable (the witness is a real feasible flow).
/// Otherwise deem infeasible.
///
/// This is the only honest version. Averaged-flow checks fail because
/// edge currents can cancel between iterations: e.g. f_e = +20 then −20
/// averages to 0 even though both individual flows violated capacity.
/// Asking for an explicit witness avoids that.
///
/// Returns `(feasible, min_max_cong_over_iters, total_pcg_iters)`.
fn test_feasibility_at_F(
    g: &Graph,
    f_target: f64,
    t_inner: usize,
    eta: f64,
    slack: f64,
    pcg_tol: f64,
) -> (bool, f64, usize) {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();

    let mut b = vec![0.0_f64; n];
    b[g.source] = f_target;
    b[g.sink] = -f_target;

    let eps_reg = 1e-10;
    let max_pcg_iter = (4 * n).max(2000);
    let mut total_pcg = 0_usize;
    let mut min_max_cong = f64::INFINITY;

    for _ in 0..t_inner {
        let csr = build_csr(&g.edges, &r, n);
        let prec = Jacobi::new(&csr, eps_reg);
        let res = pcg_solve(&csr, eps_reg, &b, &prec, pcg_tol, max_pcg_iter);
        total_pcg += res.iters;
        let phi = &res.x;

        let mut max_cong = 0.0_f64;
        let mut currents = vec![0.0_f64; m];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let f_e = (phi[u] - phi[v]) / r[i];
            currents[i] = f_e;
            let cong = f_e.abs() / caps[i];
            if cong > max_cong {
                max_cong = cong;
            }
        }
        if max_cong < min_max_cong {
            min_max_cong = max_cong;
        }
        // Witness check: must verify that the flow actually carries F units
        // out of the source (the εI regularisation in the Laplacian makes
        // the actual divergence `b_s − ε·φ_s` ≈ F only when φ_s is bounded;
        // if resistances diverge, this drifts).
        let mut div_s = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == g.source { div_s += currents[i]; }
            if v == g.source { div_s -= currents[i]; }
        }
        let value_match = (div_s - f_target).abs() <= 1e-6 * f_target.abs().max(1.0);
        if max_cong <= 1.0 + slack && value_match {
            return (true, min_max_cong, total_pcg);
        }
        // Multiplicative weight update + geometric-mean renormalisation.
        if max_cong > 0.0 {
            for i in 0..m {
                let load_norm = (currents[i].abs() / caps[i]) / max_cong;
                r[i] *= 1.0 + eta * load_norm * load_norm;
            }
            let log_sum: f64 = r.iter().map(|x| x.ln()).sum();
            let geom = (log_sum / m as f64).exp();
            if geom.is_finite() && geom > 0.0 {
                for ri in r.iter_mut() {
                    *ri /= geom;
                }
            }
        }
    }
    (false, min_max_cong, total_pcg)
}

// ─────────────────────────────────────────────────────────────────────
// Bisection outer (the GC-style sweep over F)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone)]
struct GcResult {
    f_value: f64,
    bisection_steps: usize,
    total_pcg_iters: usize,
    time_ms: f64,
}

fn gc_maxflow(
    g: &Graph,
    rel_tol: f64,
    t_inner: usize,
    eta: f64,
    slack: f64,
) -> GcResult {
    // Trivial upper bound: total capacity out of source.
    let mut f_hi: f64 = 0.0;
    for &(u, v, c) in &g.edges {
        if u == g.source || v == g.source {
            f_hi += c as f64;
        }
    }
    let mut f_lo: f64 = 0.0;
    let mut steps = 0_usize;
    let mut total_pcg = 0_usize;

    let t0 = Instant::now();
    // Standard binary bisection.
    while f_hi - f_lo > rel_tol * f_hi.max(1.0) && steps < 50 {
        let f_try = 0.5 * (f_lo + f_hi);
        let (feasible, _max_cong, pcg) =
            test_feasibility_at_F(g, f_try, t_inner, eta, slack, 1e-7);
        total_pcg += pcg;
        if feasible {
            f_lo = f_try;
        } else {
            f_hi = f_try;
        }
        steps += 1;
    }
    GcResult {
        f_value: f_lo,
        bisection_steps: steps,
        total_pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

// ─────────────────────────────────────────────────────────────────────
// Main: run on a few seeds, compare to OR-Tools, assert closeness
// ─────────────────────────────────────────────────────────────────────

fn main() {
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    if !driver.exists() {
        eprintln!("OR-Tools driver not found at {}", driver.display());
        std::process::exit(2);
    }

    let n: usize = std::env::args()
        .find_map(|a| a.strip_prefix("--n=")?.parse().ok())
        .unwrap_or(200);
    let m_target: usize = std::env::args()
        .find_map(|a| a.strip_prefix("--m=")?.parse().ok())
        .unwrap_or(1000);
    let rel_tol = 1e-3;
    let t_inner: usize = std::env::args()
        .find_map(|a| a.strip_prefix("--T=")?.parse().ok())
        .unwrap_or(60);
    let eta: f64 = std::env::args()
        .find_map(|a| a.strip_prefix("--eta=")?.parse().ok())
        .unwrap_or(0.1);
    let slack: f64 = std::env::args()
        .find_map(|a| a.strip_prefix("--slack=")?.parse().ok())
        .unwrap_or(0.01);

    println!("# n={n} random graphs, m≈{m_target}, rel_tol={rel_tol}, T_inner={t_inner}, eta={eta}, slack={slack}");
    println!("seed,V,E_undirected,F_ortools,F_GC,abs_err,rel_err,bisection_steps,pcg_iters,gc_ms,ortools_ms");

    let n_seeds: u64 = std::env::args()
        .find_map(|a| a.strip_prefix("--seeds=")?.parse().ok())
        .unwrap_or(5);
    let mut max_rel_err = 0.0_f64;
    for seed in 0..n_seeds {
        let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);

        let tmp = std::env::temp_dir().join(format!("gc_mf_seed{seed}.max"));
        write_dimacs(&g, &tmp).expect("write dimacs");

        let (f_or, us_or) = run_ortools(driver, &tmp).expect("ortools");
        let f_or = f_or as f64;
        let or_ms = us_or as f64 / 1000.0;

        let gc = gc_maxflow(&g, rel_tol, t_inner, eta, slack);

        let abs_err = (gc.f_value - f_or).abs();
        let rel_err = abs_err / f_or.max(1.0);
        if rel_err > max_rel_err {
            max_rel_err = rel_err;
        }
        println!(
            "{},{},{},{:.0},{:.2},{:.2},{:.5},{},{},{:.1},{:.2}",
            seed,
            g.n,
            g.edges.len(),
            f_or,
            gc.f_value,
            abs_err,
            rel_err,
            gc.bisection_steps,
            gc.total_pcg_iters,
            gc.time_ms,
            or_ms
        );
        let _ = std::fs::remove_file(&tmp);
    }

    eprintln!();
    eprintln!("# max relative error across seeds: {:.4}%", max_rel_err * 100.0);
    eprintln!("# pass (≤ 1% rel err): {}", max_rel_err <= 0.01);
}
