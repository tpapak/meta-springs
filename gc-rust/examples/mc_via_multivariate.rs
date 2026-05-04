//! Multi-commodity max-flow via the multivariate spring solver.
//!
//! Per `slmm/docs/spring_multivariate.html`: each treatment is a point
//! in R^K, each edge is a K-axis spring with stiffness matrix S_e.
//! K commodities = K outcome dimensions. Per outer iter we build the
//! full (T·K) × (T·K) block-Laplacian and solve **one** joint Cholesky
//! — not K independent scalar passes.
//!
//! Two stiffness modes:
//!   - `isotropic`: S_e = y_e · I_K. Block solve decomposes (Kronecker)
//!     into K decoupled scalar systems but is realised as ONE block
//!     Cholesky factorisation. Demonstrates factor-once architecture.
//!   - `coupled`: S_e includes a rank-1 `f_e f_eᵀ` cross term from the
//!     L² capacity barrier. K commodities are genuinely coupled at every
//!     edge; the system is no longer Kronecker-decomposable. This is the
//!     "real" multivariate IPM step.
//!
//! Outer loop: MWU on y_e (scalar dual weight per edge).
//!
//! Verification: K=1 matches OR-Tools single-commodity max-flow.

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use nalgebra::{Cholesky, DMatrix, DVector};

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
    /// Per commodity (s_i, t_i). All terminals must avoid the pinned vertex.
    commodities: Vec<(usize, usize)>,
}

fn gen_graph(n: usize, target_e: usize, k: usize, seed: u64) -> Graph {
    assert!(n >= 2 + 2 * k, "need enough vertices");
    let mut rng = Xs256::new(seed);
    let mut edges = Vec::with_capacity(target_e);
    let mut seen = std::collections::HashSet::with_capacity(target_e);
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
    let mut commodities = Vec::with_capacity(k);
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

#[derive(Clone, Copy, PartialEq)]
enum StiffnessMode {
    /// S_e = y_e · I_K — shared scalar weight, K-decoupled in spectrum but
    /// realised as a single block-matrix factorisation.
    Isotropic,
    /// S_e = y_e · I_K + γ · f_e f_eᵀ — adds capacity-barrier-style cross-commodity
    /// coupling at every edge. The block system is no longer Kronecker.
    Coupled,
}

/// One outer iteration: build the full (T·K) × (T·K) block-Laplacian
/// using current edge weights and current per-commodity flows, do **one**
/// Cholesky factorisation, K back-solves to get K commodity directions,
/// scale to feasibility, push, MWU update.
fn step_mc(
    g: &Graph,
    f_edge: &mut [Vec<f64>],
    f_per: &mut [f64],
    y: &mut [f64],
    mode: StiffnessMode,
    coupling_gamma: f64,
    eps_mwu: f64,
    damp: f64,
) -> usize {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // Per-edge K×K stiffness matrix.
    let mut s_e = vec![DMatrix::<f64>::zeros(k, k); m];
    for (i, _) in g.edges.iter().enumerate() {
        let mut s = DMatrix::<f64>::identity(k, k) * (1.0 / y[i]);
        if mode == StiffnessMode::Coupled {
            // Add rank-1 `γ · f_e f_eᵀ / (c_e − ||f_e||₁)`-style coupling.
            // Off-diagonals are positive when commodities flow in the same
            // direction (compete for capacity), negative otherwise.
            let f_e: DVector<f64> = DVector::from_iterator(k, (0..k).map(|j| f_edge[j][i]));
            let total = f_e.iter().map(|x| x.abs()).sum::<f64>();
            let slack = (caps[i] - total).max(1e-9);
            // Outer product f f^T scaled.
            let outer = &f_e * f_e.transpose();
            s += outer * (coupling_gamma / (slack * slack));
        }
        s_e[i] = s;
    }

    // Build block-Laplacian L_block of size n·K (we'll pin K rows/cols
    // — those for vertex 0, the global pin — at the end via Schur).
    let dim = n * k;
    let mut a = DMatrix::<f64>::zeros(dim, dim);
    let row_of = |vert: usize, com: usize| vert * k + com;
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        let s = &s_e[i];
        // Edge contribution: (e_u − e_v)(e_u − e_v)ᵀ ⊗ S_e. Each (vertex, com)
        // pair gets contribution to four block entries: (u,u), (u,v), (v,u), (v,v).
        for (sa, ta) in [(-1.0_f64, v), (1.0, u)] {
            for (sb, tb) in [(-1.0_f64, v), (1.0, u)] {
                let scalar = sa * sb;
                for ki in 0..k {
                    for kj in 0..k {
                        let ra = row_of(ta, ki);
                        let rb = row_of(tb, kj);
                        a[(ra, rb)] += scalar * s[(ki, kj)];
                    }
                }
            }
        }
    }
    // Pin vertex 0 for ALL commodities by zeroing its rows/cols and
    // setting diagonal to 1 (forces β[0, *] = 0 in the solve).
    for ki in 0..k {
        let r = row_of(0, ki);
        for c in 0..dim { a[(r, c)] = 0.0; a[(c, r)] = 0.0; }
        a[(r, r)] = 1.0;
    }

    // RHS: per-commodity demand b_i with +1 at s_i and -1 at t_i, in the i-th
    // commodity's component. Stack into a single (n·K) × K matrix (K rhs vectors).
    let mut rhs = DMatrix::<f64>::zeros(dim, k);
    for (ci, &(s_i, t_i)) in g.commodities.iter().enumerate() {
        rhs[(row_of(s_i, ci), ci)] = 1.0;
        rhs[(row_of(t_i, ci), ci)] = -1.0;
    }

    // ONE Cholesky factorisation, K back-solves.
    let chol = match Cholesky::new(a) {
        Some(c) => c,
        None => return 0,
    };
    let beta = chol.solve(&rhs);

    // For each commodity, compute direction d_i,e = (Σ_j S_e[i,j] (β_{u,j} − β_{v,j})).
    // (For isotropic this reduces to scalar `(β_{u,i} − β_{v,i}) / y_e`.)
    let mut total_pushed = 0.0;
    for ci in 0..k {
        let (s_i, _t_i) = g.commodities[ci];
        let mut d_i = vec![0.0_f64; m];
        let mut div_s = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            // d_i,e = sum over outcome k of S_e[ci, k] · (β_{u,k} − β_{v,k}) using ci's RHS column.
            let mut d = 0.0;
            for kj in 0..k {
                let bu = beta[(row_of(u, kj), ci)];
                let bv = beta[(row_of(v, kj), ci)];
                d += s_e[i][(ci, kj)] * (bu - bv);
            }
            d_i[i] = d;
            if u == s_i { div_s += d; }
            if v == s_i { div_s -= d; }
        }
        if (div_s - 1.0).abs() > 1e-3 { continue; }

        // α: max scaling such that the augmented per-commodity flow + existing
        // others' absolute flows stays ≤ capacity per edge.
        let mut existing_other = vec![0.0_f64; m];
        for j in 0..k {
            if j == ci { continue; }
            for i in 0..m { existing_other[i] += f_edge[j][i].abs(); }
        }
        let mut alpha = f64::INFINITY;
        let mut feasible = true;
        for i in 0..m {
            let d_e = d_i[i];
            let slack = (caps[i] - existing_other[i]).max(0.0);
            let f_self = f_edge[ci][i];
            if d_e.abs() < 1e-15 {
                if f_self.abs() > slack + 1e-9 { feasible = false; break; }
                continue;
            }
            let upper = if d_e > 0.0 { (slack - f_self) / d_e } else { (-slack - f_self) / d_e };
            let upper = upper.max(0.0);
            if upper < alpha { alpha = upper; }
        }
        if !feasible || !alpha.is_finite() || alpha < 1e-12 { continue; }
        let alpha = alpha * damp;
        for i in 0..m { f_edge[ci][i] += alpha * d_i[i]; }
        f_per[ci] += alpha * div_s;
        total_pushed += alpha;
    }

    // MWU on y_e based on total congestion.
    for i in 0..m {
        let total_load: f64 = (0..k).map(|j| f_edge[j][i].abs()).sum();
        let cong = total_load / caps[i];
        y[i] *= 1.0 + eps_mwu * cong * cong;
    }

    if total_pushed > 1e-9 { 1 } else { 0 }
}

fn mc_solve(g: &Graph, max_iters: usize, mode: StiffnessMode, eps: f64, damp: f64, gamma: f64) -> (Vec<f64>, usize, f64) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f_edge: Vec<Vec<f64>> = vec![vec![0.0; m]; k];
    let mut f_per: Vec<f64> = vec![0.0; k];
    let delta = 1e-3 / caps.iter().cloned().fold(0.0_f64, f64::max);
    let mut y: Vec<f64> = caps.iter().map(|c| delta / c).collect();

    let t0 = Instant::now();
    let mut iters_run = 0;
    for it in 0..max_iters {
        iters_run = it + 1;
        let pushed = step_mc(g, &mut f_edge, &mut f_per, &mut y, mode, gamma, eps, damp);
        if pushed == 0 { break; }
    }
    (f_per, iters_run, t0.elapsed().as_secs_f64() * 1000.0)
}

fn verify(g: &Graph, f_per: &[f64], _f_edge: &[Vec<f64>]) -> bool {
    // Just check non-negative.
    f_per.iter().all(|&x| x.is_finite() && x >= -1e-6)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(2);
    let max_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(1000);
    let eps: f64 = args.iter().find_map(|a| a.strip_prefix("--eps=")?.parse().ok()).unwrap_or(0.5);
    let damp: f64 = args.iter().find_map(|a| a.strip_prefix("--damp=")?.parse().ok()).unwrap_or(0.1);
    let gamma: f64 = args.iter().find_map(|a| a.strip_prefix("--gamma=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, eps={eps}, damp={damp}, gamma={gamma}, seed={seed}", g.edges.len());
    println!("# commodities: {:?}", g.commodities);

    println!();
    println!("=== Isotropic stiffness (factor-once-K-back-solves) ===");
    let f_iso = vec![vec![0.0_f64; g.edges.len()]; k];
    let (f_per, iters, ms) = mc_solve(&g, max_iters, StiffnessMode::Isotropic, eps, damp, 0.0);
    let total: f64 = f_per.iter().sum();
    let _ = (f_iso, verify(&g, &f_per, &Vec::<Vec<f64>>::new()));
    println!("F_per = {:?}", f_per.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("total = {:.4}, iters = {}, time = {:.1} ms", total, iters, ms);

    println!();
    println!("=== Coupled stiffness (cross-commodity rank-1 term) ===");
    let (f_per_c, iters_c, ms_c) = mc_solve(&g, max_iters, StiffnessMode::Coupled, eps, damp, gamma);
    let total_c: f64 = f_per_c.iter().sum();
    println!("F_per = {:?}", f_per_c.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("total = {:.4}, iters = {}, time = {:.1} ms", total_c, iters_c, ms_c);

    if k == 1 {
        let tmp = std::env::temp_dir().join(format!("mc_jt_{seed}.max"));
        write_dimacs_single(&g, 0, &tmp).unwrap();
        if let Some((f_or, us_or)) = run_ortools(driver, &tmp) {
            let f_or = f_or as f64;
            println!();
            println!("OR-Tools (single-commodity ground truth) F = {:.0} ({:.3} ms)", f_or, us_or as f64 / 1000.0);
            let rel_iso = (total - f_or).abs() / f_or.max(1.0);
            let rel_cpl = (total_c - f_or).abs() / f_or.max(1.0);
            println!("rel err (isotropic) = {:.4}%, (coupled) = {:.4}%", rel_iso * 100.0, rel_cpl * 100.0);
        }
        let _ = std::fs::remove_file(&tmp);
    }

    // LP oracle (scipy.optimize.linprog) — true fractional MC max-flow optimum.
    // Works for any K. Skip if --no-lp passed.
    let no_lp = args.iter().any(|a| a == "--no-lp");
    if !no_lp {
        // Dump graph + commodities to JSON.
        let tmp = std::env::temp_dir().join(format!("mc_lp_{seed}.json"));
        let edges_json: Vec<String> = g.edges.iter().map(|&(u, v, c)| format!("[{u},{v},{c}]")).collect();
        let comm_json: Vec<String> = g.commodities.iter().map(|&(s, t)| format!("[{s},{t}]")).collect();
        let payload = format!(
            "{{\"n\":{},\"edges\":[{}],\"commodities\":[{}]}}",
            g.n,
            edges_json.join(","),
            comm_json.join(","),
        );
        std::fs::write(&tmp, payload).unwrap();
        let oracle = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("examples").join("mc_lp_oracle.py");
        let t0 = Instant::now();
        let out = Command::new("python3").arg(&oracle).arg(&tmp).output();
        let lp_ms = t0.elapsed().as_secs_f64() * 1000.0;
        match out {
            Ok(o) if o.status.success() => {
                let s = String::from_utf8_lossy(&o.stdout);
                println!();
                println!("=== LP oracle (scipy.linprog HiGHS, fractional MC max-flow optimum) ===");
                println!("LP result: {} ({:.1} ms total)", s.trim(), lp_ms);
                // Parse "F_per": [...] manually.
                if let Some(start) = s.find("\"total\":") {
                    let rest = &s[start + 8..];
                    let end = rest.find(|c: char| c == ',' || c == '}').unwrap_or(rest.len());
                    if let Ok(lp_total) = rest[..end].trim().parse::<f64>() {
                        let rel_iso = (total - lp_total).abs() / lp_total.max(1.0);
                        let rel_cpl = (total_c - lp_total).abs() / lp_total.max(1.0);
                        println!("rel err vs LP — isotropic: {:.4}%, coupled: {:.4}%",
                            rel_iso * 100.0, rel_cpl * 100.0);
                    }
                }
            }
            Ok(o) => {
                eprintln!("# LP oracle failed: {}", String::from_utf8_lossy(&o.stderr));
            }
            Err(e) => {
                eprintln!("# python3 spawn failed: {e}");
            }
        }
        let _ = std::fs::remove_file(&tmp);
    }
}
