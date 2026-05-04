//! Deterministic cycle-basis gradient descent for max-flow.
//!
//! Same parameterisation as the sampling prototype: state =
//! `(F_1, ..., F_K, c_{i,j})` over a shared cycle basis. Same energy:
//!
//!   E = -Σ_i F_i + μ · Σ_e barrier(Σ_i |f_{i,e}|/c_e)
//!
//! Smooth barrier `−log(1 − x²)`. Replace `|f_{i,e}|` with smoothed
//! `√(f² + ε)` for differentiability at zero.
//!
//! Algorithm: gradient descent with backtracking line search, μ
//! annealed in a barrier schedule. This is interior-point-method-style
//! optimisation in cycle-basis coordinates — the deterministic
//! counterpart of the sampling prototype.
//!
//! Should converge geometrically (vs MCMC's polynomial mixing) and be
//! orders of magnitude faster on the same problem.

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

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
    commodities: Vec<(usize, usize)>,
}

fn gen_graph(n: usize, target_e: usize, k: usize, seed: u64) -> Graph {
    assert!(n >= 2 + 2 * k);
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

fn write_lp_input(g: &Graph, path: &Path) -> std::io::Result<()> {
    let edges_json: Vec<String> = g.edges.iter().map(|&(u, v, c)| format!("[{u},{v},{c}]")).collect();
    let comm_json: Vec<String> = g.commodities.iter().map(|&(s, t)| format!("[{s},{t}]")).collect();
    let payload = format!("{{\"n\":{},\"edges\":[{}],\"commodities\":[{}]}}",
        g.n, edges_json.join(","), comm_json.join(","));
    std::fs::write(path, payload)
}

fn bfs_spanning_tree(g: &Graph, root: usize) -> (Vec<usize>, Vec<(i32, i8)>) {
    let n = g.n;
    let mut parent: Vec<usize> = (0..n).collect();
    let mut parent_edge: Vec<(i32, i8)> = vec![(-1, 0); n];
    let mut adj: Vec<Vec<(usize, usize, i8)>> = vec![Vec::new(); n];
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        adj[u].push((v, i, 1));
        adj[v].push((u, i, -1));
    }
    let mut visited = vec![false; n];
    let mut q = std::collections::VecDeque::new();
    q.push_back(root);
    visited[root] = true;
    while let Some(u) = q.pop_front() {
        for &(v, e_idx, sign) in &adj[u] {
            if !visited[v] {
                visited[v] = true;
                parent[v] = u;
                parent_edge[v] = (e_idx as i32, sign);
                q.push_back(v);
            }
        }
    }
    (parent, parent_edge)
}

fn tree_path(parent: &[usize], parent_edge: &[(i32, i8)], from: usize, to: usize) -> Vec<(usize, i8)> {
    let mut anc_from: Vec<usize> = vec![from];
    let mut cur = from;
    while cur != parent[cur] { cur = parent[cur]; anc_from.push(cur); }
    let mut anc_to: Vec<usize> = vec![to];
    cur = to;
    while cur != parent[cur] { cur = parent[cur]; anc_to.push(cur); }
    let set_to: std::collections::HashSet<usize> = anc_to.iter().cloned().collect();
    let lca = anc_from.iter().find(|x| set_to.contains(x)).copied().unwrap();
    let mut path: Vec<(usize, i8)> = Vec::new();
    let mut cur = from;
    while cur != lca { let (e, s) = parent_edge[cur]; path.push((e as usize, -s)); cur = parent[cur]; }
    let mut down: Vec<(usize, i8)> = Vec::new();
    let mut cur = to;
    while cur != lca { let (e, s) = parent_edge[cur]; down.push((e as usize, s)); cur = parent[cur]; }
    down.reverse();
    path.extend(down);
    path
}

fn cycle_basis(g: &Graph, parent: &[usize], parent_edge: &[(i32, i8)]) -> Vec<Vec<(usize, i8)>> {
    let mut is_tree_edge = vec![false; g.edges.len()];
    for v in 0..g.n {
        let (e, _) = parent_edge[v];
        if e >= 0 { is_tree_edge[e as usize] = true; }
    }
    let mut cycles = Vec::new();
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        if is_tree_edge[i] { continue; }
        let mut cyc = vec![(i, 1_i8)];
        cyc.extend(tree_path(parent, parent_edge, v, u));
        cycles.push(cyc);
    }
    cycles
}

fn build_commodity_flow(
    m: usize,
    f_value: f64,
    cycles: &[Vec<(usize, i8)>],
    cycle_coefs: &[f64],
    st_path: &[(usize, i8)],
) -> Vec<f64> {
    let mut f = vec![0.0_f64; m];
    for &(e, s) in st_path { f[e] += f_value * (s as f64); }
    for (i, cyc) in cycles.iter().enumerate() {
        let c = cycle_coefs[i];
        if c == 0.0 { continue; }
        for &(e, s) in cyc { f[e] += c * (s as f64); }
    }
    f
}

const SMOOTH_EPS: f64 = 1e-8;
fn smooth_abs(x: f64) -> f64 { (x * x + SMOOTH_EPS).sqrt() }
fn smooth_abs_grad(x: f64) -> f64 { x / (x * x + SMOOTH_EPS).sqrt() }

/// Energy at current state, plus per-edge total smooth-load and barrier sub-gradient
/// (Σ-deriv ∂barrier/∂L_e per edge), useful for computing gradients downstream.
fn energy(
    f_per_edge: &[Vec<f64>],
    f_value: &[f64],
    caps: &[f64],
    mu: f64,
) -> (f64, Vec<f64>, Vec<f64>) {
    let m = caps.len();
    let k = f_per_edge.len();
    let mut loads = vec![0.0_f64; m];
    for i in 0..m {
        for ci in 0..k { loads[i] += smooth_abs(f_per_edge[ci][i]); }
    }
    let mut bar_total = 0.0_f64;
    let mut bar_grad = vec![0.0_f64; m];
    let mut feasible = true;
    for i in 0..m {
        let r = loads[i] / caps[i];
        let r2 = r * r;
        if r2 >= 1.0 - 1e-12 { feasible = false; bar_total = f64::INFINITY; break; }
        bar_total -= (1.0 - r2).ln();
        // d/dL of -log(1 - L²/c²) = 2L/(c² − L²)
        bar_grad[i] = 2.0 * loads[i] / (caps[i] * caps[i] - loads[i] * loads[i]);
    }
    let total_F: f64 = f_value.iter().sum();
    let e = if feasible { -total_F + mu * bar_total } else { f64::INFINITY };
    (e, loads, bar_grad)
}

/// Compute gradients: dE/dF_i and dE/dc_{i,j}.
fn gradients(
    g: &Graph,
    f_per_edge: &[Vec<f64>],
    bar_grad: &[f64],
    mu: f64,
    cycles: &[Vec<(usize, i8)>],
    st_paths: &[Vec<(usize, i8)>],
) -> (Vec<f64>, Vec<Vec<f64>>) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let n_cycles = cycles.len();
    let mut grad_F = vec![-1.0_f64; k];
    let mut grad_c = vec![vec![0.0_f64; n_cycles]; k];

    // Precompute per-edge contribution: μ · bar_grad[e] · smooth_abs_grad(f_{i,e})
    // (this is the partial derivative ∂barrier/∂f_{i,e}).
    let mut per_edge_per_com: Vec<Vec<f64>> = vec![vec![0.0; m]; k];
    for ci in 0..k {
        for i in 0..m {
            per_edge_per_com[ci][i] = mu * bar_grad[i] * smooth_abs_grad(f_per_edge[ci][i]);
        }
    }

    // grad_F[ci] = -1 + Σ over edges in st_path[ci] of (signed) per_edge_per_com[ci][e]
    for ci in 0..k {
        for &(e, s) in &st_paths[ci] {
            grad_F[ci] += per_edge_per_com[ci][e] * (s as f64);
        }
    }
    // grad_c[ci][j] = Σ over edges in cycle[j] of (signed) per_edge_per_com[ci][e]
    for ci in 0..k {
        for (j, cyc) in cycles.iter().enumerate() {
            let mut g_val = 0.0_f64;
            for &(e, s) in cyc {
                g_val += per_edge_per_com[ci][e] * (s as f64);
            }
            grad_c[ci][j] = g_val;
        }
    }
    (grad_F, grad_c)
}

#[derive(Clone)]
struct GdResult {
    f_per: Vec<f64>,
    total: f64,
    iters: usize,
    time_ms: f64,
}

fn cycle_gd_maxflow(g: &Graph, mu_schedule: &[f64], inner_iters: usize, init_step: f64) -> GdResult {
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let (parent, parent_edge) = bfs_spanning_tree(g, 0);
    let cycles = cycle_basis(g, &parent, &parent_edge);
    let n_cycles = cycles.len();
    let st_paths: Vec<Vec<(usize, i8)>> = g.commodities.iter()
        .map(|&(s, t)| tree_path(&parent, &parent_edge, s, t)).collect();

    let mut f_value: Vec<f64> = vec![0.1; k];
    let mut cycle_coefs: Vec<Vec<f64>> = vec![vec![0.0; n_cycles]; k];
    let mut f_per_edge: Vec<Vec<f64>> = (0..k)
        .map(|ci| build_commodity_flow(m, f_value[ci], &cycles, &cycle_coefs[ci], &st_paths[ci]))
        .collect();

    let t0 = Instant::now();
    let mut total_iters = 0_usize;

    for &mu in mu_schedule {
        let mut step = init_step;
        for _ in 0..inner_iters {
            total_iters += 1;
            let (e_curr, _loads, bar_grad) = energy(&f_per_edge, &f_value, &caps, mu);
            if !e_curr.is_finite() { break; }

            let (gF, gC) = gradients(g, &f_per_edge, &bar_grad, mu, &cycles, &st_paths);

            // Gradient step with backtracking line search.
            let mut accepted = false;
            for _ls in 0..30 {
                let mut f_value_new = f_value.clone();
                let mut cycle_coefs_new = cycle_coefs.clone();
                for ci in 0..k {
                    f_value_new[ci] -= step * gF[ci];
                    if f_value_new[ci] < 0.0 { f_value_new[ci] = 0.0; }
                    for j in 0..n_cycles {
                        cycle_coefs_new[ci][j] -= step * gC[ci][j];
                    }
                }
                let f_per_new: Vec<Vec<f64>> = (0..k)
                    .map(|ci| build_commodity_flow(m, f_value_new[ci], &cycles, &cycle_coefs_new[ci], &st_paths[ci]))
                    .collect();
                let (e_new, _, _) = energy(&f_per_new, &f_value_new, &caps, mu);
                if e_new.is_finite() && e_new < e_curr {
                    f_value = f_value_new;
                    cycle_coefs = cycle_coefs_new;
                    f_per_edge = f_per_new;
                    accepted = true;
                    if e_curr - e_new < 1e-8 { break; }
                    step = (step * 1.2).min(1e3);
                    break;
                }
                step *= 0.5;
                if step < 1e-12 { break; }
            }
            if !accepted { break; }
        }
    }

    // Final feasibility check + report.
    let _ = (parent, parent_edge);
    let (mut max_cong, _) = (0.0_f64, ());
    for i in 0..m {
        let l: f64 = (0..k).map(|ci| f_per_edge[ci][i].abs()).sum();
        let cong = l / caps[i];
        if cong > max_cong { max_cong = cong; }
    }
    let total: f64 = f_value.iter().sum();
    GdResult { f_per: f_value, total, iters: total_iters, time_ms: t0.elapsed().as_secs_f64() * 1000.0 }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let inner_iters: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(2000);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    println!("# n={n}, m={m}, K={k}, inner_iters={inner_iters}, seed={seed}");
    println!("# commodities: {:?}", g.commodities);

    // Barrier schedule: start large (smooth) → small (sharp).
    let mu_schedule: Vec<f64> = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001];
    let res = cycle_gd_maxflow(&g, &mu_schedule, inner_iters, 0.01);
    println!("F_per = {:?}", res.f_per.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("total = {:.4}, iters = {}, time = {:.1} ms", res.total, res.iters, res.time_ms);

    // LP oracle.
    let tmp = std::env::temp_dir().join(format!("cyclegd_{seed}.json"));
    write_lp_input(&g, &tmp).unwrap();
    let oracle = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples").join("mc_lp_oracle.py");
    let t0 = Instant::now();
    if let Ok(out) = Command::new("python3").arg(&oracle).arg(&tmp).output() {
        let lp_ms = t0.elapsed().as_secs_f64() * 1000.0;
        if out.status.success() {
            let s = String::from_utf8_lossy(&out.stdout);
            println!();
            println!("=== LP oracle (HiGHS) ===");
            println!("LP: {} ({:.1} ms)", s.trim(), lp_ms);
            if let Some(start) = s.find("\"total\":") {
                let rest = &s[start + 8..];
                let end = rest.find(|c: char| c == ',' || c == '}').unwrap_or(rest.len());
                if let Ok(lp_total) = rest[..end].trim().parse::<f64>() {
                    let rel = (res.total - lp_total).abs() / lp_total.max(1.0);
                    println!("rel err vs LP = {:.4}%, pass(≤5%) = {}", rel * 100.0, rel <= 0.05);
                }
            }
        }
    }
    let _ = std::fs::remove_file(&tmp);
}
