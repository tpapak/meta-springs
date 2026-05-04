//! Sample-in-flow-space max-flow.
//!
//! State: `(F, c_1, ..., c_{m−n+1})` — magnitude `F` of an s→t
//! augmenting path through a spanning tree, plus circulation
//! coefficients along a cycle basis (one cycle per non-tree edge).
//! Every state in this parameterisation is automatically divergence-
//! free: KCL holds at every internal vertex by construction.
//!
//! Energy: `E = −β·F + Σ_e barrier(|f_e|/c_e)` with smooth barrier
//! `−log(1 − x²)`.
//!
//! Each Metropolis-Hastings step computes `f` from the state vector
//! (one O(m) pass) and evaluates energy. No PCG, no electrical-flow
//! inner solve. As temperature → 0, the chain concentrates at F*
//! (in principle — assuming the parameterisation spans the optimal
//! flow, which the cycle basis does).
//!
//! Compared against OR-Tools.

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
    fn unit(&mut self) -> f64 { (self.next_u64() as f64 / u64::MAX as f64).clamp(0.0, 1.0) }
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

/// BFS spanning tree from source. Returns `parent[v]` = parent vertex,
/// `parent_edge[v]` = (edge index, +1 if edge oriented parent→v else -1).
/// For root (source), `parent[source] = source`.
fn bfs_spanning_tree(g: &Graph) -> (Vec<usize>, Vec<(i32, i8)>) {
    let n = g.n;
    let mut parent: Vec<usize> = (0..n).collect();
    let mut parent_edge: Vec<(i32, i8)> = vec![(-1, 0); n];
    let mut adj: Vec<Vec<(usize, usize, i8)>> = vec![Vec::new(); n];
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        adj[u].push((v, i, 1));
        adj[v].push((u, i, -1));
    }
    let mut visited = vec![false; n];
    let mut queue = std::collections::VecDeque::new();
    queue.push_back(g.source);
    visited[g.source] = true;
    while let Some(u) = queue.pop_front() {
        for &(v, e_idx, sign) in &adj[u] {
            if !visited[v] {
                visited[v] = true;
                parent[v] = u;
                parent_edge[v] = (e_idx as i32, sign);
                queue.push_back(v);
            }
        }
    }
    (parent, parent_edge)
}

/// Trace tree path from `from` up to `to` (assumes `to` is an ancestor).
/// Returns list of (edge_index, signed_orientation: +1 if path goes
/// from→to in the canonical edge direction).
fn tree_path(parent: &[usize], parent_edge: &[(i32, i8)], from: usize, to: usize) -> Vec<(usize, i8)> {
    // Find LCA by climbing both sides.
    // Simpler: build ancestor lists.
    let mut anc_from: Vec<usize> = vec![from];
    let mut cur = from;
    while cur != parent[cur] {
        cur = parent[cur];
        anc_from.push(cur);
    }
    let mut anc_to: Vec<usize> = vec![to];
    cur = to;
    while cur != parent[cur] {
        cur = parent[cur];
        anc_to.push(cur);
    }
    // LCA = first common.
    let set_to: std::collections::HashSet<usize> = anc_to.iter().cloned().collect();
    let lca = anc_from.iter().find(|x| set_to.contains(x)).copied().unwrap();

    let mut path: Vec<(usize, i8)> = Vec::new();
    // From `from` up to lca (path direction: child → parent on each step,
    // i.e., we traverse the parent edge in reverse).
    let mut cur = from;
    while cur != lca {
        let (e, s) = parent_edge[cur];
        // parent_edge[cur] has sign +1 if (parent → cur) is the canonical (u,v).
        // We're going cur → parent, so flip sign.
        path.push((e as usize, -s));
        cur = parent[cur];
    }
    // From lca down to `to`.
    let mut down: Vec<(usize, i8)> = Vec::new();
    let mut cur = to;
    while cur != lca {
        let (e, s) = parent_edge[cur];
        down.push((e as usize, s));
        cur = parent[cur];
    }
    down.reverse();
    path.extend(down);
    path
}

/// Build cycle basis: one cycle per non-tree edge, formed by the
/// non-tree edge plus the tree path between its endpoints.
fn cycle_basis(g: &Graph) -> (Vec<Vec<(usize, i8)>>, Vec<bool>) {
    let (parent, parent_edge) = bfs_spanning_tree(g);
    let mut is_tree_edge = vec![false; g.edges.len()];
    for v in 0..g.n {
        if v == g.source { continue; }
        let (e, _) = parent_edge[v];
        if e >= 0 { is_tree_edge[e as usize] = true; }
    }
    let mut cycles: Vec<Vec<(usize, i8)>> = Vec::new();
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        if is_tree_edge[i] { continue; }
        // Cycle: u→v via this non-tree edge (sign +1 since canonical u<v),
        // then v→u via tree path.
        let mut cyc = vec![(i, 1_i8)];
        let return_path = tree_path(&parent, &parent_edge, v, u);
        cyc.extend(return_path);
        cycles.push(cyc);
    }
    (cycles, is_tree_edge)
}

/// Build f from state: f = F · st_path_indicator + Σ c_i · cycle_i.
/// All contributions are signed per edge.
fn build_flow(
    m: usize,
    f_value: f64,
    cycles: &[Vec<(usize, i8)>],
    cycle_coefs: &[f64],
    st_path: &[(usize, i8)],
) -> Vec<f64> {
    let mut f = vec![0.0_f64; m];
    for &(e, s) in st_path {
        f[e] += f_value * (s as f64);
    }
    for (i, cyc) in cycles.iter().enumerate() {
        let c = cycle_coefs[i];
        if c == 0.0 { continue; }
        for &(e, s) in cyc {
            f[e] += c * (s as f64);
        }
    }
    f
}

/// Smooth barrier on capacity: -log(1 - (f/c)^2). Penalises |f|/c → 1.
/// Returns (barrier_total, max_cong).
fn barrier(f: &[f64], caps: &[f64]) -> (f64, f64) {
    let mut total = 0.0_f64;
    let mut max_cong = 0.0_f64;
    for i in 0..f.len() {
        let r = f[i] / caps[i];
        let r2 = r * r;
        if r2 >= 1.0 { return (f64::INFINITY, r.abs()); }
        total -= (1.0 - r2).ln();
        if r.abs() > max_cong { max_cong = r.abs(); }
    }
    (total, max_cong)
}

#[derive(Clone)]
struct Result {
    f_best: f64,
    accepted: usize,
    proposed: usize,
    time_ms: f64,
}

fn sample_flow(g: &Graph, n_iters: usize, beta_start: f64, beta_end: f64, mu_start: f64, mu_end: f64,
               step_f: f64, step_c: f64, seed: u64) -> Result {
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    let (cycles, _is_tree) = cycle_basis(g);
    let n_cycles = cycles.len();
    let (parent, parent_edge) = bfs_spanning_tree(g);
    let st_path = tree_path(&parent, &parent_edge, g.source, g.sink);

    // State.
    let mut f_value: f64 = 0.0;
    let mut cycle_coefs: Vec<f64> = vec![0.0; n_cycles];
    let mut f_curr = build_flow(m, f_value, &cycles, &cycle_coefs, &st_path);
    let (mut bar_curr, mut max_cong_curr) = barrier(&f_curr, &caps);
    let mut energy_curr = -f_value + mu_start * bar_curr;
    let mut f_best = if max_cong_curr <= 1.0 { f_value } else { 0.0 };
    let mut accepted = 0_usize;
    let mut proposed = 0_usize;

    let mut rng = Xs256::new(seed ^ 0xCAFE);
    let t0 = Instant::now();

    for k in 0..n_iters {
        let frac = k as f64 / n_iters as f64;
        let beta = beta_start * (beta_end / beta_start).powf(frac);
        let mu = mu_start * (mu_end / mu_start).powf(frac);

        // Propose: perturb either F or one random cycle coefficient.
        let propose_F = rng.unit() < 0.5;
        let f_value_prop;
        let mut cycle_coefs_prop = cycle_coefs.clone();
        if propose_F {
            f_value_prop = f_value + step_f * rng.normal();
            if f_value_prop < 0.0 { continue; }
        } else {
            f_value_prop = f_value;
            let i = rng.gen_range(0, n_cycles as u64) as usize;
            cycle_coefs_prop[i] = cycle_coefs[i] + step_c * rng.normal();
        }
        let f_prop = build_flow(m, f_value_prop, &cycles, &cycle_coefs_prop, &st_path);
        let (bar_prop, max_cong_prop) = barrier(&f_prop, &caps);
        if !bar_prop.is_finite() { continue; }
        let energy_prop = -f_value_prop + mu * bar_prop;

        proposed += 1;
        let log_alpha = -beta * (energy_prop - energy_curr);
        let accept = if log_alpha >= 0.0 { true } else { rng.unit().ln() < log_alpha };
        if accept {
            f_value = f_value_prop;
            cycle_coefs = cycle_coefs_prop;
            f_curr = f_prop;
            bar_curr = bar_prop;
            energy_curr = energy_prop;
            max_cong_curr = max_cong_prop;
            accepted += 1;
        }
        let _ = bar_curr;
        if max_cong_curr <= 1.0 && f_value > f_best {
            f_best = f_value;
        }
    }
    let _ = f_curr;
    Result {
        f_best,
        accepted,
        proposed,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(30);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(80);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(200000);
    let beta_start: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-start=")?.parse().ok()).unwrap_or(0.001);
    let beta_end: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-end=")?.parse().ok()).unwrap_or(10.0);
    let mu_start: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-start=")?.parse().ok()).unwrap_or(1.0);
    let mu_end: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-end=")?.parse().ok()).unwrap_or(0.001);
    let step_f: f64 = args.iter().find_map(|a| a.strip_prefix("--step-f=")?.parse().ok()).unwrap_or(2.0);
    let step_c: f64 = args.iter().find_map(|a| a.strip_prefix("--step-c=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    let n_cycles = m - (g.n - 1);
    println!("# n={n}, m={m}, cycles in basis = {n_cycles}, T={n_iters}, β=[{beta_start}, {beta_end}], μ=[{mu_start}, {mu_end}]");
    println!("# source={}, sink={}, step_f={step_f}, step_c={step_c}", g.source, g.sink);

    let res = sample_flow(&g, n_iters, beta_start, beta_end, mu_start, mu_end, step_f, step_c, seed);
    println!("F_best (sampled in flow space) = {:.4}", res.f_best);
    println!("accepted: {}/{} ({:.1}%), time: {:.1} ms", res.accepted, res.proposed,
        100.0 * res.accepted as f64 / res.proposed as f64, res.time_ms);

    let tmp = std::env::temp_dir().join(format!("flowsamp_{seed}.max"));
    write_dimacs(&g, &tmp).unwrap();
    if let Some((f_or, us_or)) = run_ortools(driver, &tmp) {
        let f_or = f_or as f64;
        let rel = (res.f_best - f_or).abs() / f_or.max(1.0);
        println!();
        println!("OR-Tools F* = {:.0} ({:.3} ms)", f_or, us_or as f64 / 1000.0);
        println!("rel err vs OR-Tools = {:.4}%  pass = {}", rel * 100.0, rel <= 0.05);
    }
    let _ = std::fs::remove_file(&tmp);
}
