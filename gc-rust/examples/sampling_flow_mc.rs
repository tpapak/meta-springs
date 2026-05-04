//! Multi-commodity flow-space sampler.
//!
//! State per commodity i: `(F_i, c_{i,1}, ..., c_{i,n_cycles})` —
//! magnitude of i-th s_i→t_i augmenting path through a shared
//! spanning tree, plus circulation coefficients along the shared
//! cycle basis. Each commodity's flow is automatically divergence-free.
//!
//! Energy: `E = −β·Σ_i F_i + μ·Σ_e barrier(Σ_i |f_i,e|/c_e)` —
//! commodities couple through the shared-capacity barrier per edge.
//! Smooth barrier `−log(1 − x²)`.
//!
//! Each Metropolis step picks one (commodity, F or cycle), perturbs
//! it, recomputes that commodity's flow, evaluates the joint capacity
//! barrier on the total load.
//!
//! Verified against scipy LP oracle.

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
    let mut cycles: Vec<Vec<(usize, i8)>> = Vec::new();
    for (i, &(u, v, _)) in g.edges.iter().enumerate() {
        if is_tree_edge[i] { continue; }
        let mut cyc = vec![(i, 1_i8)];
        let return_path = tree_path(parent, parent_edge, v, u);
        cyc.extend(return_path);
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

/// Per-edge total absolute load `Σ_i |f_i,e|`. Returns (total per edge, max congestion).
fn total_loads(per: &[Vec<f64>], caps: &[f64]) -> (Vec<f64>, f64) {
    let m = caps.len();
    let mut total = vec![0.0_f64; m];
    let mut max_cong = 0.0_f64;
    for i in 0..m {
        for f_i in per { total[i] += f_i[i].abs(); }
        let cong = total[i] / caps[i];
        if cong > max_cong { max_cong = cong; }
    }
    (total, max_cong)
}

/// Smooth capacity barrier on TOTAL absolute load: −log(1 − (Σ|f|/c)²).
/// Returns +∞ if any edge over capacity.
fn capacity_barrier(loads: &[f64], caps: &[f64]) -> f64 {
    let mut tot = 0.0_f64;
    for i in 0..loads.len() {
        let r = loads[i] / caps[i];
        let r2 = r * r;
        if r2 >= 1.0 { return f64::INFINITY; }
        tot -= (1.0 - r2).ln();
    }
    tot
}

#[derive(Clone)]
struct McResult {
    f_per: Vec<f64>,
    total: f64,
    accepted: usize,
    proposed: usize,
    time_ms: f64,
}

fn sample_mc(g: &Graph, n_iters: usize, beta_start: f64, beta_end: f64, mu_start: f64, mu_end: f64,
             step_f: f64, step_c: f64, seed: u64) -> McResult {
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // Spanning tree: rooted at vertex 0.
    let (parent, parent_edge) = bfs_spanning_tree(g, 0);
    let cycles = cycle_basis(g, &parent, &parent_edge);
    let n_cycles = cycles.len();

    // Per-commodity s→t path.
    let st_paths: Vec<Vec<(usize, i8)>> = g.commodities.iter()
        .map(|&(s, t)| tree_path(&parent, &parent_edge, s, t)).collect();

    // State.
    let mut f_value: Vec<f64> = vec![0.0; k];
    let mut cycle_coefs: Vec<Vec<f64>> = vec![vec![0.0; n_cycles]; k];
    let mut f_per_edge: Vec<Vec<f64>> = (0..k)
        .map(|ci| build_commodity_flow(m, f_value[ci], &cycles, &cycle_coefs[ci], &st_paths[ci]))
        .collect();
    let (mut loads, mut max_cong) = total_loads(&f_per_edge, &caps);
    let mut bar_curr = capacity_barrier(&loads, &caps);
    let mut F_total: f64 = f_value.iter().sum();
    let mut energy_curr = -F_total + mu_start * bar_curr;
    let mut f_best = vec![0.0_f64; k];
    let mut total_best = 0.0_f64;
    if max_cong <= 1.0 { f_best.copy_from_slice(&f_value); total_best = F_total; }

    let mut rng = Xs256::new(seed ^ 0xCAFE_BABE);
    let t0 = Instant::now();
    let mut accepted = 0_usize;
    let mut proposed = 0_usize;

    for it in 0..n_iters {
        let frac = it as f64 / n_iters as f64;
        let beta = beta_start * (beta_end / beta_start).powf(frac);
        let mu = mu_start * (mu_end / mu_start).powf(frac);

        // Three move types:
        //   30% — single-commodity F perturbation
        //   30% — single-commodity cycle perturbation
        //   40% — joint move: trade F between two commodities (simultaneous +δ/−δ)
        //   When K=1, only single-commodity moves apply.
        let move_type = rng.unit();
        let mut f_value_prop = f_value.clone();
        let mut cycle_coefs_prop_per: Vec<Vec<f64>> = cycle_coefs.clone();
        let mut affected: Vec<usize> = Vec::new();

        if k == 1 || move_type < 0.3 {
            let ci = rng.gen_range(0, k as u64) as usize;
            let new_F = f_value[ci] + step_f * rng.normal();
            if new_F < 0.0 { continue; }
            f_value_prop[ci] = new_F;
            affected.push(ci);
        } else if move_type < 0.6 {
            let ci = rng.gen_range(0, k as u64) as usize;
            let j = rng.gen_range(0, n_cycles as u64) as usize;
            cycle_coefs_prop_per[ci][j] = cycle_coefs[ci][j] + step_c * rng.normal();
            affected.push(ci);
        } else {
            // Joint trade: pick two commodities, transfer δ from one to the other.
            let ci = rng.gen_range(0, k as u64) as usize;
            let mut cj = rng.gen_range(0, k as u64) as usize;
            if cj == ci { cj = (cj + 1) % k; }
            let delta = step_f * rng.normal();
            let new_i = f_value[ci] + delta;
            let new_j = f_value[cj] - delta;
            if new_i < 0.0 || new_j < 0.0 { continue; }
            f_value_prop[ci] = new_i;
            f_value_prop[cj] = new_j;
            affected.push(ci); affected.push(cj);
        }

        // Rebuild only the affected commodities' flows.
        let mut f_per_prop = f_per_edge.clone();
        for &c in &affected {
            f_per_prop[c] = build_commodity_flow(m, f_value_prop[c], &cycles, &cycle_coefs_prop_per[c], &st_paths[c]);
        }
        let (loads_prop, max_cong_prop) = total_loads(&f_per_prop, &caps);
        let bar_prop = capacity_barrier(&loads_prop, &caps);
        if !bar_prop.is_finite() { continue; }
        let F_total_prop: f64 = f_value_prop.iter().sum();
        let energy_prop = -F_total_prop + mu * bar_prop;

        proposed += 1;
        let log_alpha = -beta * (energy_prop - energy_curr);
        let accept = if log_alpha >= 0.0 { true } else { rng.unit().ln() < log_alpha };
        if accept {
            f_value = f_value_prop;
            for &c in &affected { cycle_coefs[c] = cycle_coefs_prop_per[c].clone(); }
            f_per_edge = f_per_prop;
            loads = loads_prop;
            max_cong = max_cong_prop;
            bar_curr = bar_prop;
            F_total = F_total_prop;
            energy_curr = energy_prop;
            accepted += 1;
        }
        let _ = bar_curr;
        if max_cong <= 1.0 && F_total > total_best {
            total_best = F_total;
            f_best.copy_from_slice(&f_value);
        }
    }

    McResult {
        f_per: f_best, total: total_best, accepted, proposed,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(500000);
    let beta_start: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-start=")?.parse().ok()).unwrap_or(0.001);
    let beta_end: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-end=")?.parse().ok()).unwrap_or(20.0);
    let mu_start: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-start=")?.parse().ok()).unwrap_or(1.0);
    let mu_end: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-end=")?.parse().ok()).unwrap_or(0.001);
    let step_f: f64 = args.iter().find_map(|a| a.strip_prefix("--step-f=")?.parse().ok()).unwrap_or(2.0);
    let step_c: f64 = args.iter().find_map(|a| a.strip_prefix("--step-c=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    let n_cycles = m - (g.n - 1);
    println!("# n={n}, m={m}, K={k}, cycles={n_cycles}, T={n_iters}, β=[{beta_start}, {beta_end}], μ=[{mu_start}, {mu_end}]");
    println!("# commodities: {:?}", g.commodities);

    let res = sample_mc(&g, n_iters, beta_start, beta_end, mu_start, mu_end, step_f, step_c, seed);
    println!("F_per = {:?}", res.f_per.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("total = {:.4}", res.total);
    println!("accepted: {}/{} ({:.1}%), time: {:.1} ms",
        res.accepted, res.proposed, 100.0 * res.accepted as f64 / res.proposed as f64, res.time_ms);

    // LP oracle.
    let tmp = std::env::temp_dir().join(format!("mcsamp_{seed}.json"));
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
