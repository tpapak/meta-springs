//! Hamiltonian Monte Carlo on cycle-basis flow space.
//!
//! Same parameterisation and energy as `cycle_basis_gd.rs` and
//! `sampling_flow_mc.rs`. The state space is `(F_i, c_{i,j})` over a
//! shared spanning-tree cycle basis.
//!
//! Hamiltonian:
//!     H(q, p) = U(q) + (1/2) ‖p‖²
//!     U(q)    = −β · Σ F_i + μ · Σ_e barrier(Σ_i √(f_i,e² + ε)/c_e)
//!
//! Leapfrog integrator with `L` steps of size `ε_lf`:
//!     p ← p − (ε_lf/2) ∇U(q)
//!     q ← q + ε_lf · p          [repeat L times]
//!     p ← p − (ε_lf/2) ∇U(q)
//! Then Metropolis on `exp(−ΔH)`.
//!
//! Compared to plain Metropolis (random walk in q-space), HMC's momentum
//! lets the chain follow gradient-aligned trajectories, traversing
//! ridges/saddles that diffusive Metropolis can't cross. Mixing time
//! typically `O(d^{1/4})` instead of `O(d²)` for `d`-dim state.

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
    q.push_back(root); visited[root] = true;
    while let Some(u) = q.pop_front() {
        for &(v, e_idx, sign) in &adj[u] {
            if !visited[v] {
                visited[v] = true;
                parent[v] = u; parent_edge[v] = (e_idx as i32, sign);
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
    m: usize, f_value: f64, cycles: &[Vec<(usize, i8)>],
    cycle_coefs: &[f64], st_path: &[(usize, i8)],
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

/// Energy U(q) plus per-edge bar_grad (∂barrier_per_edge/∂L_e).
fn energy_and_aux(
    f_per_edge: &[Vec<f64>],
    f_value: &[f64],
    caps: &[f64],
    beta: f64,
    mu: f64,
) -> (f64, Vec<f64>) {
    let m = caps.len();
    let k = f_per_edge.len();
    let mut bar_total = 0.0_f64;
    let mut bar_grad = vec![0.0_f64; m];
    let mut feasible = true;
    for i in 0..m {
        let mut l = 0.0_f64;
        for ci in 0..k { l += smooth_abs(f_per_edge[ci][i]); }
        let r = l / caps[i];
        let r2 = r * r;
        if r2 >= 1.0 - 1e-12 { feasible = false; bar_total = f64::INFINITY; break; }
        bar_total -= (1.0 - r2).ln();
        bar_grad[i] = 2.0 * l / (caps[i] * caps[i] - l * l);
    }
    let total_F: f64 = f_value.iter().sum();
    let u = if feasible { -beta * total_F + mu * bar_total } else { f64::INFINITY };
    (u, bar_grad)
}

/// Gradients ∂U/∂F_i and ∂U/∂c_{i,j} stacked in the order
/// [F_0, F_1, ..., F_{K-1}, c_{0,0}, ..., c_{0,nc-1}, c_{1,0}, ...].
fn gradient_stacked(
    g: &Graph,
    f_per_edge: &[Vec<f64>],
    bar_grad: &[f64],
    beta: f64,
    mu: f64,
    cycles: &[Vec<(usize, i8)>],
    st_paths: &[Vec<(usize, i8)>],
    nc: usize,
) -> Vec<f64> {
    let m = g.edges.len();
    let k = g.commodities.len();
    let dim = k * (1 + nc);
    let mut out = vec![0.0_f64; dim];

    // Per-edge per-commodity scalar μ · bar_grad[e] · sign(f).
    let mut per: Vec<Vec<f64>> = vec![vec![0.0; m]; k];
    for ci in 0..k {
        for i in 0..m {
            per[ci][i] = mu * bar_grad[i] * smooth_abs_grad(f_per_edge[ci][i]);
        }
    }
    // F_i partials.
    for ci in 0..k {
        let mut s = -beta;
        for &(e, sg) in &st_paths[ci] {
            s += per[ci][e] * (sg as f64);
        }
        out[ci] = s;
    }
    // c_{i,j} partials.
    for ci in 0..k {
        for (j, cyc) in cycles.iter().enumerate() {
            let mut s = 0.0_f64;
            for &(e, sg) in cyc { s += per[ci][e] * (sg as f64); }
            out[k + ci * nc + j] = s;
        }
    }
    out
}

/// Unpack stacked q into (F, cycle_coefs).
fn unpack(q: &[f64], k: usize, nc: usize) -> (Vec<f64>, Vec<Vec<f64>>) {
    let f_value: Vec<f64> = (0..k).map(|i| q[i]).collect();
    let mut cycle_coefs: Vec<Vec<f64>> = Vec::with_capacity(k);
    for ci in 0..k {
        let mut row = vec![0.0; nc];
        for j in 0..nc { row[j] = q[k + ci * nc + j]; }
        cycle_coefs.push(row);
    }
    (f_value, cycle_coefs)
}

/// Build all per-commodity flows.
fn build_all(
    m: usize, k: usize,
    f_value: &[f64], cycle_coefs: &[Vec<f64>],
    cycles: &[Vec<(usize, i8)>], st_paths: &[Vec<(usize, i8)>],
) -> Vec<Vec<f64>> {
    (0..k).map(|ci| build_commodity_flow(m, f_value[ci], cycles, &cycle_coefs[ci], &st_paths[ci])).collect()
}

#[derive(Clone)]
struct HmcResult {
    f_per: Vec<f64>,
    total: f64,
    accepted: usize,
    proposed: usize,
    time_ms: f64,
}

fn hmc_maxflow(
    g: &Graph,
    n_iters: usize,
    leapfrog_L: usize,
    eps_lf: f64,
    beta_start: f64, beta_end: f64,
    mu_start: f64, mu_end: f64,
    seed: u64,
) -> HmcResult {
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let (parent, parent_edge) = bfs_spanning_tree(g, 0);
    let cycles = cycle_basis(g, &parent, &parent_edge);
    let nc = cycles.len();
    let st_paths: Vec<Vec<(usize, i8)>> = g.commodities.iter()
        .map(|&(s, t)| tree_path(&parent, &parent_edge, s, t)).collect();
    let dim = k * (1 + nc);

    let mut q = vec![0.0_f64; dim];
    for ci in 0..k { q[ci] = 0.1; } // small initial F_i
    let (mut f_value, mut cycle_coefs) = unpack(&q, k, nc);
    let mut f_per_edge = build_all(m, k, &f_value, &cycle_coefs, &cycles, &st_paths);
    let (mut u_curr, _) = energy_and_aux(&f_per_edge, &f_value, &caps, beta_start, mu_start);

    let mut rng = Xs256::new(seed ^ 0xCAFE_F00D);
    let mut best_total = 0.0_f64;
    let mut best_f_per: Vec<f64> = vec![0.0; k];
    let mut accepted = 0_usize;
    let mut proposed = 0_usize;
    let t0 = Instant::now();

    for k_outer in 0..n_iters {
        let frac = k_outer as f64 / n_iters as f64;
        let beta = beta_start * (beta_end / beta_start).powf(frac);
        let mu = mu_start * (mu_end / mu_start).powf(frac);

        // Sample fresh momentum p ~ N(0, I).
        let mut p = vec![0.0_f64; dim];
        for i in 0..dim { p[i] = rng.normal(); }
        let kin_curr: f64 = 0.5 * p.iter().map(|x| x * x).sum::<f64>();

        // Leapfrog from current q.
        let mut q_new = q.clone();
        let mut p_new = p.clone();
        // Initial half step on momentum.
        let (_, bg) = energy_and_aux(&f_per_edge, &f_value, &caps, beta, mu);
        let mut grad = gradient_stacked(g, &f_per_edge, &bg, beta, mu, &cycles, &st_paths, nc);
        for i in 0..dim { p_new[i] -= 0.5 * eps_lf * grad[i]; }
        let mut diverged = false;
        for _step in 0..leapfrog_L {
            for i in 0..dim { q_new[i] += eps_lf * p_new[i]; }
            // Re-evaluate gradient at new q.
            let (fv_new, cc_new) = unpack(&q_new, k, nc);
            // Project F_i to non-negative (reflect momentum if hit boundary).
            let mut fv_proj = fv_new.clone();
            let mut q_proj = q_new.clone();
            for ci in 0..k {
                if fv_proj[ci] < 0.0 {
                    fv_proj[ci] = -fv_proj[ci];
                    q_proj[ci] = -q_new[ci];
                    p_new[ci] = -p_new[ci];
                }
            }
            q_new = q_proj;
            let f_per_new = build_all(m, k, &fv_proj, &cc_new, &cycles, &st_paths);
            let (u_chk, bg_new) = energy_and_aux(&f_per_new, &fv_proj, &caps, beta, mu);
            if !u_chk.is_finite() { diverged = true; break; }
            grad = gradient_stacked(g, &f_per_new, &bg_new, beta, mu, &cycles, &st_paths, nc);
            for i in 0..dim { p_new[i] -= eps_lf * grad[i]; }
        }
        if diverged { proposed += 1; continue; }
        // Final half step on momentum (undo the last full step).
        for i in 0..dim { p_new[i] += 0.5 * eps_lf * grad[i]; }

        // Compute new H.
        let (fv_final, cc_final) = unpack(&q_new, k, nc);
        let f_per_final = build_all(m, k, &fv_final, &cc_final, &cycles, &st_paths);
        let (u_new, _) = energy_and_aux(&f_per_final, &fv_final, &caps, beta, mu);
        if !u_new.is_finite() { proposed += 1; continue; }
        let kin_new: f64 = 0.5 * p_new.iter().map(|x| x * x).sum::<f64>();
        let dh = (u_new + kin_new) - (u_curr + kin_curr);
        proposed += 1;
        let accept = if dh <= 0.0 { true } else { rng.unit() < (-dh).exp() };
        if accept {
            q = q_new;
            f_value = fv_final;
            cycle_coefs = cc_final;
            f_per_edge = f_per_final;
            u_curr = u_new;
            accepted += 1;
        }
        let _ = (cycle_coefs.len(), f_value.len()); // keep silenced

        // Track best feasible.
        let mut max_cong = 0.0_f64;
        for i in 0..m {
            let l: f64 = (0..k).map(|ci| f_per_edge[ci][i].abs()).sum();
            let cong = l / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        let total: f64 = f_value.iter().sum();
        if max_cong <= 1.0 && total > best_total {
            best_total = total;
            best_f_per = f_value.clone();
        }
    }

    HmcResult {
        f_per: best_f_per, total: best_total, accepted, proposed,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(10000);
    let leap: usize = args.iter().find_map(|a| a.strip_prefix("--L=")?.parse().ok()).unwrap_or(20);
    let eps_lf: f64 = args.iter().find_map(|a| a.strip_prefix("--eps=")?.parse().ok()).unwrap_or(0.05);
    let beta_start: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-start=")?.parse().ok()).unwrap_or(1.0);
    let beta_end: f64 = args.iter().find_map(|a| a.strip_prefix("--beta-end=")?.parse().ok()).unwrap_or(1.0);
    let mu_start: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-start=")?.parse().ok()).unwrap_or(1.0);
    let mu_end: f64 = args.iter().find_map(|a| a.strip_prefix("--mu-end=")?.parse().ok()).unwrap_or(0.001);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    println!("# n={n}, m={m}, K={k}, T={n_iters}, L={leap}, eps_lf={eps_lf}, μ=[{mu_start}, {mu_end}], β=[{beta_start}, {beta_end}]");

    let res = hmc_maxflow(&g, n_iters, leap, eps_lf, beta_start, beta_end, mu_start, mu_end, seed);
    println!("F_per = {:?}", res.f_per.iter().map(|x| format!("{:.3}", x)).collect::<Vec<_>>());
    println!("total = {:.4}", res.total);
    println!("accepted: {}/{} ({:.1}%), time: {:.1} ms",
        res.accepted, res.proposed, 100.0 * res.accepted as f64 / res.proposed as f64, res.time_ms);

    // LP oracle.
    let tmp = std::env::temp_dir().join(format!("hmc_{seed}.json"));
    write_lp_input(&g, &tmp).unwrap();
    let oracle = std::path::PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("examples").join("mc_lp_oracle.py");
    if let Ok(out) = Command::new("python3").arg(&oracle).arg(&tmp).output() {
        if out.status.success() {
            let s = String::from_utf8_lossy(&out.stdout);
            println!();
            println!("LP: {}", s.trim());
            if let Some(start) = s.find("\"total\":") {
                let rest = &s[start + 8..];
                let end = rest.find(|c: char| c == ',' || c == '}').unwrap_or(rest.len());
                if let Ok(lp_total) = rest[..end].trim().parse::<f64>() {
                    let rel = (res.total - lp_total).abs() / lp_total.max(1.0);
                    println!("rel err = {:.4}%, pass(≤5%) = {}", rel * 100.0, rel <= 0.05);
                }
            }
        }
    }
    let _ = std::fs::remove_file(&tmp);
}
