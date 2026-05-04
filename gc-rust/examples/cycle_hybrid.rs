//! Hybrid: cycle-basis gradient descent for warm-start, then Metropolis
//! sampling for refinement / escaping local minima.
//!
//! Step 1 (deterministic): GD with annealed barrier μ from large to
//! small. Converges quickly to a local optimum (great when convex,
//! often suboptimal at K≥2 due to multi-commodity coupling).
//!
//! Step 2 (stochastic): Metropolis-Hastings on `(F_i, c_{i,j})` starting
//! from GD solution. Random walk explores nearby basins; if GD got stuck
//! in a local optimum, MCMC can find better ones. Anneals temperature
//! and barrier μ similarly.
//!
//! Compared to pure sampling (which wastes huge time exploring far from
//! optimum) and pure GD (which gets stuck at K≥2), hybrid should hit
//! near-LP-optimum at every K with much less compute than pure sampling.

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
        adj[u].push((v, i, 1)); adj[v].push((u, i, -1));
    }
    let mut visited = vec![false; n];
    let mut q = std::collections::VecDeque::new();
    q.push_back(root); visited[root] = true;
    while let Some(u) = q.pop_front() {
        for &(v, e_idx, sign) in &adj[u] {
            if !visited[v] { visited[v] = true; parent[v] = u; parent_edge[v] = (e_idx as i32, sign); q.push_back(v); }
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
    down.reverse(); path.extend(down); path
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

fn energy_smooth(
    f_per_edge: &[Vec<f64>], f_value: &[f64], caps: &[f64], mu: f64,
) -> (f64, Vec<f64>) {
    let m = caps.len(); let k = f_per_edge.len();
    let mut bar_total = 0.0_f64;
    let mut bar_grad = vec![0.0_f64; m];
    let mut feas = true;
    for i in 0..m {
        let mut l = 0.0_f64;
        for ci in 0..k { l += smooth_abs(f_per_edge[ci][i]); }
        let r = l / caps[i]; let r2 = r * r;
        if r2 >= 1.0 - 1e-12 { feas = false; break; }
        bar_total -= (1.0 - r2).ln();
        bar_grad[i] = 2.0 * l / (caps[i] * caps[i] - l * l);
    }
    let total_F: f64 = f_value.iter().sum();
    let e = if feas { -total_F + mu * bar_total } else { f64::INFINITY };
    (e, bar_grad)
}
fn capacity_barrier_hard(f_per_edge: &[Vec<f64>], caps: &[f64]) -> (f64, f64) {
    // L¹ load (true) and max congestion based on actual |f|.
    let m = caps.len(); let k = f_per_edge.len();
    let mut tot = 0.0_f64; let mut max_cong = 0.0_f64;
    for i in 0..m {
        let mut l = 0.0_f64;
        for ci in 0..k { l += f_per_edge[ci][i].abs(); }
        let r = l / caps[i]; let r2 = r * r;
        if r2 >= 1.0 { tot = f64::INFINITY; max_cong = r; return (tot, max_cong); }
        tot -= (1.0 - r2).ln();
        if r > max_cong { max_cong = r; }
    }
    (tot, max_cong)
}

/// GD warm-start: run for given μ schedule with backtracking line search.
fn gd_phase(
    g: &Graph,
    f_value: &mut Vec<f64>,
    cycle_coefs: &mut Vec<Vec<f64>>,
    cycles: &[Vec<(usize, i8)>],
    st_paths: &[Vec<(usize, i8)>],
    mu_schedule: &[f64],
    inner_iters: usize,
    init_step: f64,
) -> (Vec<Vec<f64>>, usize) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let nc = cycles.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f_per_edge: Vec<Vec<f64>> = (0..k)
        .map(|ci| build_commodity_flow(m, f_value[ci], cycles, &cycle_coefs[ci], &st_paths[ci])).collect();
    let mut total_iters = 0;

    for &mu in mu_schedule {
        let mut step = init_step;
        for _ in 0..inner_iters {
            total_iters += 1;
            let (e_curr, bar_grad) = energy_smooth(&f_per_edge, f_value, &caps, mu);
            if !e_curr.is_finite() { break; }

            // Compute gradient.
            let mut per: Vec<Vec<f64>> = vec![vec![0.0; m]; k];
            for ci in 0..k { for i in 0..m {
                per[ci][i] = mu * bar_grad[i] * smooth_abs_grad(f_per_edge[ci][i]);
            }}
            let mut gF = vec![-1.0_f64; k];
            for ci in 0..k { for &(e, s) in &st_paths[ci] { gF[ci] += per[ci][e] * (s as f64); } }
            let mut gC = vec![vec![0.0_f64; nc]; k];
            for ci in 0..k { for (j, cyc) in cycles.iter().enumerate() {
                let mut s = 0.0_f64;
                for &(e, sg) in cyc { s += per[ci][e] * (sg as f64); }
                gC[ci][j] = s;
            }}

            let mut accepted = false;
            for _ls in 0..30 {
                let mut fv2 = f_value.clone();
                let mut cc2 = cycle_coefs.clone();
                for ci in 0..k {
                    fv2[ci] -= step * gF[ci];
                    if fv2[ci] < 0.0 { fv2[ci] = 0.0; }
                    for j in 0..nc { cc2[ci][j] -= step * gC[ci][j]; }
                }
                let f_per_new: Vec<Vec<f64>> = (0..k).map(|ci|
                    build_commodity_flow(m, fv2[ci], cycles, &cc2[ci], &st_paths[ci])).collect();
                let (e_new, _) = energy_smooth(&f_per_new, &fv2, &caps, mu);
                if e_new.is_finite() && e_new < e_curr {
                    *f_value = fv2;
                    *cycle_coefs = cc2;
                    f_per_edge = f_per_new;
                    accepted = true;
                    if e_curr - e_new < 1e-9 { break; }
                    step = (step * 1.2).min(1e3);
                    break;
                }
                step *= 0.5;
                if step < 1e-12 { break; }
            }
            if !accepted { break; }
        }
    }
    (f_per_edge, total_iters)
}

/// MCMC refinement starting from given state. Tracks best feasible.
fn mcmc_phase(
    g: &Graph,
    f_value: &mut Vec<f64>,
    cycle_coefs: &mut Vec<Vec<f64>>,
    f_per_edge_in: Vec<Vec<f64>>,
    cycles: &[Vec<(usize, i8)>],
    st_paths: &[Vec<(usize, i8)>],
    n_iters: usize,
    beta_start: f64, beta_end: f64,
    mu_start: f64, mu_end: f64,
    step_f: f64, step_c: f64,
    seed: u64,
) -> (Vec<f64>, f64, usize, usize) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let nc = cycles.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f_per_edge = f_per_edge_in;

    let (_, mut max_cong_curr) = capacity_barrier_hard(&f_per_edge, &caps);
    let mut F_total: f64 = f_value.iter().sum();
    let mut best_F: Vec<f64> = if max_cong_curr <= 1.0 { f_value.clone() } else { vec![0.0; k] };
    let mut best_total = if max_cong_curr <= 1.0 { F_total } else { 0.0 };
    let (mut bar_curr, _) = energy_smooth(&f_per_edge, f_value, &caps, mu_start);
    let mut energy_curr = -F_total + mu_start * bar_curr;

    let mut rng = Xs256::new(seed ^ 0xBABE_F00D);
    let mut accepted = 0_usize;
    let mut proposed = 0_usize;

    for it in 0..n_iters {
        let frac = it as f64 / n_iters as f64;
        let beta = beta_start * (beta_end / beta_start).powf(frac);
        let mu = mu_start * (mu_end / mu_start).powf(frac);

        let move_type = rng.unit();
        let mut f_value_prop = f_value.clone();
        let mut cc_prop = cycle_coefs.clone();
        let mut affected: Vec<usize> = Vec::new();
        if k == 1 || move_type < 0.5 {
            let ci = rng.gen_range(0, k as u64) as usize;
            let new_F = f_value[ci] + step_f * rng.normal();
            if new_F < 0.0 { continue; }
            f_value_prop[ci] = new_F;
            affected.push(ci);
        } else {
            let ci = rng.gen_range(0, k as u64) as usize;
            let j = rng.gen_range(0, nc as u64) as usize;
            cc_prop[ci][j] = cycle_coefs[ci][j] + step_c * rng.normal();
            affected.push(ci);
        }
        let mut f_per_prop = f_per_edge.clone();
        for &c in &affected {
            f_per_prop[c] = build_commodity_flow(m, f_value_prop[c], cycles, &cc_prop[c], &st_paths[c]);
        }
        let (bar_prop, _) = energy_smooth(&f_per_prop, &f_value_prop, &caps, mu);
        if !bar_prop.is_finite() { continue; }
        let F_total_prop: f64 = f_value_prop.iter().sum();
        let energy_prop = -F_total_prop + mu * bar_prop;
        proposed += 1;
        let log_alpha = -beta * (energy_prop - energy_curr);
        let accept = if log_alpha >= 0.0 { true } else { rng.unit().ln() < log_alpha };
        if accept {
            *f_value = f_value_prop;
            for &c in &affected { cycle_coefs[c] = cc_prop[c].clone(); }
            f_per_edge = f_per_prop;
            bar_curr = bar_prop;
            F_total = F_total_prop;
            energy_curr = energy_prop;
            let (_, mc) = capacity_barrier_hard(&f_per_edge, &caps);
            max_cong_curr = mc;
            accepted += 1;
        }
        let _ = bar_curr;
        if max_cong_curr <= 1.0 && F_total > best_total {
            best_total = F_total;
            best_F = f_value.clone();
        }
    }
    (best_F, best_total, accepted, proposed)
}

#[derive(Clone)]
struct HybResult {
    f_per_after_gd: Vec<f64>,
    total_after_gd: f64,
    f_per_after_mcmc: Vec<f64>,
    total_after_mcmc: f64,
    gd_ms: f64,
    mcmc_ms: f64,
    mcmc_accepted: usize,
    mcmc_proposed: usize,
}

fn hybrid(
    g: &Graph, gd_inner: usize, mcmc_iters: usize, step_f: f64, step_c: f64, seed: u64,
) -> HybResult {
    let (parent, parent_edge) = bfs_spanning_tree(g, 0);
    let cycles = cycle_basis(g, &parent, &parent_edge);
    let nc = cycles.len();
    let st_paths: Vec<Vec<(usize, i8)>> = g.commodities.iter()
        .map(|&(s, t)| tree_path(&parent, &parent_edge, s, t)).collect();
    let k = g.commodities.len();
    let mut f_value: Vec<f64> = vec![0.1; k];
    let mut cycle_coefs: Vec<Vec<f64>> = vec![vec![0.0; nc]; k];

    // Phase 1: GD.
    let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001];
    let t0 = Instant::now();
    let (f_per_edge, _gd_iters) = gd_phase(
        g, &mut f_value, &mut cycle_coefs, &cycles, &st_paths,
        &mu_schedule, gd_inner, 0.01,
    );
    let gd_ms = t0.elapsed().as_secs_f64() * 1000.0;
    let total_after_gd: f64 = f_value.iter().sum();
    let f_per_after_gd = f_value.clone();

    // Phase 2: MCMC starting from GD output.
    let t1 = Instant::now();
    let (best_F, best_total, acc, prop) = mcmc_phase(
        g, &mut f_value, &mut cycle_coefs, f_per_edge,
        &cycles, &st_paths,
        mcmc_iters,
        0.01, 5.0,            // beta schedule
        0.001, 0.0001,        // μ schedule (already small after GD)
        step_f, step_c, seed,
    );
    let mcmc_ms = t1.elapsed().as_secs_f64() * 1000.0;

    HybResult {
        f_per_after_gd, total_after_gd,
        f_per_after_mcmc: best_F, total_after_mcmc: best_total,
        gd_ms, mcmc_ms, mcmc_accepted: acc, mcmc_proposed: prop,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let gd_inner: usize = args.iter().find_map(|a| a.strip_prefix("--gd-inner=")?.parse().ok()).unwrap_or(2000);
    let mcmc_iters: usize = args.iter().find_map(|a| a.strip_prefix("--mcmc=")?.parse().ok()).unwrap_or(2000000);
    let step_f: f64 = args.iter().find_map(|a| a.strip_prefix("--step-f=")?.parse().ok()).unwrap_or(5.0);
    let step_c: f64 = args.iter().find_map(|a| a.strip_prefix("--step-c=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    let m = g.edges.len();
    println!("# n={n}, m={m}, K={k}, gd_inner={gd_inner}, mcmc_iters={mcmc_iters}, seed={seed}");

    let res = hybrid(&g, gd_inner, mcmc_iters, step_f, step_c, seed);
    println!("After GD: total = {:.4} ({:.1} ms), F_per = {:?}",
        res.total_after_gd, res.gd_ms,
        res.f_per_after_gd.iter().map(|x| format!("{:.2}", x)).collect::<Vec<_>>());
    println!("After MCMC: total = {:.4} ({:.1} ms), accepted {}/{} ({:.1}%), F_per = {:?}",
        res.total_after_mcmc, res.mcmc_ms,
        res.mcmc_accepted, res.mcmc_proposed,
        100.0 * res.mcmc_accepted as f64 / res.mcmc_proposed as f64,
        res.f_per_after_mcmc.iter().map(|x| format!("{:.2}", x)).collect::<Vec<_>>());

    let tmp = std::env::temp_dir().join(format!("hyb_{seed}.json"));
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
                    let r_gd = (res.total_after_gd - lp_total).abs() / lp_total.max(1.0);
                    let r_h = (res.total_after_mcmc - lp_total).abs() / lp_total.max(1.0);
                    println!("rel err — GD only: {:.4}%, GD+MCMC: {:.4}%",
                        r_gd * 100.0, r_h * 100.0);
                }
            }
        }
    }
    let _ = std::fs::remove_file(&tmp);
}
