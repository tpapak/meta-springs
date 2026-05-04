//! Garg–Könemann MWU for fractional multi-commodity max-flow.
//!
//! Classic algorithm (Garg & Könemann 2007). At each iter:
//!   1. For each commodity i, find shortest s_i-t_i path under edge length
//!      ℓ_e = w_e / c_e (Dijkstra with positive weights).
//!   2. Pick the cheapest commodity (with shortest length); push max
//!      feasible flow Δ along that path (≤ min capacity along path).
//!   3. Update w_e ← w_e · exp(ε · Δ / c_e) for edges in path.
//!   4. Stop when D = Σ_e w_e c_e ≥ 1 (dual stopping criterion).
//!
//! Returns a (1−ε)-approximation. No LP, no Hessian — just K Dijkstras
//! per "round" of length-update. Scales to large K and m gracefully.

use std::cmp::Ordering;
use std::collections::BinaryHeap;
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
    let mut rng = Xs256::new(seed);
    let mut edges = Vec::with_capacity(target_e);
    let mut seen = std::collections::HashSet::with_capacity(target_e);
    let mut perm: Vec<usize> = (0..n).collect();
    for i in (1..n).rev() {
        let j = rng.gen_range(0, (i as u64) + 1) as usize;
        perm.swap(i, j);
    }
    for k_idx in 1..n {
        let u = perm[k_idx];
        let parent = perm[rng.gen_range(0, k_idx as u64) as usize];
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
    while commodities.len() < k {
        let s = rng.gen_range(0, n as u64) as usize;
        let t = rng.gen_range(0, n as u64) as usize;
        if s == t { continue; }
        commodities.push((s, t));
    }
    Graph { n, edges, commodities }
}

fn run_lp_oracle(g: &Graph) -> Option<(f64, Vec<f64>, f64)> {
    let json_in = serde_json::json!({
        "n": g.n,
        "edges": g.edges.iter().map(|&(u,v,c)| [u, v, c as usize]).collect::<Vec<_>>(),
        "commodities": g.commodities.iter().map(|&(s,t)| [s, t]).collect::<Vec<_>>(),
    });
    let tmp = std::env::temp_dir().join(format!("mc_lp_{}.json", std::process::id()));
    std::fs::write(&tmp, json_in.to_string()).ok()?;
    let t0 = Instant::now();
    let out = Command::new("python3")
        .arg("/Users/tosku/Sync/Documents/slmm/gc-rust/examples/mc_lp_oracle.py")
        .arg(&tmp)
        .output()
        .ok()?;
    let elapsed_ms = t0.elapsed().as_secs_f64() * 1000.0;
    let _ = std::fs::remove_file(&tmp);
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let v: serde_json::Value = serde_json::from_str(&s).ok()?;
    Some((
        v["total"].as_f64()?,
        v["F_per"].as_array()?.iter().map(|x| x.as_f64().unwrap_or(0.0)).collect(),
        elapsed_ms,
    ))
}

#[derive(Clone, Copy, PartialEq)]
struct DState { dist: f64, node: usize }
impl Eq for DState {}
impl Ord for DState {
    fn cmp(&self, other: &Self) -> Ordering {
        other.dist.partial_cmp(&self.dist).unwrap_or(Ordering::Equal)
            .then(self.node.cmp(&other.node))
    }
}
impl PartialOrd for DState { fn partial_cmp(&self, o: &Self) -> Option<Ordering> { Some(self.cmp(o)) } }

/// Dijkstra on undirected graph with positive edge lengths.
/// Returns (dist[v], pred_edge[v]) where pred_edge is the edge index used.
fn dijkstra(adj: &[Vec<(usize, usize)>], lengths: &[f64], src: usize, dst: usize, n: usize)
    -> Option<(f64, Vec<usize>)>
{
    let mut dist = vec![f64::INFINITY; n];
    let mut pred = vec![usize::MAX; n];
    dist[src] = 0.0;
    let mut heap = BinaryHeap::new();
    heap.push(DState { dist: 0.0, node: src });
    while let Some(DState { dist: d, node }) = heap.pop() {
        if node == dst { break; }
        if d > dist[node] + 1e-15 { continue; }
        for &(neigh, e_idx) in &adj[node] {
            let nd = d + lengths[e_idx];
            if nd < dist[neigh] {
                dist[neigh] = nd;
                pred[neigh] = e_idx;
                heap.push(DState { dist: nd, node: neigh });
            }
        }
    }
    if dist[dst].is_infinite() { return None; }
    // Reconstruct path edges from dst back to src
    let mut path = Vec::new();
    let mut cur = dst;
    while cur != src {
        let e = pred[cur];
        if e == usize::MAX { return None; }
        path.push(e);
        let (u, v, _) = (0, 0, 0); // placeholder, we need edge endpoints
        let _ = (u, v);
        // Walk back to the OTHER endpoint
        // We need edge endpoints — caller must provide them via edges array.
        // For now, we'll do this in caller. Encode via pred only.
        break;
    }
    // Re-do reconstruction with edges access
    // Workaround: caller passes edges and we expose via closure. Simpler: do it here.
    // Actually let's just return pred and reconstruct in caller.
    Some((dist[dst], pred))
}

#[derive(Clone)]
struct MwuResult {
    f_per: Vec<f64>,
    f_total: f64,
    rounds: usize,
    augments: usize,
    time_ms: f64,
}

fn garg_konemann(g: &Graph, eps: f64, max_rounds: usize) -> MwuResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let t0 = Instant::now();

    // Adjacency (undirected, with edge index)
    let mut adj: Vec<Vec<(usize, usize)>> = vec![Vec::new(); n];
    for (e, &(u, v, _)) in g.edges.iter().enumerate() {
        adj[u].push((v, e));
        adj[v].push((u, e));
    }

    // Garg-Könemann: δ = (1+ε) · ((1+ε)·m)^{-1/ε}
    // This makes D₀ = Σ_e w_e c_e tiny initially.
    // Cap δ at a reasonable floor to avoid underflow.
    let delta = ((1.0 + eps) * (m as f64)).powf(-1.0 / eps).max(1e-300);
    let mut w: Vec<f64> = caps.iter().map(|&c| delta / c).collect();
    let mut f_per = vec![0.0_f64; k];
    let mut f_e = vec![0.0_f64; m];  // accumulated flow magnitude per edge
    let mut rounds = 0usize;
    let mut augments = 0usize;

    loop {
        // D = Σ_e w_e * c_e (dual feasibility / stopping criterion)
        let d: f64 = (0..m).map(|e| w[e] * caps[e]).sum();
        if d >= 1.0 { break; }
        if rounds >= max_rounds { break; }
        rounds += 1;

        // For each commodity, find shortest s-t path under length ℓ_e = w_e / c_e
        let mut lengths: Vec<f64> = (0..m).map(|e| w[e] / caps[e]).collect();
        // Pick the commodity with smallest shortest-path length (most beneficial to augment)
        let mut best_i = usize::MAX;
        let mut best_dist = f64::INFINITY;
        let mut best_pred: Vec<usize> = Vec::new();
        for (i, &(s, t)) in g.commodities.iter().enumerate() {
            if let Some((dist_st, pred)) = dijkstra(&adj, &lengths, s, t, n) {
                if dist_st < best_dist {
                    best_dist = dist_st;
                    best_i = i;
                    best_pred = pred;
                }
            }
        }
        if best_i == usize::MAX || !best_dist.is_finite() { break; }
        let (s, t) = g.commodities[best_i];

        // Reconstruct path edges from pred
        let mut path_edges: Vec<usize> = Vec::new();
        let mut cur = t;
        let mut walked = 0usize;
        while cur != s && walked < n {
            let e = best_pred[cur];
            if e == usize::MAX { break; }
            path_edges.push(e);
            let (u_e, v_e, _) = g.edges[e];
            cur = if u_e == cur { v_e } else { u_e };
            walked += 1;
        }
        if cur != s { break; }

        // Garg-Könemann: push min-capacity along path (ignores already-accumulated
        // flow — over-saturation is fixed by the end-scaling step).
        let cap_min = path_edges.iter().map(|&e| caps[e]).fold(f64::INFINITY, f64::min);
        if cap_min <= 0.0 { break; }
        let push = cap_min;
        for &e in &path_edges {
            f_e[e] += push;
            // Update weight: w_e *= exp(ε · push / c_e)
            w[e] *= (eps * push / caps[e]).exp();
        }
        f_per[best_i] += push;
        augments += 1;

        // Re-update lengths for next iteration (cached implicitly via w)
        let _ = &mut lengths;
    }

    let f_total: f64 = f_per.iter().sum();

    // Garg-Könemann scaling correction: divide by log_{1+ε}((1+ε)/δ)
    let scale = ((1.0 + eps) / delta).ln() / (1.0 + eps).ln();
    let f_per_scaled: Vec<f64> = f_per.iter().map(|x| x / scale).collect();
    let f_total_scaled: f64 = f_per_scaled.iter().sum();

    MwuResult {
        f_per: f_per_scaled,
        f_total: f_total_scaled,
        rounds,
        augments,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let eps: f64 = args.iter().find_map(|a| a.strip_prefix("--eps=")?.parse().ok()).unwrap_or(0.1);
    let max_rounds: usize = args.iter().find_map(|a| a.strip_prefix("--rounds=")?.parse().ok()).unwrap_or(1_000_000);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, ε={eps}, max_rounds={max_rounds}, seed={seed}", g.edges.len());

    let res = garg_konemann(&g, eps, max_rounds);
    println!("\n[Garg-Könemann MWU MC]");
    println!("  F_total = {:.4}", res.f_total);
    println!("  rounds={}, augments={}, time={:.1} ms", res.rounds, res.augments, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
