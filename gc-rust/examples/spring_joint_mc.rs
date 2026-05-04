//! Joint multi-axis spring max-flow for K commodities.
//!
//! Each iteration: ONE Laplacian (joint stiffness from shared residual),
//! K simultaneous PCG solves (K RHS), then the K augmenting directions
//! step JOINTLY — but each commodity gets its own α_i found by a small
//! per-iter "max Σα_i s.t. Σ_k |f_k,e + α_k d_k,e| ≤ c_e ∀e" LP-style
//! search. This is the proper multi-axis dynamics: the K commodities
//! solve a coupled spring system on the same residual conductance, then
//! relax their flows simultaneously into available capacity.
//!
//! For per-commodity α we use a coordinate-ascent inner loop instead of
//! a full LP — at each inner step, pick the commodity with the largest
//! marginal gain ∂(Σα)/∂α_i and grow it until a capacity wall.

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
    fn unit(&mut self) -> f64 { (self.next_u64() as f64 / u64::MAX as f64).clamp(0.0, 1.0) }
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

/// Joint per-commodity α via coordinate ascent on Σ_k α_k subject to
///     Σ_k |f_k,e + α_k d_k,e| ≤ c_e   ∀ e
/// At each inner step, pick the i with the largest growable headroom and
/// push α_i until it would violate some edge.
fn solve_joint_alpha(
    f: &[f64],
    d_per: &[Vec<f64>],
    caps: &[f64],
    div_s_per: &[f64],
    inner_iters: usize,
) -> Vec<f64> {
    let m = caps.len();
    let k = d_per.len();
    let mut alpha = vec![0.0_f64; k];
    let alive: Vec<bool> = (0..k).map(|i| (div_s_per[i] - 1.0).abs() < 1e-3).collect();
    if alive.iter().all(|&a| !a) { return alpha; }

    // Track current effective flow f_eff[i*m+e] = f[i*m+e] + α_i * d_per[i][e]
    let mut f_eff: Vec<f64> = f.to_vec();

    for _ in 0..inner_iters {
        // For each commodity, compute its max additional growth
        // δα_i max s.t. for all e:
        //   |f_eff[i*m+e] + δα_i * d_per[i][e]| + Σ_{j≠i} |f_eff[j*m+e]|  ≤  c_e
        let mut best_i = usize::MAX;
        let mut best_da = 0.0_f64;
        for i in 0..k {
            if !alive[i] { continue; }
            let mut da_max = f64::INFINITY;
            for e in 0..m {
                let d_ie = d_per[i][e];
                if d_ie.abs() < 1e-15 { continue; }
                let mut other = 0.0;
                for j in 0..k { if j != i { other += f_eff[j * m + e].abs(); } }
                let room = (caps[e] - other).max(0.0);
                let f_ie = f_eff[i * m + e];
                let bound = if d_ie > 0.0 { (room - f_ie) / d_ie } else { (-room - f_ie) / d_ie };
                if bound > 0.0 && bound < da_max { da_max = bound; }
            }
            if da_max.is_finite() && da_max > best_da {
                best_da = da_max;
                best_i = i;
            }
        }
        if best_i == usize::MAX || best_da < 1e-12 { break; }
        // Take 95% of the wall to keep room for further coordinate steps.
        let take = best_da * 0.95;
        alpha[best_i] += take;
        for e in 0..m { f_eff[best_i * m + e] += take * d_per[best_i][e]; }
    }
    alpha
}

#[derive(Clone)]
struct AugResult {
    f_per: Vec<f64>,
    f_total: f64,
    augmentations: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_joint_mc(
    g: &Graph,
    n_iters: usize,
    coord_inner: usize,
    seed: u64,
) -> AugResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    let mut f = vec![0.0_f64; k * m];
    let mut f_per_total = vec![0.0_f64; k];
    let mut f_best_total = 0.0_f64;
    let mut f_best_per = vec![0.0_f64; k];

    let mut b_per: Vec<Vec<f64>> = (0..k).map(|i| {
        let (s_i, _t_i) = g.commodities[i];
        let mut b = vec![0.0_f64; n];
        b[s_i] = 1.0;
        b
    }).collect();

    let mut total_pcg = 0_usize;
    let mut augmentations = 0_usize;
    let mut consecutive_zero = 0_usize;
    let _ = seed;
    let t0 = Instant::now();

    for _it in 0..n_iters {
        // Build joint Laplacian from shared residual.
        let mut resid = vec![0.0_f64; m];
        for e in 0..m {
            let mut used = 0.0;
            for j in 0..k { used += f[j * m + e].abs(); }
            resid[e] = (caps[e] - used).max(1e-12);
        }
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(e, &(u, v, _))| (u as u32, v as u32, resid[e]))
            .collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);

        // K simultaneous solves on the SAME L (multi-axis joint stiffness).
        let mut d_per: Vec<Vec<f64>> = Vec::with_capacity(k);
        let mut div_s_per: Vec<f64> = Vec::with_capacity(k);
        for i in 0..k {
            let (s_i, t_i) = g.commodities[i];
            b_per[i][s_i] = 1.0;
            let (phi, pcg_it) = pcg_pinned(&csr, &b_per[i], t_i, 1e-10, 4 * n);
            total_pcg += pcg_it;

            let mut d = vec![0.0_f64; m];
            let mut div_s = 0.0_f64;
            for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                d[e] = (phi[u] - phi[v]) * resid[e];
                if u == s_i { div_s += d[e]; }
                if v == s_i { div_s -= d[e]; }
            }
            d_per.push(d);
            div_s_per.push(div_s);
        }

        // Joint α — coordinate ascent on Σα_k.
        let alpha = solve_joint_alpha(&f, &d_per, &caps, &div_s_per, coord_inner);
        let total_step: f64 = alpha.iter().sum();
        if total_step < 1e-10 {
            consecutive_zero += 1;
            if consecutive_zero >= 3 { break; }
            continue;
        }
        consecutive_zero = 0;

        for i in 0..k {
            if alpha[i] <= 0.0 { continue; }
            for e in 0..m { f[i * m + e] += alpha[i] * d_per[i][e]; }
            f_per_total[i] += alpha[i] * div_s_per[i];
        }
        augmentations += 1;

        let total: f64 = f_per_total.iter().sum();
        if total > f_best_total {
            f_best_total = total;
            f_best_per.copy_from_slice(&f_per_total);
        }
    }

    AugResult {
        f_per: f_best_per,
        f_total: f_best_total,
        augmentations,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(500);
    let coord_inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(50);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T={n_iters}, inner={coord_inner}, seed={seed}",
        g.edges.len());

    let res = spring_joint_mc(&g, n_iters, coord_inner, seed);
    println!("\n[Spring JOINT MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  augs={}, pcg={}, time={:.1} ms", res.augmentations, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_per   = {:?}", lp_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.0}×", rel, lp_ms / res.time_ms);
        }
    }
}
