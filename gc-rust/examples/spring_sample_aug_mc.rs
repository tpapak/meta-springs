//! Multi-commodity spring-based stochastic augmenting flow.
//!
//! K commodities share edge capacities. Each iteration:
//!   1. Compute residual r_e = c_e − Σ_k |f_k,e|.
//!   2. Build a SINGLE Laplacian with conductance = r_e (multi-axis stiffness
//!      shared across commodities — the "joint solve" the user demanded
//!      instead of per-commodity passes).
//!   3. Solve K systems with the same L (K RHS, one per commodity) via PCG.
//!   4. Get K augmenting directions d_k from the K potentials.
//!   5. Find joint α_max s.t. Σ_k |f_k,e + α d_k,e| ≤ c_e, ∀e
//!      (conservative bound: α_max = min_e r_e / Σ_k |d_k,e|).
//!   6. Sample α ∈ [α_lo, α_hi] · α_max. Augment all K commodities.
//!   7. Stop when no commodity can augment anymore.
//!
//! Compare to scipy.linprog HiGHS LP oracle (`mc_lp_oracle.py`).

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
    // Spanning tree first to keep graph connected.
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
    // K commodities — random distinct (s, t) pairs
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
    if !out.status.success() { eprintln!("LP error: {}", String::from_utf8_lossy(&out.stderr)); return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let v: serde_json::Value = serde_json::from_str(&s).ok()?;
    let total = v["total"].as_f64()?;
    let f_per: Vec<f64> = v["F_per"].as_array()?.iter().map(|x| x.as_f64().unwrap_or(0.0)).collect();
    Some((total, f_per, elapsed_ms))
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

#[derive(Clone)]
struct AugResult {
    f_per: Vec<f64>,
    f_total: f64,
    augmentations: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_sample_augment_mc(
    g: &Graph,
    n_iters: usize,
    alpha_lo: f64,
    alpha_hi: f64,
    seed: u64,
) -> AugResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // Per-commodity signed flow per edge: f[i*m + e].
    let mut f = vec![0.0_f64; k * m];
    let mut f_per_total = vec![0.0_f64; k];
    let mut f_best_total = 0.0_f64;
    let mut f_best_per = vec![0.0_f64; k];

    // Per-commodity demand vector b_k (used after pinning sink)
    let mut b_per: Vec<Vec<f64>> = (0..k).map(|i| {
        let (s_i, _t_i) = g.commodities[i];
        let mut b = vec![0.0_f64; n];
        b[s_i] = 1.0;
        b
    }).collect();

    let mut total_pcg = 0_usize;
    let mut augmentations = 0_usize;
    let mut rng = Xs256::new(seed ^ 0xCAFE_F00D_BEEF_CAFE);
    let t0 = Instant::now();

    let mut consecutive_fail = 0usize;
    for _it in 0..n_iters {
        // Pick a commodity (round-robin with random offset).
        let i = (augmentations + (rng.gen_range(0, k as u64) as usize)) % k;
        let (s_i, t_i) = g.commodities[i];

        // Residual = c_e − Σ_j |f_j,e| (capacity left after all current MC flow).
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
        if (div_s - 1.0).abs() > 1e-3 {
            consecutive_fail += 1;
            if consecutive_fail >= k { break; }
            continue;
        }

        // Max α for THIS commodity holding others fixed:
        //   |f_i,e + α d_e| ≤ c_e − Σ_{j≠i} |f_j,e|
        let mut alpha_i_max = f64::INFINITY;
        for e in 0..m {
            let d_e = d[e]; let f_e = f[i * m + e];
            if d_e.abs() < 1e-15 { continue; }
            let other = {
                let mut s = 0.0;
                for j in 0..k { if j != i { s += f[j * m + e].abs(); } }
                s
            };
            let room = (caps[e] - other).max(0.0);
            let bound = if d_e > 0.0 {
                (room - f_e) / d_e
            } else {
                (-room - f_e) / d_e
            };
            if bound > 0.0 && bound < alpha_i_max { alpha_i_max = bound; }
        }
        if !alpha_i_max.is_finite() || alpha_i_max < 1e-10 {
            consecutive_fail += 1;
            if consecutive_fail >= k { break; }
            continue;
        }
        consecutive_fail = 0;

        let frac = alpha_lo + (alpha_hi - alpha_lo) * rng.unit();
        let alpha_i = alpha_i_max * frac;
        for e in 0..m { f[i * m + e] += alpha_i * d[e]; }
        f_per_total[i] += alpha_i * div_s;
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
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(2);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(500);
    let alpha_lo: f64 = args.iter().find_map(|a| a.strip_prefix("--lo=")?.parse().ok()).unwrap_or(0.5);
    let alpha_hi: f64 = args.iter().find_map(|a| a.strip_prefix("--hi=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp: bool = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T={n_iters}, α∈[{alpha_lo}, {alpha_hi}], seed={seed}",
        g.edges.len());
    println!("# commodities: {:?}", g.commodities);

    let res = spring_sample_augment_mc(&g, n_iters, alpha_lo, alpha_hi, seed);
    println!("\n[Spring sample-augment MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  augs={}, pcg={}, time={:.1} ms", res.augmentations, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_per   = {:?}", lp_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, slowdown = {:.1}×", rel, res.time_ms / lp_ms);
        } else {
            println!("\n[LP oracle unavailable — install scipy or skip with --no-lp]");
        }
    }
}
