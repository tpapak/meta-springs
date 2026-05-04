//! Projected gradient descent on the full per-commodity divergence-free
//! manifold for K-commodity max-flow.
//!
//! State: f ∈ R^{K·m} (signed flow per commodity per edge).
//! Hamiltonian:
//!   U(f) = -Σ_k F_k(f_k) + μ · Σ_e V(s_e)
//!   F_k  = div_{s_k}(f_k),    s_e = Σ_k|f_{k,e}|/c_e,    V(s) = -log(1-s²)
//!
//! Constraint: div(f_k)_v = 0 at every internal vertex of every commodity.
//! Tangent space null(C) explored via per-commodity Laplacian solve.
//!
//! Each step:
//!   1. ∇U(f) in K·m space (smooth-abs for V).
//!   2. Per commodity k, project grad_k onto null(div) via PCG with both
//!      s_k, t_k pinned (one Laplacian solve per commodity).
//!   3. Backtracking line search keeping s_e < 1 - tol AND U decreasing.
//!   4. f ← f − η · projected_grad.
//!   5. When ‖grad‖ small, decrease μ (IPM anneal).

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

fn pcg_two_pin(csr: &CsrLap, b: &[f64], pin_a: usize, pin_b: usize, tol: f64, max_iter: usize) -> (Vec<f64>, usize) {
    let n = csr.n();
    let mut x = vec![0.0_f64; n];
    let mut r = b.to_vec();
    r[pin_a] = 0.0; r[pin_b] = 0.0;
    let diag = csr.diag(0.0);
    let m_inv: Vec<f64> = (0..n).map(|i| {
        if i != pin_a && i != pin_b && diag[i] > 0.0 { 1.0 / diag[i] } else { 0.0 }
    }).collect();
    let mut z: Vec<f64> = r.iter().zip(m_inv.iter()).map(|(ri, mi)| ri * mi).collect();
    let mut p = z.clone();
    let mut ap = vec![0.0_f64; n];
    let mut p_pin_zero = vec![0.0_f64; n];
    let mut rz_old: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
    let b_norm: f64 = b.iter().map(|v| v * v).sum::<f64>().sqrt().max(1.0);
    for it in 0..max_iter {
        p_pin_zero.copy_from_slice(&p);
        p_pin_zero[pin_a] = 0.0; p_pin_zero[pin_b] = 0.0;
        csr.apply(0.0, &p_pin_zero, &mut ap);
        ap[pin_a] = 0.0; ap[pin_b] = 0.0;
        let p_ap: f64 = p.iter().zip(ap.iter()).map(|(a, b)| a * b).sum();
        if p_ap.abs() < 1e-30 { return (x, it); }
        let alpha = rz_old / p_ap;
        for i in 0..n { x[i] += alpha * p[i]; r[i] -= alpha * ap[i]; }
        x[pin_a] = 0.0; r[pin_a] = 0.0;
        x[pin_b] = 0.0; r[pin_b] = 0.0;
        let r_norm: f64 = r.iter().map(|v| v * v).sum::<f64>().sqrt();
        if r_norm / b_norm < tol { return (x, it + 1); }
        for i in 0..n { z[i] = r[i] * m_inv[i]; }
        let rz_new: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
        let beta = rz_new / rz_old;
        for i in 0..n { p[i] = z[i] + beta * p[i]; }
        p[pin_a] = 0.0; p[pin_b] = 0.0;
        rz_old = rz_new;
    }
    (x, max_iter)
}

/// Project an edge-vector p_k onto the divergence-free manifold for
/// commodity k (one Laplacian solve, both s_k and t_k pinned).
fn project_div_free(
    g: &Graph,
    p: &mut [f64],
    s: usize,
    t: usize,
    csr_unit: &CsrLap,
) -> usize {
    let mut div = vec![0.0_f64; g.n];
    for (e, &(u, v, _)) in g.edges.iter().enumerate() {
        div[u] += p[e];
        div[v] -= p[e];
    }
    let (phi, n_pcg) = pcg_two_pin(csr_unit, &div, s, t, 1e-10, 4 * g.n);
    for (e, &(u, v, _)) in g.edges.iter().enumerate() {
        p[e] -= phi[u] - phi[v];
    }
    n_pcg
}

const ABS_EPS: f64 = 1e-6;
fn smooth_abs(x: f64) -> f64 { (x * x + ABS_EPS * ABS_EPS).sqrt() }
fn smooth_abs_grad(x: f64) -> f64 { x / smooth_abs(x) }

fn potential_u(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
) -> f64 {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m {
            load[e] += smooth_abs(f[kk * m + e]);
        }
    }
    let mut barrier = 0.0;
    for e in 0..m {
        let s = load[e] / caps[e];
        if s >= 1.0 - 1e-9 { return f64::INFINITY; }
        barrier += -(1.0 - s * s).ln();
    }
    let mut f_total = 0.0;
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == s_kk { div_s += f[kk * m + e]; }
            if v == s_kk { div_s -= f[kk * m + e]; }
        }
        f_total += div_s;
    }
    -f_total + mu * barrier
}

fn grad_u(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    grad: &mut [f64],
    f_per: &mut [f64],
) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m {
            load[e] += smooth_abs(f[kk * m + e]);
        }
    }
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s_e = (load[e] / caps[e]).min(0.9999);
            let v_prime = 2.0 * s_e / (1.0 - s_e * s_e);
            let grad_barrier = mu * v_prime * smooth_abs_grad(f[kk * m + e]) / caps[e];
            // ∂F_k/∂f_{k,e}: +1 if u=s_k, -1 if v=s_k, 0 else.
            // ∂U/∂f_{k,e} = -∂F_k/∂f + grad_barrier
            let grad_throughput = if u == s_kk { 1.0 }
                                  else if v == s_kk { -1.0 }
                                  else { 0.0 };
            grad[kk * m + e] = grad_barrier - grad_throughput;
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }
        }
        f_per[kk] = div_s;
    }
}

#[derive(Clone)]
struct PgdResult {
    f_per: Vec<f64>,
    f_total: f64,
    iters: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_pgd_mc(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    eta_init: f64,
    seed: u64,
) -> PgdResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let _ = seed;

    let unit_weighted: Vec<(u32, u32, f64)> = g.edges.iter()
        .map(|&(u, v, _)| (u as u32, v as u32, 1.0))
        .collect();
    let csr_unit = CsrLap::from_canonical_weights(&unit_weighted, n);

    let mut f = vec![0.0_f64; k * m];
    let mut grad = vec![0.0_f64; k * m];
    let mut f_per = vec![0.0_f64; k];
    let mut total_pcg = 0_usize;
    let mut iters = 0_usize;
    let t0 = Instant::now();

    let mut eta = eta_init;
    for &mu in mu_schedule {
        for _inner in 0..inner_iters {
            iters += 1;
            grad_u(&f, g, &caps, mu, &mut grad, &mut f_per);
            // Project per commodity
            for kk in 0..k {
                let (s_kk, t_kk) = g.commodities[kk];
                let gk = &mut grad[kk * m..(kk + 1) * m];
                total_pcg += project_div_free(g, gk, s_kk, t_kk, &csr_unit);
            }

            let grad_norm: f64 = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
            if grad_norm < 1e-8 { break; }

            // Backtracking line search.
            let u_curr = potential_u(&f, g, &caps, mu);
            let mut t = eta;
            let mut accepted = false;
            for _ls in 0..40 {
                let mut f_try = f.clone();
                for i in 0..k * m { f_try[i] -= t * grad[i]; }
                let u_try = potential_u(&f_try, g, &caps, mu);
                if u_try.is_finite() && u_try < u_curr - 1e-10 * t * grad_norm * grad_norm {
                    f = f_try;
                    accepted = true;
                    break;
                }
                t *= 0.5;
            }
            if !accepted {
                eta *= 0.5;
                if eta < 1e-12 { break; }
                continue;
            }
            // Mild eta growth on success
            eta = (eta * 1.1).min(eta_init);
        }
    }

    grad_u(&f, g, &caps, *mu_schedule.last().unwrap_or(&0.001), &mut grad, &mut f_per);
    let f_total: f64 = f_per.iter().sum();
    PgdResult {
        f_per,
        f_total,
        iters,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(60);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, eta_init={eta}, seed={seed}",
        g.edges.len());

    let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001, 0.00003];
    let res = spring_pgd_mc(&g, &mu_schedule, inner, eta, seed);
    println!("\n[Spring projected GD MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  iters={}, pcg={}, time={:.1} ms", res.iters, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
