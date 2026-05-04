//! Projected Newton on full per-commodity f-manifold for K-commodity max-flow.
//!
//! Same Hamiltonian as spring_pgd_mc / spring_hmc_mc:
//!   U(f) = -Σ_k F_k + μ · Σ_e V(s_e)
//!   F_k  = div_{s_k}(f_k),   s_e = Σ_k|f_{k,e}|/c_e,   V(s) = -log(1-s²)
//!
//! Newton system per inner iter:
//!   H Δf = -g    s.t. C Δf = 0
//! where C = block-diag conservation, H = full Hamiltonian Hessian.
//!
//! Approximation: take H diagonal (per (k,e)), so the Schur complement
//! C H⁻¹ Cᵀ becomes block-diagonal in K — K independent WEIGHTED LAPLACIAN
//! solves with conductance 1/H_{k,e}. The H_{k,e} grow as edge load
//! approaches saturation (V''(s)→∞), so stiff/saturated edges become
//! low-conductance in the projection metric — exactly the right thing.
//!
//! Per Newton step:
//!   1. ∇U(f), per-(k,e) diagonal Hessian H.
//!   2. For each k: weighted Laplacian solve via PCG on c_e = 1/H_{k,e}
//!      (s_k, t_k pinned) to compute λ_k = (CH⁻¹Cᵀ)⁻¹ CH⁻¹g.
//!   3. Δf_{k,e} = -(g_{k,e} + λ_{k,u} - λ_{k,v}) / H_{k,e}.
//!   4. Backtracking on Δf to keep U finite + decreasing.
//!   5. Anneal μ → 0.

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

const ABS_EPS: f64 = 1e-9;
fn smooth_abs(x: f64) -> f64 { (x * x + ABS_EPS * ABS_EPS).sqrt() }
fn smooth_abs_grad(x: f64) -> f64 { x / smooth_abs(x) }
fn smooth_abs_hess(x: f64) -> f64 {
    let s = smooth_abs(x);
    (ABS_EPS * ABS_EPS) / (s * s * s)
}

fn potential_u(f: &[f64], g: &Graph, caps: &[f64], mu: f64) -> f64 {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); }
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

/// Compute ∇U and per-(k,e) diagonal Hessian H_{k,e}.
/// Also returns per-commodity throughput.
fn grad_and_hess_diag(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    grad: &mut [f64],
    h_diag: &mut [f64],
    f_per: &mut [f64],
) {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); }
    }
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s_e = (load[e] / caps[e]).min(0.9999);
            let v_prime = 2.0 * s_e / (1.0 - s_e * s_e);
            let v_pp = 2.0 * (1.0 + s_e * s_e) / ((1.0 - s_e * s_e).powi(2));

            let sap = smooth_abs_grad(f[kk * m + e]);
            let sapp = smooth_abs_hess(f[kk * m + e]);

            // Gradient: barrier + throughput
            let grad_barrier = mu * v_prime * sap / caps[e];
            let grad_throughput = if u == s_kk { 1.0 }
                                  else if v == s_kk { -1.0 }
                                  else { 0.0 };
            grad[kk * m + e] = grad_barrier - grad_throughput;
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }

            // Diagonal Hessian:
            //   ∂²U/∂f_{k,e}² = μ V''(s_e) [sa'(f_{k,e})/c_e]² + μ V'(s_e) sa''(f_{k,e})/c_e
            let term1 = mu * v_pp * (sap * sap) / (caps[e] * caps[e]);
            let term2 = mu * v_prime * sapp / caps[e];
            h_diag[kk * m + e] = (term1 + term2).max(1e-10);
        }
        f_per[kk] = div_s;
    }
}

#[derive(Clone)]
struct PnResult {
    f_per: Vec<f64>,
    f_total: f64,
    iters: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_pn_mc(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    eta_init: f64,
    seed: u64,
) -> PnResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let _ = seed;

    let mut f = vec![0.0_f64; k * m];
    let mut grad = vec![0.0_f64; k * m];
    let mut h_diag = vec![1.0_f64; k * m];
    let mut f_per = vec![0.0_f64; k];
    let mut total_pcg = 0_usize;
    let mut iters = 0_usize;
    let t0 = Instant::now();

    let mut eta = eta_init;
    for &mu in mu_schedule {
        for _inner in 0..inner_iters {
            iters += 1;
            grad_and_hess_diag(&f, g, &caps, mu, &mut grad, &mut h_diag, &mut f_per);

            // Build per-commodity weighted Laplacian and solve for λ.
            // Conductance for projection metric: 1/H_{k,e} (Newton metric).
            // System: L_k(1/H_k) λ_k = div(g_k / H_k) with s_k, t_k pinned.
            let mut delta = vec![0.0_f64; k * m];
            for kk in 0..k {
                let (s_kk, t_kk) = g.commodities[kk];
                // Build conductance per edge: 1 / H_{kk,e}
                let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                    .map(|(e, &(u, v, _))| (u as u32, v as u32, 1.0 / h_diag[kk * m + e]))
                    .collect();
                let csr_k = CsrLap::from_canonical_weights(&weighted, n);

                // RHS for λ in Schur: -C H^{-1} g = -div(g/H) at internal vertices.
                let mut neg_div = vec![0.0_f64; n];
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    let val = grad[kk * m + e] / h_diag[kk * m + e];
                    neg_div[u] -= val;
                    neg_div[v] += val;
                }
                let (lambda, n_pcg) = pcg_two_pin(&csr_k, &neg_div, s_kk, t_kk, 1e-10, 4 * n);
                total_pcg += n_pcg;

                // Δf_{kk,e} = -(g + B^T λ) / H
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    let val = grad[kk * m + e] + lambda[u] - lambda[v];
                    delta[kk * m + e] = -val / h_diag[kk * m + e];
                }
            }

            let grad_norm: f64 = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
            if grad_norm < 1e-9 { break; }

            // Backtracking: keep U finite and decrease.
            let u_curr = potential_u(&f, g, &caps, mu);
            let mut t = eta;
            let mut accepted = false;
            for _ls in 0..40 {
                let mut f_try = f.clone();
                for i in 0..k * m { f_try[i] += t * delta[i]; }
                let u_try = potential_u(&f_try, g, &caps, mu);
                if u_try.is_finite() && u_try < u_curr - 1e-12 {
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
            eta = (eta * 1.5).min(eta_init);
        }
    }

    // ===== POLISH PHASE: Ford-Fulkerson on residual graph per commodity =====
    // Use the IPM solution as a warm start, then run greedy augmentation
    // (one commodity at a time, electrical-flow oracle on residual capacity)
    // to top off any remaining capacity that the smooth-barrier IPM couldn't
    // saturate exactly.
    {
        // Recompute current f_per
        grad_and_hess_diag(&f, g, &caps, *mu_schedule.last().unwrap_or(&0.001), &mut grad, &mut h_diag, &mut f_per);
        let mut consecutive_fail = 0usize;
        let polish_iters = 20 * k;
        for it in 0..polish_iters {
            let i = it % k;
            let (s_i, t_i) = g.commodities[i];

            // Residual capacity: r_e = c_e - Σ_j |f_{j,e}|
            let mut resid = vec![0.0_f64; m];
            for e in 0..m {
                let mut used = 0.0;
                for j in 0..k { used += f[j * m + e].abs(); }
                resid[e] = (caps[e] - used).max(0.0);
            }

            // Skip if no residual capacity at all
            if resid.iter().sum::<f64>() < 1e-6 * (m as f64) { break; }

            // Spring solve on residual conductance
            let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                .map(|(e, &(u, v, _))| (u as u32, v as u32, resid[e].max(1e-12)))
                .collect();
            let csr = CsrLap::from_canonical_weights(&weighted, n);
            let mut b = vec![0.0_f64; n];
            b[s_i] = 1.0;
            let (phi, n_pcg) = pcg_two_pin(&csr, &b, s_i, t_i, 1e-10, 4 * n);
            let _ = n_pcg;

            // Direction d_e = (φ_u − φ_v) · resid_e
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

            // Max α: ensure |f_{i,e} + α d_e| + Σ_{j≠i}|f_{j,e}| ≤ c_e
            let mut alpha_max = f64::INFINITY;
            for e in 0..m {
                let d_e = d[e]; let f_e = f[i * m + e];
                if d_e.abs() < 1e-15 { continue; }
                let mut other = 0.0;
                for j in 0..k { if j != i { other += f[j * m + e].abs(); } }
                let room = (caps[e] - other).max(0.0);
                let bound = if d_e > 0.0 { (room - f_e) / d_e } else { (-room - f_e) / d_e };
                if bound > 0.0 && bound < alpha_max { alpha_max = bound; }
            }
            if !alpha_max.is_finite() || alpha_max < 1e-10 {
                consecutive_fail += 1;
                if consecutive_fail >= k { break; }
                continue;
            }
            consecutive_fail = 0;

            // Take 99% of max (leave headroom for further polish)
            let alpha = alpha_max * 0.99;
            for e in 0..m { f[i * m + e] += alpha * d[e]; }
            f_per[i] += alpha * div_s;
        }
    }
    // ============================================================

    grad_and_hess_diag(&f, g, &caps, *mu_schedule.last().unwrap_or(&0.001), &mut grad, &mut h_diag, &mut f_per);
    let f_total: f64 = f_per.iter().sum();

    // Feasibility report: max edge load fraction Σ_k|f_{k,e}|/c_e
    let mut max_load_frac = 0.0_f64;
    let mut viol_count = 0usize;
    let mut max_viol = 0.0_f64;
    for e in 0..m {
        let mut load = 0.0;
        for kk in 0..k { load += f[kk * m + e].abs(); }
        let frac = load / caps[e];
        if frac > max_load_frac { max_load_frac = frac; }
        if frac > 1.0 + 1e-6 {
            viol_count += 1;
            if frac - 1.0 > max_viol { max_viol = frac - 1.0; }
        }
    }
    println!("# feasibility: max load fraction = {:.6}, violations = {} (max overshoot = {:.6})",
        max_load_frac, viol_count, max_viol);

    PnResult { f_per, f_total, iters, pcg_iters: total_pcg, time_ms: t0.elapsed().as_secs_f64() * 1000.0 }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(40);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, eta={eta}, seed={seed}", g.edges.len());

    // Optional graph dump for cross-solver comparison
    if let Some(path) = args.iter().find_map(|a| a.strip_prefix("--dump=")) {
        let json = serde_json::json!({
            "n": g.n,
            "edges": g.edges.iter().map(|&(u,v,c)| [u, v, c as usize]).collect::<Vec<_>>(),
            "commodities": g.commodities.iter().map(|&(s,t)| [s, t]).collect::<Vec<_>>(),
        });
        std::fs::write(path, json.to_string()).expect("write dump");
        eprintln!("# graph dumped to {}", path);
    }

    // Extended μ schedule with deep tail to drive closer to LP optimum.
    let mu_schedule = vec![
        1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001,
        3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7,
    ];
    let res = spring_pn_mc(&g, &mu_schedule, inner, eta, seed);
    println!("\n[Spring projected Newton MC]");
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
