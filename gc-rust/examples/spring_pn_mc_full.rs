//! Projected Newton with FULL block Hessian (cross-commodity coupling).
//!
//! Same Hamiltonian as spring_pn_mc but per-edge Hessian H_e is the full
//! K×K block:
//!     H_e = diag(d_e) + u_e u_e^T
//! where:
//!     d_{k,e} = μ V'(s_e) sa''(f_{k,e}) / c_e
//!     u_{k,e} = sqrt(μ V''(s_e)) · sa'(f_{k,e}) / c_e
//!     s_e    = Σ_k |f_{k,e}| / c_e
//!
//! The rank-1 part u_e u_e^T couples ALL K commodities at edge e — this is
//! the off-diagonal that the diagonal-only PN missed.
//!
//! Per Newton step we solve the Schur system
//!     C H_full^{-1} C^T λ = -C H_full^{-1} g
//! via Richardson iteration:
//!   1. Apply Sherman-Morrison per edge to compute H_full^{-1}(g + C^T λ).
//!   2. Compute residual r = -C H_full^{-1}(g + C^T λ).
//!   3. Precondition with K independent weighted Laplacian solves (diag-H).
//!   4. Update λ.
//!   5. Repeat until r small.

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

fn run_lp_oracle(g: &Graph, use_gurobi: bool) -> Option<(f64, Vec<f64>, f64)> {
    let json_in = serde_json::json!({
        "n": g.n,
        "edges": g.edges.iter().map(|&(u,v,c)| [u, v, c as usize]).collect::<Vec<_>>(),
        "commodities": g.commodities.iter().map(|&(s,t)| [s, t]).collect::<Vec<_>>(),
    });
    let tmp = std::env::temp_dir().join(format!("mc_lp_{}.json", std::process::id()));
    std::fs::write(&tmp, json_in.to_string()).ok()?;
    let (cmd, script) = if use_gurobi {
        ("/opt/homebrew/Caskroom/miniconda/base/bin/python", "mc_lp_oracle_gurobi.py")
    } else {
        ("python3", "mc_lp_oracle.py")
    };
    let t0 = Instant::now();
    let out = Command::new(cmd)
        .arg(format!("/Users/tosku/Sync/Documents/slmm/gc-rust/examples/{script}"))
        .arg(&tmp)
        .output()
        .ok()?;
    let elapsed_ms = t0.elapsed().as_secs_f64() * 1000.0;
    let _ = std::fs::remove_file(&tmp);
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().last()?;
    let v: serde_json::Value = serde_json::from_str(line).ok()?;
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
fn smooth_abs_hess(x: f64) -> f64 { let s = smooth_abs(x); (ABS_EPS * ABS_EPS) / (s * s * s) }

fn potential_u(f: &[f64], g: &Graph, caps: &[f64], mu: f64) -> f64 {
    let m = g.edges.len(); let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k { for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); } }
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

fn grad_and_hess_full(
    f: &[f64], g: &Graph, caps: &[f64], mu: f64,
    grad: &mut [f64],
    d_diag: &mut [f64],
    u_rank1: &mut [f64],
    sigma_e: &mut [f64],
    f_per: &mut [f64],
) {
    let m = g.edges.len(); let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k { for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); } }
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s_e = (load[e] / caps[e]).min(0.999999);
            let v_prime = 2.0 * s_e / (1.0 - s_e * s_e);
            let v_pp = 2.0 * (1.0 + s_e * s_e) / ((1.0 - s_e * s_e).powi(2));
            let sap = smooth_abs_grad(f[kk * m + e]);
            let sapp = smooth_abs_hess(f[kk * m + e]);
            // Gradient
            let grad_barrier = mu * v_prime * sap / caps[e];
            let grad_throughput = if u == s_kk { 1.0 } else if v == s_kk { -1.0 } else { 0.0 };
            grad[kk * m + e] = grad_barrier - grad_throughput;
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }
            // H_e = diag(d_pure) + u u^T  — true decomposition.
            // Regularise d_pure with a small floor (proportional to local rank-1 strength)
            // so Sherman-Morrison stays well-conditioned even when sa''(f) ≈ 0.
            let d_pure = mu * v_prime * sapp / caps[e];
            let uu = (mu * v_pp).sqrt() * sap / caps[e];
            // Tikhonov regularisation: H + εI for small ε.
            // ε scales with the local rank-1 strength so 1+σ stays O(K).
            let d_reg = (d_pure + 0.01 * uu * uu).max(1e-12);
            d_diag[kk * m + e] = d_reg;
            u_rank1[kk * m + e] = uu;
        }
        f_per[kk] = div_s;
    }
    // σ_e = Σ_k u_{k,e}² / d_{k,e}
    for e in 0..m {
        let mut s = 0.0;
        for kk in 0..k { s += u_rank1[kk * m + e].powi(2) / d_diag[kk * m + e]; }
        sigma_e[e] = s;
    }
}

/// Apply H_full^{-1} (per-edge Sherman-Morrison) to a K·m vector v in place.
/// H_full^{-1} v_e = (v_e - u_e (u_e^T v_e / d_e) / (1 + σ_e)) / d_e
fn apply_h_full_inv(
    v: &mut [f64], d_diag: &[f64], u_rank1: &[f64], sigma_e: &[f64],
    m: usize, k: usize,
) {
    for e in 0..m {
        // s = u_e^T (v_e / d_e)
        let mut s = 0.0;
        for kk in 0..k {
            s += u_rank1[kk * m + e] * v[kk * m + e] / d_diag[kk * m + e];
        }
        let coef = s / (1.0 + sigma_e[e]);
        // v_{k,e} ← (v_{k,e} − u_{k,e} · coef) / d_{k,e}
        for kk in 0..k {
            v[kk * m + e] = (v[kk * m + e] - u_rank1[kk * m + e] * coef) / d_diag[kk * m + e];
        }
    }
}

#[derive(Clone)]
struct PnFullResult {
    f_per: Vec<f64>,
    f_total: f64,
    iters: usize,
    pcg_iters: usize,
    richardson_iters: usize,
    time_ms: f64,
}

fn spring_pn_full_mc(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    eta_init: f64,
    max_richardson: usize,
) -> PnFullResult {
    let n = g.n; let m = g.edges.len(); let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    let mut f = vec![0.0_f64; k * m];
    let mut grad = vec![0.0_f64; k * m];
    let mut d_diag = vec![1.0_f64; k * m];
    let mut u_rank1 = vec![0.0_f64; k * m];
    let mut sigma_e = vec![0.0_f64; m];
    let mut f_per = vec![0.0_f64; k];
    let mut total_pcg = 0_usize;
    let mut iters = 0_usize;
    let mut total_richardson = 0_usize;
    let t0 = Instant::now();
    let mut eta = eta_init;

    for &mu in mu_schedule {
        for _inner in 0..inner_iters {
            iters += 1;
            grad_and_hess_full(&f, g, &caps, mu, &mut grad, &mut d_diag, &mut u_rank1, &mut sigma_e, &mut f_per);

            // Richardson on Schur: solve S λ = -C H_full^{-1} g where S = C H_full^{-1} C^T.
            // Preconditioner: M = block-diag (C diag(1/d_k) C^T) per commodity.
            let mut lambda = vec![0.0_f64; k * n];
            let mut delta = vec![0.0_f64; k * m];
            let mut richardson_done = 0;
            let mut last_residual = f64::INFINITY;
            for r_iter in 0..max_richardson {
                // Compute v = g + B^T λ per (k, e)
                let mut v = vec![0.0_f64; k * m];
                for kk in 0..k {
                    for (e, &(u, vtx, _)) in g.edges.iter().enumerate() {
                        v[kk * m + e] = grad[kk * m + e] + lambda[kk * n + u] - lambda[kk * n + vtx];
                    }
                }
                // Apply H_full^{-1} in place: now v contains Δf candidate (sign flipped)
                apply_h_full_inv(&mut v, &d_diag, &u_rank1, &sigma_e, m, k);
                // Δf = -H_full^{-1} v (we computed v = g + C^T λ; Δf solves H Δf = -v)
                for i in 0..k * m { delta[i] = -v[i]; }

                // Residual for Schur: r = -C Δf at internal vertices
                let mut residual = vec![0.0_f64; k * n];
                for kk in 0..k {
                    let (s_kk, t_kk) = g.commodities[kk];
                    for (e, &(u, vtx, _)) in g.edges.iter().enumerate() {
                        residual[kk * n + u] -= delta[kk * m + e];
                        residual[kk * n + vtx] += delta[kk * m + e];
                    }
                    residual[kk * n + s_kk] = 0.0;
                    residual[kk * n + t_kk] = 0.0;
                }

                let r_norm: f64 = residual.iter().map(|x| x * x).sum::<f64>().sqrt();
                if r_norm < 1e-8 || (r_iter > 0 && r_norm > last_residual * 0.99) {
                    richardson_done = r_iter + 1;
                    break;
                }
                last_residual = r_norm;

                // Preconditioner solve: M δλ = -residual (Richardson update: λ ← λ + δλ
                // such that new C Δf becomes ≈ 0; we have residual = -C Δf, so RHS = -residual = +C Δf,
                // but want to drive residual to 0 ⇒ pass -residual)
                for kk in 0..k {
                    let (s_kk, t_kk) = g.commodities[kk];
                    let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                        .map(|(e, &(u, vtx, _))| (u as u32, vtx as u32, 1.0 / d_diag[kk * m + e]))
                        .collect();
                    let csr_k = CsrLap::from_canonical_weights(&weighted, n);
                    let rhs: Vec<f64> = (0..n).map(|i| -residual[kk * n + i]).collect();
                    let (delta_lambda, n_pcg) = pcg_two_pin(&csr_k, &rhs, s_kk, t_kk, 1e-10, 4 * n);
                    total_pcg += n_pcg;
                    for i in 0..n { lambda[kk * n + i] += delta_lambda[i]; }
                }
                richardson_done = r_iter + 1;
            }
            total_richardson += richardson_done;

            // Final delta is the latest computed Δf
            let grad_norm: f64 = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
            if grad_norm < 1e-9 { break; }

            // Backtracking line search
            let u_curr = potential_u(&f, g, &caps, mu);
            let mut t = eta; let mut accepted = false;
            for _ls in 0..40 {
                let mut f_try = f.clone();
                for i in 0..k * m { f_try[i] += t * delta[i]; }
                let u_try = potential_u(&f_try, g, &caps, mu);
                if u_try.is_finite() && u_try < u_curr - 1e-12 {
                    f = f_try; accepted = true; break;
                }
                t *= 0.5;
            }
            if !accepted { eta *= 0.5; if eta < 1e-12 { break; } continue; }
            eta = (eta * 1.5).min(eta_init);
        }
    }

    grad_and_hess_full(&f, g, &caps, *mu_schedule.last().unwrap_or(&0.001), &mut grad, &mut d_diag, &mut u_rank1, &mut sigma_e, &mut f_per);
    let f_total: f64 = f_per.iter().sum();

    // Feasibility
    let mut max_load_frac = 0.0_f64;
    let mut viol_count = 0usize;
    let mut max_viol = 0.0_f64;
    for e in 0..m {
        let mut load = 0.0;
        for kk in 0..k { load += f[kk * m + e].abs(); }
        let frac = load / caps[e];
        if frac > max_load_frac { max_load_frac = frac; }
        if frac > 1.0 + 1e-6 { viol_count += 1; if frac - 1.0 > max_viol { max_viol = frac - 1.0; } }
    }
    println!("# feasibility: max load = {:.6}, violations = {} (max overshoot = {:.6})",
        max_load_frac, viol_count, max_viol);

    PnFullResult { f_per, f_total, iters, pcg_iters: total_pcg, richardson_iters: total_richardson,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0 }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(40);
    let max_richardson: usize = args.iter().find_map(|a| a.strip_prefix("--rich=")?.parse().ok()).unwrap_or(20);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");
    let use_gurobi = args.iter().any(|a| a == "--gurobi");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, rich={max_richardson}, seed={seed}", g.edges.len());

    let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001, 0.00003];
    let res = spring_pn_full_mc(&g, &mu_schedule, inner, eta, max_richardson);
    println!("\n[Spring projected Newton FULL Hessian MC]");
    println!("  F_total = {:.4}", res.f_total);
    println!("  newton_iters={}, richardson_total={}, pcg={}, time={:.1} ms",
        res.iters, res.richardson_iters, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _, lp_ms)) = run_lp_oracle(&g, use_gurobi) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[{} LP]", if use_gurobi { "Gurobi" } else { "HiGHS" });
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
