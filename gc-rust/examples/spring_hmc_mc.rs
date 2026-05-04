//! Constrained HMC for K-commodity max-flow on the full divergence-free
//! manifold (per-commodity).
//!
//! State: f ∈ R^{K·m} (signed flow per commodity per edge).
//! Hamiltonian potential:
//!   U(f) = -β · Σ_k F_k(f_k)  +  μ · Σ_e V(s_e)
//!   F_k  = -(B f_k)_{s_k},   s_e = Σ_k|f_{k,e}|/c_e,  V(s) = -log(1-s²)
//!
//! Conservation constraint: for every internal vertex v ≠ s_k, t_k of every
//! commodity k, (B f_k)_v = 0. Linear constraint Cf = 0 with C block-diagonal
//! in K. Tangent space null(C) has dim K·(m−n+2).
//!
//! HMC step (RATTLE):
//!   1. Sample p ~ N(0, I) in R^{K·m}
//!   2. Project p → null(C): for each k, solve a Laplacian on internal
//!      vertices via PCG (both s_k, t_k pinned)
//!   3. Leapfrog L steps:
//!        p ← p − (dt/2) ∇U_proj(f)
//!        f ← f + dt · p
//!        re-project f onto manifold (Laplacian solve per commodity)
//!        p ← p − (dt/2) ∇U_proj(f)
//!      with smooth-abs in V to keep gradient defined.
//!   4. MH accept on H = U + (1/2)|p|²
//!
//! Crucially the proposal momentum is unrestricted Gaussian → projected.
//! The chain explores the FULL per-commodity divergence-free polytope, not
//! the K-dim subspace that traps Newton.

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
    fn normal(&mut self) -> f64 {
        let u1 = self.unit().max(1e-300);
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

/// PCG with **two** pins (source and sink both held at φ = 0).
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

/// Project a flow vector p ∈ R^m onto the null space of the
/// internal-vertex divergence operator (i.e., make div(p) = 0 at all
/// vertices except source and sink). One Laplacian solve.
/// Uses unit conductance (the projection metric).
fn project_to_div_free(
    g: &Graph,
    p: &mut [f64],
    s: usize,
    t: usize,
    csr_unit: &CsrLap,
) -> usize {
    let n = g.n;
    // Compute divergence at each vertex from p
    let mut div = vec![0.0_f64; n];
    for (e, &(u, v, _)) in g.edges.iter().enumerate() {
        div[u] += p[e];
        div[v] -= p[e];
    }
    // Solve L φ = div on internal vertices (s and t pinned to 0)
    let (phi, n_pcg) = pcg_two_pin(csr_unit, &div, s, t, 1e-10, 4 * n);
    // Subtract gradient: p_e -= φ_u - φ_v on edge (u,v)
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
    beta: f64,
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
    // F_k = -(B f_k)_{s_k} (signed throughput out of source)
    let mut f_total = 0.0;
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == s_kk { div_s += f[kk * m + e]; }
            if v == s_kk { div_s -= f[kk * m + e]; }
        }
        f_total += div_s;  // F_k = div_{s_k} = current leaving source
    }
    -beta * f_total + mu * barrier
}

/// In-place gradient of U wrt f. Returns the per-commodity F throughputs.
fn grad_u(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    beta: f64,
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
    // ∂U/∂f_{k,e} comes from:
    //   barrier term: μ V'(s_e) · sa'(f_{k,e}) / c_e where V'(s) = 2s/(1-s²)
    //   throughput term: -β · (signed contribution to F_k)
    //                    F_k = -(B f_k)_{s_k} = -sum over edges with u=s_k, +sum with v=s_k
    // So ∂F_k/∂f_{k,e=(u,v)} = -1 if u=s_k, +1 if v=s_k, 0 otherwise
    // Hence ∂U/∂f_{k,e} = μ V'(s_e) sa'(f_{k,e})/c_e - β · ∂F_k/∂f_{k,e}
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s_e = (load[e] / caps[e]).min(0.9999);
            let v_prime = 2.0 * s_e / (1.0 - s_e * s_e);
            let grad_barrier = mu * v_prime * smooth_abs_grad(f[kk * m + e]) / caps[e];
            let mut grad_throughput = 0.0;
            if u == s_kk { grad_throughput = 1.0; div_s += f[kk * m + e]; }
            else if v == s_kk { grad_throughput = -1.0; div_s -= f[kk * m + e]; }
            grad[kk * m + e] = grad_barrier - beta * grad_throughput;
        }
        f_per[kk] = div_s;
    }
}

#[derive(Clone)]
struct HmcResult {
    f_per: Vec<f64>,
    f_total: f64,
    accepts: usize,
    proposals: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_hmc_mc(
    g: &Graph,
    n_samples: usize,
    n_leapfrog: usize,
    step: f64,
    beta_lo: f64,
    beta_hi: f64,
    mu: f64,
    seed: u64,
) -> HmcResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    // Build the unit-conductance Laplacian once for the projection metric.
    let unit_weighted: Vec<(u32, u32, f64)> = g.edges.iter()
        .map(|&(u, v, _)| (u as u32, v as u32, 1.0))
        .collect();
    let csr_unit = CsrLap::from_canonical_weights(&unit_weighted, n);

    let mut f = vec![0.0_f64; k * m];
    let mut f_per = vec![0.0_f64; k];
    let mut f_best_per = vec![0.0_f64; k];
    let mut f_best_total = 0.0_f64;

    let mut total_pcg = 0_usize;
    let mut accepts = 0_usize;
    let mut proposals = 0_usize;
    let mut rng = Xs256::new(seed ^ 0xCAFE_F00D_BEEF_CAFE);
    let t0 = Instant::now();

    let mut grad = vec![0.0_f64; k * m];

    for it in 0..n_samples {
        let frac = if n_samples > 1 { it as f64 / (n_samples - 1) as f64 } else { 1.0 };
        let beta = beta_lo + (beta_hi - beta_lo) * frac;

        // Sample p, project per-commodity onto div-free.
        let mut p = vec![0.0_f64; k * m];
        for v in p.iter_mut() { *v = rng.normal(); }
        for kk in 0..k {
            let (s_kk, t_kk) = g.commodities[kk];
            let pk = &mut p[kk * m..(kk + 1) * m];
            total_pcg += project_to_div_free(g, pk, s_kk, t_kk, &csr_unit);
        }

        // Save current state and energy.
        let f0 = f.clone();
        let u0 = potential_u(&f, g, &caps, beta, mu);
        let kin0: f64 = p.iter().map(|x| 0.5 * x * x).sum();

        // Leapfrog.
        let mut f_lf = f.clone();
        let mut p_lf = p.clone();
        let mut accept = false;
        let mut diverged = false;

        for lf in 0..n_leapfrog {
            grad_u(&f_lf, g, &caps, beta, mu, &mut grad, &mut f_per);
            // Project gradient
            for kk in 0..k {
                let (s_kk, t_kk) = g.commodities[kk];
                let gk = &mut grad[kk * m..(kk + 1) * m];
                total_pcg += project_to_div_free(g, gk, s_kk, t_kk, &csr_unit);
            }
            // half-step momentum
            for i in 0..k * m { p_lf[i] -= 0.5 * step * grad[i]; }
            // full step f
            for i in 0..k * m { f_lf[i] += step * p_lf[i]; }
            // re-project f onto manifold (correct any drift) — NOP since p
            // was projected and f starts on manifold
            // (skip explicit re-projection since linear constraints + projected p preserve)
            // half-step momentum again
            grad_u(&f_lf, g, &caps, beta, mu, &mut grad, &mut f_per);
            for kk in 0..k {
                let (s_kk, t_kk) = g.commodities[kk];
                let gk = &mut grad[kk * m..(kk + 1) * m];
                total_pcg += project_to_div_free(g, gk, s_kk, t_kk, &csr_unit);
            }
            for i in 0..k * m { p_lf[i] -= 0.5 * step * grad[i]; }

            // Sanity check
            if f_lf.iter().any(|x| !x.is_finite()) || p_lf.iter().any(|x| !x.is_finite()) {
                diverged = true;
                break;
            }
            let _ = lf;
        }

        proposals += 1;
        if !diverged {
            let u1 = potential_u(&f_lf, g, &caps, beta, mu);
            let kin1: f64 = p_lf.iter().map(|x| 0.5 * x * x).sum();
            if u1.is_finite() {
                let dh = (u1 + kin1) - (u0 + kin0);
                if dh <= 0.0 || rng.unit() < (-dh).exp() {
                    f = f_lf;
                    accept = true;
                }
            }
        }

        if accept {
            accepts += 1;
            // Recompute throughputs
            grad_u(&f, g, &caps, beta, mu, &mut grad, &mut f_per);
            let total: f64 = f_per.iter().sum();
            if total > f_best_total {
                f_best_total = total;
                f_best_per.copy_from_slice(&f_per);
            }
        } else {
            f = f0;
        }
    }

    HmcResult {
        f_per: f_best_per,
        f_total: f_best_total,
        accepts,
        proposals,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(20);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(60);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(2);
    let n_samples: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(200);
    let n_leapfrog: usize = args.iter().find_map(|a| a.strip_prefix("--L=")?.parse().ok()).unwrap_or(20);
    let step: f64 = args.iter().find_map(|a| a.strip_prefix("--step=")?.parse().ok()).unwrap_or(0.05);
    let beta_lo: f64 = args.iter().find_map(|a| a.strip_prefix("--bmin=")?.parse().ok()).unwrap_or(0.1);
    let beta_hi: f64 = args.iter().find_map(|a| a.strip_prefix("--bmax=")?.parse().ok()).unwrap_or(5.0);
    let mu: f64 = args.iter().find_map(|a| a.strip_prefix("--mu=")?.parse().ok()).unwrap_or(0.5);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T={n_samples}, L={n_leapfrog}, step={step}, β=[{beta_lo}, {beta_hi}], μ={mu}",
        g.edges.len());

    let res = spring_hmc_mc(&g, n_samples, n_leapfrog, step, beta_lo, beta_hi, mu, seed);
    let acc = res.accepts as f64 / res.proposals.max(1) as f64;
    println!("\n[Spring HMC MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  acc={:.1}%, proposals={}, pcg={}, time={:.1} ms",
        acc * 100.0, res.proposals, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
