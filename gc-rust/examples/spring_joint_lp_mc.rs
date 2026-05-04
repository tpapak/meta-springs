//! Joint multi-axis spring max-flow with TRUE all-at-once joint α step.
//!
//! Per outer iter:
//!   1. Build shared-residual Laplacian (joint stiffness across K).
//!   2. K simultaneous PCG solves (K RHS).
//!   3. Linearise capacity constraint at the current point:
//!        |f_k,e + α_k d_k,e| ≈ s_k,e · (f_k,e + α_k d_k,e)
//!      where s_k,e = sign(f_k,e) (or sign(d_k,e) if f_k,e = 0).
//!      Edge constraint becomes  Σ_k h_k,e · α_k ≤ slack_e
//!      with h_k,e = s_k,e · d_k,e and slack_e = c_e − Σ_k |f_k,e|.
//!   4. Solve K-var LP via log-barrier interior-point:
//!        max Σ α_k  s.t.  A α ≤ b, α ≥ 0
//!   5. Augment with the joint α* (all commodities at once, single shot).
//!
//! This is the "all-at-once" version: every commodity gets its own α
//! found jointly, in one tiny LP per outer iter.

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

/// Joint α LP: max Σ α_k  s.t.  A α ≤ b, α ≥ 0
/// Solved via log-barrier IPM (one Newton step per outer; few outer steps).
/// `a_rows`[e] is K-vector A[e, :] (h_{k,e}); `b`[e] is slack_e.
/// K is small (<=50), m moderate. Per IPM iter is O(K² + K·m).
fn solve_joint_alpha_lp(
    a_rows: &[Vec<f64>],
    b: &[f64],
    k: usize,
    n_outer: usize,
    n_newton: usize,
) -> Vec<f64> {
    let m = a_rows.len();
    if k == 0 { return vec![]; }
    // Find a strict-interior start: scale ε so all constraints are satisfied with margin.
    let mut alpha = vec![0.0_f64; k];
    // Verify A·0 ≤ b? Yes since b ≥ 0 (slack non-negative). But we need strict <.
    let mut violated = false;
    for e in 0..m { if b[e] <= 1e-12 { violated = true; break; } }
    let _ = violated;

    let mut mu = 1.0_f64;
    for _outer in 0..n_outer {
        for _newton in 0..n_newton {
            // residual_e = b_e - <a_e, α>
            // gradient = -1 + μ · Σ_e a_e / r_e   (we minimise -Σα_k + μ·barrier)
            // ALSO α_k ≥ 0: barrier -μ Σ log(α_k); add μ·(-1/α_k) to gradient
            let mut r = vec![0.0_f64; m];
            let mut feasible = true;
            for e in 0..m {
                let mut ax = 0.0;
                for kk in 0..k { ax += a_rows[e][kk] * alpha[kk]; }
                r[e] = b[e] - ax;
                if r[e] <= 0.0 { feasible = false; break; }
            }
            if !feasible { break; }

            let mut grad = vec![-1.0_f64; k];
            for e in 0..m {
                for kk in 0..k {
                    grad[kk] += mu * a_rows[e][kk] / r[e];
                }
            }
            for kk in 0..k {
                let a_clamped = alpha[kk].max(1e-12);
                grad[kk] -= mu / a_clamped;
            }

            // Approximate Hessian diag (Jacobi-preconditioned gradient step)
            let mut hess_diag = vec![0.0_f64; k];
            for e in 0..m {
                let inv_r2 = 1.0 / (r[e] * r[e]);
                for kk in 0..k {
                    hess_diag[kk] += mu * a_rows[e][kk] * a_rows[e][kk] * inv_r2;
                }
            }
            for kk in 0..k {
                let a_clamped = alpha[kk].max(1e-12);
                hess_diag[kk] += mu / (a_clamped * a_clamped);
            }

            let mut delta = vec![0.0_f64; k];
            for kk in 0..k {
                if hess_diag[kk] > 1e-30 { delta[kk] = -grad[kk] / hess_diag[kk]; }
            }

            // Backtracking line search to keep r > 0 and α ≥ 0.
            let mut t = 1.0_f64;
            for _ in 0..30 {
                let mut ok = true;
                for kk in 0..k {
                    if alpha[kk] + t * delta[kk] <= 1e-15 { ok = false; break; }
                }
                if ok {
                    for e in 0..m {
                        let mut new_ax = 0.0;
                        for kk in 0..k { new_ax += a_rows[e][kk] * (alpha[kk] + t * delta[kk]); }
                        if b[e] - new_ax <= 1e-15 { ok = false; break; }
                    }
                }
                if ok { break; }
                t *= 0.5;
            }
            if t < 1e-12 { break; }
            for kk in 0..k { alpha[kk] += t * delta[kk]; }

            // Convergence: gradient small.
            let gn: f64 = grad.iter().map(|g| g * g).sum::<f64>().sqrt();
            if gn < 1e-8 { break; }
        }
        mu *= 0.1;
    }
    // Clamp tiny negatives.
    for kk in 0..k { if alpha[kk] < 0.0 { alpha[kk] = 0.0; } }
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

fn spring_joint_lp_mc(
    g: &Graph,
    n_iters: usize,
    lp_outer: usize,
    lp_newton: usize,
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

        // Build linearised LP rows.
        // h_{k,e} = sign(f_k,e)·d_{k,e} (or sign(d_{k,e}) if f_k,e ≈ 0)
        // slack_e = c_e − Σ_k |f_k,e|
        let mut a_rows: Vec<Vec<f64>> = Vec::with_capacity(m);
        let mut b_lp: Vec<f64> = Vec::with_capacity(m);
        for e in 0..m {
            let mut row = vec![0.0_f64; k];
            let mut used = 0.0;
            for kk in 0..k {
                let f_e = f[kk * m + e];
                used += f_e.abs();
                let sgn = if f_e.abs() > 1e-12 { f_e.signum() } else { d_per[kk][e].signum() };
                row[kk] = sgn * d_per[kk][e];
            }
            // Drop dead commodities (div_s != 1) by zeroing their column.
            for kk in 0..k {
                if (div_s_per[kk] - 1.0).abs() >= 1e-3 { row[kk] = 0.0; }
            }
            let slack = (caps[e] - used).max(1e-9);
            a_rows.push(row);
            b_lp.push(slack);
        }

        // Solve joint α LP in Rust (interior-point).
        let mut alpha_lp = solve_joint_alpha_lp(&a_rows, &b_lp, k, lp_outer, lp_newton);

        // Zero out dead commodities defensively.
        for kk in 0..k {
            if (div_s_per[kk] - 1.0).abs() >= 1e-3 { alpha_lp[kk] = 0.0; }
        }

        // Verify feasibility — if linearisation overshoots due to sign flip,
        // backtrack scalar on alpha_lp until Σ_k |f_k,e + α_k d_k,e| ≤ c_e.
        let mut t_back = 1.0_f64;
        for _ in 0..40 {
            let mut ok = true;
            for e in 0..m {
                let mut s = 0.0;
                for kk in 0..k { s += (f[kk * m + e] + t_back * alpha_lp[kk] * d_per[kk][e]).abs(); }
                if s > caps[e] + 1e-6 { ok = false; break; }
            }
            if ok { break; }
            t_back *= 0.7;
        }
        let total_step: f64 = alpha_lp.iter().sum::<f64>() * t_back;
        if total_step < 1e-10 {
            consecutive_zero += 1;
            if consecutive_zero >= 3 { break; }
            continue;
        }
        consecutive_zero = 0;

        for kk in 0..k {
            if alpha_lp[kk] <= 0.0 { continue; }
            let a = alpha_lp[kk] * t_back;
            for e in 0..m { f[kk * m + e] += a * d_per[kk][e]; }
            f_per_total[kk] += a * div_s_per[kk];
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
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(200);
    let lp_outer: usize = args.iter().find_map(|a| a.strip_prefix("--outer=")?.parse().ok()).unwrap_or(8);
    let lp_newton: usize = args.iter().find_map(|a| a.strip_prefix("--newton=")?.parse().ok()).unwrap_or(15);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T={n_iters}, lp_outer={lp_outer}, lp_newton={lp_newton}, seed={seed}",
        g.edges.len());

    let res = spring_joint_lp_mc(&g, n_iters, lp_outer, lp_newton, seed);
    println!("\n[Spring JOINT-LP MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  augs={}, pcg={}, time={:.1} ms", res.augmentations, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
