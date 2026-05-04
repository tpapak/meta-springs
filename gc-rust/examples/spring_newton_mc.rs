//! Joint Hamiltonian Newton for K-axis multi-commodity springs.
//!
//! Hamiltonian (proper, fully-coupled):
//!   H(F) = -Σ_k F_k  +  μ · Σ_e V( s_e )
//!   s_e  = Σ_k F_k · |π_{k,e}| / c_e
//!   V(s) = -log(1 - s²)
//!
//! where π_k ∈ R^m is the unit-throughput spring flow for commodity k on
//! the *current* shared edge stiffness G_e = V''(s_e) / c_e². Each Newton
//! iter:
//!   1. Build G_e = V''(s_e) / c_e² from current loads
//!   2. K spring solves on shared L (K RHS, same Laplacian)
//!   3. K×K Hessian
//!        H_{k,l} = μ · Σ_e V''(s_e) · |π_{k,e}| · |π_{l,e}| / c_e²
//!      (full, not diagonal — this is the multi-commodity coupling that
//!      shared-conductance round-robin/joint-LP all missed)
//!   4. K-vector gradient
//!        g_k = -1 + μ · Σ_e V'(s_e) · |π_{k,e}| / c_e
//!   5. ΔF = -H⁻¹ g  (K×K solve via Cholesky)
//!   6. Backtrack to keep s_e < 1
//!
//! IPM annealing μ → 0 drives F to the LP optimum.

use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::CsrLap;
use nalgebra::{DMatrix, DVector};

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

#[derive(Clone)]
struct NewtonResult {
    f_per: Vec<f64>,
    f_total: f64,
    newton_iters: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_newton_mc(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    seed: u64,
) -> NewtonResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let _ = seed;

    // F_k = throughput per commodity
    let mut f_throughput = vec![0.01_f64; k];
    let mut total_pcg = 0_usize;
    let mut newton_iters = 0_usize;
    let t0 = Instant::now();

    // Per commodity, build the unit-injection RHS once.
    let b_per: Vec<Vec<f64>> = (0..k).map(|i| {
        let (s_i, _t_i) = g.commodities[i];
        let mut b = vec![0.0_f64; n];
        b[s_i] = 1.0;
        b
    }).collect();

    let mut pi_per: Vec<Vec<f64>> = vec![vec![0.0_f64; m]; k];
    let mut s_e = vec![0.0_f64; m];

    for &mu in mu_schedule {
        for _inner in 0..inner_iters {
            newton_iters += 1;

            // 1. Update s_e from current F via current π (or use last estimate).
            //    On first iter pi is zero — handle specially with uniform conductance.
            for e in 0..m {
                let mut s = 0.0;
                for i in 0..k { s += f_throughput[i] * pi_per[i][e].abs(); }
                s_e[e] = (s / caps[e]).min(0.999);
            }

            // 2. Build effective edge stiffness G_e = max(V''(s_e), 1) / c_e².
            //    On first iter (s_e=0) V''(0)=2; use that.
            let stiffness: Vec<f64> = (0..m).map(|e| {
                let s = s_e[e];
                let v2 = 2.0 * (1.0 + s * s) / ((1.0 - s * s).powi(2));
                (v2 / (caps[e] * caps[e])).max(1e-12)
            }).collect();

            // 3. K spring solves on shared L (with conductance = stiffness).
            //    Convert to "conductance-based weights" (high stiffness = high
            //    conductance for routing purposes).
            let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                .map(|(e, &(u, v, _))| (u as u32, v as u32, stiffness[e]))
                .collect();
            let csr = CsrLap::from_canonical_weights(&weighted, n);

            for i in 0..k {
                let (_s_i, t_i) = g.commodities[i];
                let (phi, pcg_it) = pcg_pinned(&csr, &b_per[i], t_i, 1e-10, 4 * n);
                total_pcg += pcg_it;
                // π_{k,e} = stiffness_e · (φ_u − φ_v)
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    pi_per[i][e] = stiffness[e] * (phi[u] - phi[v]);
                }
                // Normalise to unit throughput at source.
                let (s_i, _t_i2) = g.commodities[i];
                let mut div_s = 0.0;
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    if u == s_i { div_s += pi_per[i][e]; }
                    if v == s_i { div_s -= pi_per[i][e]; }
                }
                if div_s.abs() > 1e-12 {
                    let scale = 1.0 / div_s;
                    for e in 0..m { pi_per[i][e] *= scale; }
                }
            }

            // 4. Update s_e with new π (non-linear; we use current F & new π).
            let mut s_local = vec![0.0_f64; m];
            for e in 0..m {
                let mut s = 0.0;
                for i in 0..k { s += f_throughput[i] * pi_per[i][e].abs(); }
                s_local[e] = (s / caps[e]).min(0.999);
            }

            // 5. Per-edge V'(s) and V''(s).
            let mut vp = vec![0.0_f64; m];
            let mut vpp = vec![0.0_f64; m];
            for e in 0..m {
                let s = s_local[e];
                vp[e] = 2.0 * s / (1.0 - s * s);
                vpp[e] = 2.0 * (1.0 + s * s) / ((1.0 - s * s).powi(2));
            }

            // 6. K-vector gradient g_k = -1 + μ · Σ_e V'(s_e) · |π_{k,e}|/c_e
            let mut grad = DVector::zeros(k);
            for i in 0..k {
                let mut g = -1.0;
                for e in 0..m {
                    g += mu * vp[e] * pi_per[i][e].abs() / caps[e];
                }
                grad[i] = g;
            }

            // 7. K×K Hessian H_{k,l} = μ · Σ_e V''(s_e) · |π_{k,e}|·|π_{l,e}|/c_e²
            //    + small ridge for numerical PSD.
            let mut hess = DMatrix::zeros(k, k);
            for i in 0..k {
                for j in 0..k {
                    let mut h = 0.0;
                    for e in 0..m {
                        h += mu * vpp[e] * pi_per[i][e].abs() * pi_per[j][e].abs()
                              / (caps[e] * caps[e]);
                    }
                    hess[(i, j)] = h;
                }
                hess[(i, i)] += 1e-8;
            }

            // 8. ΔF = -H⁻¹ g via Cholesky.
            let chol = match hess.clone().cholesky() {
                Some(c) => c,
                None => break,
            };
            let delta = chol.solve(&(-grad.clone()));

            // 9. Backtracking line search keeping s_e < 1.
            let mut t = 1.0_f64;
            for _ls in 0..40 {
                let mut ok = true;
                for e in 0..m {
                    let mut s_new = 0.0;
                    for i in 0..k {
                        let f_new = (f_throughput[i] + t * delta[i]).max(0.0);
                        s_new += f_new * pi_per[i][e].abs();
                    }
                    if s_new / caps[e] >= 0.97 { ok = false; break; }
                }
                if ok { break; }
                t *= 0.5;
            }
            if t < 1e-12 { break; }

            for i in 0..k {
                f_throughput[i] = (f_throughput[i] + t * delta[i]).max(0.0);
            }

            if std::env::var("NEWTON_DEBUG").is_ok() {
                let f_total: f64 = f_throughput.iter().sum();
                let grad_norm = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
                let delta_norm = delta.iter().map(|x| x * x).sum::<f64>().sqrt();
                println!("  μ={:.0e} nit={} t={:.2e} |g|={:.2e} |Δ|={:.2e} F={:.3} F_per={:?}",
                    mu, newton_iters, t, grad_norm, delta_norm, f_total,
                    f_throughput.iter().map(|x| format!("{x:.2}")).collect::<Vec<_>>());
            }

            let grad_norm = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
            if grad_norm < 1e-7 { break; }
        }
    }

    let f_total: f64 = f_throughput.iter().sum();
    NewtonResult {
        f_per: f_throughput,
        f_total,
        newton_iters,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(20);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, seed={seed}", g.edges.len());

    let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001];
    let res = spring_newton_mc(&g, &mu_schedule, inner, seed);
    println!("\n[Spring Hamiltonian NEWTON MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  newton_iters={}, pcg={}, time={:.1} ms",
        res.newton_iters, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
