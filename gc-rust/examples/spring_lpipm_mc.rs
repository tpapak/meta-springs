//! Slack-variable LP-IPM for multi-commodity max-flow, solved through
//! the multi-axis spring framework.
//!
//! VARIABLE EXPANSION (textbook L1-norm via slacks):
//!   For each (commodity k, edge e), introduce slack t_{k,e} ≥ 0 with the
//!   pair of constraints
//!       t_{k,e} ≥  f_{k,e}    (equivalently  t − f ≥ 0)
//!       t_{k,e} ≥ -f_{k,e}    (equivalently  t + f ≥ 0)
//!   Capacity becomes
//!       Σ_k t_{k,e} ≤ c_e
//!   Conservation constraints are unchanged (act on f only).
//!
//! BARRIER:
//!   B(f, t) = -Σ_{k,e} log(t_{k,e} - f_{k,e})   (slack ≥ flow)
//!           − Σ_{k,e} log(t_{k,e} + f_{k,e})   (slack ≥ -flow)
//!           − Σ_e     log(c_e − Σ_k t_{k,e})   (capacity)
//!
//!   Per-edge Hessian over (f, t) ∈ R^{2K}:
//!     H_ff = diag(α_k),        α_k = 1/(t−f)² + 1/(t+f)²
//!     H_tt = diag(β_k) + γ 1 1ᵀ,
//!                              β_k = 1/(t−f)² + 1/(t+f)²,
//!                              γ   = 1/(c − Σt)²
//!     H_ft = diag(δ_k),        δ_k = -1/(t−f)² + 1/(t+f)²
//!
//!   This is full-rank (PD) at every interior (t > |f|, Σt < c) point —
//!   the rank-deficiency that broke the smooth-abs barrier is gone.
//!
//! NEWTON STEP — TWO-STAGE SCHUR:
//!   (1) Schur over t (per-edge K×K, Sherman-Morrison) gives effective
//!       H_eff = H_ff − H_ft H_tt^{-1} H_tf  =  D' + r rᵀ
//!       where D'_k = (β² − δ²)/β   (provably > 0)
//!             r    = √(γ/(1+γ Σ 1/β)) · (δ/β)
//!       and effective gradient g_eff = g_f − H_ft H_tt^{-1} g_t.
//!   (2) Schur over conservation Δf-constraints uses the multi-axis
//!       block-Laplacian L_block = Σ_e B_eᵀ H_eff^{-1} B_e, ONE Cholesky,
//!       with per-axis pinning of (s_k, k) and (t_k, k) inside the SINGLE
//!       (n·K)×(n·K) matrix.
//!   (3) Recover Δt per edge from
//!       Δt = -H_tt^{-1} (H_tf Δf + g_t).
//!
//! μ → 0 schedule drives the central path to the LP vertex; full-rank
//! Hessian means no rank-deficient lockout of the "all-commodities-
//! together" direction. Should reach LP without polish.

use std::process::Command;
use std::time::Instant;

use nalgebra::{Cholesky, DMatrix, DVector};

// ---------- RNG / graph ----------

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

// ---------- Gurobi LP oracle ----------

fn run_lp_oracle_gurobi(g: &Graph) -> Option<(f64, Vec<f64>, f64)> {
    let json_in = serde_json::json!({
        "n": g.n,
        "edges": g.edges.iter().map(|&(u,v,c)| [u, v, c as usize]).collect::<Vec<_>>(),
        "commodities": g.commodities.iter().map(|&(s,t)| [s, t]).collect::<Vec<_>>(),
    });
    let tmp = std::env::temp_dir().join(format!("mc_lp_{}.json", std::process::id()));
    std::fs::write(&tmp, json_in.to_string()).ok()?;
    let t0 = Instant::now();
    let out = Command::new("/opt/homebrew/Caskroom/miniconda/base/bin/python")
        .arg("/Users/tosku/Sync/Documents/slmm/gc-rust/examples/mc_lp_oracle_gurobi.py")
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

// ---------- Augmented Hamiltonian U(f, t) ----------

fn potential_u(f: &[f64], t: &[f64], g: &Graph, caps: &[f64], mu: f64) -> f64 {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut barrier = 0.0;
    for e in 0..m {
        let mut sum_t = 0.0;
        for kk in 0..k {
            let f_ke = f[kk * m + e];
            let t_ke = t[kk * m + e];
            let tmf = t_ke - f_ke;
            let tpf = t_ke + f_ke;
            if tmf <= 0.0 || tpf <= 0.0 || t_ke < 0.0 { return f64::INFINITY; }
            barrier += -tmf.ln() - tpf.ln();
            sum_t += t_ke;
        }
        let slack = caps[e] - sum_t;
        if slack <= 0.0 { return f64::INFINITY; }
        barrier += -slack.ln();
    }
    let mut f_total = 0.0;
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }
        }
        f_total += div_s;
    }
    -f_total + mu * barrier
}

// ---------- Newton step ----------

/// One Newton step on (f, t) under the slack-variable LP-IPM Hamiltonian.
/// Two-stage Schur: per-edge K×K reduction over t, then multi-axis
/// block-Laplacian solve over the conservation constraint on f.
/// Writes Δf and Δt; returns gradient norm (gradient on f, post-t-Schur).
fn newton_step(
    f: &[f64],
    t: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    df: &mut [f64],
    dt: &mut [f64],
) -> Option<f64> {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();

    // Per-edge α, β, δ, γ and their consequences.
    // Plus per-edge gradient g_f, g_t (barrier parts; throughput added to g_f).
    let mut alpha = vec![0.0_f64; k * m];
    let mut beta = vec![0.0_f64; k * m];
    let mut delta = vec![0.0_f64; k * m];
    let mut gamma = vec![0.0_f64; m];
    let mut g_f = vec![0.0_f64; k * m];
    let mut g_t = vec![0.0_f64; k * m];

    for (e, _) in g.edges.iter().enumerate() {
        let mut sum_t = 0.0;
        for kk in 0..k { sum_t += t[kk * m + e]; }
        let slack = caps[e] - sum_t;
        let inv_slack = 1.0 / slack;
        gamma[e] = inv_slack * inv_slack;
        for kk in 0..k {
            let f_ke = f[kk * m + e];
            let t_ke = t[kk * m + e];
            let inv_tmf = 1.0 / (t_ke - f_ke);
            let inv_tpf = 1.0 / (t_ke + f_ke);
            let inv_tmf_sq = inv_tmf * inv_tmf;
            let inv_tpf_sq = inv_tpf * inv_tpf;
            // Hessian entries (per-edge K×K block before cross-edge assembly).
            alpha[kk * m + e] = inv_tmf_sq + inv_tpf_sq;
            beta[kk * m + e]  = inv_tmf_sq + inv_tpf_sq;
            delta[kk * m + e] = -inv_tmf_sq + inv_tpf_sq;
            // Gradient (barrier part, will add throughput below).
            //   ∂B/∂f = +1/(t-f) - 1/(t+f)    (= -ln(t-f) deriv minus -ln(t+f) deriv)
            //   ∂B/∂t = -1/(t-f) - 1/(t+f) + 1/(c-Σt)  (capacity included)
            g_f[kk * m + e] = mu * (inv_tmf - inv_tpf);
            g_t[kk * m + e] = mu * (-inv_tmf - inv_tpf + inv_slack);
        }
    }
    // Throughput contribution to g_f: g_f += -∂F/∂f.
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let throughput_grad = if u == s_kk { 1.0 } else if v == s_kk { -1.0 } else { 0.0 };
            g_f[kk * m + e] -= throughput_grad;
        }
    }

    // Per-edge effective compliance H_eff^{-1} via two-step inversion.
    //   H_eff = D' + r r^T  where D'_k = (β²−δ²)/β, r_k = √(γ/(1+γ·Σ 1/β)) · (δ/β).
    //   H_eff^{-1} = D'^{-1} - (D'^{-1} r r^T D'^{-1})/(1 + r^T D'^{-1} r)
    // We also need g_eff = g_f - H_ft H_tt^{-1} g_t where H_ft = diag(δ),
    //   H_tt^{-1} = diag(1/β) - (γ/(1+γ Σ 1/β))(1/β)(1/β)^T,
    //   so H_ft H_tt^{-1} g_t per axis k:
    //     = δ_k [g_t_k/β_k - (1/β_k)(γ/(1+γ Σ 1/β)) Σ_j g_t_j/β_j].
    let mut s_e: Vec<DMatrix<f64>> = Vec::with_capacity(m);
    let mut g_eff = vec![0.0_f64; k * m];
    for e in 0..m {
        // Per-edge K-vectors.
        let mut d_prime = DVector::<f64>::zeros(k);
        let mut rvec    = DVector::<f64>::zeros(k);
        let mut sum_inv_beta = 0.0_f64;
        let mut sum_gt_over_beta = 0.0_f64;
        for kk in 0..k {
            let b = beta[kk * m + e];
            sum_inv_beta += 1.0 / b;
            sum_gt_over_beta += g_t[kk * m + e] / b;
        }
        let denom = 1.0 + gamma[e] * sum_inv_beta;
        let scale = (gamma[e] / denom).sqrt();
        let woodbury_coef = gamma[e] / denom; // for g_t correction
        for kk in 0..k {
            let a = alpha[kk * m + e];
            let b = beta[kk * m + e];
            let d = delta[kk * m + e];
            let dp = (b * b - d * d) / b; // β² − δ² over β
            d_prime[kk] = dp.max(1e-14);
            rvec[kk] = scale * (d / b);
            // g_eff contribution per axis kk:
            //   H_ft H_tt^{-1} g_t at kk = δ_k · [(1/β_k) g_t_k − (1/β_k) · woodbury_coef · Σ_j g_t_j/β_j]
            let gt_corrected = g_t[kk * m + e] / b
                - (1.0 / b) * woodbury_coef * sum_gt_over_beta;
            g_eff[kk * m + e] = g_f[kk * m + e] - d * gt_corrected;
            let _ = a;
        }
        // Build H_eff^{-1} = D'^{-1} - (D'^{-1} r r^T D'^{-1})/(1 + r^T D'^{-1} r).
        let mut a_vec = DVector::<f64>::zeros(k);
        let mut p = 0.0_f64;
        for kk in 0..k {
            a_vec[kk] = rvec[kk] / d_prime[kk];
            p += rvec[kk] * a_vec[kk];
        }
        let denom_inner = 1.0 + p;
        let mut s_mat = DMatrix::<f64>::zeros(k, k);
        for ki in 0..k {
            s_mat[(ki, ki)] = 1.0 / d_prime[ki];
            for kj in 0..k {
                s_mat[(ki, kj)] -= a_vec[ki] * a_vec[kj] / denom_inner;
            }
        }
        s_e.push(s_mat);
    }

    // Build the multi-axis block-Laplacian Σ_e B_eᵀ H_eff^{-1} B_e.
    let dim = n * k;
    let mut a = DMatrix::<f64>::zeros(dim, dim);
    let row_of = |vert: usize, com: usize| vert * k + com;
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        let s = &s_e[e];
        for (sa_, ta) in [(-1.0_f64, v_idx), (1.0, u)] {
            for (sb_, tb) in [(-1.0_f64, v_idx), (1.0, u)] {
                let scalar = sa_ * sb_;
                for ki in 0..k {
                    for kj in 0..k {
                        a[(row_of(ta, ki), row_of(tb, kj))] += scalar * s[(ki, kj)];
                    }
                }
            }
        }
    }

    // Schur RHS: rhs = -C H_eff^{-1} g_eff (matches sign convention used elsewhere).
    let mut v_per_edge = vec![0.0_f64; k * m];
    for e in 0..m {
        let s = &s_e[e];
        for ki in 0..k {
            let mut acc = 0.0;
            for kj in 0..k {
                acc += s[(ki, kj)] * g_eff[kj * m + e];
            }
            v_per_edge[ki * m + e] = acc;
        }
    }
    let mut rhs = DVector::<f64>::zeros(dim);
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        for ki in 0..k {
            let val = v_per_edge[ki * m + e];
            rhs[row_of(u, ki)] -= val;
            rhs[row_of(v_idx, ki)] += val;
        }
    }

    // Per-axis pinning of (s_k, k) and (t_k, k) — single matrix.
    for ki in 0..k {
        let (s_k, t_k) = g.commodities[ki];
        for &pv in &[s_k, t_k] {
            let r = row_of(pv, ki);
            for c in 0..dim { a[(r, c)] = 0.0; a[(c, r)] = 0.0; }
            a[(r, r)] = 1.0;
            rhs[r] = 0.0;
        }
    }
    // Ridge for numerical safety.
    let ridge = 1e-12 * a.diagonal().abs().max();
    for i in 0..dim { a[(i, i)] += ridge; }

    let chol = Cholesky::new(a)?;
    let lambda = chol.solve(&rhs);

    // Δf per edge: Δf = -H_eff^{-1} (g_eff + B^T λ).
    for kk in 0..k * m { df[kk] = 0.0; }
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        let s = &s_e[e];
        let mut h_e = DVector::<f64>::zeros(k);
        for ki in 0..k {
            h_e[ki] = g_eff[ki * m + e] + (lambda[row_of(u, ki)] - lambda[row_of(v_idx, ki)]);
        }
        let de = -(s * &h_e);
        for ki in 0..k { df[ki * m + e] = de[ki]; }
    }

    // Δt per edge: Δt = -H_tt^{-1} (H_tf Δf + g_t).
    //   H_tt^{-1} v = diag(1/β) v - (γ/(1+γΣ1/β)) (1/β) (Σ_j v_j/β_j) (1/β)... wait
    //   Sherman-Morrison on (diag(β) + γ 1 1^T):
    //     H_tt^{-1} v = D^{-1} v - (γ/(1+γ Σ 1/β)) D^{-1} 1 (1^T D^{-1} v)
    //     where D = diag(β). 1^T D^{-1} v = Σ v_j/β_j.
    for kk in 0..k * m { dt[kk] = 0.0; }
    for e in 0..m {
        let mut sum_inv_beta = 0.0_f64;
        for kk in 0..k { sum_inv_beta += 1.0 / beta[kk * m + e]; }
        let denom = 1.0 + gamma[e] * sum_inv_beta;
        let coef = gamma[e] / denom;
        // arg = H_tf Δf + g_t = diag(δ) Δf + g_t   (per axis)
        let mut arg_over_beta_sum = 0.0_f64;
        let mut arg = vec![0.0_f64; k];
        for kk in 0..k {
            arg[kk] = delta[kk * m + e] * df[kk * m + e] + g_t[kk * m + e];
            arg_over_beta_sum += arg[kk] / beta[kk * m + e];
        }
        for kk in 0..k {
            let unconstrained = arg[kk] / beta[kk * m + e]
                - coef * (1.0 / beta[kk * m + e]) * arg_over_beta_sum;
            dt[kk * m + e] = -unconstrained;
        }
    }

    // Gradient norm on the f-block (after t-Schur reduction).
    let grad_norm: f64 = g_eff.iter().map(|x| x * x).sum::<f64>().sqrt();
    Some(grad_norm)
}

// ---------- Top-level IPM ----------

#[derive(Clone)]
struct IpmResult {
    f_per: Vec<f64>,
    f_total: f64,
    iters: usize,
    time_ms: f64,
    max_load_frac: f64,
    max_overshoot: f64,
    max_cons_viol: f64,
}

fn solve_lpipm(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    eta_init: f64,
    verbose: bool,
) -> IpmResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let t0 = Instant::now();

    // Initial interior point: f=0, t=c/(2K). Then t-f = t+f = c/(2K) > 0, Σt=c/2 < c.
    let mut f = vec![0.0_f64; k * m];
    let mut t = vec![0.0_f64; k * m];
    for e in 0..m {
        let init_t = caps[e] / (2.0 * k as f64);
        for kk in 0..k { t[kk * m + e] = init_t; }
    }
    let mut df = vec![0.0_f64; k * m];
    let mut dt = vec![0.0_f64; k * m];
    let mut iters = 0usize;
    let mut eta = eta_init;
    for &mu in mu_schedule {
        for inner in 0..inner_iters {
            iters += 1;
            let grad_norm = match newton_step(&f, &t, g, &caps, mu, &mut df, &mut dt) {
                Some(g) => g,
                None => {
                    if verbose { eprintln!("[lpipm] Cholesky failed at mu={mu:.1e} inner={inner}"); }
                    break;
                }
            };
            if grad_norm < 1e-12 { break; }

            // Backtracking line search.
            let u_curr = potential_u(&f, &t, g, &caps, mu);
            let mut step = eta;
            let mut accepted = false;
            for _ls in 0..60 {
                let mut f_try = f.clone();
                let mut t_try = t.clone();
                for i in 0..k * m {
                    f_try[i] += step * df[i];
                    t_try[i] += step * dt[i];
                }
                let u_try = potential_u(&f_try, &t_try, g, &caps, mu);
                if u_try.is_finite() && u_try < u_curr - 1e-14 {
                    f = f_try; t = t_try; accepted = true; break;
                }
                step *= 0.5;
            }
            if !accepted {
                eta *= 0.5;
                if eta < 1e-12 { break; }
                continue;
            }
            eta = (eta * 1.5).min(eta_init);
        }
        if verbose {
            let mut f_total = 0.0;
            for kk in 0..k {
                let (s_kk, _t_kk) = g.commodities[kk];
                let mut div_s = 0.0;
                for (e, &(u, vtx, _)) in g.edges.iter().enumerate() {
                    if u == s_kk { div_s += f[kk * m + e]; }
                    else if vtx == s_kk { div_s -= f[kk * m + e]; }
                }
                f_total += div_s;
            }
            eprintln!("[lpipm] after mu={mu:.1e}: F = {:.6}", f_total);
        }
    }

    // Per-commodity throughput and feasibility.
    let mut f_per = vec![0.0_f64; k];
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }
        }
        f_per[kk] = div_s;
    }
    let f_total: f64 = f_per.iter().sum();
    let mut max_load_frac = 0.0_f64;
    let mut max_overshoot = 0.0_f64;
    for e in 0..m {
        let mut load = 0.0;
        for kk in 0..k { load += f[kk * m + e].abs(); }
        let frac = load / caps[e];
        if frac > max_load_frac { max_load_frac = frac; }
        let over = (load - caps[e]).max(0.0);
        if over > max_overshoot { max_overshoot = over; }
    }
    let mut max_cons_viol = 0.0_f64;
    for kk in 0..k {
        let (s_kk, t_kk) = g.commodities[kk];
        for v in 0..n {
            if v == s_kk || v == t_kk { continue; }
            let mut div = 0.0;
            for (e, &(u, vtx, _)) in g.edges.iter().enumerate() {
                if u == v { div += f[kk * m + e]; }
                else if vtx == v { div -= f[kk * m + e]; }
            }
            if div.abs() > max_cons_viol { max_cons_viol = div.abs(); }
        }
    }

    IpmResult {
        f_per, f_total, iters,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
        max_load_frac, max_overshoot, max_cons_viol,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(20);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(60);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(40);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");
    let verbose = args.iter().any(|a| a == "--verbose" || a == "-v");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, eta={eta}, seed={seed}", g.edges.len());

    let mu_schedule = vec![
        1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001,
        3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7, 1e-8,
    ];
    let res = solve_lpipm(&g, &mu_schedule, inner, eta, verbose);
    println!("\n[Slack-variable LP-IPM (multi-axis)]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.6}", res.f_total);
    println!("  iters   = {}, time = {:.1} ms", res.iters, res.time_ms);
    println!("  feas   max_load={:.6}  overshoot={:.3e}  cons_viol={:.3e}",
        res.max_load_frac, res.max_overshoot, res.max_cons_viol);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle_gurobi(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[Gurobi LP]");
            println!("  F_total = {:.6} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.2}×", rel, lp_ms / res.time_ms);
        }
    }
}
