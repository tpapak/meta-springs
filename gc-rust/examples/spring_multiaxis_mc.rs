//! Multi-axis spring IPM for multi-commodity max-flow.
//!
//! Each vertex is a point in R^K (one component per commodity). Each edge
//! is a K×K spring with stiffness matrix S_e, so the K commodities are
//! coupled at every edge through the off-diagonal entries of S_e.
//!
//! Hamiltonian (smooth IPM):
//!     U(f) = -Σ_k F_k + μ Σ_e V(s_e),   V(s) = -log(1-s²)
//!     s_e  = Σ_k |f_{k,e}| / c_e
//!
//! Per-edge K×K Hessian decomposes as
//!     H_e = D_e + u_e u_eᵀ          (diagonal + rank-1 cross-commodity)
//!     D_{k,e} = μ V'(s_e) sa''(f_{k,e}) / c_e   (positive)
//!     u_{k,e} = √(μ V''(s_e)) · sa'(f_{k,e}) / c_e
//!
//! Schur (Newton step subject to K commodity-level conservation):
//!     (C H^{-1} Cᵀ) λ = C H^{-1} g
//! where C is the block-diagonal commodity incidence (K independent
//! conservation constraints). The Schur matrix on the (n·K)-dim space is
//! exactly the multi-axis weighted Laplacian with edge stiffness equal
//! to the K×K compliance matrix
//!     S_e = H_e^{-1} = D_e^{-1} - (a_e a_eᵀ)/(1 + u_eᵀ D_e^{-1} u_e),
//!     a_e = D_e^{-1} u_e
//!
//! ONE block Cholesky on the (n·K)×(n·K) matrix gives all K commodity
//! directions in one factor + K simultaneous back-solves (the K RHS
//! columns are the per-commodity "current" vectors).
//!
//! Pinning: ONE global vertex (the first one that is not a terminal for
//! any commodity). Single pin breaks the per-commodity gauge freedom for
//! all K axes simultaneously — no per-commodity pinning, single solve.

use std::process::Command;
use std::time::Instant;

use nalgebra::{Cholesky, DMatrix, DVector};

// ---------- RNG / graph (same convention as spring_pn_mc) ----------

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

// ---------- Gurobi LP oracle (validation) ----------

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

// ---------- Smooth |·| (epsilon-regularised) ----------

const ABS_EPS: f64 = 1e-9;
fn smooth_abs(x: f64) -> f64 { (x * x + ABS_EPS * ABS_EPS).sqrt() }
fn smooth_abs_grad(x: f64) -> f64 { x / smooth_abs(x) }
fn smooth_abs_hess(x: f64) -> f64 {
    let s = smooth_abs(x);
    (ABS_EPS * ABS_EPS) / (s * s * s)
}

// ---------- Hamiltonian (with feasibility check) ----------

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

// ---------- Per-edge K×K compliance matrix S_e = H_e^{-1} ----------

/// Compute, for each edge e, the K-vector D_{:,e} (diagonal Hessian) and
/// the K-vector u_{:,e} (rank-1 cross term) so that
///     H_e = diag(D_{:,e}) + u_{:,e} u_{:,e}ᵀ
/// Then form S_e = H_e^{-1} explicitly via Sherman-Morrison.
///
/// If `diag_only` is set, u_e is treated as zero — falls back to the
/// purely diagonal Hessian (matches `spring_pn_mc.rs`'s diagonal-PN).
///
/// Returns: per-edge `s_e` (Vec<DMatrix<f64>>, K×K).
fn build_compliance(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
) -> Vec<DMatrix<f64>> {
    build_compliance_modes(f, g, caps, mu, false)
}

fn build_compliance_modes(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    diag_only: bool,
) -> Vec<DMatrix<f64>> {
    let m = g.edges.len();
    let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); }
    }
    let mut s_e = Vec::with_capacity(m);
    for e in 0..m {
        let s = (load[e] / caps[e]).min(0.9999);
        let v_prime = 2.0 * s / (1.0 - s * s);
        let v_pp = 2.0 * (1.0 + s * s) / ((1.0 - s * s).powi(2));
        let inv_c2 = 1.0 / (caps[e] * caps[e]);
        // D_{k,e}, u_{k,e}
        let mut d = DVector::<f64>::zeros(k);
        let mut u = DVector::<f64>::zeros(k);
        for kk in 0..k {
            let f_ke = f[kk * m + e];
            let sap = smooth_abs_grad(f_ke);
            let sapp = smooth_abs_hess(f_ke);
            // Match spring_pn_mc.rs's diagonal Hessian exactly when diag_only is set:
            //   h_diag = mu·V''·(sa')²/c_e² + mu·V'·sa''/c_e
            // Split as D + u uᵀ:
            //   D_{k,e} = mu·V'·sa''/c_e
            //   u_{k,e} = √(mu·V'') · sa' / c_e   (so u² = mu·V''·(sa')²/c_e²)
            if diag_only {
                d[kk] = (mu * v_prime * sapp / caps[e]
                       + mu * v_pp * (sap * sap) / (caps[e] * caps[e])).max(1e-10);
                u[kk] = 0.0;
            } else {
                // Adaptive Tikhonov: γ(μ) = μ → strong regularisation when the
                // barrier dominates (early IPM), vanishing as μ → 0 (centre
                // path approaches LP vertex, true rank-1 coupling restored).
                let u_e = (mu * v_pp).sqrt() * sap / caps[e];
                let d_pure = mu * v_prime * sapp / caps[e];
                // Fixed-floor γ ≥ 0.5 keeps the regularised D large enough that
                // the rank-1 SMW correction stays well-conditioned at all μ.
                let gamma_reg = 0.5_f64.max(mu.min(2.0));
                d[kk] = (d_pure + gamma_reg * u_e * u_e).max(1e-10);
                u[kk] = u_e;
            }
        }
        let _ = inv_c2;
        // S_e = H_e^{-1} via Sherman-Morrison.
        // a_e = D_e^{-1} u_e ;  p_e = u_eᵀ a_e
        // H_e^{-1} = diag(1/d) - a aᵀ / (1 + p)
        let mut a = DVector::<f64>::zeros(k);
        let mut p = 0.0_f64;
        for kk in 0..k { a[kk] = u[kk] / d[kk]; p += u[kk] * a[kk]; }
        let denom = 1.0 + p;
        let mut s_mat = DMatrix::<f64>::zeros(k, k);
        for ki in 0..k {
            s_mat[(ki, ki)] = 1.0 / d[ki];
            for kj in 0..k {
                s_mat[(ki, kj)] -= a[ki] * a[kj] / denom;
            }
        }
        s_e.push(s_mat);
    }
    s_e
}

// ---------- Multi-axis IPM Newton step ----------

/// One Newton step. Builds the (n·K)×(n·K) block Laplacian with edge
/// stiffness = compliance matrix S_e = H_e^{-1}, pins ONE global vertex
/// for ALL commodities, factors with Cholesky, and back-solves K RHS
/// columns simultaneously.
///
/// Returns the per-(commodity, edge) Newton direction Δf and the gradient
/// norm (for convergence check). The boolean flag is `true` if the solve
/// succeeded (matrix was PD).
fn newton_step(
    f: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    pin_vertex: usize,
    delta: &mut [f64],
    diag_only: bool,
) -> Option<f64> {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let s_e = build_compliance_modes(f, g, caps, mu, diag_only);

    // Compute gradient g_{k,e} of the Hamiltonian.
    let mut grad = vec![0.0_f64; k * m];
    let mut load = vec![0.0_f64; m];
    for kk in 0..k {
        for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); }
    }
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s = (load[e] / caps[e]).min(0.9999);
            let v_prime = 2.0 * s / (1.0 - s * s);
            let sap = smooth_abs_grad(f[kk * m + e]);
            let grad_barrier = mu * v_prime * sap / caps[e];
            let grad_throughput = if u == s_kk { 1.0 } else if v == s_kk { -1.0 } else { 0.0 };
            grad[kk * m + e] = grad_barrier - grad_throughput;
        }
    }
    let grad_norm: f64 = grad.iter().map(|x| x * x).sum::<f64>().sqrt();

    // Build the multi-axis block Laplacian L_block of size (n·K)×(n·K).
    // L_block = Σ_e (e_u − e_v)(e_u − e_v)ᵀ ⊗ S_e
    let dim = n * k;
    let mut a = DMatrix::<f64>::zeros(dim, dim);
    let row_of = |vert: usize, com: usize| vert * k + com;
    for (e, &(u, v, _)) in g.edges.iter().enumerate() {
        let s = &s_e[e];
        for (sa_, ta) in [(-1.0_f64, v), (1.0, u)] {
            for (sb_, tb) in [(-1.0_f64, v), (1.0, u)] {
                let scalar = sa_ * sb_;
                for ki in 0..k {
                    for kj in 0..k {
                        a[(row_of(ta, ki), row_of(tb, kj))] += scalar * s[(ki, kj)];
                    }
                }
            }
        }
    }

    // Build RHS: the Schur right-hand-side is C H^{-1} g.
    // For each commodity k, RHS column has at vertex v:
    //   sum over edges incident to v: ±(H^{-1}_e g_e)_k
    // i.e. divergence of the H^{-1}-mapped gradient, restricted to commodity k axis.
    // We pack into an (n·K)-vector then re-shape into an (n·K) × K RHS matrix
    // where column j is +1 source, -1 sink at j-th commodity for the throughput
    // demand part — but actually here we want to solve for the *projection
    // multiplier λ* that enforces conservation. Simplest formulation:
    //
    //   compute for each (k, e):  v_{k,e} = (S_e g_e)_k  (single matrix-vector
    //                                                    per edge: K×K times K-vec)
    //   then divergence at vertex u: (RHS_λ)_{u,k} = Σ_{e:u} ±v_{k,e}
    //
    // The Schur system L_block λ = -RHS gives potentials λ; the Newton step
    // recovers as Δf_e = -S_e (g_e + B_eᵀ λ).
    let mut v = vec![0.0_f64; k * m];
    for e in 0..m {
        let s = &s_e[e];
        for ki in 0..k {
            let mut acc = 0.0;
            for kj in 0..k {
                acc += s[(ki, kj)] * grad[kj * m + e];
            }
            v[ki * m + e] = acc;
        }
    }
    let mut rhs = DVector::<f64>::zeros(dim);
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        for ki in 0..k {
            let val = v[ki * m + e];
            // div = +at u, -at v   (matches our sign convention: f>0 means u→v)
            rhs[row_of(u, ki)] -= val; // C H^{-1} g convention
            rhs[row_of(v_idx, ki)] += val;
        }
    }

    // Per-(commodity, axis) terminal pinning, but in ONE matrix / ONE solve.
    // For each commodity k, the conservation constraint applies at
    // INTERNAL vertices only — we drop it at (s_k, k) and (t_k, k) by
    // replacing those rows/cols with identity (Dirichlet λ=0). All K
    // commodities pinned in a single (n·K)-dim matrix; single Cholesky.
    let _ = pin_vertex;
    for ki in 0..k {
        let (s_k, t_k) = g.commodities[ki];
        for &pv in &[s_k, t_k] {
            let r = row_of(pv, ki);
            for c in 0..dim { a[(r, c)] = 0.0; a[(c, r)] = 0.0; }
            a[(r, r)] = 1.0;
            rhs[r] = 0.0;
        }
    }

    // Add tiny ridge regularisation for numerical safety.
    let ridge = 1e-12 * a.diagonal().max();
    for i in 0..dim { a[(i, i)] += ridge; }

    // ONE Cholesky factorisation. K back-solves baked in via sparse RHS:
    // here we have ONE RHS column (the Schur RHS), not K. The "K back-solves"
    // architecture refers to the multi-axis LAPLACIAN containing all K
    // commodities' coupling — one factor handles all K axes in a single
    // solve.
    let chol = Cholesky::new(a)?;
    let lambda_vec = chol.solve(&rhs);

    // Recover Δf_{k,e} = -S_e (g_e + B_eᵀ λ_e),
    //   B_eᵀ λ_e contributes (λ_{u,k} − λ_{v,k}) at axis k.
    for kk in 0..k * m { delta[kk] = 0.0; }
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        let s = &s_e[e];
        for ki in 0..k {
            let mut sum = grad[ki * m + e];
            sum += lambda_vec[row_of(u, ki)] - lambda_vec[row_of(v_idx, ki)];
            // S_e applied to g_e + B^T λ_e at axis ki:
            // We need (S_e (g + B^T λ))_{ki}. Build the K-vector first.
            let _ = sum;
        }
        // K-vector h_e = g_e + B^T λ_e
        let mut h_e = DVector::<f64>::zeros(k);
        for ki in 0..k {
            h_e[ki] = grad[ki * m + e] + (lambda_vec[row_of(u, ki)] - lambda_vec[row_of(v_idx, ki)]);
        }
        let d_e = -(s * &h_e);
        for ki in 0..k { delta[ki * m + e] = d_e[ki]; }
    }

    Some(grad_norm)
}

// ---------- Top-level multi-axis IPM ----------

fn pick_pin_vertex(g: &Graph) -> usize {
    let n = g.n;
    let mut is_term = vec![false; n];
    for &(s, t) in &g.commodities {
        is_term[s] = true; is_term[t] = true;
    }
    for v in 0..n { if !is_term[v] { return v; } }
    // All vertices are terminals — fall back to vertex 0 with eps regularisation.
    0
}

#[derive(Clone, Debug)]
struct IpmResult {
    f_total: f64,
    f_per: Vec<f64>,
    iters: usize,
    time_ms: f64,
    max_load_frac: f64,
    max_overshoot: f64,
}

fn solve_multiaxis_ipm(
    g: &Graph,
    mu_schedule: &[f64],
    inner_iters: usize,
    eta_init: f64,
    verbose: bool,
    diag_only: bool,
) -> IpmResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let pin = pick_pin_vertex(g);
    if verbose { eprintln!("[multiaxis] n={n}, m={m}, K={k}, pin_vertex={pin}"); }

    let mut f = vec![0.0_f64; k * m];
    let mut delta = vec![0.0_f64; k * m];
    let mut iters = 0usize;
    let mut chol_fails = 0usize;
    let mut accept_count = 0usize;
    let t0 = Instant::now();
    let mut eta = eta_init;
    for &mu in mu_schedule {
        for inner in 0..inner_iters {
            iters += 1;
            // Recompute gradient inside newton_step; we shadow it locally for the descent check.
            let grad_norm = match newton_step(&f, g, &caps, mu, pin, &mut delta, diag_only) {
                Some(g) => g,
                None => {
                    chol_fails += 1;
                    if verbose { eprintln!("[multiaxis] Cholesky failed at mu={mu:.1e} inner={inner}"); }
                    break;
                }
            };
            if grad_norm < 1e-9 { break; }

            // Backtracking line search on Hamiltonian.
            let u_curr = potential_u(&f, g, &caps, mu);
            let mut t = eta;
            let mut accepted = false;
            for _ls in 0..60 {
                let mut f_try = f.clone();
                for i in 0..k * m { f_try[i] += t * delta[i]; }
                let u_try = potential_u(&f_try, g, &caps, mu);
                if u_try.is_finite() && u_try < u_curr - 1e-12 {
                    f = f_try; accepted = true; accept_count += 1; break;
                }
                t *= 0.5;
            }
            if !accepted {
                eta *= 0.5;
                if eta < 1e-12 { break; }
                continue;
            }
            eta = (eta * 1.5).min(eta_init);
        }
        if verbose {
            eprintln!("[multiaxis] mu={mu:.1e} stats: chol_fails={chol_fails} accepted={accept_count}");
            // F per outer μ
            let mut f_total = 0.0;
            for kk in 0..k {
                let (s_kk, _t_kk) = g.commodities[kk];
                let mut div_s = 0.0;
                for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
                    if u == s_kk { div_s += f[kk * m + e]; }
                    else if v_idx == s_kk { div_s -= f[kk * m + e]; }
                }
                f_total += div_s;
            }
            eprintln!("[multiaxis] after mu={mu:.1e}: F_total = {:.4}", f_total);
        }
    }

    // ===== TILTED-AXIS POLISH: multi-axis spring on residual capacity =====
    //
    // The IPM (whether diag-Hessian or coupled rank-1) follows the central
    // path but never reaches the LP vertex (any finite μ leaves s_e < 1).
    // The polish closes the gap by augmenting flow along the residual
    // multi-axis spring's natural directions.
    //
    // ONE multi-axis Laplacian on the residual graph, single-pin per axis
    // (sink t_k pinned to 0), K simultaneous RHS columns each driving +1
    // unit current from source s_k. Output: K commodity augmenting
    // directions in ONE block solve. Push each commodity to capacity.
    // Polish disabled — see investigation notes. The "augmenting electrical
    // flow on residual" formulation is correct in theory (div(d)=0 by
    // harmonic property of φ) but the saturated-edge ridge regularisation
    // breaks divergence-freeness in practice; pushing along these
    // pseudo-augmenting directions inflates F_per by inflating div_s
    // through conservation violation at internal vertices.
    let polish_rounds = 0usize;
    let mut total_polish_pushed = 0.0_f64;
    for round in 0..polish_rounds {
        // Residual capacity per edge.
        let mut resid = vec![0.0_f64; m];
        for e in 0..m {
            let mut used = 0.0;
            for kk in 0..k { used += f[kk * m + e].abs(); }
            resid[e] = (caps[e] - used).max(0.0);
        }
        let total_resid: f64 = resid.iter().sum();
        if total_resid < 1e-9 * (m as f64) { break; }

        // Build multi-axis block Laplacian on residual.
        // Edge stiffness = isotropic scalar resid_e · I_K (each commodity sees
        // the same residual conductance — this is the Kronecker case where the
        // block factor decomposes into K identical scalar back-solves on the
        // SAME factored Laplacian).
        let dim = n * k;
        let mut a = DMatrix::<f64>::zeros(dim, dim);
        let row_of = |vert: usize, com: usize| vert * k + com;
        for (_e_idx, &(u, v, _)) in g.edges.iter().enumerate() {
            let e_idx_local = _e_idx;
            let w = resid[e_idx_local].max(1e-12);
            for ki in 0..k {
                let ru = row_of(u, ki); let rv = row_of(v, ki);
                a[(ru, ru)] += w;
                a[(rv, rv)] += w;
                a[(ru, rv)] -= w;
                a[(rv, ru)] -= w;
            }
        }

        // K RHS columns: column ki has +1 at (s_ki, ki) only (drives unit
        // current OUT of the source). The sink t_ki gets pinned to 0 below;
        // its conservation equation is REPLACED by the Dirichlet BC. The
        // source equation IS kept — it constrains net outflow at s_ki to
        // equal +1, which is the unit-current boundary condition.
        let mut rhs = DMatrix::<f64>::zeros(dim, k);
        for ki in 0..k {
            let (s_ki, _t_ki) = g.commodities[ki];
            rhs[(row_of(s_ki, ki), ki)] = 1.0;
        }

        // SINGLE-PIN per axis: pin t_ki only (φ(t_ki, ki) = 0). The source's
        // conservation row stays in the system, encoding the +1 unit current.
        for ki in 0..k {
            let (_s_ki, t_ki) = g.commodities[ki];
            let r = row_of(t_ki, ki);
            for c in 0..dim { a[(r, c)] = 0.0; a[(c, r)] = 0.0; }
            a[(r, r)] = 1.0;
            for col in 0..k { rhs[(r, col)] = 0.0; }
        }
        // Tiny ridge for numerical safety against per-axis nullspace from
        // saturated edges (where resid = 0 → some axis components decoupled).
        for i in 0..dim { a[(i, i)] += 1e-10; }

        let chol = match Cholesky::new(a) {
            Some(c) => c,
            None => break,
        };
        let phi = chol.solve(&rhs);

        // Per commodity ki: extract augmenting direction d_e = (φ_u − φ_v) · resid_e.
        let mut consecutive_fail = 0usize;
        let mut any_pushed = false;
        let mut round_pushed = 0.0_f64;
        for ki in 0..k {
            let (s_ki, _t_ki) = g.commodities[ki];
            let mut d = vec![0.0_f64; m];
            let mut div_s = 0.0_f64;
            for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
                let pu = phi[(row_of(u, ki), ki)];
                let pv = phi[(row_of(v_idx, ki), ki)];
                d[e] = (pu - pv) * resid[e];
                if u == s_ki { div_s += d[e]; }
                if v_idx == s_ki { div_s -= d[e]; }
            }
            // Want positive throughput direction. If div_s < 0, flip d.
            if div_s.abs() < 1e-12 { consecutive_fail += 1; continue; }
            if div_s < 0.0 { for e in 0..m { d[e] = -d[e]; } }
            let div_s = div_s.abs();
            // Don't rescale d — keep its natural magnitude (rescaling
            // amplifies numerical noise when div_s is tiny). alpha_max
            // bounds the push by capacity feasibility per edge.
            let mut alpha_max = f64::INFINITY;
            for e in 0..m {
                let d_e = d[e]; let f_e = f[ki * m + e];
                if d_e.abs() < 1e-15 { continue; }
                let mut other = 0.0;
                for j in 0..k { if j != ki { other += f[j * m + e].abs(); } }
                let room = (caps[e] - other).max(0.0);
                let bound = if d_e > 0.0 { (room - f_e) / d_e } else { (-room - f_e) / d_e };
                if bound > 0.0 && bound < alpha_max { alpha_max = bound; }
            }
            if !alpha_max.is_finite() || alpha_max < 1e-12 { consecutive_fail += 1; continue; }
            let alpha = alpha_max * 0.99999;
            for e in 0..m { f[ki * m + e] += alpha * d[e]; }
            round_pushed += alpha * div_s;
            any_pushed = true;
            consecutive_fail = 0;
        }
        total_polish_pushed += round_pushed;
        if verbose && round < 5 {
            eprintln!("[polish round {}] pushed = {:.4}", round, round_pushed);
        }
        if !any_pushed || consecutive_fail >= k { break; }
        if round_pushed < 1e-9 { break; }
    }
    if verbose {
        eprintln!("[polish] total throughput added = {:.4}", total_polish_pushed);
    }

    // Compute per-commodity throughput and feasibility.
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
    // Conservation check: at every INTERNAL vertex (not s_k, not t_k for commodity k),
    // divergence of f_k must be zero. Report worst violation.
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
    if verbose {
        eprintln!("[multiaxis] max conservation violation = {:.3e}", max_cons_viol);
    }

    IpmResult {
        f_total, f_per, iters,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
        max_load_frac, max_overshoot,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(20);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");
    let verbose = args.iter().any(|a| a == "--verbose" || a == "-v");
    let diag_only = args.iter().any(|a| a == "--diag-only");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, inner={inner}, eta={eta}, seed={seed}", g.edges.len());

    if let Some(path) = args.iter().find_map(|a| a.strip_prefix("--dump=")) {
        let json = serde_json::json!({
            "n": g.n,
            "edges": g.edges.iter().map(|&(u,v,c)| [u, v, c as usize]).collect::<Vec<_>>(),
            "commodities": g.commodities.iter().map(|&(s,t)| [s, t]).collect::<Vec<_>>(),
        });
        std::fs::write(path, json.to_string()).expect("write dump");
        eprintln!("# graph dumped to {}", path);
    }

    let mu_schedule = vec![
        1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001,
        3e-4, 1e-4, 3e-5, 1e-5, 3e-6, 1e-6, 3e-7, 1e-7, 1e-8,
    ];
    let res = solve_multiaxis_ipm(&g, &mu_schedule, inner, eta, verbose, diag_only);
    println!("\n[Multi-axis spring IPM]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.6}", res.f_total);
    println!("  iters   = {}, time = {:.1} ms", res.iters, res.time_ms);
    println!("  max load frac = {:.6}, max overshoot = {:.3e}", res.max_load_frac, res.max_overshoot);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle_gurobi(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[Gurobi LP]");
            println!("  F_total = {:.6} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.2}×", rel, lp_ms / res.time_ms);
        }
    }
}
