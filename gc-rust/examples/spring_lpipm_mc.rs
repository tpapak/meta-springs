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

use nalgebra::{DMatrix, DVector};
use rayon::prelude::*;

use gc_rust::lap_solver::{CsrLap, Ic0, Preconditioner};

// ---------- Block PCG matrix-free for multi-axis Laplacian ----------
//
// The (n*K)*(n*K) block-Laplacian is L = Σ_e (e_u-e_v)(e_u-e_v)ᵀ ⊗ S_e.
// We never materialise it. Matvec iterates edges:
//   for each edge e=(u,v):
//     diff_e = phi[u,:] - phi[v,:]              (K-vector)
//     contrib = S_e * diff_e                     (K-vector)
//     out[u,:] += contrib;  out[v,:] -= contrib
//
// Pinning: pinned (vertex, axis) entries get identity action — out[i] =
// phi[i], and we maintain phi[pinned] = 0 throughout PCG.
//
// Preconditioner: block-Jacobi. Per-vertex K*K diagonal block is
// Σ_{e at v} S_e (each edge contributes its full S_e to BOTH endpoints'
// diagonal block). Invert per-vertex K*K once, apply per-iteration.

/// Per-edge compliance H_eff^{-1} = diag(d_inv) − r·rᵀ stored as just
/// (d_inv, r) (length-K vectors per edge). Exploits the rank-1 structure
/// for O(K) per-edge matvec instead of O(K²) — at K=100 that's 100× faster.
struct EdgeCompliance {
    d_prime: Vec<f64>, // d_inv[k] = (D')^{-1}_k per edge axis k
    r:       Vec<f64>, // rank-1 vector with sign baked in (subtracted)
}

fn apply_l_block_factored(
    s_e_fac: &[EdgeCompliance],
    edges: &[(usize, usize, i64)],
    _n: usize, k: usize,
    pinned: &[bool],
    phi: &DVector<f64>,
    out: &mut DVector<f64>,
) {
    let dim = phi.len();
    for i in 0..dim { out[i] = 0.0; }
    let mut diff = vec![0.0_f64; k];
    let mut contrib = vec![0.0_f64; k];
    for (e, &(u, v, _)) in edges.iter().enumerate() {
        let s = &s_e_fac[e];
        // diff_e = phi[u,:] − phi[v,:]
        let mut rdot = 0.0_f64;
        for ki in 0..k {
            let d = phi[u * k + ki] - phi[v * k + ki];
            diff[ki] = d;
            rdot += s.r[ki] * d;
        }
        // contrib = diag(d_inv) · diff − r · (r · diff)   (rank-1 SUBTRACT)
        for ki in 0..k {
            contrib[ki] = s.d_prime[ki] * diff[ki] - s.r[ki] * rdot;
        }
        for ki in 0..k {
            out[u * k + ki] += contrib[ki];
            out[v * k + ki] -= contrib[ki];
        }
    }
    for i in 0..dim {
        if pinned[i] { out[i] = phi[i]; }
    }
}

#[allow(dead_code)]
fn apply_l_block(
    s_e: &[DMatrix<f64>],
    edges: &[(usize, usize, i64)],
    n: usize, k: usize,
    pinned: &[bool],
    phi: &DVector<f64>,
    out: &mut DVector<f64>,
) {
    let dim = n * k;
    for i in 0..dim { out[i] = 0.0; }
    let mut diff = vec![0.0_f64; k];
    let mut contrib = vec![0.0_f64; k];
    for (e, &(u, v, _)) in edges.iter().enumerate() {
        for ki in 0..k { diff[ki] = phi[u * k + ki] - phi[v * k + ki]; }
        for ki in 0..k {
            let mut c = 0.0;
            for kj in 0..k { c += s_e[e][(ki, kj)] * diff[kj]; }
            contrib[ki] = c;
        }
        for ki in 0..k {
            out[u * k + ki] += contrib[ki];
            out[v * k + ki] -= contrib[ki];
        }
    }
    for i in 0..dim {
        if pinned[i] { out[i] = phi[i]; }
    }
}

/// Graph-aware preconditioner: K independent scalar weighted Laplacians,
/// edge weight = S_e[k, k] = d_inv[k] − r[k]² for axis k.
///
/// Two factor backends:
///   - `Dense`: nalgebra dense Cholesky. Build O(n³), apply O(n²). Used
///     for small n where the cubic build is fast enough that the high
///     per-apply cost is amortised by deeper PCG convergence.
///   - `Ic0`: incomplete Cholesky (no fill) on sparse CSR Laplacian.
///     Build O(n·d²), apply O(n·d) where d = avg degree. For our random
///     sparse graphs this is 50-200× faster per apply than dense.
///
/// Both built per-axis, parallelised across K with rayon. Pinning in
/// the outer matvec handles the per-axis terminal constraints; the
/// preconditioner uses a tiny ridge ε so the factor stays SPD.
///
/// Negative findings (don't redo):
///   - SHARED Laplacian (mean S_e[k,k] across axes): blew PCG iters 100×.
///   - SMW low-rank rank-1 correction: made convergence ~2× worse.
enum AxisFactor {
    Dense(nalgebra::Cholesky<f64, nalgebra::Dyn>),
    Ic0(Ic0),
}

struct AxisLaplaciansPrec {
    factors: Vec<AxisFactor>,
}

fn build_axis_laplacians_prec(
    s_e_fac: &[EdgeCompliance],
    edges: &[(usize, usize, i64)],
    n: usize, k: usize,
    pinned: &[bool],
) -> AxisLaplaciansPrec {
    // Use IC0 (sparse, incomplete Cholesky no-fill) when n is large
    // enough that dense Cholesky wastes work; otherwise dense.
    // Crossover: empirically n ≥ 100 → IC0 is faster to build AND apply.
    // IC0 looked promising in isolation but turns out to give a slightly
    // worse preconditioner that hits the WHOLE Newton convergence loop —
    // the IPM line search has more rejected steps with approximate prec,
    // so total Newton iters go up and net time is worse than dense even
    // at n = 200. Keep dense as default; revisit IC0 only if a per-axis
    // sparse direct factor (not incomplete) becomes available.
    let use_ic0 = false;
    // Build serial too (Ic0 not Sync). For dense Cholesky build we could
    // parallelise, but the per-axis n^3 cost is small at our test sizes.
    let factors: Vec<AxisFactor> = (0..k).map(|ki| {
        if use_ic0 {
            // Build CSR Laplacian for axis ki. Pinning is enforced in the
            // outer matvec / PCG; the preconditioner uses a tiny ridge ε
            // to keep the factor SPD.
            let weighted: Vec<(u32, u32, f64)> = edges.iter().map(|&(u, v, _e_idx)| {
                let _ = _e_idx;
                (u as u32, v as u32, 0.0)
            }).collect();
            let mut weighted_full: Vec<(u32, u32, f64)> = Vec::with_capacity(edges.len());
            for (e, &(u, v, _)) in edges.iter().enumerate() {
                let s_kk = (s_e_fac[e].d_prime[ki] - s_e_fac[e].r[ki] * s_e_fac[e].r[ki]).max(1e-14);
                weighted_full.push((u as u32, v as u32, s_kk));
            }
            let _ = weighted;
            let csr = CsrLap::from_canonical_weights(&weighted_full, n);
            // Use larger ε on pinned vertices so the preconditioner doesn't
            // try to "solve" for the constrained DOFs (we'll zero them in apply).
            // Simpler: use a small uniform ridge; outer PCG handles pin.
            let ic = Ic0::new(&csr, 1e-8);
            AxisFactor::Ic0(ic)
        } else {
            let mut a = DMatrix::<f64>::zeros(n, n);
            for (e, &(u, v, _)) in edges.iter().enumerate() {
                let s_kk = (s_e_fac[e].d_prime[ki] - s_e_fac[e].r[ki] * s_e_fac[e].r[ki]).max(1e-14);
                a[(u, u)] += s_kk;
                a[(v, v)] += s_kk;
                a[(u, v)] -= s_kk;
                a[(v, u)] -= s_kk;
            }
            for v in 0..n {
                if pinned[v * k + ki] {
                    for c in 0..n { a[(v, c)] = 0.0; a[(c, v)] = 0.0; }
                    a[(v, v)] = 1.0;
                }
            }
            for v in 0..n { a[(v, v)] += 1e-12; }
            AxisFactor::Dense(nalgebra::Cholesky::new(a).expect("axis-Laplacian preconditioner factor"))
        }
    }).collect();

    AxisLaplaciansPrec { factors }
}

fn apply_axis_laplacians_prec(
    prec: &AxisLaplaciansPrec,
    n: usize, k: usize,
    pinned: &[bool],
    v: &DVector<f64>,
    z: &mut DVector<f64>,
) {
    // Per-axis solve. Either dense Cholesky (`Dense`) or sparse IC(0)
    // back-solve (`Ic0`). Parallelise across K when work-per-axis is
    // big enough to amortise rayon overhead.
    let solve_axis = |factor: &AxisFactor, ki: usize, rhs_in: &[f64], out: &mut [f64]| {
        let mut tmp_in = vec![0.0_f64; n];
        for vert in 0..n {
            let val = rhs_in[vert * k + ki];
            tmp_in[vert] = if pinned[vert * k + ki] { 0.0 } else { val };
        }
        match factor {
            AxisFactor::Dense(chol) => {
                let dv = DVector::<f64>::from_vec(tmp_in);
                let sol = chol.solve(&dv);
                for vert in 0..n { out[vert * k + ki] = sol[vert]; }
            }
            AxisFactor::Ic0(ic) => {
                let mut sol = vec![0.0_f64; n];
                ic.apply(&tmp_in, &mut sol);
                for vert in 0..n { out[vert * k + ki] = sol[vert]; }
            }
        }
    };
    // Note: rayon parallel removed because Ic0 contains RefCell (not Sync).
    // For Dense factors we could parallelise, but the per-axis back-solve
    // is fast enough that serial is fine — measurable speedup only when
    // K is large AND n*n is large.
    let z_slice = z.as_mut_slice();
    for ki in 0..k {
        solve_axis(&prec.factors[ki], ki, v.as_slice(), z_slice);
    }
    for i in 0..(n * k) {
        if pinned[i] { z[i] = v[i]; }
    }
}

/// Wrapper: zero warm-start, fresh PCG solve with on-the-fly preconditioner.
#[allow(dead_code)]
fn block_pcg_solve(
    s_e_fac: &[EdgeCompliance],
    edges: &[(usize, usize, i64)],
    n: usize, k: usize,
    pinned: &[bool],
    rhs: &DVector<f64>,
    tol: f64,
    max_iter: usize,
) -> (DVector<f64>, usize) {
    let warm = DVector::<f64>::zeros(n * k);
    let prec = build_axis_laplacians_prec(s_e_fac, edges, n, k, pinned);
    block_pcg_solve_with_prec(s_e_fac, edges, n, k, pinned, &prec, rhs, &warm, tol, max_iter)
}

/// PCG with warm-start AND a pre-built preconditioner. Uses factored
/// edge compliance (D' + r·rᵀ) for O(K) per-edge matvec instead of O(K²).
fn block_pcg_solve_with_prec(
    s_e_fac: &[EdgeCompliance],
    edges: &[(usize, usize, i64)],
    n: usize, k: usize,
    pinned: &[bool],
    prec: &AxisLaplaciansPrec,
    rhs: &DVector<f64>,
    x0: &DVector<f64>,
    tol: f64,
    max_iter: usize,
) -> (DVector<f64>, usize) {
    let dim = n * k;
    let mut x = x0.clone();
    for i in 0..dim { if pinned[i] { x[i] = 0.0; } }
    // r = rhs - L_block * x
    let mut r = rhs.clone();
    let mut lx = DVector::<f64>::zeros(dim);
    apply_l_block_factored(s_e_fac, edges, n, k, pinned, &x, &mut lx);
    for i in 0..dim { r[i] -= lx[i]; if pinned[i] { r[i] = 0.0; } }
    let mut z = DVector::<f64>::zeros(dim);
    apply_axis_laplacians_prec(prec, n, k, pinned, &r, &mut z);
    let mut p = z.clone();
    let mut ap = DVector::<f64>::zeros(dim);
    let mut rz_old: f64 = r.dot(&z);
    let b_norm: f64 = rhs.norm().max(1.0);
    for it in 0..max_iter {
        apply_l_block_factored(s_e_fac, edges, n, k, pinned, &p, &mut ap);
        let p_ap: f64 = p.dot(&ap);
        if p_ap.abs() < 1e-30 { return (x, it); }
        let alpha = rz_old / p_ap;
        for i in 0..dim {
            x[i] += alpha * p[i];
            r[i] -= alpha * ap[i];
        }
        for i in 0..dim { if pinned[i] { x[i] = 0.0; r[i] = 0.0; } }
        let r_norm = r.norm();
        if r_norm / b_norm < tol { return (x, it + 1); }
        apply_axis_laplacians_prec(prec, n, k, pinned, &r, &mut z);
        let rz_new: f64 = r.dot(&z);
        let beta = rz_new / rz_old;
        for i in 0..dim { p[i] = z[i] + beta * p[i]; }
        for i in 0..dim { if pinned[i] { p[i] = 0.0; } }
        rz_old = rz_new;
    }
    (x, max_iter)
}

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
/// `prec_cache` holds the K-axis Laplacian preconditioner from a previous
/// step; if `force_rebuild` is true (or cache is empty), we rebuild it.
/// The Hessian changes slowly across Newton iters so a stale preconditioner
/// gives near-optimal PCG convergence at much lower amortised cost.
fn newton_step(
    f: &[f64],
    t: &[f64],
    g: &Graph,
    caps: &[f64],
    mu: f64,
    df: &mut [f64],
    dt: &mut [f64],
    lambda_warm: &mut Option<DVector<f64>>,
    pcg_iters_acc: &mut usize,
    prec_cache: &mut Option<AxisLaplaciansPrec>,
    force_rebuild_prec: bool,
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
    // We need H_eff^{-1} per edge. H_eff = D' + r·rᵀ; the inverse is
    // (D')^{-1} - α' · a'·a'ᵀ where a' = (D')^{-1} r, α' = 1/(1+rᵀa').
    // STORE the inverse in factored form: D'_inv (vector) and a' (vector),
    // α' (scalar). Apply via O(K) per edge: x → diag(D'_inv)·x − α'·(a'·x)·a'.
    let mut s_e_fac: Vec<EdgeCompliance> = Vec::with_capacity(m);
    let mut g_eff = vec![0.0_f64; k * m];
    for e in 0..m {
        let mut d_prime = vec![0.0_f64; k];
        let mut rvec = vec![0.0_f64; k];
        let mut sum_inv_beta = 0.0_f64;
        let mut sum_gt_over_beta = 0.0_f64;
        for kk in 0..k {
            let b = beta[kk * m + e];
            sum_inv_beta += 1.0 / b;
            sum_gt_over_beta += g_t[kk * m + e] / b;
        }
        let denom = 1.0 + gamma[e] * sum_inv_beta;
        let scale = (gamma[e] / denom).sqrt();
        let woodbury_coef = gamma[e] / denom;
        for kk in 0..k {
            let b = beta[kk * m + e];
            let d = delta[kk * m + e];
            let dp = (b * b - d * d) / b;
            d_prime[kk] = dp.max(1e-14);
            rvec[kk] = scale * (d / b);
            let gt_corrected = g_t[kk * m + e] / b
                - (1.0 / b) * woodbury_coef * sum_gt_over_beta;
            g_eff[kk * m + e] = g_f[kk * m + e] - d * gt_corrected;
        }
        // Build H_eff^{-1} = (D')^{-1} - α' a' a'ᵀ where
        //   a' = (D')^{-1} r, α' = 1/(1 + rᵀ a').
        // Store as (d_prime_inv, a_prime_neg_alpha_sqrt) so that
        //   H_eff^{-1} v = diag(d_prime_inv)·v + a_prime_neg·(a_prime_neg·v)
        // with a_prime_neg = a' · sqrt(α') (sign flipped to fold into +rᵀr form).
        let mut d_prime_inv = vec![0.0_f64; k];
        let mut a_prime = vec![0.0_f64; k];
        let mut p = 0.0_f64;
        for kk in 0..k {
            d_prime_inv[kk] = 1.0 / d_prime[kk];
            a_prime[kk] = rvec[kk] / d_prime[kk];
            p += rvec[kk] * a_prime[kk];
        }
        let alpha_w = 1.0 / (1.0 + p);
        let neg_sqrt_alpha = -(alpha_w).sqrt();
        let r_compl: Vec<f64> = a_prime.iter().map(|&a| a * neg_sqrt_alpha).collect();
        // H_eff^{-1} = diag(d_prime_inv) + r_compl · r_complᵀ
        // (because (-√α a')·(-√α a')ᵀ = α a'a'ᵀ — wait that's +α a'a'ᵀ but we
        // want −α. Sign trick: store as diag + rrᵀ and SUBTRACT in matvec.
        // Simpler: store with a sign field — but easier yet, just keep the
        // diag-plus-rank-one-MINUS form. For convenience, store r and a sign,
        // or just store r so H_eff^{-1} v = diag(d_inv)·v − (a_p · v)·a_p · α_w.
        // We use r = a' · √α_w with the convention: matvec subtracts r·(r·v).
        let _ = neg_sqrt_alpha;
        let _ = r_compl;
        let r_stored: Vec<f64> = a_prime.iter().map(|&a| a * alpha_w.sqrt()).collect();
        s_e_fac.push(EdgeCompliance { d_prime: d_prime_inv, r: r_stored });
    }

    // Schur RHS: rhs = -C H_eff^{-1} g_eff (matches sign convention used elsewhere).
    // Built by edge-iteration; never materialise the (n·K)*(n·K) Laplacian.
    // Per-edge apply of H_eff^{-1} = diag(d_inv) − r·rᵀ in O(K).
    let dim = n * k;
    let row_of = |vert: usize, com: usize| vert * k + com;
    let mut v_per_edge = vec![0.0_f64; k * m];
    for e in 0..m {
        let s = &s_e_fac[e];
        let mut rdot = 0.0_f64;
        for ki in 0..k { rdot += s.r[ki] * g_eff[ki * m + e]; }
        for ki in 0..k {
            v_per_edge[ki * m + e] = s.d_prime[ki] * g_eff[ki * m + e] - s.r[ki] * rdot;
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

    // Per-axis pinning mask: (s_k, k) and (t_k, k) for each commodity.
    let mut pinned = vec![false; dim];
    for ki in 0..k {
        let (s_k, t_k) = g.commodities[ki];
        pinned[row_of(s_k, ki)] = true;
        pinned[row_of(t_k, ki)] = true;
    }
    for i in 0..dim { if pinned[i] { rhs[i] = 0.0; } }

    // Hybrid: dense Cholesky for small dim (cache-friendly cubic),
    // block-PCG matrix-free with axis-Lapl preconditioner for large dim.
    // Crossover empirically around dim ≈ 200.
    let lambda = if dim <= 200 {
        // Materialise the (n·K)*(n·K) matrix from factored compliance:
        //   S_e[ki, kj] = δ_ki,kj · d_inv[ki] − r[ki]·r[kj]
        let mut a_dense = DMatrix::<f64>::zeros(dim, dim);
        for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
            let s = &s_e_fac[e];
            for (sa_, ta) in [(-1.0_f64, v_idx), (1.0, u)] {
                for (sb_, tb) in [(-1.0_f64, v_idx), (1.0, u)] {
                    let scalar = sa_ * sb_;
                    for ki in 0..k {
                        // diag piece
                        a_dense[(row_of(ta, ki), row_of(tb, ki))] += scalar * s.d_prime[ki];
                        // rank-1 piece (negative)
                        for kj in 0..k {
                            a_dense[(row_of(ta, ki), row_of(tb, kj))] -= scalar * s.r[ki] * s.r[kj];
                        }
                    }
                }
            }
        }
        for i in 0..dim {
            if pinned[i] {
                for c in 0..dim { a_dense[(i, c)] = 0.0; a_dense[(c, i)] = 0.0; }
                a_dense[(i, i)] = 1.0;
            }
        }
        let ridge = 1e-12 * a_dense.diagonal().abs().max();
        for i in 0..dim { a_dense[(i, i)] += ridge; }
        let chol = nalgebra::Cholesky::new(a_dense)?;
        chol.solve(&rhs)
    } else {
        // PCG must be tight enough that Schur residual doesn't break
        // conservation. We tried loose tol (1e-8 ish, max_pcg = 4n) — it
        // saved no measurable time AND inflated F_total by 2-3% via
        // cons-violation accumulation across Newton steps. Stick with
        // tight tol; the spring-projection error has to stay near machine.
        let pcg_tol = 1e-12_f64;
        let max_pcg = (40 * n).max(2000);
        let warm = lambda_warm.clone().unwrap_or_else(|| DVector::<f64>::zeros(dim));
        if force_rebuild_prec || prec_cache.is_none() {
            *prec_cache = Some(build_axis_laplacians_prec(&s_e_fac, &g.edges, n, k, &pinned));
        }
        let prec = prec_cache.as_ref().unwrap();
        let (lambda_pcg, n_it) = block_pcg_solve_with_prec(
            &s_e_fac, &g.edges, n, k, &pinned, prec, &rhs, &warm, pcg_tol, max_pcg,
        );
        *pcg_iters_acc += n_it;
        *lambda_warm = Some(lambda_pcg.clone());
        lambda_pcg
    };

    // Δf per edge: Δf = -H_eff^{-1} (g_eff + B^T λ).
    // Apply factored H_eff^{-1} = diag(d_inv) − r·rᵀ in O(K) per edge.
    for kk in 0..k * m { df[kk] = 0.0; }
    let mut h_e = vec![0.0_f64; k];
    for (e, &(u, v_idx, _)) in g.edges.iter().enumerate() {
        let s = &s_e_fac[e];
        let mut rdot = 0.0_f64;
        for ki in 0..k {
            h_e[ki] = g_eff[ki * m + e] + (lambda[row_of(u, ki)] - lambda[row_of(v_idx, ki)]);
            rdot += s.r[ki] * h_e[ki];
        }
        for ki in 0..k {
            df[ki * m + e] = -(s.d_prime[ki] * h_e[ki] - s.r[ki] * rdot);
        }
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
    let mut total_pcg_iters = 0usize;
    let mut lambda_warm: Option<DVector<f64>> = None;
    let mut prec_cache: Option<AxisLaplaciansPrec> = None;
    let mut eta = eta_init;
    // Rebuild preconditioner every PREC_REBUILD Newton steps. Stale prec
    // costs a few extra PCG iters but saves ~10× on factor builds.
    const PREC_REBUILD: usize = 32;
    let mut prev_f_total = 0.0_f64;
    for (_mu_idx, &mu) in mu_schedule.iter().enumerate() {
        for inner in 0..inner_iters {
            iters += 1;
            let force_rebuild = iters % PREC_REBUILD == 1;
            let grad_norm = match newton_step(
                &f, &t, g, &caps, mu, &mut df, &mut dt,
                &mut lambda_warm, &mut total_pcg_iters,
                &mut prec_cache, force_rebuild,
            ) {
                Some(g) => g,
                None => {
                    if verbose { eprintln!("[lpipm] Cholesky failed at mu={mu:.1e} inner={inner}"); }
                    break;
                }
            };
            if grad_norm < 1e-12 { break; }
            // Early termination per μ: if F_total stops moving (centring done),
            // drop μ instead of grinding more inner iters.
            if inner >= 3 && inner % 3 == 0 {
                let mut cur_f_total = 0.0;
                for kk in 0..k {
                    let (s_kk, _t_kk) = g.commodities[kk];
                    let mut div_s = 0.0;
                    for (e, &(u, vtx, _)) in g.edges.iter().enumerate() {
                        if u == s_kk { div_s += f[kk * m + e]; }
                        else if vtx == s_kk { div_s -= f[kk * m + e]; }
                    }
                    cur_f_total += div_s;
                }
                if (cur_f_total - prev_f_total).abs() < 1e-10 * cur_f_total.abs().max(1.0) {
                    prev_f_total = cur_f_total;
                    break;
                }
                prev_f_total = cur_f_total;
            }

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
            eprintln!("[lpipm] after mu={mu:.1e}: F = {:.6}, total_pcg_iters = {}",
                f_total, total_pcg_iters);
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

    // Deep μ schedule for tight LP-vertex convergence; early-termination
    // per μ level skips already-converged levels so we don't pay full
    // inner_iters everywhere.
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
