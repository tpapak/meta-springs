//! EM-style outer loop for max-flow on top of `lap_solver::pcg_solve`.
//!
//! Single loop, no bisection on F. At each iteration:
//!   1. Solve electrical flow at the current resistances for unit demand.
//!   2. Compute max congestion = max |f_e|/c_e of the unit-demand flow.
//!      The achievable feasible flow at this resistance config is
//!      F_k = 1 / max_cong_k. Track best across iterations.
//!   3. Multiplicative-weights update on resistances: edges with high
//!      relative load get harder, redirecting flow next iteration.
//!   4. Renormalise resistances by their geometric mean for stability.
//!
//! This is the "best-feasible-iter" CKMST variant — equivalent to a damped
//! Newton update on F (since the Newton step at fixed resistances is
//! exactly F = 1/max_cong, the closed-form maximum scaling that keeps the
//! current electrical-flow solution feasible).
//!
//! Compared against OR-Tools deterministic max-flow as ground truth.

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::CsrLap;

/// PCG with one vertex pinned to 0 — solves the exact (n−1)-dimensional
/// Laplacian system `L_uu φ_u = b_u` with `φ[pin] = 0` enforced. No
/// εI regularisation, so the flow `f_e = (φ_u − φ_v) · 1/r_e` is
/// exactly conservation-respecting at every internal vertex by KCL.
fn pcg_pinned(
    csr: &CsrLap,
    b: &[f64],
    pin: usize,
    tol: f64,
    max_iter: usize,
) -> (Vec<f64>, usize) {
    let n = csr.n();
    let mut x = vec![0.0_f64; n];
    let mut r = b.to_vec();
    r[pin] = 0.0;

    let diag = csr.diag(0.0);
    let m_inv: Vec<f64> = (0..n)
        .map(|i| if i != pin && diag[i] > 0.0 { 1.0 / diag[i] } else { 0.0 })
        .collect();

    let mut z: Vec<f64> = r.iter().zip(m_inv.iter()).map(|(ri, mi)| ri * mi).collect();
    let mut p = z.clone();
    let mut ap = vec![0.0_f64; n];
    let mut p_pin_zero = vec![0.0_f64; n];
    let mut rz_old: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
    let b_norm: f64 = b.iter().map(|v| v * v).sum::<f64>().sqrt().max(1.0);

    for it in 0..max_iter {
        // Apply L on subspace where x[pin] = 0: zero out p[pin] before
        // SpMV, then zero out result at pin.
        p_pin_zero.copy_from_slice(&p);
        p_pin_zero[pin] = 0.0;
        csr.apply(0.0, &p_pin_zero, &mut ap);
        ap[pin] = 0.0;

        let p_ap: f64 = p.iter().zip(ap.iter()).map(|(a, b)| a * b).sum();
        if p_ap.abs() < 1e-30 {
            return (x, it);
        }
        let alpha = rz_old / p_ap;
        for i in 0..n {
            x[i] += alpha * p[i];
            r[i] -= alpha * ap[i];
        }
        x[pin] = 0.0;
        r[pin] = 0.0;

        let r_norm: f64 = r.iter().map(|v| v * v).sum::<f64>().sqrt();
        if r_norm / b_norm < tol {
            return (x, it + 1);
        }
        for i in 0..n {
            z[i] = r[i] * m_inv[i];
        }
        let rz_new: f64 = r.iter().zip(z.iter()).map(|(a, b)| a * b).sum();
        let beta = rz_new / rz_old;
        for i in 0..n {
            p[i] = z[i] + beta * p[i];
        }
        p[pin] = 0.0;
        rz_old = rz_new;
    }
    (x, max_iter)
}

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
    source: usize,
    sink: usize,
}

fn gen_graph(n: usize, target_e: usize, seed: u64) -> Graph {
    let mut rng = Xs256::new(seed);
    let mut edges = Vec::with_capacity(target_e);
    let mut seen = std::collections::HashSet::with_capacity(target_e);
    let mut perm: Vec<usize> = (0..n).collect();
    for i in (1..n).rev() {
        let j = rng.gen_range(0, (i as u64) + 1) as usize;
        perm.swap(i, j);
    }
    for k in 1..n {
        let u = perm[k];
        let parent = perm[rng.gen_range(0, k as u64) as usize];
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
    Graph { n, edges, source: perm[0], sink: perm[n - 1] }
}

fn write_dimacs(g: &Graph, path: &Path) -> std::io::Result<()> {
    let mut f = std::fs::File::create(path)?;
    let m_directed = g.edges.len() * 2;
    writeln!(f, "c em-maxflow")?;
    writeln!(f, "p max {} {}", g.n, m_directed)?;
    writeln!(f, "n {} s", g.source + 1)?;
    writeln!(f, "n {} t", g.sink + 1)?;
    for &(u, v, c) in &g.edges {
        writeln!(f, "a {} {} {}", u + 1, v + 1, c)?;
        writeln!(f, "a {} {} {}", v + 1, u + 1, c)?;
    }
    Ok(())
}

fn run_ortools(driver: &Path, file: &Path) -> Option<(i64, u64)> {
    let out = Command::new("python3").arg(driver).arg(file).output().ok()?;
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
}

/// EM-style single-loop max-flow.
///
/// Returns `(F_best, iters_run, total_pcg_iters, time_ms)`.
fn em_maxflow(g: &Graph, t_outer: usize, eta: f64, conv_tol: f64) -> (f64, usize, usize, f64) {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();

    // Unit demand: pin the sink at φ = 0; solve L φ = b with b[s] = 1
    // and other entries 0. The exact (n−1)-dim system gives a true unit
    // flow with conservation at every internal vertex.
    let mut b = vec![0.0_f64; n];
    b[g.source] = 1.0;
    let pin = g.sink;

    let pcg_tol = 1e-10;
    let max_pcg_iter = (4 * n).max(2000);

    let mut f_best = 0.0_f64;
    let mut total_pcg = 0_usize;
    let mut iters_run = 0_usize;
    let mut last_f = 0.0_f64;
    let mut stale = 0usize;

    let t0 = Instant::now();
    for k in 0..t_outer {
        iters_run = k + 1;
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i])).collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);
        let (phi_vec, pcg_it) = pcg_pinned(&csr, &b, pin, pcg_tol, max_pcg_iter);
        total_pcg += pcg_it;
        let phi = &phi_vec;

        let mut max_cong = 0.0_f64;
        let mut currents = vec![0.0_f64; m];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let f_e = (phi[u] - phi[v]) / r[i];
            currents[i] = f_e;
            let cong = f_e.abs() / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        // Verify the flow is actually a flow. Source divergence ≈ 1,
        // and AFTER scaling by 1/max_cong, every internal vertex's
        // divergence is small relative to the reported flow value
        // F_EM = 1/max_cong. The εI regularisation leaves spurious
        // divergence -ε·φ_i at internal vertices; scaling amplifies it.
        let mut div = vec![0.0_f64; n];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            div[u] += currents[i];
            div[v] -= currents[i];
        }
        let div_s = div[g.source];
        let max_internal_div: f64 = (0..n)
            .filter(|&i| i != g.source && i != g.sink)
            .map(|i| div[i].abs())
            .fold(0.0, f64::max);
        // Scaled internal divergence relative to scaled flow value (= 1):
        // (max_internal_div / max_cong) / (1 / max_cong) = max_internal_div.
        // Wait: scaled flow has value 1/max_cong; internal div after scaling
        // is max_internal_div / max_cong. Relative error =
        // (max_internal_div / max_cong) / (1 / max_cong) = max_internal_div.
        // No — relative error = abs_div / flow_value = max_internal_div /
        // max_cong / (1/max_cong) = max_internal_div. So the unscaled
        // max_internal_div IS the relative error. With ε=1e-10 and φ ~ F*,
        // expect max_internal_div ~ ε·F* ~ 1e-6 typically. But when
        // resistances become extreme, φ_i can reach 1/max_cong scale ~ 1e3
        // and max_internal_div hits 1e-7, still under the threshold —
        // yet the *scaled* flow's divergence is 1e-7 / max_cong = 1e-4
        // relative to value 1, giving 1.9% overshoot in F.
        // Tighten: require max_internal_div * (1/max_cong) < 1% of F_EM.
        // = max_internal_div / max_cong / (1 / max_cong) < 0.01
        // = max_internal_div < 0.01.   Hmm that's not tight enough.
        // Right: scaled_internal_div = max_internal_div / max_cong.
        // We want scaled_internal_div / F_EM < tol, i.e.
        // max_internal_div < tol * F_EM * max_cong = tol * div_s.
        // For div_s ≈ 1, tol = 1e-3 gives max_internal_div < 1e-3.
        let scaled_max_div_relative = if max_cong > 0.0 {
            (max_internal_div / max_cong).abs() / (div_s / max_cong).abs().max(1e-30)
        } else {
            f64::INFINITY
        };
        if (div_s - 1.0).abs() > 1e-4 || scaled_max_div_relative > 1e-3 {
            continue;
        }
        if max_cong > 0.0 {
            let f_k = 1.0 / max_cong;
            let scaled_f = f_k * div_s; // ≈ f_k since div_s ≈ 1
            if scaled_f > f_best {
                f_best = scaled_f;
                stale = 0;
            } else {
                stale += 1;
            }
            if stale >= 50 && (f_best - last_f).abs() < conv_tol * f_best.max(1.0) {
                break;
            }
            last_f = f_best;

            // MWU update on resistances + geometric-mean renormalise.
            for i in 0..m {
                let load_norm = (currents[i].abs() / caps[i]) / max_cong;
                r[i] *= 1.0 + eta * load_norm * load_norm;
            }
            let log_sum: f64 = r.iter().map(|x| x.ln()).sum();
            let geom = (log_sum / m as f64).exp();
            if geom.is_finite() && geom > 0.0 {
                for ri in r.iter_mut() { *ri /= geom; }
            }
        }
    }
    (f_best, iters_run, total_pcg, t0.elapsed().as_secs_f64() * 1000.0)
}

fn main() {
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n: usize = std::env::args().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(200);
    let m_target: usize = std::env::args().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(1000);
    let t_outer: usize = std::env::args().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(500);
    let eta: f64 = std::env::args().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(0.1);
    let n_seeds: u64 = std::env::args().find_map(|a| a.strip_prefix("--seeds=")?.parse().ok()).unwrap_or(5);

    println!("# EM-style outer loop. n={n}, m={m_target}, T_outer={t_outer}, eta={eta}");
    println!("seed,V,E,F_ortools,F_EM,abs_err,rel_err,iters,pcg_iters,em_ms,ortools_ms");

    let mut max_rel_err = 0.0_f64;
    for seed in 0..n_seeds {
        let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
        let tmp = std::env::temp_dir().join(format!("em_mf_seed{seed}.max"));
        write_dimacs(&g, &tmp).unwrap();
        let (f_or, us_or) = run_ortools(driver, &tmp).expect("ortools");
        let (f_em, iters, pcg, em_ms) = em_maxflow(&g, t_outer, eta, 1e-5);
        let f_or = f_or as f64;
        let abs_err = (f_em - f_or).abs();
        let rel_err = abs_err / f_or.max(1.0);
        if rel_err > max_rel_err { max_rel_err = rel_err; }
        println!("{},{},{},{:.0},{:.2},{:.2},{:.5},{},{},{:.1},{:.2}",
            seed, g.n, g.edges.len(), f_or, f_em, abs_err, rel_err, iters, pcg, em_ms, us_or as f64 / 1000.0);
        let _ = std::fs::remove_file(&tmp);
    }
    eprintln!();
    eprintln!("# max rel err: {:.4}%", max_rel_err * 100.0);
    eprintln!("# pass (≤ 1%): {}", max_rel_err <= 0.01);
}
