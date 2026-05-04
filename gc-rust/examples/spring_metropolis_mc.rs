//! Metropolis sampling on joint K-commodity flow state with spring
//! steps as proposals.
//!
//! State: K signed-flow vectors f_k ∈ R^m, each divergence-free at all
//! internal vertices for commodity k (maintained by always proposing
//! along electrical-flow directions).
//!
//! Hamiltonian:
//!   H(f) = -β · Σ_k F_k(f_k) + μ · Σ_e barrier( Σ_k |f_k,e| / c_e )
//! with barrier(x) = -log(1 - x²) for x ∈ [0, 1) and +∞ otherwise.
//!
//! Proposal: pick commodity i, compute electrical-flow d_i on current
//! shared residual (PCG), draw α ~ N(0, σ²), propose f → f + α·d_i e_i.
//! By construction the proposal preserves commodity-i divergence (s_i
//! ↔ t_i conservation). Metropolis-accept on ΔH.
//!
//! β annealed from β₀ → β_max: cold→hot makes the chain explore the
//! polytope, then concentrates on the F-maximising vertex.

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

/// H(f) = -β Σ F_k + μ Σ_e barrier(Σ_k |f_k,e|/c_e).
/// Returns ∞ if any edge violates capacity.
fn hamiltonian(
    f: &[f64],
    f_per_total: &[f64],
    caps: &[f64],
    beta: f64,
    mu: f64,
) -> f64 {
    let m = caps.len();
    let k = f_per_total.len();
    let mut barrier = 0.0;
    for e in 0..m {
        let mut sum_abs = 0.0;
        for i in 0..k { sum_abs += f[i * m + e].abs(); }
        let x = sum_abs / caps[e];
        if x >= 1.0 - 1e-12 { return f64::INFINITY; }
        barrier += -(1.0 - x * x).ln();
    }
    let f_total: f64 = f_per_total.iter().sum();
    -beta * f_total + mu * barrier
}

#[derive(Clone)]
struct SampleResult {
    f_per: Vec<f64>,
    f_total: f64,
    accepts: usize,
    proposals: usize,
    pcg_iters: usize,
    time_ms: f64,
}

/// Greedy warm-start: run a few rounds of round-robin Ford-Fulkerson
/// (the same logic as spring_sample_aug_mc) to fill `f` close to a feasible
/// near-LP-optimum, then hand off to Metropolis.
fn warm_start_greedy(
    g: &Graph,
    f: &mut [f64],
    f_per_total: &mut [f64],
    n_warmup: usize,
    rng: &mut Xs256,
) -> usize {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut b_per: Vec<Vec<f64>> = (0..k).map(|i| {
        let (s_i, _t_i) = g.commodities[i];
        let mut b = vec![0.0_f64; n];
        b[s_i] = 1.0;
        b
    }).collect();
    let mut total_pcg = 0_usize;
    let mut consecutive_fail = 0_usize;
    let mut augmentations = 0_usize;
    for _it in 0..n_warmup {
        let i = (augmentations + (rng.gen_range(0, k as u64) as usize)) % k;
        let (s_i, t_i) = g.commodities[i];
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
        if (div_s - 1.0).abs() > 1e-3 {
            consecutive_fail += 1;
            if consecutive_fail >= k { break; }
            continue;
        }
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
        // Greedy with margin: take only 70% of α_max so the chain has
        // slack to propose moves at the warm-start.
        let alpha = alpha_max * 0.7;
        for e in 0..m { f[i * m + e] += alpha * d[e]; }
        f_per_total[i] += alpha * div_s;
        augmentations += 1;
    }
    total_pcg
}

fn spring_metropolis_mc(
    g: &Graph,
    n_iters: usize,
    n_warmup: usize,
    sigma_frac: f64,
    beta_lo: f64,
    beta_hi: f64,
    mu: f64,
    seed: u64,
) -> SampleResult {
    let n = g.n;
    let m = g.edges.len();
    let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();

    let mut f = vec![0.0_f64; k * m];
    let mut f_per_total = vec![0.0_f64; k];
    let mut rng_pre = Xs256::new(seed ^ 0xCAFE_F00D_BEEF_CAFE);
    let warm_pcg = warm_start_greedy(g, &mut f, &mut f_per_total, n_warmup, &mut rng_pre);
    let mut f_best_total: f64 = f_per_total.iter().sum();
    let mut f_best_per = f_per_total.clone();

    let mut b_per: Vec<Vec<f64>> = (0..k).map(|i| {
        let (s_i, _t_i) = g.commodities[i];
        let mut b = vec![0.0_f64; n];
        b[s_i] = 1.0;
        b
    }).collect();

    let mut total_pcg = warm_pcg;
    let mut accepts = 0_usize;
    let mut proposals = 0_usize;
    let mut rng = rng_pre;
    let t0 = Instant::now();

    let mut h_curr = hamiltonian(&f, &f_per_total, &caps, beta_lo, mu);

    for it in 0..n_iters {
        let frac = if n_iters > 1 { it as f64 / (n_iters - 1) as f64 } else { 1.0 };
        let beta = beta_lo + (beta_hi - beta_lo) * frac;

        // 50/50: spring-step augment vs zero-sum commodity exchange.
        let do_exchange = k >= 2 && rng.unit() < 0.5;

        let i = rng.gen_range(0, k as u64) as usize;
        let (s_i, t_i) = g.commodities[i];

        // Always need electrical-flow direction for commodity i.
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
        b_per[i][s_i] = 1.0;
        let (phi_i, pcg_it_i) = pcg_pinned(&csr, &b_per[i], t_i, 1e-10, 4 * n);
        total_pcg += pcg_it_i;
        let mut d_i = vec![0.0_f64; m];
        let mut div_si = 0.0_f64;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            d_i[e] = (phi_i[u] - phi_i[v]) * resid[e];
            if u == s_i { div_si += d_i[e]; }
            if v == s_i { div_si -= d_i[e]; }
        }
        if (div_si - 1.0).abs() > 1e-3 { proposals += 1; continue; }

        let mut f_prop = f.clone();
        let mut f_per_prop = f_per_total.clone();

        if do_exchange {
            // Pick partner j ≠ i, get its electrical flow on the SAME residual L,
            // propose: shrink j by ε, grow i by ε (the shrink frees capacity).
            let mut j = rng.gen_range(0, (k - 1) as u64) as usize;
            if j >= i { j += 1; }
            let (s_j, t_j) = g.commodities[j];
            b_per[j][s_j] = 1.0;
            let (phi_j, pcg_it_j) = pcg_pinned(&csr, &b_per[j], t_j, 1e-10, 4 * n);
            total_pcg += pcg_it_j;
            let mut d_j = vec![0.0_f64; m];
            let mut div_sj = 0.0_f64;
            for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                d_j[e] = (phi_j[u] - phi_j[v]) * resid[e];
                if u == s_j { div_sj += d_j[e]; }
                if v == s_j { div_sj -= d_j[e]; }
            }
            if (div_sj - 1.0).abs() > 1e-3 { proposals += 1; continue; }

            // Trade magnitude: scale of f_per_total[j] (so we can swap a
            // meaningful chunk).
            let scale = f_per_total[j].abs().max(1.0);
            let eps = sigma_frac * scale * rng.normal();
            for e in 0..m {
                f_prop[i * m + e] += eps * d_i[e];
                f_prop[j * m + e] -= eps * d_j[e];
            }
            f_per_prop[i] += eps * div_si;
            f_per_prop[j] -= eps * div_sj;
        } else {
            // Augmenting proposal for commodity i.
            let mut alpha_wall = f64::INFINITY;
            for e in 0..m {
                let d_e = d_i[e]; let f_e = f[i * m + e];
                if d_e.abs() < 1e-15 { continue; }
                let mut other = 0.0;
                for j in 0..k { if j != i { other += f[j * m + e].abs(); } }
                let room = (caps[e] - other).max(0.0);
                let bound = if d_e > 0.0 { (room - f_e) / d_e } else { (-room - f_e) / d_e };
                if bound > 0.0 && bound < alpha_wall { alpha_wall = bound; }
            }
            if !alpha_wall.is_finite() { proposals += 1; continue; }
            let sigma = sigma_frac * alpha_wall;
            let alpha = sigma * rng.normal();
            for e in 0..m { f_prop[i * m + e] += alpha * d_i[e]; }
            f_per_prop[i] += alpha * div_si;
        }

        let h_prop = hamiltonian(&f_prop, &f_per_prop, &caps, beta, mu);
        proposals += 1;
        if h_prop.is_finite() {
            let dh = h_prop - h_curr;
            let accept = dh <= 0.0 || rng.unit() < (-dh).exp();
            if accept {
                f = f_prop;
                f_per_total = f_per_prop;
                h_curr = h_prop;
                accepts += 1;
                let total: f64 = f_per_total.iter().sum();
                if total > f_best_total {
                    f_best_total = total;
                    f_best_per.copy_from_slice(&f_per_total);
                }
            }
        }
    }

    SampleResult {
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
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(40);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(150);
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(5);
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(2000);
    let n_warmup: usize = args.iter().find_map(|a| a.strip_prefix("--warm=")?.parse().ok()).unwrap_or(200);
    let sigma_frac: f64 = args.iter().find_map(|a| a.strip_prefix("--sigma=")?.parse().ok()).unwrap_or(0.3);
    let beta_lo: f64 = args.iter().find_map(|a| a.strip_prefix("--bmin=")?.parse().ok()).unwrap_or(0.5);
    let beta_hi: f64 = args.iter().find_map(|a| a.strip_prefix("--bmax=")?.parse().ok()).unwrap_or(50.0);
    let mu: f64 = args.iter().find_map(|a| a.strip_prefix("--mu=")?.parse().ok()).unwrap_or(0.1);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let g = gen_graph(n, m_target, k, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# n={n}, m={}, K={k}, T={n_iters}, σ={sigma_frac}, β=[{beta_lo}, {beta_hi}], μ={mu}, seed={seed}",
        g.edges.len());

    let res = spring_metropolis_mc(&g, n_iters, n_warmup, sigma_frac, beta_lo, beta_hi, mu, seed);
    let acc_rate = res.accepts as f64 / res.proposals.max(1) as f64;
    println!("\n[Spring METROPOLIS MC]");
    println!("  F_per   = {:?}", res.f_per.iter().map(|x| format!("{x:.4}")).collect::<Vec<_>>());
    println!("  F_total = {:.4}", res.f_total);
    println!("  acc rate = {:.2}%, props={}, pcg={}, time={:.1} ms",
        acc_rate * 100.0, res.proposals, res.pcg_iters, res.time_ms);

    if !no_lp {
        if let Some((lp_total, _lp_per, lp_ms)) = run_lp_oracle(&g) {
            let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
            println!("\n[LP oracle (HiGHS)]");
            println!("  F_total = {:.4} ({:.1} ms)", lp_total, lp_ms);
            println!("  rel err = {:.4}%, speedup = {:.1}×", rel, lp_ms / res.time_ms);
        }
    }
}
