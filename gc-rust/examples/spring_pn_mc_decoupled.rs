//! Decoupled-commodity verification.
//!
//! Builds K disjoint subgraphs joined by NO shared edges. Each commodity
//! lives entirely inside one subgraph. PN's per-commodity F_per[i] must
//! equal the single-commodity max-flow on that subgraph.
//!
//! Runs PN MC + spring_sample_aug per subgraph (single-commodity exact)
//! and compares.

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

/// Build K disjoint random subgraphs concatenated into one big graph.
/// Each subgraph has its own internal vertices and a commodity (s_k, t_k)
/// chosen entirely within that subgraph. The sub-graphs share NO vertices.
fn gen_disjoint(k: usize, n_per: usize, m_per: usize, seed: u64) -> Graph {
    let mut rng = Xs256::new(seed);
    let mut edges: Vec<(usize, usize, i64)> = Vec::with_capacity(k * m_per);
    let mut commodities = Vec::with_capacity(k);

    let mut offset = 0usize;
    for _kk in 0..k {
        // Random spanning tree on n_per vertices, plus extra random edges
        let mut perm: Vec<usize> = (0..n_per).collect();
        for i in (1..n_per).rev() {
            let j = rng.gen_range(0, (i as u64) + 1) as usize;
            perm.swap(i, j);
        }
        let mut seen = std::collections::HashSet::with_capacity(m_per);
        for ki in 1..n_per {
            let u = perm[ki];
            let parent = perm[rng.gen_range(0, ki as u64) as usize];
            let cap = (rng.gen_range(1, 101)) as i64;
            let (a, b) = if u < parent { (u, parent) } else { (parent, u) };
            if seen.insert((a, b)) {
                edges.push((a + offset, b + offset, cap));
            }
        }
        while seen.len() < m_per.min(n_per * (n_per - 1) / 2) {
            let u = rng.gen_range(0, n_per as u64) as usize;
            let v = rng.gen_range(0, n_per as u64) as usize;
            if u == v { continue; }
            let (a, b) = if u < v { (u, v) } else { (v, u) };
            if seen.insert((a, b)) {
                let cap = (rng.gen_range(1, 101)) as i64;
                edges.push((a + offset, b + offset, cap));
            }
        }
        // Commodity: source = first perm vertex, sink = last perm vertex
        commodities.push((perm[0] + offset, perm[n_per - 1] + offset));
        offset += n_per;
    }

    Graph { n: k * n_per, edges, commodities }
}

/// Run single-commodity max-flow via OR-Tools driver.
fn run_ortools(g: &Graph, source: usize, sink: usize) -> Option<(i64, u64)> {
    let driver = "/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py";
    let mut content = String::new();
    content.push_str(&format!("p max {} {}\n", g.n, g.edges.len() * 2));
    content.push_str(&format!("n {} s\n", source + 1));
    content.push_str(&format!("n {} t\n", sink + 1));
    for &(u, v, c) in &g.edges {
        content.push_str(&format!("a {} {} {}\n", u + 1, v + 1, c));
        content.push_str(&format!("a {} {} {}\n", v + 1, u + 1, c));
    }
    let tmp = std::env::temp_dir().join(format!("decoup_{}_{}.max", std::process::id(), source));
    std::fs::write(&tmp, content).ok()?;
    let out = Command::new("python3").arg(driver).arg(&tmp).output().ok()?;
    let _ = std::fs::remove_file(&tmp);
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
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

// === Spring PN code (copy from spring_pn_mc.rs) ===

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

const ABS_EPS: f64 = 1e-6;
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

fn grad_and_hess_diag(f: &[f64], g: &Graph, caps: &[f64], mu: f64,
    grad: &mut [f64], h_diag: &mut [f64], f_per: &mut [f64]) {
    let m = g.edges.len(); let k = g.commodities.len();
    let mut load = vec![0.0_f64; m];
    for kk in 0..k { for e in 0..m { load[e] += smooth_abs(f[kk * m + e]); } }
    for kk in 0..k {
        let (s_kk, _t_kk) = g.commodities[kk];
        let mut div_s = 0.0;
        for (e, &(u, v, _)) in g.edges.iter().enumerate() {
            let s_e = (load[e] / caps[e]).min(0.9999);
            let v_prime = 2.0 * s_e / (1.0 - s_e * s_e);
            let v_pp = 2.0 * (1.0 + s_e * s_e) / ((1.0 - s_e * s_e).powi(2));
            let sap = smooth_abs_grad(f[kk * m + e]);
            let sapp = smooth_abs_hess(f[kk * m + e]);
            let grad_barrier = mu * v_prime * sap / caps[e];
            let grad_throughput = if u == s_kk { 1.0 } else if v == s_kk { -1.0 } else { 0.0 };
            grad[kk * m + e] = grad_barrier - grad_throughput;
            if u == s_kk { div_s += f[kk * m + e]; }
            else if v == s_kk { div_s -= f[kk * m + e]; }
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

fn spring_pn_mc(g: &Graph, mu_schedule: &[f64], inner_iters: usize, eta_init: f64) -> PnResult {
    let n = g.n; let m = g.edges.len(); let k = g.commodities.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f = vec![0.0_f64; k * m];
    let mut grad = vec![0.0_f64; k * m];
    let mut h_diag = vec![1.0_f64; k * m];
    let mut f_per = vec![0.0_f64; k];
    let mut total_pcg = 0_usize; let mut iters = 0_usize;
    let t0 = Instant::now();
    let mut eta = eta_init;
    for &mu in mu_schedule {
        for _inner in 0..inner_iters {
            iters += 1;
            grad_and_hess_diag(&f, g, &caps, mu, &mut grad, &mut h_diag, &mut f_per);
            let mut delta = vec![0.0_f64; k * m];
            for kk in 0..k {
                let (s_kk, t_kk) = g.commodities[kk];
                let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
                    .map(|(e, &(u, v, _))| (u as u32, v as u32, 1.0 / h_diag[kk * m + e]))
                    .collect();
                let csr_k = CsrLap::from_canonical_weights(&weighted, n);
                let mut neg_div = vec![0.0_f64; n];
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    let val = grad[kk * m + e] / h_diag[kk * m + e];
                    neg_div[u] -= val; neg_div[v] += val;
                }
                let (lambda, n_pcg) = pcg_two_pin(&csr_k, &neg_div, s_kk, t_kk, 1e-10, 4 * n);
                total_pcg += n_pcg;
                for (e, &(u, v, _)) in g.edges.iter().enumerate() {
                    let val = grad[kk * m + e] + lambda[u] - lambda[v];
                    delta[kk * m + e] = -val / h_diag[kk * m + e];
                }
            }
            let grad_norm: f64 = grad.iter().map(|x| x * x).sum::<f64>().sqrt();
            if grad_norm < 1e-9 { break; }
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
    grad_and_hess_diag(&f, g, &caps, *mu_schedule.last().unwrap_or(&0.001), &mut grad, &mut h_diag, &mut f_per);
    let f_total: f64 = f_per.iter().sum();
    PnResult { f_per, f_total, iters, pcg_iters: total_pcg, time_ms: t0.elapsed().as_secs_f64() * 1000.0 }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let k: usize = args.iter().find_map(|a| a.strip_prefix("--k=")?.parse().ok()).unwrap_or(3);
    let n_per: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(30);
    let m_per: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(120);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_disjoint(k, n_per, m_per, 0xDEAD_BEEF_FEED ^ seed);
    println!("# Disjoint subgraphs: K={k}, n_per={n_per}, m_per={m_per}, total n={}, m={}",
        g.n, g.edges.len());
    println!("# Each commodity isolated in its own component — F_per[i] must equal single-commodity max-flow on its component.");

    // Run single-commodity max-flow per commodity (ground truth for decoupled)
    println!("\n[Per-commodity OR-Tools (single-commodity ground truth)]");
    let mut sum_individual = 0.0;
    for (i, &(s, t)) in g.commodities.iter().enumerate() {
        match run_ortools(&g, s, t) {
            Some((f, _us)) => {
                println!("  commodity {i}: s={s}, t={t}, F* = {f}");
                sum_individual += f as f64;
            }
            None => println!("  commodity {i}: OR-Tools failed"),
        }
    }
    println!("  sum of single-commodity F* = {:.0}", sum_individual);

    // Run PN MC on the joined graph
    let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001];
    let res = spring_pn_mc(&g, &mu_schedule, 40, 1.0);
    println!("\n[Spring PN MC on joint disjoint graph]");
    for (i, f) in res.f_per.iter().enumerate() {
        println!("  commodity {i}: F_PN = {f:.4}");
    }
    println!("  F_total = {:.4}, iters={}, time={:.1} ms", res.f_total, res.iters, res.time_ms);

    let rel = (res.f_total - sum_individual).abs() / sum_individual.max(1.0) * 100.0;
    println!("\n  rel err vs single-commodity sum = {:.4}%", rel);
    println!("  This should be sub-1% if PN handles the decoupled case correctly.");

    // Also LP for sanity
    if let Some((lp_total, _, lp_ms)) = run_lp_oracle(&g) {
        let rel_lp = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
        println!("\n[LP oracle (HiGHS) — should also equal sum of singles]");
        println!("  F_total = {lp_total:.4} ({:.0} ms)", lp_ms);
        println!("  PN rel err vs LP: {rel_lp:.4}%, speedup = {:.1}×", lp_ms / res.time_ms);
    }
}
