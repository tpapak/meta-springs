//! Projected Newton MC on real DIMACS .max graphs.
//!
//! Loads a structured graph, picks K random source/sink pairs deterministically
//! from the seed, runs spring-PN MC, compares to LP and to the per-commodity
//! single-commodity max-flow sum (decoupled bound).

use std::path::{Path, PathBuf};
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
    source: usize,
    sink: usize,
    commodities: Vec<(usize, usize)>,
}

fn parse_dimacs<P: AsRef<Path>>(path: P) -> Result<Graph, String> {
    let text = std::fs::read_to_string(path.as_ref()).map_err(|e| format!("{e}"))?;
    let mut n = 0usize;
    let mut source = 0usize;
    let mut sink = 0usize;
    let mut directed: Vec<(usize, usize, i64)> = Vec::new();
    for line in text.lines() {
        let line = line.trim();
        if line.is_empty() || line.starts_with('c') { continue; }
        let mut it = line.split_whitespace();
        match it.next().unwrap_or("") {
            "p" => { let _ = it.next(); n = it.next().ok_or("p N")?.parse().map_err(|e| format!("{e}"))?; }
            "n" => {
                let id: usize = it.next().ok_or("n id")?.parse().map_err(|e| format!("{e}"))?;
                match it.next().unwrap_or("") { "s" => source = id, "t" => sink = id, _ => {} }
            }
            "a" => {
                let u: usize = it.next().ok_or("a u")?.parse().map_err(|e| format!("{e}"))?;
                let v: usize = it.next().ok_or("a v")?.parse().map_err(|e| format!("{e}"))?;
                let c: i64 = it.next().ok_or("a c")?.parse().map_err(|e| format!("{e}"))?;
                directed.push((u, v, c));
            }
            _ => {}
        }
    }
    if n == 0 || source == 0 || sink == 0 { return Err("incomplete".into()); }
    let mut buf: Vec<(u32, u32, i64)> = directed
        .into_iter()
        .filter_map(|(u, v, c)| {
            if u == v { return None; }
            let a0 = (u - 1) as u32;
            let b0 = (v - 1) as u32;
            Some(if a0 < b0 { (a0, b0, c) } else { (b0, a0, c) })
        })
        .collect();
    buf.sort_unstable_by_key(|&(a, b, _)| (a, b));
    let mut edges: Vec<(usize, usize, i64)> = Vec::with_capacity(buf.len());
    for entry in buf {
        if let Some(last) = edges.last_mut() {
            if last.0 as u32 == entry.0 as u32 && last.1 as u32 == entry.1 as u32 {
                last.2 = last.2.max(entry.2);
                continue;
            }
        }
        edges.push((entry.0 as usize, entry.1 as usize, entry.2));
    }
    Ok(Graph {
        n,
        edges,
        source: source - 1,
        sink: sink - 1,
        commodities: vec![],
    })
}

fn add_random_commodities(g: &mut Graph, k: usize, seed: u64) {
    let mut rng = Xs256::new(seed);
    let mut commodities = Vec::with_capacity(k);
    // First commodity = the file's native s,t. Remaining = random pairs.
    commodities.push((g.source, g.sink));
    while commodities.len() < k {
        let s = rng.gen_range(0, g.n as u64) as usize;
        let t = rng.gen_range(0, g.n as u64) as usize;
        if s == t { continue; }
        commodities.push((s, t));
    }
    g.commodities = commodities;
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
fn smooth_abs_hess(x: f64) -> f64 {
    let s = smooth_abs(x); (ABS_EPS * ABS_EPS) / (s * s * s)
}

fn potential_u(f: &[f64], g: &Graph, caps: &[f64], mu: f64) -> f64 {
    let m = g.edges.len();
    let k = g.commodities.len();
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

fn grad_and_hess_diag(
    f: &[f64], g: &Graph, caps: &[f64], mu: f64,
    grad: &mut [f64], h_diag: &mut [f64], f_per: &mut [f64],
) {
    let m = g.edges.len();
    let k = g.commodities.len();
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
    let inner: usize = args.iter().find_map(|a| a.strip_prefix("--inner=")?.parse().ok()).unwrap_or(40);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);
    let no_lp = args.iter().any(|a| a == "--no-lp");

    let files: Vec<PathBuf> = args.iter().skip(1)
        .filter(|a| !a.starts_with("--"))
        .map(PathBuf::from)
        .collect();
    if files.is_empty() {
        eprintln!("usage: spring_pn_mc_image graph1.max ... [--k=N] [--inner=N] [--seed=N] [--no-lp]");
        std::process::exit(1);
    }

    println!("graph,n,m,K,F_PN,F_LP,rel_err_pct,iters,PN_ms,LP_ms,speedup");
    for path in &files {
        let name = path.file_name().and_then(|s| s.to_str()).unwrap_or("").trim_end_matches(".max").to_string();
        let mut g = match parse_dimacs(path) {
            Ok(g) => g,
            Err(e) => { eprintln!("# {} parse error: {e}", path.display()); continue; }
        };
        add_random_commodities(&mut g, k, 0xDEAD_BEEF ^ seed);

        let mu_schedule = vec![1.0, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001, 0.0003, 0.0001];
        let res = spring_pn_mc(&g, &mu_schedule, inner, 1.0);

        let (lp_str, rel_str, spd_str, lp_ms_str) = if no_lp {
            ("(skipped)".to_string(), "?".to_string(), "?".to_string(), "?".to_string())
        } else {
            match run_lp_oracle(&g) {
                Some((lp_total, _, lp_ms)) => {
                    let rel = (res.f_total - lp_total).abs() / lp_total.max(1.0) * 100.0;
                    let spd = if lp_ms > 0.0 { lp_ms / res.time_ms } else { f64::NAN };
                    (format!("{lp_total:.2}"), format!("{rel:.4}"), format!("{spd:.1}x"), format!("{lp_ms:.0}"))
                }
                None => ("(LP failed)".to_string(), "?".to_string(), "?".to_string(), "?".to_string()),
            }
        };
        println!("{},{},{},{},{:.2},{},{},{},{:.0},{},{}",
            name, g.n, g.edges.len(), k, res.f_total, lp_str, rel_str,
            res.iters, res.time_ms, lp_ms_str, spd_str);
    }
}
