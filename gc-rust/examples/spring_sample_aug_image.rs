//! Spring-based stochastic augmenting flow on DIMACS image graphs.
//! Parses .max files (vision2d/3d, grids), runs spring sample-augment,
//! compares to OR-Tools.

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
    fn unit(&mut self) -> f64 { (self.next_u64() as f64 / u64::MAX as f64).clamp(0.0, 1.0) }
}

#[derive(Clone)]
struct Graph {
    n: usize,
    edges: Vec<(usize, usize, i64)>,
    source: usize,
    sink: usize,
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
    // Collapse opposite-direction arcs to a single undirected edge with the
    // larger capacity (matches em_maxflow_image convention).
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
    Ok(Graph { n, edges, source: source - 1, sink: sink - 1 })
}

fn run_ortools(driver: &Path, file: &Path) -> Option<(i64, u64)> {
    let out = Command::new("python3").arg(driver).arg(file).output().ok()?;
    if !out.status.success() { return None; }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
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
struct AugResult {
    f_best: f64,
    augmentations: usize,
    pcg_iters: usize,
    time_ms: f64,
}

fn spring_sample_augment(
    g: &Graph,
    n_iters: usize,
    alpha_lo: f64,
    alpha_hi: f64,
    seed: u64,
) -> AugResult {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut f = vec![0.0_f64; m];
    let mut f_total = 0.0_f64;
    let mut f_best = 0.0_f64;

    let mut b = vec![0.0_f64; n];
    b[g.source] = 1.0;

    let mut total_pcg = 0_usize;
    let mut augmentations = 0_usize;
    let mut rng = Xs256::new(seed ^ 0xCAFE_F00D);
    let t0 = Instant::now();

    for _it in 0..n_iters {
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| {
                let resid = (caps[i] - f[i].abs()).max(1e-12);
                (u as u32, v as u32, resid)
            })
            .collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);

        let (phi, pcg_it) = pcg_pinned(&csr, &b, g.sink, 1e-10, 4 * n);
        total_pcg += pcg_it;

        let mut d = vec![0.0_f64; m];
        let mut div_s = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let resid = (caps[i] - f[i].abs()).max(1e-12);
            d[i] = (phi[u] - phi[v]) * resid;
            if u == g.source { div_s += d[i]; }
            if v == g.source { div_s -= d[i]; }
        }
        if (div_s - 1.0).abs() > 1e-3 { break; }

        let mut alpha_max = f64::INFINITY;
        for i in 0..m {
            let d_e = d[i]; let f_e = f[i]; let c_e = caps[i];
            if d_e.abs() < 1e-15 { continue; }
            let bound = if d_e > 0.0 { (c_e - f_e) / d_e } else { (-c_e - f_e) / d_e };
            if bound > 0.0 && bound < alpha_max { alpha_max = bound; }
        }
        if !alpha_max.is_finite() || alpha_max < 1e-9 { break; }

        let frac = alpha_lo + (alpha_hi - alpha_lo) * rng.unit();
        let alpha = alpha_max * frac;

        for i in 0..m { f[i] += alpha * d[i]; }
        f_total += alpha * div_s;
        augmentations += 1;
        if f_total > f_best { f_best = f_total; }
    }

    AugResult {
        f_best,
        augmentations,
        pcg_iters: total_pcg,
        time_ms: t0.elapsed().as_secs_f64() * 1000.0,
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let n_iters: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(2000);
    let alpha_lo: f64 = args.iter().find_map(|a| a.strip_prefix("--lo=")?.parse().ok()).unwrap_or(0.5);
    let alpha_hi: f64 = args.iter().find_map(|a| a.strip_prefix("--hi=")?.parse().ok()).unwrap_or(1.0);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let files: Vec<PathBuf> = args.iter()
        .skip(1)
        .filter(|a| !a.starts_with("--"))
        .map(PathBuf::from)
        .collect();
    if files.is_empty() {
        eprintln!("usage: spring_sample_aug_image graph1.max ... [--T=N] [--lo=F] [--hi=F] [--seed=N]");
        std::process::exit(1);
    }

    println!("# graph,n,m,F_sample,F_OR,rel_err_pct,augs,pcg_total,sample_ms,or_ms,slowdown");
    for gf in &files {
        let name = gf.file_name().and_then(|s| s.to_str()).unwrap_or("").trim_end_matches(".max").to_string();
        let g = match parse_dimacs(gf) { Ok(g) => g, Err(e) => { eprintln!("# parse {}: {e}", gf.display()); continue; } };
        let res = spring_sample_augment(&g, n_iters, alpha_lo, alpha_hi, seed);
        let ortools = run_ortools(driver, gf);
        let (f_or_str, or_ms_str, rel_str, slow_str) = match ortools {
            Some((f_or, us_or)) => {
                let or_ms = us_or as f64 / 1000.0;
                let f_or_f = f_or as f64;
                let abs_err = (res.f_best - f_or_f).abs();
                let rel = abs_err / f_or_f.max(1.0) * 100.0;
                let slow = if or_ms > 0.0 { res.time_ms / or_ms } else { f64::NAN };
                (format!("{f_or}"), format!("{or_ms:.3}"), format!("{rel:.4}"), format!("{slow:.1}x"))
            }
            None => ("?".into(), "?".into(), "?".into(), "?".into()),
        };
        println!("{},{},{},{:.4},{},{},{},{},{:.1},{},{}",
            name, g.n, g.edges.len(), res.f_best, f_or_str, rel_str,
            res.augmentations, res.pcg_iters, res.time_ms, or_ms_str, slow_str);
    }
}
