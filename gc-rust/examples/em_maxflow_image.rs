//! EM-style max-flow with vertex-pinned PCG, on DIMACS image graphs.
//! OR-Tools as ground truth.

use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::CsrLap;

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
            if last.0 as u32 == entry.0 && last.1 as u32 == entry.1 {
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

/// PCG with `pin` vertex held at φ = 0. Solves the exact (n−1)-dim
/// Laplacian without εI regularisation, so flows are conservation-
/// respecting at every internal vertex.
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

fn em_maxflow(g: &Graph, t_outer: usize, eta: f64) -> (f64, usize, usize, f64) {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();
    let mut b = vec![0.0_f64; n];
    b[g.source] = 1.0;
    let pin = g.sink;

    let pcg_tol = 1e-10;
    let max_pcg_iter = (4 * n).max(2000);
    let mut f_best = 0.0_f64;
    let mut total_pcg = 0_usize;
    let mut iters_run = 0_usize;

    let t0 = Instant::now();
    for k in 0..t_outer {
        iters_run = k + 1;
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i])).collect();
        let csr = CsrLap::from_canonical_weights(&weighted, n);
        let (phi, pcg_it) = pcg_pinned(&csr, &b, pin, pcg_tol, max_pcg_iter);
        total_pcg += pcg_it;

        let mut max_cong = 0.0_f64;
        let mut currents = vec![0.0_f64; m];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let f_e = (phi[u] - phi[v]) / r[i];
            currents[i] = f_e;
            let cong = f_e.abs() / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        if max_cong > 0.0 {
            let f_k = 1.0 / max_cong;
            if f_k > f_best { f_best = f_k; }
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
    let args: Vec<String> = std::env::args().collect();
    let driver_default = "/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py";
    let driver: PathBuf = args.iter().find_map(|a| a.strip_prefix("--ortools-driver=")).map(PathBuf::from).unwrap_or_else(|| PathBuf::from(driver_default));
    let t_outer: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(500);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(0.1);
    let graph_files: Vec<PathBuf> = args.iter().skip(1).filter(|a| !a.starts_with("--")).map(PathBuf::from).collect();
    if graph_files.is_empty() {
        eprintln!("usage: em_maxflow_image graph1.max ... [--T=N] [--eta=F]");
        std::process::exit(2);
    }
    println!("# T_outer={t_outer}, eta={eta}");
    println!("name,V,E,F_ortools,F_EM,abs_err,rel_err,iters,pcg_iters,em_ms,ortools_ms");
    for gf in &graph_files {
        let name = gf.file_name().and_then(|s| s.to_str()).unwrap_or("").trim_end_matches(".max").to_string();
        let g = match parse_dimacs(gf) { Ok(g) => g, Err(e) => { eprintln!("# parse {}: {e}", gf.display()); continue; } };
        let (f_or, us_or) = match run_ortools(&driver, gf) { Some(p) => p, None => { eprintln!("# ortools fail {name}"); continue; } };
        let (f_em, iters, pcg, em_ms) = em_maxflow(&g, t_outer, eta);
        let f_or = f_or as f64;
        let abs_err = (f_em - f_or).abs();
        let rel_err = abs_err / f_or.max(1.0);
        println!("{},{},{},{:.0},{:.2},{:.2},{:.5},{},{},{:.1},{:.2}",
            name, g.n, g.edges.len(), f_or, f_em, abs_err, rel_err, iters, pcg, em_ms, us_or as f64 / 1000.0);
    }
}
