//! Same correctness-verified algorithm as `gc_maxflow.rs`, run on
//! DIMACS image graphs from Tide's graph-pool. OR-Tools as ground truth.
//!
//! Run:
//!   cargo run --release --example gc_maxflow_image -- \
//!       /path/to/vision2d_100x100.max /path/to/rfim3d_64.max ...

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Instant;

use gc_rust::lap_solver::{pcg_solve, CsrLap, Jacobi};

#[derive(Debug, Clone)]
struct Graph {
    n: usize,
    edges: Vec<(usize, usize, i64)>, // canonical undirected, u < v
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
        if line.is_empty() || line.starts_with('c') {
            continue;
        }
        let mut it = line.split_whitespace();
        let tok = it.next().unwrap_or("");
        match tok {
            "p" => {
                let _ = it.next();
                n = it.next().ok_or("p N")?.parse().map_err(|e| format!("{e}"))?;
                let _: usize = it.next().ok_or("p M")?.parse().map_err(|e| format!("{e}"))?;
            }
            "n" => {
                let id: usize = it.next().ok_or("n id")?.parse().map_err(|e| format!("{e}"))?;
                match it.next().unwrap_or("") {
                    "s" => source = id,
                    "t" => sink = id,
                    _ => {}
                }
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
    if n == 0 || source == 0 || sink == 0 {
        return Err(format!("incomplete DIMACS: n={n} s={source} t={sink}"));
    }
    // Canonicalise: for each (u, v) pair (lex), take max of directed caps.
    // Symmetric arcs (c_uv = c_vu) collapse to single edge of cap c.
    let mut buf: Vec<(u32, u32, i64)> = directed
        .into_iter()
        .filter_map(|(u, v, c)| {
            if u == v {
                return None;
            }
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

fn run_ortools(driver: &Path, graph_file: &Path) -> Option<(i64, u64)> {
    let out = Command::new("python3")
        .arg(driver)
        .arg(graph_file)
        .output()
        .ok()?;
    if !out.status.success() {
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    let flow: i64 = it.next()?.parse().ok()?;
    let us: u64 = it.next()?.parse().ok()?;
    Some((flow, us))
}

fn build_csr(edges: &[(usize, usize, i64)], r: &[f64], n: usize) -> CsrLap {
    let weighted: Vec<(u32, u32, f64)> = edges
        .iter()
        .enumerate()
        .map(|(i, &(u, v, _))| (u as u32, v as u32, 1.0 / r[i]))
        .collect();
    CsrLap::from_canonical_weights(&weighted, n)
}

fn test_feasibility_at_F(
    g: &Graph,
    f_target: f64,
    t_inner: usize,
    eta: f64,
    slack: f64,
    pcg_tol: f64,
) -> (bool, f64, usize) {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();
    let mut b = vec![0.0_f64; n];
    b[g.source] = f_target;
    b[g.sink] = -f_target;
    let eps_reg = 1e-10;
    let max_pcg_iter = (4 * n).max(2000);
    let mut total_pcg = 0_usize;
    let mut min_max_cong = f64::INFINITY;

    for _ in 0..t_inner {
        let csr = build_csr(&g.edges, &r, n);
        let prec = Jacobi::new(&csr, eps_reg);
        let res = pcg_solve(&csr, eps_reg, &b, &prec, pcg_tol, max_pcg_iter);
        total_pcg += res.iters;
        let phi = &res.x;

        let mut max_cong = 0.0_f64;
        let mut currents = vec![0.0_f64; m];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            let f_e = (phi[u] - phi[v]) / r[i];
            currents[i] = f_e;
            let cong = f_e.abs() / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        if max_cong < min_max_cong { min_max_cong = max_cong; }

        // Witness: explicit divergence check at source.
        let mut div_s = 0.0_f64;
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            if u == g.source { div_s += currents[i]; }
            if v == g.source { div_s -= currents[i]; }
        }
        let value_match = (div_s - f_target).abs() <= 1e-6 * f_target.abs().max(1.0);
        if max_cong <= 1.0 + slack && value_match {
            return (true, min_max_cong, total_pcg);
        }
        if max_cong > 0.0 {
            for i in 0..m {
                let load_norm = (currents[i].abs() / caps[i]) / max_cong;
                r[i] *= 1.0 + eta * load_norm * load_norm;
            }
            let log_sum: f64 = r.iter().map(|x| x.ln()).sum();
            let geom = (log_sum / m as f64).exp();
            if geom.is_finite() && geom > 0.0 {
                for ri in r.iter_mut() {
                    *ri /= geom;
                }
            }
        }
    }
    (false, min_max_cong, total_pcg)
}

fn gc_maxflow(g: &Graph, rel_tol: f64, t_inner: usize, eta: f64, slack: f64) -> (f64, usize, usize, f64) {
    let mut f_hi: f64 = 0.0;
    for &(u, v, c) in &g.edges {
        if u == g.source || v == g.source {
            f_hi += c as f64;
        }
    }
    let mut f_lo: f64 = 0.0;
    let mut steps = 0_usize;
    let mut total_pcg = 0_usize;
    let t0 = Instant::now();
    while f_hi - f_lo > rel_tol * f_hi.max(1.0) && steps < 50 {
        let f_try = 0.5 * (f_lo + f_hi);
        let (feasible, _mc, pcg) = test_feasibility_at_F(g, f_try, t_inner, eta, slack, 1e-7);
        total_pcg += pcg;
        if feasible {
            f_lo = f_try;
        } else {
            f_hi = f_try;
        }
        steps += 1;
    }
    (f_lo, steps, total_pcg, t0.elapsed().as_secs_f64() * 1000.0)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver_default = "/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py";
    let driver: PathBuf = args
        .iter()
        .find_map(|a| a.strip_prefix("--ortools-driver="))
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(driver_default));
    let t_inner: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(300);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(0.1);
    let slack: f64 = args.iter().find_map(|a| a.strip_prefix("--slack=")?.parse().ok()).unwrap_or(0.001);
    let rel_tol: f64 = args.iter().find_map(|a| a.strip_prefix("--rel-tol=")?.parse().ok()).unwrap_or(1e-3);

    let graph_files: Vec<PathBuf> = args
        .iter()
        .skip(1)
        .filter(|a| !a.starts_with("--"))
        .map(PathBuf::from)
        .collect();

    if graph_files.is_empty() {
        eprintln!("usage: gc_maxflow_image graph1.max graph2.max ... [--T=N] [--eta=F] [--slack=F]");
        std::process::exit(2);
    }

    println!("# T_inner={t_inner}, eta={eta}, slack={slack}, rel_tol={rel_tol}");
    println!("name,V,E,F_ortools,F_GC,abs_err,rel_err,bisect_steps,pcg_iters,gc_ms,ortools_ms");

    for gf in &graph_files {
        let name = gf
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("")
            .trim_end_matches(".max")
            .to_string();
        let g = match parse_dimacs(gf) {
            Ok(g) => g,
            Err(e) => {
                eprintln!("# parse error on {}: {e}", gf.display());
                continue;
            }
        };
        let (f_or, us_or) = match run_ortools(&driver, gf) {
            Some(p) => p,
            None => {
                eprintln!("# ortools failed on {name}");
                continue;
            }
        };
        let f_or = f_or as f64;
        let or_ms = us_or as f64 / 1000.0;

        let (f_gc, steps, pcg_iters, gc_ms) = gc_maxflow(&g, rel_tol, t_inner, eta, slack);
        let abs_err = (f_gc - f_or).abs();
        let rel_err = abs_err / f_or.max(1.0);
        println!(
            "{},{},{},{:.0},{:.2},{:.2},{:.5},{},{},{:.1},{:.2}",
            name, g.n, g.edges.len(), f_or, f_gc, abs_err, rel_err, steps, pcg_iters, gc_ms, or_ms
        );
    }
    let _ = std::io::stdout().flush();
}
