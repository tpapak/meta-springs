//! Capacity-scaling EM max-flow.
//!
//! Outer schedule: from the largest capacity down to 1, restrict the
//! graph to "high-capacity" edges (c_e ≥ Δ) at each scale and run EM
//! on the restricted subgraph. Lower scales reuse the resistance state
//! from the previous scale (warm start).
//!
//! Idea: at large Δ, only the highest-capacity edges are active — the
//! subproblem has fewer edges and a tighter cut. EM should plateau near
//! optimal on this easier problem. As Δ shrinks, more edges activate
//! and EM refines the flow on the bigger graph from a good starting r.
//!
//! Vertex pinning is kept (no εI). OR-Tools is ground truth.

use std::io::Write;
use std::path::Path;
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
    writeln!(f, "p max {} {}", g.n, g.edges.len() * 2)?;
    writeln!(f, "n {} s", g.source + 1)?;
    writeln!(f, "n {} t", g.sink + 1)?;
    for &(u, v, c) in &g.edges {
        writeln!(f, "a {} {} {}", u + 1, v + 1, c)?;
        writeln!(f, "a {} {} {}", v + 1, u + 1, c)?;
    }
    Ok(())
}

fn parse_dimacs<P: AsRef<Path>>(path: P) -> Result<Graph, String> {
    let text = std::fs::read_to_string(path.as_ref()).map_err(|e| format!("{e}"))?;
    let mut n = 0usize; let mut source = 0usize; let mut sink = 0usize;
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
    let mut buf: Vec<(u32, u32, i64)> = directed.into_iter().filter_map(|(u, v, c)| {
        if u == v { None } else {
            let a0 = (u - 1) as u32; let b0 = (v - 1) as u32;
            Some(if a0 < b0 { (a0, b0, c) } else { (b0, a0, c) })
        }
    }).collect();
    buf.sort_unstable_by_key(|&(a, b, _)| (a, b));
    let mut edges: Vec<(usize, usize, i64)> = Vec::with_capacity(buf.len());
    for entry in buf {
        if let Some(last) = edges.last_mut() {
            if last.0 as u32 == entry.0 && last.1 as u32 == entry.1 {
                last.2 = last.2.max(entry.2); continue;
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

/// One EM stage: takes the *active edges* (subset of g.edges by index)
/// and runs T iters of MWU + electrical-flow updates. Returns updated
/// `r` and the best F seen on this restricted subgraph.
fn em_stage(
    g: &Graph,
    active: &[bool],
    caps: &[f64],
    r: &mut [f64],
    pin: usize,
    t_inner: usize,
    eta: f64,
) -> (f64, usize) {
    let n = g.n;
    let m = g.edges.len();
    let mut b = vec![0.0_f64; n];
    b[g.source] = 1.0;
    let pcg_tol = 1e-10;
    let max_pcg_iter = (4 * n).max(2000);

    let mut f_best = 0.0_f64;
    let mut total_pcg = 0;
    for _ in 0..t_inner {
        // Build CSR using only active edges; inactive edges contribute zero conductance.
        let weighted: Vec<(u32, u32, f64)> = g.edges.iter().enumerate()
            .filter_map(|(i, &(u, v, _))| if active[i] {
                Some((u as u32, v as u32, 1.0 / r[i]))
            } else { None })
            .collect();
        if weighted.is_empty() { return (0.0, 0); }
        let csr = CsrLap::from_canonical_weights(&weighted, n);
        let (phi, pcg_it) = pcg_pinned(&csr, &b, pin, pcg_tol, max_pcg_iter);
        total_pcg += pcg_it;

        let mut max_cong = 0.0_f64;
        let mut currents = vec![0.0_f64; m];
        for (i, &(u, v, _)) in g.edges.iter().enumerate() {
            if !active[i] { continue; }
            let f_e = (phi[u] - phi[v]) / r[i];
            currents[i] = f_e;
            let cong = f_e.abs() / caps[i];
            if cong > max_cong { max_cong = cong; }
        }
        if max_cong > 0.0 {
            let f_k = 1.0 / max_cong;
            if f_k > f_best { f_best = f_k; }
            for i in 0..m {
                if !active[i] { continue; }
                let load_norm = (currents[i].abs() / caps[i]) / max_cong;
                r[i] *= 1.0 + eta * load_norm * load_norm;
            }
            // geom-mean renormalise over active edges
            let n_active: usize = active.iter().filter(|&&a| a).count();
            if n_active > 0 {
                let log_sum: f64 = active.iter().enumerate()
                    .filter(|&(_, &a)| a)
                    .map(|(i, _)| r[i].ln()).sum();
                let geom = (log_sum / n_active as f64).exp();
                if geom.is_finite() && geom > 0.0 {
                    for (i, ri) in r.iter_mut().enumerate() {
                        if active[i] { *ri /= geom; }
                    }
                }
            }
        }
    }
    (f_best, total_pcg)
}

/// Capacity-scaling EM: outer outer loop sweeps Δ from c_max down to 1
/// in powers of 2; inner stage runs EM on edges with c_e ≥ Δ.
fn em_capscale(
    g: &Graph,
    t_per_scale: usize,
    eta: f64,
) -> (f64, usize, usize, f64) {
    let n = g.n;
    let m = g.edges.len();
    let caps: Vec<f64> = g.edges.iter().map(|&(_, _, c)| c as f64).collect();
    let c_max: f64 = caps.iter().cloned().fold(0.0, f64::max);
    let mut r: Vec<f64> = caps.iter().map(|c| 1.0 / (c * c).max(1e-12)).collect();
    let pin = g.sink;

    let mut f_best = 0.0_f64;
    let mut total_pcg = 0_usize;
    let mut total_iters = 0_usize;

    let n_levels = c_max.log2().ceil() as i32;
    let t0 = Instant::now();

    for level in (0..=n_levels).rev() {
        let threshold = 2.0_f64.powi(level);
        let active: Vec<bool> = caps.iter().map(|&c| c >= threshold).collect();
        let n_active: usize = active.iter().filter(|&&a| a).count();
        if n_active == 0 { continue; }
        let (f_lvl, pcg) = em_stage(g, &active, &caps, &mut r, pin, t_per_scale, eta);
        total_pcg += pcg;
        total_iters += t_per_scale;
        if f_lvl > f_best { f_best = f_lvl; }
    }
    // Final pass: all edges active, lots of iters.
    let active_all = vec![true; m];
    let (f_final, pcg_final) = em_stage(g, &active_all, &caps, &mut r, pin, t_per_scale * 4, eta);
    total_pcg += pcg_final;
    total_iters += t_per_scale * 4;
    if f_final > f_best { f_best = f_final; }

    (f_best, total_iters, total_pcg, t0.elapsed().as_secs_f64() * 1000.0)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let driver = Path::new("/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py");
    let dimacs: Option<String> = args.iter().find_map(|a| a.strip_prefix("--dimacs=")).map(String::from);
    let t_per_scale: usize = args.iter().find_map(|a| a.strip_prefix("--T=")?.parse().ok()).unwrap_or(100);
    let eta: f64 = args.iter().find_map(|a| a.strip_prefix("--eta=")?.parse().ok()).unwrap_or(0.1);

    if let Some(path) = dimacs {
        let pb = Path::new(&path);
        let g = parse_dimacs(pb).expect("parse dimacs");
        let (f_or, us_or) = run_ortools(driver, pb).expect("ortools");
        let (f_em, iters, pcg, em_ms) = em_capscale(&g, t_per_scale, eta);
        let f_or = f_or as f64;
        let abs_err = (f_em - f_or).abs();
        let rel_err = abs_err / f_or.max(1.0);
        let name = pb.file_name().and_then(|s| s.to_str()).unwrap_or("").trim_end_matches(".max");
        println!("name,V,E,F_ortools,F_EM,abs_err,rel_err,iters,pcg_iters,em_ms,ortools_ms");
        println!("{},{},{},{:.0},{:.2},{:.2},{:.5},{},{},{:.1},{:.2}",
            name, g.n, g.edges.len(), f_or, f_em, abs_err, rel_err, iters, pcg, em_ms, us_or as f64 / 1000.0);
        return;
    }

    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(200);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(1000);
    let n_seeds: u64 = args.iter().find_map(|a| a.strip_prefix("--seeds=")?.parse().ok()).unwrap_or(5);

    println!("# Cap-scaling EM. n={n}, m={m_target}, T_per_scale={t_per_scale}, eta={eta}");
    println!("seed,V,E,F_ortools,F_EM,abs_err,rel_err,iters,pcg_iters,em_ms,ortools_ms");
    let mut max_rel_err = 0.0_f64;
    for seed in 0..n_seeds {
        let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
        let tmp = std::env::temp_dir().join(format!("emcs_{seed}.max"));
        write_dimacs(&g, &tmp).unwrap();
        let (f_or, us_or) = run_ortools(driver, &tmp).expect("ortools");
        let (f_em, iters, pcg, em_ms) = em_capscale(&g, t_per_scale, eta);
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
