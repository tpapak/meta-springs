//! K=1 (single-commodity) head-to-head: our slack-variable LP-IPM vs
//! OR-Tools push-relabel max-flow.
//!
//! For K=1 the multi-axis machinery collapses to a single-axis Newton-CG
//! IPM on a sparse Laplacian. Push-relabel is the gold standard for
//! single-commodity max-flow — much faster than any LP-based method on
//! integer-capacity graphs. We don't expect to win, but the gap tells
//! us how much pure-IPM overhead vs combinatorial structure costs.

use std::path::Path;
use std::process::Command;
use std::time::Instant;

// ---------- RNG / graph (same as spring_lpipm_mc) ----------

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
    // Single commodity: pick source = 0, sink = n-1 (deterministic, comparable).
    let source = 0;
    let sink = n - 1;
    Graph { n, edges, source, sink }
}

fn dump_dimacs(g: &Graph, path: &Path) -> std::io::Result<()> {
    use std::io::Write;
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

fn run_ortools(file: &Path) -> Option<(i64, u64)> {
    let driver = "/Users/tosku/Sync/Documents/tide-maxflow/solvers/drivers/ortools_maxflow.py";
    let out = Command::new("python3").arg(driver).arg(file).output().ok()?;
    if !out.status.success() {
        eprintln!("OR-Tools err: {}", String::from_utf8_lossy(&out.stderr));
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout);
    let line = s.lines().next()?;
    let mut it = line.split(',');
    Some((it.next()?.parse().ok()?, it.next()?.parse().ok()?))
}

/// Run our spring_lpipm_mc binary as subprocess with K=1.
/// Returns (F_total, time_ms).
fn run_spring(g: &Graph, n: usize, m: usize, seed: u64) -> Option<(f64, f64)> {
    let _ = (g, m);
    let bin = "/Users/tosku/Sync/Documents/slmm/gc-rust/target/release/examples/spring_lpipm_mc";
    let t0 = Instant::now();
    let out = Command::new(bin)
        .arg(format!("--n={}", n))
        .arg(format!("--m={}", g.edges.len()))
        .arg("--k=1")
        .arg(format!("--seed={}", seed))
        .arg("--inner=40")
        .arg("--no-lp")
        .output().ok()?;
    let elapsed_ms = t0.elapsed().as_secs_f64() * 1000.0;
    if !out.status.success() {
        eprintln!("spring err: {}", String::from_utf8_lossy(&out.stderr));
        return None;
    }
    let s = String::from_utf8_lossy(&out.stdout);
    // Parse "F_total = X" — but spring uses different commodity assignment.
    // Find F_total line.
    let f_total: f64 = s.lines()
        .find_map(|l| l.trim().strip_prefix("F_total ="))
        .and_then(|rest| rest.split_whitespace().next())
        .and_then(|x| x.parse().ok())
        .unwrap_or(f64::NAN);
    Some((f_total, elapsed_ms))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let n: usize = args.iter().find_map(|a| a.strip_prefix("--n=")?.parse().ok()).unwrap_or(200);
    let m_target: usize = args.iter().find_map(|a| a.strip_prefix("--m=")?.parse().ok()).unwrap_or(1000);
    let seed: u64 = args.iter().find_map(|a| a.strip_prefix("--seed=")?.parse().ok()).unwrap_or(0);

    let g = gen_graph(n, m_target, 0xDEAD_BEEF_C0DE_F00D ^ seed);
    println!("# K=1 head-to-head: spring LP-IPM vs OR-Tools push-relabel");
    println!("# n={n}, m={}, seed={seed}, source={}, sink={}", g.edges.len(), g.source, g.sink);
    println!();

    // OR-Tools.
    let dimacs = std::env::temp_dir().join(format!("k1_{}.max", std::process::id()));
    dump_dimacs(&g, &dimacs).expect("write dimacs");
    let or = run_ortools(&dimacs);
    let _ = std::fs::remove_file(&dimacs);
    match or {
        Some((f_or, us_or)) => {
            println!("[OR-Tools push-relabel]");
            println!("  F = {}, solve = {} μs ({:.3} ms)", f_or, us_or, us_or as f64 / 1000.0);
        }
        None => {
            println!("[OR-Tools] FAILED");
            return;
        }
    }
    println!();

    // Our spring solver. Note: spring_lpipm_mc generates its own graph from
    // (n, m, seed); we use the same RNG seed so the graph IS the same.
    // But spring_lpipm_mc picks K random commodities — for K=1 that's one
    // random (s, t) pair from its own RNG. The throughput value should
    // still be a valid max-flow on the same graph between OUR spring's
    // chosen terminals (different from OR-Tools' source=0 sink=n-1).
    //
    // For an apples-to-apples test we'd need to align the (s, t) choice.
    // Since spring_lpipm_mc doesn't expose --source/--sink, we report
    // both numbers but caveat that the s-t pair differs — F values will
    // differ, only the runtime comparison is meaningful.
    if let Some((f_sp, ms_sp)) = run_spring(&g, n, m_target, seed) {
        println!("[Spring LP-IPM (K=1, slack-variable + multi-axis Schur)]");
        println!("  F = {:.4}, total wall = {:.1} ms", f_sp, ms_sp);
        println!("  (s,t pair differs from OR-Tools — runtime is the comparable metric)");
    }
}
