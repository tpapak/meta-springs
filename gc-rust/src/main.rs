//! CLI entry point. Reads a binary NMA arm-level JSON, runs the binomial
//! Grand Canonical pipeline, and writes a posterior-summary JSON.
//!
//! Two modes:
//!   - Single-rep (default, `--input <FILE>`): full per-prior payload with
//!     the heavy `tau2_grid` and per-effect `points` arrays — same schema as
//!     `proto_diabetes_gc.hs`.
//!   - Multi-rep (`--multi-rep <DIR>`): discovers `rep_*.json` files in DIR,
//!     runs the GC pipeline on each, and writes the lite multi-rep schema
//!     used by `proto_t30_reps_gc.hs` / `proto_t50_rare_gc.hs`.

use clap::Parser;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::Instant;

use gc_rust::data::{read_dataset, Dataset, Mode};
use gc_rust::gc::{grid_weights, posterior_under, run_adaptive_auto, Tau2Prior};
use gc_rust::output::{
    EffectEntry, EffectPoint, GridWeight, MultiRepReport, PriorReport,
    RepReport, Report, Tau2Summary,
};
use gc_rust::solver::Topology;

#[derive(Parser, Debug)]
#[command(name = "gc-rust", about = "Binomial GC NMA — Rust port")]
struct Args {
    /// Input arm-level JSON (single-rep mode). Mutually exclusive with --multi-rep.
    #[arg(long)]
    input: Option<String>,
    /// Directory of replicate JSONs (matched by `rep_*.json`, sorted by name).
    /// Triggers multi-rep mode.
    #[arg(long)]
    multi_rep: Option<String>,
    /// Output JSON path.
    #[arg(long)]
    output: String,
    /// Adaptive grid size (number of log-spaced τ² points).
    #[arg(long, default_value_t = 100)]
    n_grid: usize,
}

/// The four priors used by both multi-rep regimes (T=30 and T=50). Single-rep
/// uses the same set.
fn default_priors() -> Vec<Tau2Prior> {
    vec![
        Tau2Prior::Flat,
        Tau2Prior::HalfNormalTau(0.5),
        Tau2Prior::HalfNormalTau(1.0),
        Tau2Prior::HalfCauchyTau(0.5),
    ]
}

/// Run the GC pipeline + per-prior summaries on a single dataset.
///
/// `with_heavy = true` populates `tau2_grid` and per-effect `points` for the
/// single-rep schema.  `with_heavy = false` (multi-rep) leaves them `None`.
fn fit_one(
    ds: &Dataset,
    n_grid: usize,
    with_heavy: bool,
) -> (Topology, Vec<PriorReport>, f64, f64) {
    let t0 = Instant::now();
    let (topo, reml_mode, grid_full) = run_adaptive_auto(ds, n_grid);

    let ts: Vec<f64> = grid_full.iter().map(|g| g.tau2).collect();
    let lzs: Vec<f64> = grid_full.iter().map(|g| g.log_z).collect();
    let grid_lz: Vec<(f64, f64)> = ts.iter().copied().zip(lzs.iter().copied()).collect();

    let target_tids: Vec<(usize, String)> = topo
        .treatment_order
        .iter()
        .enumerate()
        .filter(|(i, _)| *i != 0)
        .map(|(i, t)| (i, t.clone()))
        .collect();

    let priors = default_priors();
    let mut prior_reports: Vec<PriorReport> = Vec::with_capacity(priors.len());
    for pr in &priors {
        let post = posterior_under(&grid_lz, *pr);
        let ws = grid_weights(&grid_lz, *pr);

        let mut effects = Vec::with_capacity(target_tids.len());
        for (tid_idx, tid) in &target_tids {
            let mus: Vec<f64> = grid_full.iter().map(|g| g.effects[*tid_idx]).collect();
            let vars: Vec<f64> = grid_full.iter().map(|g| g.eff_vars[*tid_idx]).collect();
            let mu: f64 = ws.iter().zip(mus.iter()).map(|(w, m)| w * m).sum();
            let mxx: f64 = ws
                .iter()
                .zip(mus.iter().zip(vars.iter()))
                .map(|(w, (m, v))| w * (m * m + v))
                .sum();
            let sd = (mxx - mu * mu).max(0.0).sqrt();
            let points = if with_heavy {
                Some(
                    ts.iter()
                        .zip(ws.iter())
                        .zip(mus.iter())
                        .zip(vars.iter())
                        .map(|(((&t, &w), &m), &v)| EffectPoint {
                            tau2: t,
                            weight: w,
                            mean: m,
                            var: v,
                        })
                        .collect(),
                )
            } else {
                None
            };
            effects.push(EffectEntry {
                to: tid.clone(),
                mean: mu,
                sd,
                points,
            });
        }

        let tau2_grid = if with_heavy {
            Some(
                ts.iter()
                    .zip(ws.iter())
                    .map(|(&t, &w)| GridWeight { tau2: t, weight: w })
                    .collect(),
            )
        } else {
            None
        };

        prior_reports.push(PriorReport {
            label: pr.label(),
            tau2: Tau2Summary {
                mode: post.mode,
                median: post.median,
                mean: post.mean,
                ci_lo: post.ci_lo,
                ci_hi: post.ci_hi,
            },
            tau2_grid,
            effects,
        });
    }

    let wall = t0.elapsed().as_secs_f64();
    (topo, prior_reports, wall, reml_mode)
}

/// Single-rep pipeline (preserves the original CLI behaviour).
fn run_single(input: &str, output: &str, n_grid: usize) {
    let ds = match read_dataset(input) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("error reading {input}: {e}");
            std::process::exit(1);
        }
    };
    let mode_label = match ds.mode {
        Mode::Binary => "binary",
        Mode::Continuous => "continuous",
    };
    eprintln!(
        "{}: {} studies, {} treatments ({} likelihood)",
        input,
        ds.studies.len(),
        ds.treatments.len(),
        mode_label
    );

    let (topo, prior_reports, fit_wall, reml_mode) = fit_one(&ds, n_grid, true);

    let report = Report {
        dataset: input.into(),
        fit_wall_sec: fit_wall,
        ref_treatment: topo.treatment_order[0].clone(),
        reml_mode_iterative: reml_mode,
        priors: prior_reports,
    };

    let json = serde_json::to_string(&report).expect("serialise json");
    fs::write(output, json).expect("write output");
    eprintln!(
        "GC fit + 4 priors: {:.3} s\nWrote {}",
        fit_wall, output
    );
}

/// Discover replicate JSONs in `dir`. Matches `rep_*.json` (any prefix
/// suffix), sorted by filename.
fn discover_reps(dir: &Path) -> Vec<PathBuf> {
    let entries = match fs::read_dir(dir) {
        Ok(e) => e,
        Err(e) => {
            eprintln!("error reading dir {}: {}", dir.display(), e);
            std::process::exit(1);
        }
    };
    let mut paths: Vec<PathBuf> = entries
        .filter_map(|e| e.ok().map(|e| e.path()))
        .filter(|p| {
            let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
            name.starts_with("rep_") && name.ends_with(".json")
        })
        .collect();
    paths.sort();
    paths
}

/// Multi-rep pipeline: same per-rep GC fit, lite per-prior payload, top-level
/// `total_wall_sec` / `n_reps` / `results` schema.
fn run_multi(dir: &str, output: &str, n_grid: usize) {
    let dir_p = Path::new(dir);
    let rep_paths = discover_reps(dir_p);
    if rep_paths.is_empty() {
        eprintln!("no rep_*.json files in {dir}");
        std::process::exit(1);
    }
    eprintln!(
        "GC multi-rep sweep: {} replicates from {} (n_grid={})",
        rep_paths.len(),
        dir,
        n_grid
    );

    let t_start = Instant::now();
    let mut results: Vec<RepReport> = Vec::with_capacity(rep_paths.len());
    for (i, p) in rep_paths.iter().enumerate() {
        let rep_no = i + 1;
        let ds = match read_dataset(p) {
            Ok(d) => d,
            Err(e) => {
                eprintln!("rep {rep_no}: read err: {e}");
                continue;
            }
        };
        let (_topo, prior_reports, wall, reml_mode) = fit_one(&ds, n_grid, false);
        eprintln!(
            "rep {:02}: {:.2}s ({} studies, {} treatments)",
            rep_no,
            wall,
            ds.studies.len(),
            ds.treatments.len()
        );
        results.push(RepReport {
            rep: rep_no,
            wall_sec: wall,
            reml_mode_iterative: reml_mode,
            priors: prior_reports,
        });
    }
    let total = t_start.elapsed().as_secs_f64();

    let report = MultiRepReport {
        total_wall_sec: total,
        n_reps: results.len(),
        results,
    };
    let json = serde_json::to_string(&report).expect("serialise json");
    fs::write(output, json).expect("write output");
    eprintln!(
        "\nGC multi-rep total wall: {:.1}s\nWrote {}",
        total, output
    );
}

fn main() {
    let args = Args::parse();
    match (&args.input, &args.multi_rep) {
        (None, None) => {
            eprintln!("must supply either --input or --multi-rep");
            std::process::exit(2);
        }
        (Some(_), Some(_)) => {
            eprintln!("--input and --multi-rep are mutually exclusive");
            std::process::exit(2);
        }
        (Some(inp), None) => run_single(inp, &args.output, args.n_grid),
        (None, Some(dir)) => run_multi(dir, &args.output, args.n_grid),
    }
}
