//! Standalone CLI for the contribution-matrix port (shortest-path only).
//!
//! Reads a wide-form hat-matrix JSON (`[{"row":..., "comparison":..., "value":...}, ...]`)
//! and writes:
//!   * the per-comparison contribution matrix (always),
//!   * the per-row stream list (path contributions, with `--streams`),
//!   * the per-study contribution table (with `--study-weights` + `--studies`).

use clap::Parser;
use std::fs;

use gc_rust::contribution::{
    contribution_matrix, contribution_matrix_to_list, hat_matrix_from_list,
    stream_matrix_to_list, study_contributions, sum_contribution_row, HElement,
    PairwiseWeight,
};

#[derive(Parser, Debug)]
#[command(name = "contribution-rust", about = "Hat matrix → contribution / stream / study tables (shortest path)")]
struct Args {
    /// Input hat-matrix JSON (wide form).
    #[arg(long)]
    input: String,
    /// Output contribution-matrix JSON (wide form: contrast contributions).
    #[arg(long)]
    output: String,
    /// If set, write per-row streams (path contributions) here.
    #[arg(long)]
    streams: Option<String>,
    /// Per-study IV weights JSON (`[{"study":..., "comparison":..., "weight":...}]`).
    /// Required for `--studies` output.
    #[arg(long)]
    study_weights: Option<String>,
    /// If set, write per-study contributions here. Requires `--study-weights`.
    #[arg(long)]
    studies: Option<String>,
    /// Print per-row sums and max residual flow to stderr.
    #[arg(long, default_value_t = false)]
    check_sums: bool,
}

fn main() {
    let args = Args::parse();
    let raw = match fs::read_to_string(&args.input) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error reading {}: {e}", args.input);
            std::process::exit(1);
        }
    };
    let els: Vec<HElement> = match serde_json::from_str(&raw) {
        Ok(v) => v,
        Err(e) => {
            eprintln!("error parsing {}: {e}", args.input);
            std::process::exit(1);
        }
    };
    let hm = hat_matrix_from_list(&els);

    let (cm, sm, graphs) = contribution_matrix(&hm);

    if args.check_sums {
        let mut max_dev = 0.0_f64;
        for (rid, cr) in &cm {
            let s = sum_contribution_row(cr);
            let dev = (s - 1.0).abs();
            if dev > max_dev { max_dev = dev; }
            eprintln!("row {rid}: sum = {s:.6}");
        }
        eprintln!("max |sum - 1| = {max_dev:.6}");
        let mut max_residue = 0.0_f64;
        for g in graphs.values() {
            for f in g.residual_flows() {
                if f > max_residue { max_residue = f; }
            }
        }
        eprintln!("max residual flow = {max_residue:.6}");
    }

    let cm_list = contribution_matrix_to_list(&cm);
    let cm_json = serde_json::to_string(&cm_list).expect("serialise contribution matrix");
    if let Err(e) = fs::write(&args.output, cm_json) {
        eprintln!("error writing {}: {e}", args.output);
        std::process::exit(1);
    }
    eprintln!("wrote {}", args.output);

    if let Some(p) = &args.streams {
        let sm_list = stream_matrix_to_list(&sm);
        let sm_json = serde_json::to_string(&sm_list).expect("serialise stream matrix");
        if let Err(e) = fs::write(p, sm_json) {
            eprintln!("error writing {p}: {e}");
            std::process::exit(1);
        }
        eprintln!("wrote {p}");
    }

    if let Some(out_p) = &args.studies {
        let weights_path = match &args.study_weights {
            Some(p) => p,
            None => {
                eprintln!("--studies requires --study-weights");
                std::process::exit(2);
            }
        };
        let raw_w = match fs::read_to_string(weights_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error reading {weights_path}: {e}");
                std::process::exit(1);
            }
        };
        let weights: Vec<PairwiseWeight> = match serde_json::from_str(&raw_w) {
            Ok(v) => v,
            Err(e) => {
                eprintln!("error parsing {weights_path}: {e}");
                std::process::exit(1);
            }
        };
        let table = study_contributions(&cm, &weights);
        let sc_json = serde_json::to_string(&table).expect("serialise study contributions");
        if let Err(e) = fs::write(out_p, sc_json) {
            eprintln!("error writing {out_p}: {e}");
            std::process::exit(1);
        }
        eprintln!("wrote {out_p}");
    }
}
