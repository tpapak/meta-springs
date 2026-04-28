//! End-to-end Rust pipeline: IV-format dataset → CINeMA v3 `.cnm`.
//!
//! No R. The chain is:
//!   1. Load IV contrasts (CSV or `runNMA` request JSON).
//!   2. Fit random-effects NMA via the spring / GC REML mode finder
//!      (`gc-rust::nma_iv::fit_iv_reml`).
//!   3. Compute Davies long hat matrix from the converged fit.
//!   4. Run shortest-path streams (`gc-rust::contribution::*`) on H to
//!      get contribution matrix + per-row streams.
//!   5. Redistribute contributions across studies using the per-row IV
//!      weights produced at step 2.
//!   6. Compute pairwise heterogeneity, design-by-treatment Q, netsplit
//!      (SIDE), and league table.
//!   7. Assemble v3 `.cnm`.

use clap::Parser;
use gc_rust::contribution::{
    contribution_matrix_shortest_path, hat_matrix_from_list, study_contributions,
    sum_contribution_row, HElement, PairwiseWeight,
};
use gc_rust::nma_iv::{
    decomp_design, fit_iv_reml, hatmatrix_long, iv_weights_per_row, league_table,
    netsplit, nma_results, pairwise_heterogeneity, IvDataset, IvRow,
};
use serde_json::{json, Map, Value};
use std::collections::BTreeMap;
use std::fs;
use std::time::Instant;

#[derive(Parser, Debug)]
#[command(name = "cnm-from-iv", about = "IV-format dataset → v3 .cnm (no R)")]
struct Args {
    /// IV-format input. Either a CSV with columns
    /// `id,t1,t2,effect,se[,rob,indirectness]`, or a `runNMA` request
    /// JSON `{indata: [...], type: "iv", ...}`.
    #[arg(long)]
    input: String,
    /// Optional explicit input format hint: `csv` or `json`. If omitted,
    /// inferred from the file extension.
    #[arg(long)]
    format: Option<String>,
    /// Project title written into the `.cnm`.
    #[arg(long, default_value = "rust_export")]
    title: String,
    /// Effect-measure label written into the `.cnm` (controls league
    /// table scaling — `"OR"`/`"RR"`/`"HR"` use exp; otherwise identity).
    #[arg(long, default_value = "OR")]
    sm: String,
    /// Output `.cnm` path.
    #[arg(long)]
    output: String,
    /// Print per-stage wall times to stderr.
    #[arg(long, default_value_t = false)]
    timing: bool,
}

fn now_iso() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    format!("{}T00:00:00.000Z", chrono_like_date(secs))
}
fn chrono_like_date(secs: u64) -> String {
    let days = secs / 86400;
    let mut y = 1970i64;
    let mut d = days as i64;
    loop {
        let yd = if is_leap(y) { 366 } else { 365 };
        if d < yd { break; }
        d -= yd; y += 1;
    }
    let months_d: [i64; 12] = if is_leap(y) {
        [31,29,31,30,31,30,31,31,30,31,30,31]
    } else {
        [31,28,31,30,31,30,31,31,30,31,30,31]
    };
    let mut m = 0;
    while d >= months_d[m] { d -= months_d[m]; m += 1; }
    format!("{:04}-{:02}-{:02}", y, m + 1, (d + 1) as i64)
}
fn is_leap(y: i64) -> bool { (y % 4 == 0 && y % 100 != 0) || y % 400 == 0 }

fn studies_v3(rows: &[IvRow]) -> Vec<Value> {
    rows.iter().map(|r| {
        json!({
            "id": r.id,
            "t1": r.t1,
            "t2": r.t2,
            "effect": r.effect,
            "se": r.se,
            "rob": r.rob,
            "indirectness": r.indirectness,
        })
    }).collect()
}

fn main() {
    let args = Args::parse();
    let total = Instant::now();

    // ── 1. Load dataset ──────────────────────────────────────────────
    let t0 = Instant::now();
    let fmt = args.format.clone().unwrap_or_else(|| {
        if args.input.to_lowercase().ends_with(".csv") { "csv".into() } else { "json".into() }
    });
    let d = match fmt.as_str() {
        "csv"  => IvDataset::from_csv(&args.input),
        "json" => IvDataset::from_request_json(&args.input),
        other  => Err(format!("unknown --format `{other}` (expected csv|json)")),
    }
    .unwrap_or_else(|e| { eprintln!("load: {e}"); std::process::exit(1) });
    if args.timing {
        eprintln!("  load            : {:>7.2}s ({} contrasts, {} studies, {} treatments)",
            t0.elapsed().as_secs_f64(), d.n_rows(),
            d.rows.iter().map(|r| &r.id).collect::<std::collections::BTreeSet<_>>().len(),
            d.n_treats());
    }

    // ── 2. NMA fit (REML τ² via spring / GC mode finder) ─────────────
    let t0 = Instant::now();
    let fit = fit_iv_reml(&d);
    if args.timing {
        eprintln!("  fit_iv_reml     : {:>7.2}s (τ² = {:.6})", t0.elapsed().as_secs_f64(), fit.tau2);
    }

    // ── 3. Hat matrix ────────────────────────────────────────────────
    let t0 = Instant::now();
    let hm = hatmatrix_long(&d, &fit);
    if args.timing {
        eprintln!("  hatmatrix       : {:>7.2}s ({}x{})",
            t0.elapsed().as_secs_f64(), hm.row_names.len(), hm.col_names.len());
    }

    // ── 4. Contribution matrix via streams ───────────────────────────
    let t0 = Instant::now();
    let wide: Vec<HElement> = hm.data.iter().zip(hm.row_names.iter())
        .flat_map(|(row, rid)| row.iter().map(move |(cid, v)| HElement {
            row: rid.clone(),
            comparison: cid.clone(),
            value: *v,
        })).collect();
    let cm = contribution_matrix_shortest_path(&hat_matrix_from_list(&wide));
    if args.timing {
        eprintln!("  contribution    : {:>7.2}s ({} cells)", t0.elapsed().as_secs_f64(),
            cm.values().map(|r| r.len()).sum::<usize>());
    }

    // ── 5. Per-study redistribution ──────────────────────────────────
    let t0 = Instant::now();
    let weights: Vec<PairwiseWeight> = iv_weights_per_row(&d, &fit)
        .into_iter()
        .map(|(study, comparison, weight)| PairwiseWeight { study, comparison, weight })
        .collect();
    let study_table = study_contributions(&cm, &weights);
    if args.timing {
        eprintln!("  study_contribs  : {:>7.2}s ({} cells)", t0.elapsed().as_secs_f64(), study_table.len());
    }

    // ── 6. Aux statistics ────────────────────────────────────────────
    let t0 = Instant::now();
    let nma_rows  = nma_results(&d, &fit);
    let pairwise  = pairwise_heterogeneity(&d);
    let dd        = decomp_design(&d);
    let side_rows = netsplit(&d, &fit);
    let league    = league_table(&d, &fit, &args.sm);
    if args.timing {
        eprintln!("  nma stats       : {:>7.2}s (nma {}, pw {}, side {})",
            t0.elapsed().as_secs_f64(),
            nma_rows.len(), pairwise.len(), side_rows.len());
    }

    // ── 7. Assemble v3 .cnm ─────────────────────────────────────────
    let t0 = Instant::now();
    // Contribution-matrix sanity check.
    let mut max_dev = 0.0_f64;
    for cr in cm.values() {
        let dev = (sum_contribution_row(cr) - 1.0).abs();
        if dev > max_dev { max_dev = dev; }
    }

    // Hat matrix as dense matrix with explicit colNames ordering.
    let h_cols: Vec<String> = hm.col_names.clone();
    let h_dense: Vec<Vec<f64>> = hm.data.iter().map(|row| {
        h_cols.iter().map(|c| row.get(c).copied().unwrap_or(0.0)).collect()
    }).collect();

    // Nested studyContributions: {comparison: {study: contribution}}.
    let mut sc_nested: BTreeMap<String, BTreeMap<String, f64>> = BTreeMap::new();
    for r in &study_table {
        sc_nested.entry(r.comparison.clone()).or_default()
            .insert(r.study.clone(), r.contribution);
    }

    // SIDE merged into NMA results in v3 shape.
    let side_by_cmp: std::collections::HashMap<String, _> =
        side_rows.iter().map(|s| (s.comparison.clone(), s)).collect();
    let nma_v3: Vec<Value> = nma_rows.iter().map(|r| {
        let mut o = Map::new();
        o.insert("comparison".into(), json!(r.row));
        o.insert("effect".into(), json!(r.te));
        o.insert("se".into(), json!(r.se));
        o.insert("ciLower".into(), json!(r.lower_ci));
        o.insert("ciUpper".into(), json!(r.upper_ci));
        o.insert("priLower".into(), json!(r.lower_pri));
        o.insert("priUpper".into(), json!(r.upper_pri));
        if let Some(s) = side_by_cmp.get(&r.row) {
            if let Some(p) = s.prop_dir { o.insert("propDirect".into(), json!(p)); }
            if let (Some(d), Some(l), Some(u)) = (s.direct, s.direct_l, s.direct_u) {
                o.insert("direct".into(), json!({"effect":d,"ciLower":l,"ciUpper":u}));
            }
            if let (Some(d), Some(l), Some(u)) = (s.indirect, s.indirect_l, s.indirect_u) {
                o.insert("indirect".into(), json!({"effect":d,"ciLower":l,"ciUpper":u}));
            }
            if let (Some(eff), Some(l), Some(u), Some(z), Some(p)) =
                (s.side_if, s.side_if_l, s.side_if_u, s.side_z, s.side_p) {
                o.insert("incoherence".into(), json!({
                    "effect": eff, "ciLower": l, "ciUpper": u, "z": z, "pvalue": p,
                }));
            }
        }
        Value::Object(o)
    }).collect();

    let pairwise_v3: Vec<Value> = pairwise.iter().map(|p| json!({
        "comparison": p.comparison,
        "tau2": p.tau2,
        "I2": p.i2,
        "I2Lower": p.i2_lower,
        "I2Upper": p.i2_upper,
    })).collect();

    let now = now_iso();
    let project_id = format!("rust_cnm_{}", std::process::id());

    let cnm = json!({
        "cinema": {
            "version": "3.0.0",
            "title": args.title,
            "createdAt": now,
            "updatedAt": now,
            "projects": [{
                "id": project_id,
                "title": args.title,
                "outcome": "",
                "createdAt": now,
                "updatedAt": now,
                "hasEvaluation": false,
                "dataset": {
                    "format": "iv",
                    "type": "binary",
                    "studies": studies_v3(&d.rows),
                },
                "analysis": {
                    "params": { "model": "random", "sm": args.sm, "framework": "frequentist" },
                    "contributionMatrix": {
                        "hatMatrix": {
                            "H": h_dense,
                            "rowNames": hm.row_names,
                            "colNames": h_cols,
                        },
                        "studyContributions": sc_nested,
                    },
                    "frequentist": {
                        "nmaResults": nma_v3,
                        "pairwise": pairwise_v3,
                        "networkHeterogeneity": {
                            "tau2": fit.tau2,
                            "Qoverall": dd.q_overall,
                            "Qheterogeneity": dd.q_heterogeneity,
                            "Qinconsistency": dd.q_inconsistency,
                        },
                        "designByTreatment": {
                            "Q": dd.dbt_q,
                            "df": dd.dbt_df,
                            "pvalue": dd.dbt_pvalue,
                        },
                        "leagueTable": league,
                    },
                    "bayesian": Value::Null,
                },
                "evaluation": Value::Null,
            }],
        },
        "_tau": fit.tau2.sqrt(),
        "_treatments": d.treats,
        "_max_contribution_row_dev": max_dev,
    });

    let json = serde_json::to_string(&cnm).expect("serialise .cnm");
    fs::write(&args.output, json).expect("write --output");
    if args.timing {
        eprintln!("  cnm assemble    : {:>7.2}s", t0.elapsed().as_secs_f64());
        eprintln!("  TOTAL           : {:>7.2}s", total.elapsed().as_secs_f64());
    }
    eprintln!("wrote {} (max contribution-row dev = {:.2e})", args.output, max_dev);
}
