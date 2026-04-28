//! Build a CINeMA v3 `.cnm` project file.
//!
//! Inputs:
//!   * `--data`         — arm-level NMA dataset, in the
//!                        `runNMA` request shape ({indata, type, model, sm}).
//!   * `--nma-response` — the full `runNMA` JSON (hat matrix, NMA effects,
//!                        SIDE, design-by-treatment, league-table inputs,
//!                        etc.). Produced locally via cinema's
//!                        `backend/R/run_cli.R` or live via
//!                        `cinema.med.auth.gr/api/runNMA`.
//!   * `--output`       — where to write the `.cnm`.
//!
//! What Rust contributes:
//!   * `analysis.contributionMatrix.hatMatrix.H` — copied from R's
//!     `hatmatrix(..., method="Davies", type="long")`.
//!   * `analysis.contributionMatrix.studyContributions` — recomputed from
//!     scratch with our shortest-path streams port (matches netmeta to
//!     7.5e-5 = CINeMA's JSON rounding floor) on top of the netmeta IV
//!     weights.
//!   * Frequentist block (NMA effects, SIDE, heterogeneity, dbt, league
//!     table) — copied from the runNMA response and reshaped into the
//!     v3 schema.
//!
//! The resulting `.cnm` opens in CINeMA's webapp via the
//! "import .cnm" path.

use clap::Parser;
use serde::de::Deserializer;

fn deser_or_empty<'de, D, T>(d: D) -> Result<Vec<T>, D::Error>
where
    D: Deserializer<'de>,
    T: serde::de::DeserializeOwned,
{
    let v: Option<Vec<T>> = serde::Deserialize::deserialize(d)?;
    Ok(v.unwrap_or_default())
}

use gc_rust::contribution::{
    contribution_matrix_shortest_path, hat_matrix_from_list, study_contributions,
    HElement, PairwiseWeight,
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};
use std::collections::BTreeMap;
use std::fs;

#[derive(Parser, Debug)]
#[command(name = "cnm", about = "Build a CINeMA v3 .cnm project file")]
struct Args {
    /// `runNMA`-shape JSON: `{action?, indata, type, model, sm}`.
    #[arg(long)]
    data: String,
    /// Full runNMA response JSON.
    #[arg(long)]
    nma_response: String,
    /// Per-(study, comparison) IV weights JSON for study-contribution
    /// redistribution (output of `local_cinema.R`).
    #[arg(long)]
    iv_weights: String,
    /// Project title (defaults to `"rust_export"`).
    #[arg(long, default_value = "rust_export")]
    title: String,
    /// Output `.cnm` path.
    #[arg(long)]
    output: String,
}

// ── Inputs ────────────────────────────────────────────────────────────

#[derive(Debug, Deserialize)]
struct Request {
    indata: Vec<Value>,
    #[serde(rename = "type")]
    rtype: String,
    model: String,
    sm: String,
}

/// Wide-form CINeMA matrix block: `{data: [{cid: v, ...}], rowNames, colNames}`.
#[derive(Debug, Deserialize, Serialize)]
struct WideMatrix {
    data: Vec<Map<String, Value>>,
    #[serde(rename = "rowNames")]
    row_names: Vec<String>,
    #[serde(rename = "colNames")]
    col_names: Vec<String>,
}

#[derive(Debug, Deserialize)]
struct StudyContribRow {
    comparison: String,
    study: String,
    #[allow(dead_code)]
    contribution: f64,
}

#[derive(Debug, Deserialize)]
struct NmaRow {
    #[serde(rename = "_row")]
    row: String,
    #[serde(rename = "TE")]
    te: f64,
    #[serde(rename = "seTE")]
    se: f64,
    #[serde(rename = "lowerCI")]
    lower_ci: f64,
    #[serde(rename = "upperCI")]
    upper_ci: f64,
    #[serde(rename = "lowerPrI")]
    lower_pri: f64,
    #[serde(rename = "upperPrI")]
    upper_pri: f64,
    #[serde(rename = "PropDir", default)]
    prop_dir: Option<f64>,
}

#[derive(Debug, Deserialize)]
struct SideRow {
    comparison: String,
    #[serde(rename = "Direct", default)]
    direct: Option<f64>,
    #[serde(rename = "DirectL", default)]
    direct_l: Option<f64>,
    #[serde(rename = "DirectU", default)]
    direct_u: Option<f64>,
    #[serde(rename = "Indirect", default)]
    indirect: Option<f64>,
    #[serde(rename = "IndirectL", default)]
    indirect_l: Option<f64>,
    #[serde(rename = "IndirectU", default)]
    indirect_u: Option<f64>,
    #[serde(rename = "SideIF", default)]
    side_if: Option<f64>,
    #[serde(rename = "SideIFlower", default)]
    side_if_l: Option<f64>,
    #[serde(rename = "SideIFupper", default)]
    side_if_u: Option<f64>,
    #[serde(rename = "SideZ", default)]
    side_z: Option<f64>,
    #[serde(rename = "SidePvalue", default)]
    side_p: Option<f64>,
    #[serde(rename = "PropDir", default)]
    prop_dir: Option<f64>,
}

#[derive(Debug, Deserialize)]
struct PairwiseRow {
    comparison: String,
    #[serde(default)]
    tau2: Option<f64>,
    #[serde(rename = "I2", default)]
    i2: Option<f64>,
    #[serde(rename = "I2lower", default)]
    i2_lower: Option<f64>,
    #[serde(rename = "I2upper", default)]
    i2_upper: Option<f64>,
}

#[derive(Debug, Deserialize)]
struct NmaResponse {
    #[serde(rename = "H")]
    h: WideMatrix,
    #[serde(rename = "NMAresults")]
    nma_results: Vec<NmaRow>,
    side: Vec<SideRow>,
    #[serde(rename = "Pairwise")]
    pairwise: Vec<PairwiseRow>,
    #[serde(rename = "NMAheter")]
    nma_heter: Value,
    dbt: Value,
    forleaguetable: Map<String, Value>,
    #[serde(default, deserialize_with = "deser_or_empty")]
    #[allow(non_snake_case)]
    studyContributions: Vec<StudyContribRow>,
    treatnames: Vec<String>,
    model: String,
    sm: String,
    tau: f64,
}

// ── Helpers ───────────────────────────────────────────────────────────

fn norm_pair(a: &str, b: &str) -> String {
    if a <= b { format!("{a}:{b}") } else { format!("{b}:{a}") }
}

fn now_iso() -> String {
    use std::time::{SystemTime, UNIX_EPOCH};
    let secs = SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs();
    // Round-trip-clean ISO-8601-ish stamp; CINeMA accepts any string here.
    format!(
        "{}T00:00:00.000Z",
        chrono_like_date(secs)
    )
}

/// Tiny date formatter — avoids pulling in `chrono` for one timestamp.
fn chrono_like_date(secs: u64) -> String {
    let days = secs / 86400;
    // Days since 1970-01-01 → (year, month, day).
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

/// Convert wide matrix block to dense `Vec<Vec<f64>>` with rowNames / colNames.
fn wide_to_dense(m: &WideMatrix) -> (Vec<Vec<f64>>, Vec<String>, Vec<String>) {
    let mut rows = Vec::with_capacity(m.row_names.len());
    for row in &m.data {
        let mut r = Vec::with_capacity(m.col_names.len());
        for c in &m.col_names {
            let v = row.get(c).and_then(|x| x.as_f64()).unwrap_or(0.0);
            r.push(v);
        }
        rows.push(r);
    }
    (rows, m.row_names.clone(), m.col_names.clone())
}

/// Reshape arm rows into v3 study records (binary path; mirrors
/// generateOfflineScript's per-arm layout).
fn studies_v3(indata: &[Value], rtype: &str) -> Vec<Value> {
    indata
        .iter()
        .map(|arm| {
            let g = |k: &str| arm.get(k).cloned().unwrap_or(Value::Null);
            let mut o = Map::new();
            o.insert("study".into(), g("study"));
            o.insert("id".into(), g("id"));
            o.insert("treatment".into(), g("t"));
            o.insert("n".into(), g("n"));
            match rtype {
                "long_binary" => {
                    o.insert("events".into(), g("r"));
                }
                "long_continuous" => {
                    o.insert("mean".into(), g("y"));
                    o.insert("sd".into(), g("sd"));
                }
                _ => {}
            }
            o.insert("rob".into(), g("rob"));
            o.insert("indirectness".into(), g("indirectness"));
            Value::Object(o)
        })
        .collect()
}

/// Build the v3 `nmaResults` block: merge `NMAresults` with per-comparison
/// `side` (direct/indirect/SIDE) by canonical comparison id.
fn build_nma_results(nma: &[NmaRow], side: &[SideRow]) -> Vec<Value> {
    use std::collections::HashMap;
    let by_cid: HashMap<String, &SideRow> = side
        .iter()
        .map(|s| {
            let parts: Vec<&str> = s.comparison.splitn(2, ':').collect();
            let key = if parts.len() == 2 {
                norm_pair(parts[0], parts[1])
            } else {
                s.comparison.clone()
            };
            (key, s)
        })
        .collect();
    nma.iter()
        .map(|r| {
            let parts: Vec<&str> = r.row.splitn(2, ':').collect();
            let cid = if parts.len() == 2 { norm_pair(parts[0], parts[1]) } else { r.row.clone() };
            let mut o = Map::new();
            o.insert("comparison".into(), Value::String(cid.clone()));
            o.insert("effect".into(), json!(r.te));
            o.insert("se".into(), json!(r.se));
            o.insert("ciLower".into(), json!(r.lower_ci));
            o.insert("ciUpper".into(), json!(r.upper_ci));
            o.insert("priLower".into(), json!(r.lower_pri));
            o.insert("priUpper".into(), json!(r.upper_pri));
            o.insert(
                "propDirect".into(),
                json!(r.prop_dir.unwrap_or(0.0)),
            );
            if let Some(s) = by_cid.get(&cid) {
                if let (Some(d), Some(l), Some(u)) = (s.direct, s.direct_l, s.direct_u) {
                    o.insert(
                        "direct".into(),
                        json!({"effect": d, "ciLower": l, "ciUpper": u}),
                    );
                }
                if let (Some(d), Some(l), Some(u)) =
                    (s.indirect, s.indirect_l, s.indirect_u)
                {
                    o.insert(
                        "indirect".into(),
                        json!({"effect": d, "ciLower": l, "ciUpper": u}),
                    );
                }
                if let (Some(eff), Some(l), Some(u), Some(z), Some(p)) =
                    (s.side_if, s.side_if_l, s.side_if_u, s.side_z, s.side_p)
                {
                    o.insert(
                        "incoherence".into(),
                        json!({
                            "effect": eff,
                            "ciLower": l,
                            "ciUpper": u,
                            "z": z,
                            "pvalue": p,
                        }),
                    );
                }
            }
            Value::Object(o)
        })
        .collect()
}

/// Format a simple league table on the OR scale: `"<eff> (lo, up)"`.
fn league_table(forlt: &Map<String, Value>, model: &str, sm: &str) -> Vec<Vec<String>> {
    let pick = |fixed_key: &str, random_key: &str| -> Option<&Vec<Vec<f64>>> {
        let key = if model == "fixed" { fixed_key } else { random_key };
        let _ = forlt.get(key)?;
        None::<&Vec<Vec<f64>>>
    };
    let _ = pick; // suppress unused warning if forlt fields differ

    let key_te = if model == "fixed" { "TE.fixed" } else { "TE.random" };
    let key_lo = if model == "fixed" { "lower.fixed" } else { "lower.random" };
    let key_up = if model == "fixed" { "upper.fixed" } else { "upper.random" };

    let names: Vec<String> = forlt
        .get("treatnames")
        .and_then(|v| v.as_array())
        .map(|a| a.iter().filter_map(|x| x.as_str().map(String::from)).collect())
        .unwrap_or_default();

    // run_cli.R wraps each matrix in `{data: [{col: v, ...}], rowNames, colNames}`.
    let to_mat = |key: &str| -> Vec<Vec<f64>> {
        let v = match forlt.get(key) { Some(v) => v, None => return Vec::new() };
        if let Some(obj) = v.as_object() {
            let cols: Vec<String> = obj
                .get("colNames")
                .and_then(|x| x.as_array())
                .map(|a| a.iter().filter_map(|x| x.as_str().map(String::from)).collect())
                .unwrap_or_else(|| names.clone());
            obj.get("data")
                .and_then(|x| x.as_array())
                .map(|rows| {
                    rows.iter()
                        .map(|row| {
                            let m = row.as_object();
                            cols.iter()
                                .map(|c| {
                                    m.and_then(|o| o.get(c))
                                        .and_then(|x| x.as_f64())
                                        .unwrap_or(0.0)
                                })
                                .collect()
                        })
                        .collect()
                })
                .unwrap_or_default()
        } else if let Some(rows) = v.as_array() {
            rows.iter()
                .map(|row| {
                    row.as_array()
                        .map(|cs| cs.iter().map(|x| x.as_f64().unwrap_or(0.0)).collect())
                        .unwrap_or_default()
                })
                .collect()
        } else {
            Vec::new()
        }
    };
    let te = to_mat(key_te);
    let lo = to_mat(key_lo);
    let up = to_mat(key_up);

    let exp_scale = matches!(sm, "OR" | "RR" | "HR");
    let n = names.len();
    let mut out = vec![vec![String::new(); n]; n];
    for i in 0..n {
        for j in 0..n {
            if i == j {
                out[i][j] = names[i].clone();
            } else {
                let (e, l, u) = if exp_scale {
                    (te[i][j].exp(), lo[i][j].exp(), up[i][j].exp())
                } else {
                    (te[i][j], lo[i][j], up[i][j])
                };
                out[i][j] = format!("{:.3} ({:.3}, {:.3})", e, l, u);
            }
        }
    }
    out
}

// ── Main ──────────────────────────────────────────────────────────────

fn main() {
    let args = Args::parse();

    // Load arm-level request.
    let req: Request = serde_json::from_str(
        &fs::read_to_string(&args.data).expect("read --data"),
    )
    .expect("parse --data");

    // Load runNMA response.
    let resp: NmaResponse = serde_json::from_str(
        &fs::read_to_string(&args.nma_response).expect("read --nma-response"),
    )
    .expect("parse --nma-response");

    // Load IV weights for the per-study redistribution.
    let weights: Vec<PairwiseWeight> = serde_json::from_str(
        &fs::read_to_string(&args.iv_weights).expect("read --iv-weights"),
    )
    .expect("parse --iv-weights");

    // Recompute contribution matrix from R's hat matrix using our port.
    let h_wide_list: Vec<HElement> = resp
        .h
        .data
        .iter()
        .zip(resp.h.row_names.iter())
        .flat_map(|(row, rid)| {
            row.iter().map(move |(cid, v)| HElement {
                row: rid.clone(),
                comparison: cid.clone(),
                value: v.as_f64().unwrap_or(0.0),
            })
        })
        .collect();
    let hm = hat_matrix_from_list(&h_wide_list);
    let cm = contribution_matrix_shortest_path(&hm);
    let study_table = study_contributions(&cm, &weights);

    // ── Assemble v3 .cnm ────────────────────────────────────────────

    // Hat matrix as dense matrix.
    let (h_dense, h_rows, h_cols) = wide_to_dense(&resp.h);

    // studyContributions: { comparison: { study: contribution } }
    let mut sc_nested: BTreeMap<String, BTreeMap<String, f64>> = BTreeMap::new();
    for r in &study_table {
        sc_nested
            .entry(r.comparison.clone())
            .or_default()
            .insert(r.study.clone(), r.contribution);
    }

    let now = now_iso();
    let project_id = format!("rust_cnm_{}", std::process::id());

    let nma_results = build_nma_results(&resp.nma_results, &resp.side);

    let pairwise_v3: Vec<Value> = resp
        .pairwise
        .iter()
        .map(|p| {
            json!({
                "comparison": p.comparison,
                "tau2": p.tau2,
                "I2": p.i2,
                "I2Lower": p.i2_lower,
                "I2Upper": p.i2_upper,
            })
        })
        .collect();
    let _ = pairwise_v3.len(); // touch to avoid unused-must-use grumbles

    // designByTreatment: pull the first row (overall) from `dbt`.
    let dbt_top = resp
        .dbt
        .as_array()
        .and_then(|a| a.first())
        .cloned()
        .unwrap_or(Value::Null);
    let dbt_v3 = match dbt_top {
        Value::Object(o) => json!({
            "Q": o.get("Q").cloned().unwrap_or(Value::Null),
            "df": o.get("df").cloned().unwrap_or(Value::Null),
            "pvalue": o.get("pval").cloned().unwrap_or(Value::Null),
        }),
        _ => Value::Null,
    };

    let league = league_table(&resp.forleaguetable, &resp.model, &resp.sm);

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
                    "format": "long",
                    "type": match req.rtype.as_str() {
                        "long_binary" => "binary",
                        "long_continuous" => "continuous",
                        other => other,
                    },
                    "studies": studies_v3(&req.indata, &req.rtype),
                },
                "analysis": {
                    "params": {
                        "model": resp.model,
                        "sm": resp.sm,
                        "framework": "frequentist",
                    },
                    "contributionMatrix": {
                        "hatMatrix": {
                            "H": h_dense,
                            "rowNames": h_rows,
                            "colNames": h_cols,
                        },
                        "studyContributions": sc_nested,
                    },
                    "frequentist": {
                        "nmaResults": nma_results,
                        "pairwise": pairwise_v3,
                        "networkHeterogeneity": resp.nma_heter,
                        "designByTreatment": dbt_v3,
                        "leagueTable": league,
                    },
                    "bayesian": Value::Null,
                },
                "evaluation": Value::Null,
            }],
        },
        // Echo τ for downstream consumers; not part of the strict v3 schema.
        "_tau": resp.tau,
        "_treatments": resp.treatnames,
    });

    let out = serde_json::to_string_pretty(&cnm).expect("serialise .cnm");
    fs::write(&args.output, out).expect("write --output");
    eprintln!("wrote {}", args.output);
}
