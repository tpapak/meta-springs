//! Dump the raw τ² grid of a consistency NMA so a browser can do prior
//! sensitivity LIVE: the expensive part — Z(τ²) and the conditional effect
//! vector at each grid τ² — is prior-independent, computed ONCE here. Any
//! prior π(τ²) is then just a reweighting of these same grid points, which
//! the slide does in JavaScript for an arbitrary, slider-driven prior.
//!
//!   dump_tau_grid <arms.json> <out.json> [n_grid]
//!
//! Emits {treatments, param_names, reml_mode, grid:[{tau2, logZ, effects, effSd}]}.

use gc_rust::data::read_dataset;
use gc_rust::gc::run_adaptive_auto_min_hi;
use serde::Serialize;

#[derive(Serialize)]
struct GridRow {
    tau2: f64,
    #[serde(rename = "logZ")]
    log_z: f64,
    /// effect of treatment_order[t] vs ref, t = 1..n_t (ref dropped; effects[0]=0)
    effects: Vec<f64>,
    /// Laplace sd of each effect vs ref
    #[serde(rename = "effSd")]
    eff_sd: Vec<f64>,
}

#[derive(Serialize)]
struct GridDump {
    dataset: String,
    #[serde(rename = "ref")]
    ref_treatment: String,
    treatments: Vec<String>,
    param_names: Vec<String>,
    reml_mode: f64,
    grid: Vec<GridRow>,
}

fn main() {
    let mut args = std::env::args().skip(1);
    let input = args.next().expect("usage: dump_tau_grid <arms.json> <out.json> [n_grid]");
    let output = args.next().expect("usage: dump_tau_grid <arms.json> <out.json> [n_grid]");
    let n_grid: usize = args.next().map(|s| s.parse().expect("n_grid")).unwrap_or(120);

    let ds = read_dataset(&input).unwrap_or_else(|e| panic!("read dataset: {e}"));
    let (topo, reml_mode, grid) = run_adaptive_auto_min_hi(&ds, n_grid, 0.0);
    let treatments = topo.treatment_order.clone();
    let ref_t = treatments[0].clone();
    let param_names: Vec<String> =
        treatments[1..].iter().map(|t| format!("d.{ref_t}.{t}")).collect();

    let rows: Vec<GridRow> = grid
        .iter()
        .map(|g| GridRow {
            tau2: g.tau2,
            log_z: g.log_z,
            // drop the pinned ref (index 0); keep the basic effects vs ref
            effects: g.effects[1..].to_vec(),
            eff_sd: g.eff_vars[1..].iter().map(|v| v.max(0.0).sqrt()).collect(),
        })
        .collect();

    let dump = GridDump {
        dataset: input.clone(),
        ref_treatment: ref_t,
        treatments,
        param_names,
        reml_mode,
        grid: rows,
    };
    std::fs::write(&output, serde_json::to_string(&dump).expect("serialise")).expect("write");
    eprintln!(
        "dump_tau_grid: {} grid points over τ² ∈ [{:.4}, {:.4}], REML mode {:.4} -> {}",
        dump.grid.len(),
        dump.grid.first().map(|r| r.tau2).unwrap_or(0.0),
        dump.grid.last().map(|r| r.tau2).unwrap_or(0.0),
        dump.reml_mode,
        output
    );
}
