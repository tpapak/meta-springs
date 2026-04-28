//! Output JSON formatting. Single-rep schema matches `proto_diabetes_gc.hs`
//! so the existing Python compare scripts work unchanged. Multi-rep schema
//! matches `proto_t30_reps_gc.hs` / `proto_t50_rare_gc.hs`.

use serde::Serialize;

#[derive(Serialize)]
pub struct Tau2Summary {
    pub mode: f64,
    pub median: f64,
    pub mean: f64,
    pub ci_lo: f64,
    pub ci_hi: f64,
}

#[derive(Serialize)]
pub struct GridWeight {
    pub tau2: f64,
    pub weight: f64,
}

#[derive(Serialize)]
pub struct EffectPoint {
    pub tau2: f64,
    pub weight: f64,
    pub mean: f64,
    pub var: f64,
}

#[derive(Serialize)]
pub struct EffectEntry {
    pub to: String,
    pub mean: f64,
    pub sd: f64,
    /// Heavy per-grid-point payload (single-rep mode only). Multi-rep mode
    /// leaves this `None` so the field is skipped on serialisation.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub points: Option<Vec<EffectPoint>>,
}

#[derive(Serialize)]
pub struct PriorReport {
    pub label: String,
    pub tau2: Tau2Summary,
    /// Heavy log-posterior τ² weight grid (single-rep mode only).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tau2_grid: Option<Vec<GridWeight>>,
    pub effects: Vec<EffectEntry>,
}

/// Single-rep top-level payload (matches the diabetes proto).
///
/// `reml_mode_iterative` is the τ̂² returned by the bisection root-finder on
/// `dlogZ/dτ² = 0` (prior-free, ignoring grid discretisation).  This is the
/// classical REML estimate that frequentist tools like `netmeta` produce
/// via iterative optimisation; the per-prior `tau2.mode` field is the
/// argmax over the discrete log-spaced grid under that prior's density.
/// They differ by O(grid step) under the Flat prior; differ by more under
/// informative priors.
#[derive(Serialize)]
pub struct Report {
    pub dataset: String,
    pub fit_wall_sec: f64,
    #[serde(rename = "ref")]
    pub ref_treatment: String,
    pub reml_mode_iterative: f64,
    pub priors: Vec<PriorReport>,
}

/// One replicate's lite summary block (no `tau2_grid`, no per-effect `points`).
#[derive(Serialize)]
pub struct RepReport {
    pub rep: usize,
    pub wall_sec: f64,
    pub reml_mode_iterative: f64,
    pub priors: Vec<PriorReport>,
}

/// Multi-rep top-level payload (matches `proto_t30_reps_gc.hs`).
#[derive(Serialize)]
pub struct MultiRepReport {
    pub total_wall_sec: f64,
    pub n_reps: usize,
    pub results: Vec<RepReport>,
}
