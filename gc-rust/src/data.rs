//! Input JSON parsing for the NMA arm-level format.
//!
//! Two modes are supported:
//!   * Binary outcomes — required fields per record: `study`, `treatment`,
//!     `events`, `n`. Mirrors the Haskell `readStudies` for binary outcomes
//!     used by `proto_diabetes_gc.hs`.
//!   * Continuous outcomes — required fields per record: `study`,
//!     `treatment`, `mean`, `sd`, `n`. Mirrors the Haskell continuous arm
//!     reader used by `proto_t30_reps_cont_gc.hs`.
//!
//! The mode is detected from the first record's fields. Mixed datasets are
//! rejected.

use serde::{Deserialize, Deserializer};
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::Path;

/// Accept treatment ids that arrive as either JSON strings or JSON integers.
/// Integers are rendered with their decimal representation to match the
/// Haskell `show (TreatmentId (IntId n))` convention used in
/// `proto_t30_reps_gc.hs`.
fn de_treatment_id<'de, D>(d: D) -> Result<String, D::Error>
where
    D: Deserializer<'de>,
{
    let v = Value::deserialize(d)?;
    match v {
        Value::String(s) => Ok(s),
        Value::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(i.to_string())
            } else if let Some(u) = n.as_u64() {
                Ok(u.to_string())
            } else {
                // Floats are unusual but stringify them too.
                Ok(n.to_string())
            }
        }
        other => Err(serde::de::Error::custom(format!(
            "treatment must be string or integer, got {other}"
        ))),
    }
}

/// Single arm row as it appears in the JSON file. Both binary and continuous
/// fields are optional at parse time; `read_dataset` validates which set is
/// populated.
#[derive(Debug, Deserialize)]
pub struct ArmRecord {
    #[serde(default)]
    pub studyname: Option<String>,
    pub study: i64,
    #[serde(deserialize_with = "de_treatment_id")]
    pub treatment: String,
    // Binary fields
    #[serde(default)]
    pub events: Option<i64>,
    // Continuous fields
    #[serde(default)]
    pub mean: Option<f64>,
    #[serde(default)]
    pub sd: Option<f64>,
    // Sample size — present in both modes.
    pub n: i64,
}

/// Per-arm payload after dataset normalisation. We keep both shapes in one
/// struct to keep `Topology` reusable; only the field used by the active
/// likelihood is read.
#[derive(Debug, Clone)]
pub struct Arm {
    pub treatment: String,
    /// Binary-mode payload.
    pub events: i64,
    pub n: i64,
    /// Continuous-mode payload (sample mean and sample variance). For binary
    /// arms these stay at zero and are unused.
    pub mean: f64,
    pub sd: f64,
}

#[derive(Debug, Clone)]
pub struct Study {
    pub study_id: i64,
    pub arms: Vec<Arm>,
}

/// Likelihood family inferred from the arm-level JSON.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Mode {
    Binary,
    Continuous,
}

#[derive(Debug, Clone)]
pub struct Dataset {
    /// Studies in input order (preserves study-id ordering).
    pub studies: Vec<Study>,
    /// All distinct treatment ids, sorted lexicographically (matches the
    /// Haskell `sortOn show` used to pick the ref treatment).
    pub treatments: Vec<String>,
    /// Detected likelihood family.
    pub mode: Mode,
}

/// Decide what flavour the record is. Returns an error if neither shape is
/// fully populated, or if both shapes are partially mixed within a single
/// record.
fn classify(rec: &ArmRecord) -> Result<Mode, String> {
    let has_bin = rec.events.is_some();
    let has_cont = rec.mean.is_some() && rec.sd.is_some();
    match (has_bin, has_cont) {
        (true, false) => Ok(Mode::Binary),
        (false, true) => Ok(Mode::Continuous),
        (true, true) => Err(format!(
            "study {} treatment {}: both binary (events) and continuous (mean,sd) fields present",
            rec.study, rec.treatment
        )),
        (false, false) => Err(format!(
            "study {} treatment {}: missing either `events` (binary) or `mean`+`sd` (continuous)",
            rec.study, rec.treatment
        )),
    }
}

pub fn read_dataset<P: AsRef<Path>>(path: P) -> Result<Dataset, String> {
    let s = std::fs::read_to_string(&path)
        .map_err(|e| format!("read {}: {}", path.as_ref().display(), e))?;
    let recs: Vec<ArmRecord> =
        serde_json::from_str(&s).map_err(|e| format!("parse json: {e}"))?;
    if recs.is_empty() {
        return Err("empty dataset".into());
    }

    // Detect mode from the first record, then enforce consistency.
    let mode = classify(&recs[0])?;
    for r in &recs[1..] {
        let m = classify(r)?;
        if m != mode {
            return Err(format!(
                "mixed dataset: first record was {:?} but study {} treatment {} is {:?}",
                mode, r.study, r.treatment, m
            ));
        }
    }

    // Group arms by study id. Use BTreeMap to keep deterministic ordering.
    let mut by_study: BTreeMap<i64, Vec<Arm>> = BTreeMap::new();
    let mut treatments: BTreeMap<String, ()> = BTreeMap::new();
    for r in recs {
        treatments.insert(r.treatment.clone(), ());
        let arm = match mode {
            Mode::Binary => Arm {
                treatment: r.treatment,
                events: r.events.unwrap(),
                n: r.n,
                mean: 0.0,
                sd: 0.0,
            },
            Mode::Continuous => Arm {
                treatment: r.treatment,
                events: 0,
                n: r.n,
                mean: r.mean.unwrap(),
                sd: r.sd.unwrap(),
            },
        };
        by_study.entry(r.study).or_default().push(arm);
    }
    let studies: Vec<Study> = by_study
        .into_iter()
        .map(|(study_id, arms)| Study { study_id, arms })
        .collect();
    let treatments: Vec<String> = treatments.into_keys().collect();
    Ok(Dataset { studies, treatments, mode })
}
