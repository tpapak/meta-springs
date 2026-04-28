//! IV-format NMA fitter on top of the spring / GC infrastructure.
//!
//! Replaces the `netmeta()` + `hatmatrix()` + `decomp.design()` +
//! `netsplit()` + `metagen` subgroup chain that CINeMA's `cinema_nma.R`
//! currently runs against R's `netmeta` package, so the whole pipeline
//! up to `.cnm` assembly can run without R.
//!
//! Inputs are the contrast-level dataset CINeMA uses for IV format:
//!   `[{id, t1, t2, effect, se, rob, indirectness}, …]`.
//!
//! For a fixed contrast variance `τ²`, the random-effects NMA reduces
//! to one weighted least-squares system on the treatment vertices:
//!     A = X' W X,  b = X' W y,  β̂ = A⁻¹ b
//! with W = diag(1 / (se² + τ²)), one treatment pinned at 0, and X the
//! contrast design matrix (`+1` at the t2 vertex, `-1` at t1, zero on
//! the pinned reference).
//!
//! `τ²` itself is found by REML — bisection on dlogZ/dτ² = 0 — through
//! the same `gc::find_reml_mode_with` helper the binomial / continuous
//! GC pipelines use. This is the spring REML mode the project's GC
//! solver was built for, applied to the linear-Gaussian likelihood the
//! IV format gives us directly.

use crate::gc::find_reml_mode_with;
use nalgebra::{DMatrix, DVector};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::path::Path;

// ─────────────────────────────────────────────────────────────────────
// Inputs
// ─────────────────────────────────────────────────────────────────────

/// One contrast row of the IV dataset.
#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct IvRow {
    pub id: String,
    pub t1: String,
    pub t2: String,
    pub effect: f64,
    pub se: f64,
    #[serde(default)]
    pub rob: i64,
    #[serde(default)]
    pub indirectness: i64,
}

/// Whole IV dataset with treatments sorted lex (the first entry is the
/// pinned reference, matching the rest of the codebase's convention).
#[derive(Debug, Clone)]
pub struct IvDataset {
    pub treats: Vec<String>,
    pub treat_idx: BTreeMap<String, usize>,
    pub rows: Vec<IvRow>,
}

impl IvDataset {
    pub fn from_rows(mut rows: Vec<IvRow>) -> Self {
        let mut treats: BTreeMap<String, ()> = BTreeMap::new();
        for r in &rows {
            treats.insert(r.t1.clone(), ());
            treats.insert(r.t2.clone(), ());
        }
        let treats: Vec<String> = treats.into_keys().collect();
        let mut treat_idx: BTreeMap<String, usize> = BTreeMap::new();
        for (i, t) in treats.iter().enumerate() {
            treat_idx.insert(t.clone(), i);
        }
        // Stable sort rows by (id, t1, t2) so output ordering is reproducible.
        rows.sort_by(|a, b| {
            a.id.cmp(&b.id).then(a.t1.cmp(&b.t1)).then(a.t2.cmp(&b.t2))
        });
        IvDataset { treats, treat_idx, rows }
    }

    /// Read the `runNMA` request shape: `{indata: [...], type, model, sm}`.
    pub fn from_request_json<P: AsRef<Path>>(p: P) -> Result<Self, String> {
        #[derive(Deserialize)]
        struct Req {
            indata: Vec<IvRow>,
        }
        let raw = std::fs::read_to_string(&p)
            .map_err(|e| format!("read {}: {e}", p.as_ref().display()))?;
        let req: Req = serde_json::from_str(&raw)
            .map_err(|e| format!("parse {}: {e}", p.as_ref().display()))?;
        Ok(Self::from_rows(req.indata))
    }

    /// Read CINeMA's IV-format CSV (header `id,t1,t2,effect,se,rob,indirectness`).
    pub fn from_csv<P: AsRef<Path>>(p: P) -> Result<Self, String> {
        let raw = std::fs::read_to_string(&p)
            .map_err(|e| format!("read {}: {e}", p.as_ref().display()))?;
        let mut lines = raw.lines();
        let header = lines.next().ok_or("empty csv")?;
        let cols: Vec<&str> = header.split(',').map(|s| s.trim()).collect();
        let idx = |name: &str| -> Result<usize, String> {
            cols.iter()
                .position(|c| *c == name)
                .ok_or_else(|| format!("missing column `{name}`"))
        };
        let i_id = idx("id")?;
        let i_t1 = idx("t1")?;
        let i_t2 = idx("t2")?;
        let i_eff = idx("effect")?;
        let i_se = idx("se")?;
        let i_rob = cols.iter().position(|c| *c == "rob");
        let i_ind = cols.iter().position(|c| *c == "indirectness");
        let mut rows = Vec::new();
        for (lineno, line) in lines.enumerate() {
            if line.trim().is_empty() {
                continue;
            }
            let parts: Vec<&str> = line.split(',').collect();
            let parse_f = |i: usize| -> Result<f64, String> {
                parts
                    .get(i)
                    .ok_or_else(|| format!("line {}: short row", lineno + 2))?
                    .parse::<f64>()
                    .map_err(|e| format!("line {}: parse f64: {e}", lineno + 2))
            };
            let parse_i = |i: Option<usize>| -> i64 {
                i.and_then(|j| parts.get(j))
                    .and_then(|s| s.parse::<i64>().ok())
                    .unwrap_or(1)
            };
            rows.push(IvRow {
                id: parts[i_id].to_string(),
                t1: parts[i_t1].to_string(),
                t2: parts[i_t2].to_string(),
                effect: parse_f(i_eff)?,
                se: parse_f(i_se)?,
                rob: parse_i(i_rob),
                indirectness: parse_i(i_ind),
            });
        }
        Ok(Self::from_rows(rows))
    }

    pub fn n_treats(&self) -> usize { self.treats.len() }
    pub fn n_rows(&self) -> usize { self.rows.len() }
}

// ─────────────────────────────────────────────────────────────────────
// Fit at fixed τ²
// ─────────────────────────────────────────────────────────────────────

/// Linear-Gaussian NMA fit at a fixed contrast variance τ².
#[derive(Debug, Clone)]
pub struct IvFitAt {
    pub tau2: f64,
    /// Treatment effects vs the pinned reference (length `T`, `effects[0] = 0`).
    pub effects: Vec<f64>,
    /// Treatment-vs-ref covariance matrix (size `(T-1) × (T-1)`).
    pub a_inv: DMatrix<f64>,
    /// Per-row weights `w_k = 1 / (se_k² + τ²)`.
    pub w: Vec<f64>,
    /// Per-row residuals `y_k − x_k' β̂`.
    pub residuals: Vec<f64>,
    /// REML log-evidence at this τ² (drops constants).
    pub log_z: f64,
}

/// Build the contrast design matrix X (N × (T−1)). Treatment 0 is the
/// pinned reference; columns are treatments 1..T−1. Row k for contrast
/// y = β_{t2} − β_{t1} has `+1` at t2 (if t2 ≠ ref), `−1` at t1 (if
/// t1 ≠ ref), zero elsewhere.
fn design_x(d: &IvDataset) -> DMatrix<f64> {
    let n = d.rows.len();
    let p = d.treats.len() - 1;
    let mut x = DMatrix::<f64>::zeros(n, p);
    for (k, r) in d.rows.iter().enumerate() {
        let i1 = d.treat_idx[&r.t1];
        let i2 = d.treat_idx[&r.t2];
        if i2 > 0 {
            x[(k, i2 - 1)] += 1.0;
        }
        if i1 > 0 {
            x[(k, i1 - 1)] -= 1.0;
        }
    }
    x
}

/// Solve the random-effects WLS at one τ². Returns `None` if the
/// system is singular (disconnected network after dropping rows for a
/// netsplit refit).
pub fn try_fit_iv_at(d: &IvDataset, tau2: f64) -> Option<IvFitAt> {
    fit_iv_inner(d, tau2)
}

/// Solve the random-effects WLS at one τ². Panics on singular system —
/// use `try_fit_iv_at` for the netsplit refit case.
pub fn fit_iv_at(d: &IvDataset, tau2: f64) -> IvFitAt {
    fit_iv_inner(d, tau2)
        .unwrap_or_else(|| panic!("NMA system singular at τ² = {tau2}"))
}

fn fit_iv_inner(d: &IvDataset, tau2: f64) -> Option<IvFitAt> {
    let n = d.rows.len();
    let p = d.treats.len() - 1;

    let x = design_x(d);
    let y: DVector<f64> = DVector::from_iterator(n, d.rows.iter().map(|r| r.effect));
    let mut w = vec![0.0_f64; n];
    let mut sum_log_v = 0.0_f64;
    for (k, r) in d.rows.iter().enumerate() {
        let v = r.se * r.se + tau2;
        let vsafe = v.max(1e-12);
        w[k] = 1.0 / vsafe;
        sum_log_v += vsafe.ln();
    }

    // A = X' W X, b = X' W y.
    let mut a = DMatrix::<f64>::zeros(p, p);
    let mut b = DVector::<f64>::zeros(p);
    for k in 0..n {
        let wk = w[k];
        let xk = x.row(k).into_owned();
        for i in 0..p {
            let xki = xk[i];
            if xki == 0.0 {
                continue;
            }
            let wkxki = wk * xki;
            for j in 0..p {
                a[(i, j)] += wkxki * xk[j];
            }
            b[i] += wkxki * y[k];
        }
    }

    // β̂ = A⁻¹ b. Use LU; A is symmetric positive definite for τ² > 0.
    let lu = a.clone().lu();
    let beta = lu.solve(&b)?;
    let a_inv = a.clone().try_inverse()?;

    // Effects vector with pinned reference at index 0.
    let mut effects = vec![0.0_f64; d.treats.len()];
    for j in 0..p {
        effects[j + 1] = beta[j];
    }

    // Residuals + weighted sum-of-squares.
    let mut residuals = vec![0.0_f64; n];
    let mut wss = 0.0_f64;
    for k in 0..n {
        let yhat: f64 = (0..p).map(|j| x[(k, j)] * beta[j]).sum();
        let r = y[k] - yhat;
        residuals[k] = r;
        wss += w[k] * r * r;
    }

    // REML log-evidence (drops constants).
    let log_det_a = lu_log_det_abs(&lu, p);
    let log_z = -0.5 * sum_log_v - 0.5 * log_det_a - 0.5 * wss;

    Some(IvFitAt { tau2, effects, a_inv, w, residuals, log_z })
}

fn lu_log_det_abs(lu: &nalgebra::LU<f64, nalgebra::Dyn, nalgebra::Dyn>, n: usize) -> f64 {
    let u = lu.u();
    let mut s = 0.0_f64;
    for i in 0..n {
        let d = u[(i, i)].abs();
        s += d.ln();
    }
    s
}

/// REML τ̂² via bisection on `dlog Z / dτ² = 0`. Reuses the GC solver's
/// mode finder, which the rest of the project uses for the binomial /
/// continuous spring path. Returns the converged fit at the REML mode.
pub fn fit_iv_reml(d: &IvDataset) -> IvFitAt {
    let mode = find_reml_mode_with(|t2| {
        try_fit_iv_at(d, t2).map(|f| f.log_z).unwrap_or(f64::NEG_INFINITY)
    });
    fit_iv_at(d, mode)
}

// ─────────────────────────────────────────────────────────────────────
// NMA-results table (sign convention matching CINeMA's _row order)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct NmaRowOut {
    pub row: String, // "tA:tB" with tA <= tB
    pub te: f64,
    pub se: f64,
    pub lower_ci: f64,
    pub upper_ci: f64,
    pub lower_pri: f64,
    pub upper_pri: f64,
}

/// Per-(tA:tB) random-effects estimate + 95% CI + 95% PrI.
/// Sign convention follows cinema_nma.R: for `i < j` (lex), the effect
/// reported is `−(β_j − β_i) = β_i − β_j`.
pub fn nma_results(d: &IvDataset, fit: &IvFitAt) -> Vec<NmaRowOut> {
    let t = d.treats.len();
    // Effective NMA "df" for prediction interval: number of distinct designs minus 1 (matches netmeta).
    let n_designs = {
        let mut seen: BTreeMap<(usize, usize), ()> = BTreeMap::new();
        for r in &d.rows {
            let i1 = d.treat_idx[&r.t1];
            let i2 = d.treat_idx[&r.t2];
            let pair = if i1 < i2 { (i1, i2) } else { (i2, i1) };
            seen.insert(pair, ());
        }
        seen.len()
    };
    let pri_df = (n_designs as f64 - (t as f64 - 1.0)).max(1.0);
    let t_quant = student_t_quant_975(pri_df);
    let z = 1.959964_f64; // 0.975 quantile of standard normal

    let var_diff = |i: usize, j: usize| -> f64 {
        // Var(β_j - β_i) = a_inv[j-1,j-1] + a_inv[i-1,i-1] - 2 a_inv[i-1,j-1].
        // β_0 ≡ 0 → if either index is 0, the corresponding row/col is dropped.
        let aij = |a: usize, b: usize| -> f64 {
            if a == 0 || b == 0 { 0.0 } else { fit.a_inv[(a - 1, b - 1)] }
        };
        aij(j, j) + aij(i, i) - 2.0 * aij(i, j)
    };

    let mut out = Vec::new();
    for j in 0..t {
        for i in 0..t {
            if i >= j {
                continue;
            }
            // CINeMA's _row is "treat_i:treat_j" with i<j (lex).
            let row = format!("{}:{}", d.treats[i], d.treats[j]);
            // TE convention from cinema_nma.R: -(β_j - β_i).
            let te = -(fit.effects[j] - fit.effects[i]);
            let var = var_diff(i, j).max(0.0);
            let se = var.sqrt();
            let lci = te - z * se;
            let uci = te + z * se;
            let pri_se = (var + fit.tau2).sqrt();
            let lpi = te - t_quant * pri_se;
            let upi = te + t_quant * pri_se;
            out.push(NmaRowOut {
                row,
                te,
                se,
                lower_ci: lci,
                upper_ci: uci,
                lower_pri: lpi,
                upper_pri: upi,
            });
        }
    }
    // Match netmeta's column-major lower-triangle ordering: outer loop on
    // ref column j (1..T), inner on row i (j+1..T). Already produced in
    // (i<j) form above; reorder:
    out.sort_by(|a, b| a.row.cmp(&b.row));
    out
}

/// Student-t 97.5% quantile via a simple Cornish-Fisher expansion;
/// netmeta uses `qt(0.975, df)`. Accurate enough for prediction intervals
/// (df ≥ 1 typically, error < 0.01 for df ≥ 5).
fn student_t_quant_975(df: f64) -> f64 {
    if df >= 1e6 { return 1.959964; }
    let z = 1.959964_f64;
    // Hill (1970) expansion.
    let g1 = (z.powi(3) + z) / 4.0;
    let g2 = (5.0 * z.powi(5) + 16.0 * z.powi(3) + 3.0 * z) / 96.0;
    let g3 = (3.0 * z.powi(7) + 19.0 * z.powi(5) + 17.0 * z.powi(3) - 15.0 * z) / 384.0;
    z + g1 / df + g2 / df.powi(2) + g3 / df.powi(3)
}

// ─────────────────────────────────────────────────────────────────────
// Hat matrix (Davies long form, summed across studies per direct pair)
// ─────────────────────────────────────────────────────────────────────

/// `H[network_comparison, direct_comparison]` aggregated across studies.
/// Same shape as netmeta's `hatmatrix(method="Davies", type="long")$random`.
pub struct HatMatrixLong {
    pub row_names: Vec<String>,
    pub col_names: Vec<String>,
    /// `data[i]` = a map (direct_comparison → hat value) for row `i`.
    pub data: Vec<BTreeMap<String, f64>>,
}

/// Build the Davies long hat matrix from a fit. For network comparison
/// (i, j) and contrast row k touching treatments (t1, t2):
///   H[ij, k] = w_k · ((a⁻¹[t2,j] − a⁻¹[t1,j]) − (a⁻¹[t2,i] − a⁻¹[t1,i]))
/// (with a⁻¹[0, ·] = a⁻¹[·, 0] = 0 because treatment 0 is pinned).
/// We then aggregate row-level entries by canonical direct-comparison
/// id `min(t1,t2):max(t1,t2)`.
pub fn hatmatrix_long(d: &IvDataset, fit: &IvFitAt) -> HatMatrixLong {
    let t = d.treats.len();
    let aij = |a: usize, b: usize| -> f64 {
        if a == 0 || b == 0 { 0.0 } else { fit.a_inv[(a - 1, b - 1)] }
    };

    // Network comparisons (rows) — lex-canonical.
    let mut row_names = Vec::new();
    let mut row_ij: Vec<(usize, usize)> = Vec::new();
    for j in 0..t {
        for i in 0..t {
            if i >= j { continue; }
            row_names.push(format!("{}:{}", d.treats[i], d.treats[j]));
            row_ij.push((i, j));
        }
    }

    // Unique direct comparisons (columns) — sorted lex.
    let mut col_set: BTreeMap<String, ()> = BTreeMap::new();
    for r in &d.rows {
        let i1 = d.treat_idx[&r.t1];
        let i2 = d.treat_idx[&r.t2];
        let (a, b) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
        col_set.insert(format!("{}:{}", d.treats[a], d.treats[b]), ());
    }
    let col_names: Vec<String> = col_set.into_keys().collect();

    let mut data: Vec<BTreeMap<String, f64>> = vec![BTreeMap::new(); row_names.len()];
    for (idx_row, &(i, j)) in row_ij.iter().enumerate() {
        for (k, r) in d.rows.iter().enumerate() {
            let i1 = d.treat_idx[&r.t1];
            let i2 = d.treat_idx[&r.t2];
            // Canonical column orientation is `(a, b)` with `a < b` lex.
            // Define H[ij, k] in terms of the canonical direction so that
            // two studies on the same pair with opposite (t1, t2) order
            // sum coherently. For canonical direction `b - a`:
            //   H[ij, k]_canonical = w_k · ((a⁻¹[b,j] − a⁻¹[a,j])
            //                              − (a⁻¹[b,i] − a⁻¹[a,i]))
            let (a, b) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
            let h = fit.w[k]
                * ((aij(b, j) - aij(a, j)) - (aij(b, i) - aij(a, i)));
            if h.abs() < 1e-15 { continue; }
            let key = format!("{}:{}", d.treats[a], d.treats[b]);
            *data[idx_row].entry(key).or_default() += h;
        }
    }
    HatMatrixLong { row_names, col_names, data }
}

// ─────────────────────────────────────────────────────────────────────
// IV-weight extraction for Rust's per-study redistribution
// ─────────────────────────────────────────────────────────────────────

/// `[(study, comparison, weight), …]` matching the `iv_weights_netmeta`
/// JSON shape — but computed entirely from this fit, no R involved.
pub fn iv_weights_per_row(d: &IvDataset, fit: &IvFitAt) -> Vec<(String, String, f64)> {
    d.rows
        .iter()
        .enumerate()
        .map(|(k, r)| {
            let i1 = d.treat_idx[&r.t1];
            let i2 = d.treat_idx[&r.t2];
            let (a, b) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
            let cmp = format!("{}:{}", d.treats[a], d.treats[b]);
            (r.id.clone(), cmp, fit.w[k])
        })
        .collect()
}

// ─────────────────────────────────────────────────────────────────────
// Pairwise heterogeneity per direct comparison (DerSimonian-Laird)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct PairwiseHetRow {
    pub comparison: String,
    pub tau2: f64,
    pub i2: f64,
    pub i2_lower: f64,
    pub i2_upper: f64,
}

/// Per-direct-comparison τ² via DerSimonian-Laird, plus I² with Higgins'
/// 95% CI from a chi-squared approximation. Comparisons with only one
/// study are reported with τ² = 0 and `I² = NaN`.
pub fn pairwise_heterogeneity(d: &IvDataset) -> Vec<PairwiseHetRow> {
    use std::collections::BTreeMap;
    let mut groups: BTreeMap<String, Vec<usize>> = BTreeMap::new();
    for (k, r) in d.rows.iter().enumerate() {
        let i1 = d.treat_idx[&r.t1];
        let i2 = d.treat_idx[&r.t2];
        let (a, b) = if i1 < i2 { (i1, i2) } else { (i2, i1) };
        let cmp = format!("{}:{}", d.treats[a], d.treats[b]);
        groups.entry(cmp).or_default().push(k);
    }
    let mut out = Vec::new();
    for (cmp, ks) in groups {
        if ks.len() < 2 {
            out.push(PairwiseHetRow {
                comparison: cmp,
                tau2: 0.0,
                i2: f64::NAN,
                i2_lower: f64::NAN,
                i2_upper: f64::NAN,
            });
            continue;
        }
        // Inverse-variance pool (fixed-effect) on signed direct contrasts.
        // Contrast direction within group is (t1, t2) of each row;
        // align by flipping when needed.
        let i_a = d.treat_idx[cmp.split(':').next().unwrap()];
        let mut wsum = 0.0_f64;
        let mut wy = 0.0_f64;
        for &k in &ks {
            let r = &d.rows[k];
            let i1 = d.treat_idx[&r.t1];
            let yk = if i1 == i_a { r.effect } else { -r.effect };
            let wk = 1.0 / (r.se * r.se);
            wsum += wk;
            wy += wk * yk;
        }
        let mu_fe = wy / wsum;
        let mut q = 0.0_f64;
        let mut sum_w2 = 0.0_f64;
        for &k in &ks {
            let r = &d.rows[k];
            let i1 = d.treat_idx[&r.t1];
            let yk = if i1 == i_a { r.effect } else { -r.effect };
            let wk = 1.0 / (r.se * r.se);
            q += wk * (yk - mu_fe).powi(2);
            sum_w2 += wk * wk;
        }
        let n = ks.len() as f64;
        let df = n - 1.0;
        let tau2 = ((q - df) / (wsum - sum_w2 / wsum)).max(0.0);
        let i2 = (q - df).max(0.0) / q.max(1e-12) * 100.0;
        // Higgins (2002) 95% CI for I² via H = sqrt(Q/df).
        let h_lo;
        let h_hi;
        if q > df {
            let log_h = 0.5 * (q / df).ln();
            // SE of log H from Higgins (2002) eq. (6), simplified.
            let denom = (2.0 * (n - 1.0)).max(1.0);
            let inner = (1.0 - 1.0 / (3.0 * (n - 1.0).powi(2))).max(1e-9);
            let se = (inner / denom).sqrt();
            h_lo = (log_h - 1.959964 * se).exp();
            h_hi = (log_h + 1.959964 * se).exp();
        } else {
            h_lo = 1.0;
            h_hi = 1.0;
        }
        let i2_l = ((h_lo.powi(2) - 1.0) / h_lo.powi(2)).max(0.0) * 100.0;
        let i2_u = ((h_hi.powi(2) - 1.0) / h_hi.powi(2)).max(0.0) * 100.0;
        out.push(PairwiseHetRow {
            comparison: cmp,
            tau2,
            i2,
            i2_lower: i2_l,
            i2_upper: i2_u,
        });
    }
    out
}

// ─────────────────────────────────────────────────────────────────────
// Network heterogeneity / inconsistency (decomp.design)
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct DecompDesign {
    pub q_overall: f64,
    pub q_heterogeneity: f64,
    pub q_inconsistency: f64,
    /// Design-by-treatment Q + df + p-value for the inconsistency block.
    pub dbt_q: f64,
    pub dbt_df: f64,
    pub dbt_pvalue: f64,
}

/// Compute the standard NMA Q-decomposition.
///
///   Q.overall       = Σ_k (1/se_k²) (y_k − x_k' β̂_common)²  (fixed-effect fit)
///   Q.heterogeneity = Σ_design Σ_{k∈design} (1/se_k²) (y_k − ȳ_design)²
///   Q.inconsistency = Q.overall − Q.heterogeneity
///
/// Used for the v3 .cnm `networkHeterogeneity` and `designByTreatment`
/// blocks.
pub fn decomp_design(d: &IvDataset) -> DecompDesign {
    // Fixed-effect fit (τ² = 0) for Q.overall.
    let fit_fe = fit_iv_at(d, 0.0);
    let q_overall: f64 = (0..d.rows.len())
        .map(|k| fit_fe.w[k] * fit_fe.residuals[k].powi(2))
        .sum();

    // Within-design Q: pool studies sharing the same direct (t1, t2).
    use std::collections::BTreeMap;
    let mut groups: BTreeMap<(usize, usize), Vec<usize>> = BTreeMap::new();
    for (k, r) in d.rows.iter().enumerate() {
        let i1 = d.treat_idx[&r.t1];
        let i2 = d.treat_idx[&r.t2];
        let pair = if i1 < i2 { (i1, i2) } else { (i2, i1) };
        groups.entry(pair).or_default().push(k);
    }
    let mut q_het = 0.0_f64;
    for ((a, _b), ks) in &groups {
        if ks.len() < 2 { continue; }
        let mut wsum = 0.0_f64;
        let mut wy = 0.0_f64;
        for &k in ks {
            let r = &d.rows[k];
            let i1 = d.treat_idx[&r.t1];
            let yk = if i1 == *a { r.effect } else { -r.effect };
            let wk = 1.0 / (r.se * r.se);
            wsum += wk;
            wy += wk * yk;
        }
        let mu = wy / wsum;
        for &k in ks {
            let r = &d.rows[k];
            let i1 = d.treat_idx[&r.t1];
            let yk = if i1 == *a { r.effect } else { -r.effect };
            let wk = 1.0 / (r.se * r.se);
            q_het += wk * (yk - mu).powi(2);
        }
    }
    let q_inc = (q_overall - q_het).max(0.0);
    // Degrees of freedom: Q.overall ≈ N - p, Q.het ≈ N - n_designs.
    let n = d.rows.len() as f64;
    let p = d.treats.len() as f64 - 1.0;
    let n_designs = groups.len() as f64;
    let df_overall = (n - p).max(1.0);
    let _ = df_overall;
    let df_het = (n - n_designs).max(1.0);
    let _ = df_het;
    let df_inc = (n_designs - p).max(1.0);
    let pval = chi2_p_upper(q_inc, df_inc);
    DecompDesign {
        q_overall,
        q_heterogeneity: q_het,
        q_inconsistency: q_inc,
        dbt_q: q_inc,
        dbt_df: df_inc,
        dbt_pvalue: pval,
    }
}

/// Upper-tail chi-squared p-value via a simple Wilson-Hilferty
/// approximation, plenty accurate at the precision the .cnm reports.
fn chi2_p_upper(q: f64, df: f64) -> f64 {
    if df <= 0.0 || q < 0.0 { return 1.0; }
    let h = 2.0 / (9.0 * df);
    let z = ((q / df).powf(1.0 / 3.0) - (1.0 - h)) / h.sqrt();
    1.0 - normal_cdf(z)
}

fn normal_cdf(z: f64) -> f64 {
    0.5 * (1.0 + erf(z / std::f64::consts::SQRT_2))
}

/// Abramowitz & Stegun 7.1.26 — relative error < 1.5e-7.
fn erf(x: f64) -> f64 {
    let s = x.signum();
    let a = x.abs();
    let p = 0.3275911_f64;
    let a1 = 0.254829592_f64;
    let a2 = -0.284496736_f64;
    let a3 = 1.421413741_f64;
    let a4 = -1.453152027_f64;
    let a5 = 1.061405429_f64;
    let t = 1.0 / (1.0 + p * a);
    let y = 1.0
        - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * (-a * a).exp();
    s * y
}

// ─────────────────────────────────────────────────────────────────────
// Netsplit / SIDE
// ─────────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Serialize)]
pub struct SideRow {
    pub comparison: String, // "tA:tB", lex
    pub direct: Option<f64>,
    pub direct_l: Option<f64>,
    pub direct_u: Option<f64>,
    pub indirect: Option<f64>,
    pub indirect_l: Option<f64>,
    pub indirect_u: Option<f64>,
    pub side_if: Option<f64>,
    pub side_if_l: Option<f64>,
    pub side_if_u: Option<f64>,
    pub side_z: Option<f64>,
    pub side_p: Option<f64>,
    pub prop_dir: Option<f64>,
}

/// Node-splitting (SIDE) for every direct comparison. The direct estimate
/// pools the studies for that comparison via inverse-variance + the
/// network τ²; the indirect estimate is the network estimate computed on
/// the dataset with those direct studies dropped. Incoherence statistic
/// uses Var(direct) + Var(indirect).
pub fn netsplit(d: &IvDataset, fit: &IvFitAt) -> Vec<SideRow> {
    let mut out = Vec::new();
    // Index rows by direct (t1, t2) lex-canonical pair.
    use std::collections::BTreeMap;
    let mut groups: BTreeMap<(usize, usize), Vec<usize>> = BTreeMap::new();
    for (k, r) in d.rows.iter().enumerate() {
        let i1 = d.treat_idx[&r.t1];
        let i2 = d.treat_idx[&r.t2];
        let pair = if i1 < i2 { (i1, i2) } else { (i2, i1) };
        groups.entry(pair).or_default().push(k);
    }
    let z_q = 1.959964_f64;
    let tau2 = fit.tau2;
    for (&(a, b), ks) in &groups {
        let cmp = format!("{}:{}", d.treats[a], d.treats[b]);
        // Direct estimate: random-effects IV pool with τ² from the network fit.
        let mut wsum = 0.0_f64;
        let mut wy = 0.0_f64;
        for &k in ks {
            let r = &d.rows[k];
            let i1 = d.treat_idx[&r.t1];
            let yk = if i1 == a { r.effect } else { -r.effect };
            let wk = 1.0 / (r.se * r.se + tau2);
            wsum += wk;
            wy += wk * yk;
        }
        let direct = wy / wsum;
        let var_direct = 1.0 / wsum;
        let se_direct = var_direct.sqrt();

        // Sign convention from cinema_nma.R: report -(β_b - β_a).
        let direct_te = -direct;
        let direct_l = direct_te - z_q * se_direct;
        let direct_u = direct_te + z_q * se_direct;

        // Indirect: refit NMA dropping these rows.
        let mut sub = d.clone();
        let drop: std::collections::HashSet<usize> = ks.iter().copied().collect();
        sub.rows = sub
            .rows
            .into_iter()
            .enumerate()
            .filter(|(i, _)| !drop.contains(i))
            .map(|(_, r)| r)
            .collect();
        let indirect_pack: Option<(f64, f64)> = if sub.rows.len() < (sub.treats.len() - 1) {
            None
        } else {
            try_fit_iv_at(&sub, tau2).map(|f2| {
                let i = a; let j = b;
                let aij = |x: usize, y: usize| -> f64 {
                    if x == 0 || y == 0 { 0.0 } else { f2.a_inv[(x - 1, y - 1)] }
                };
                let var = (aij(j, j) + aij(i, i) - 2.0 * aij(i, j)).max(0.0);
                let est = -(f2.effects[j] - f2.effects[i]);
                (est, var)
            })
        };

        let (indirect_te, indirect_l, indirect_u, side_if, side_if_l, side_if_u, side_z, side_p) =
            if let Some((est, var)) = indirect_pack {
                let se_i = var.sqrt();
                let il = est - z_q * se_i;
                let iu = est + z_q * se_i;
                // Incoherence = direct - indirect.
                let inc = direct_te - est;
                let var_inc = (var_direct + var).max(0.0);
                let se_inc = var_inc.sqrt();
                let inc_l = inc - z_q * se_inc;
                let inc_u = inc + z_q * se_inc;
                let zstat = inc / se_inc.max(1e-12);
                let p = 2.0 * (1.0 - normal_cdf(zstat.abs()));
                (
                    Some(est),
                    Some(il),
                    Some(iu),
                    Some(inc),
                    Some(inc_l),
                    Some(inc_u),
                    Some(zstat),
                    Some(p),
                )
            } else {
                (None, None, None, None, None, None, None, None)
            };

        // Proportion direct: from the network hat row for (a, b),
        // h_direct / sum |h|. We approximate: proportion = w_direct_total /
        // (w_direct_total + 1/var_indirect).
        let prop = if let Some((_, var_i)) = indirect_pack {
            let w_i = 1.0 / var_i.max(1e-12);
            Some(wsum / (wsum + w_i))
        } else {
            None
        };

        out.push(SideRow {
            comparison: cmp,
            direct: Some(direct_te),
            direct_l: Some(direct_l),
            direct_u: Some(direct_u),
            indirect: indirect_te,
            indirect_l,
            indirect_u,
            side_if,
            side_if_l,
            side_if_u,
            side_z,
            side_p,
            prop_dir: prop,
        });
    }
    out
}

// ─────────────────────────────────────────────────────────────────────
// League table
// ─────────────────────────────────────────────────────────────────────

/// Format a T×T league table on the OR scale (or identity for MD/SMD/etc).
/// Diagonal = treatment names; off-diagonal = `"<eff> (<lo>, <up>)"`.
pub fn league_table(d: &IvDataset, fit: &IvFitAt, sm: &str) -> Vec<Vec<String>> {
    let t = d.treats.len();
    let aij = |a: usize, b: usize| -> f64 {
        if a == 0 || b == 0 { 0.0 } else { fit.a_inv[(a - 1, b - 1)] }
    };
    let exp_scale = matches!(sm, "OR" | "RR" | "HR");
    let mut out = vec![vec![String::new(); t]; t];
    for i in 0..t {
        for j in 0..t {
            if i == j {
                out[i][j] = d.treats[i].clone();
            } else {
                let te = -(fit.effects[j] - fit.effects[i]);
                let var = (aij(j, j) + aij(i, i) - 2.0 * aij(i, j)).max(0.0);
                let se = var.sqrt();
                let z = 1.959964_f64;
                let (e, l, u) = if exp_scale {
                    (te.exp(), (te - z * se).exp(), (te + z * se).exp())
                } else {
                    (te, te - z * se, te + z * se)
                };
                out[i][j] = format!("{:.3} ({:.3}, {:.3})", e, l, u);
            }
        }
    }
    out
}

// ─────────────────────────────────────────────────────────────────────
// Tests
// ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    /// Tiny 3-treatment IV dataset with a closed loop.
    fn triangle() -> IvDataset {
        IvDataset::from_rows(vec![
            IvRow { id: "s1".into(), t1: "A".into(), t2: "B".into(), effect: 0.5, se: 0.2, rob: 1, indirectness: 1 },
            IvRow { id: "s2".into(), t1: "A".into(), t2: "B".into(), effect: 0.4, se: 0.25, rob: 1, indirectness: 1 },
            IvRow { id: "s3".into(), t1: "A".into(), t2: "C".into(), effect: 1.0, se: 0.3, rob: 1, indirectness: 1 },
            IvRow { id: "s4".into(), t1: "B".into(), t2: "C".into(), effect: 0.4, se: 0.25, rob: 1, indirectness: 1 },
        ])
    }

    #[test]
    fn fits_at_zero_tau() {
        let d = triangle();
        let fit = fit_iv_at(&d, 0.0);
        // β_B should be near the IV-pooled A-vs-B mean (~0.46) and
        // β_C should be near 0.85 (mix of direct + indirect via B).
        assert!((fit.effects[1] - 0.46).abs() < 0.05);
        assert!((fit.effects[2] - 0.85).abs() < 0.10);
    }

    #[test]
    fn reml_finite() {
        let d = triangle();
        let fit = fit_iv_reml(&d);
        assert!(fit.tau2 >= 0.0);
        assert!(fit.log_z.is_finite());
    }

    #[test]
    fn hat_matrix_streams_sum_to_one() {
        // Convert the Davies long hat matrix to wide-form input for the
        // streams algorithm; each row's contribution sum should be ≈ 1.
        use crate::contribution::{
            contribution_matrix_shortest_path, hat_matrix_from_list,
            sum_contribution_row, HElement,
        };
        let d = triangle();
        let fit = fit_iv_at(&d, 0.0);
        let hm = hatmatrix_long(&d, &fit);
        let wide: Vec<HElement> = hm
            .data
            .iter()
            .zip(hm.row_names.iter())
            .flat_map(|(row, rid)| {
                row.iter().map(move |(cid, v)| HElement {
                    row: rid.clone(),
                    comparison: cid.clone(),
                    value: *v,
                })
            })
            .collect();
        let cm = contribution_matrix_shortest_path(&hat_matrix_from_list(&wide));
        for (rid, cr) in &cm {
            let s = sum_contribution_row(cr);
            assert!(
                (s - 1.0).abs() < 1e-6,
                "row {rid} contribution sum = {s}"
            );
        }
    }

    #[test]
    fn nma_results_count() {
        let d = triangle();
        let fit = fit_iv_at(&d, 0.0);
        let nr = nma_results(&d, &fit);
        // C(3,2) = 3 comparisons.
        assert_eq!(nr.len(), 3);
    }

    #[test]
    fn pairwise_het_sane() {
        let d = triangle();
        let pw = pairwise_heterogeneity(&d);
        // 3 direct comparisons; A:B has 2 studies.
        assert_eq!(pw.len(), 3);
        let ab = pw.iter().find(|p| p.comparison == "A:B").unwrap();
        assert!(ab.tau2 >= 0.0);
    }

    #[test]
    fn decomp_works() {
        let d = triangle();
        let dd = decomp_design(&d);
        assert!(dd.q_overall >= 0.0);
        assert!(dd.q_heterogeneity >= 0.0);
        assert!(dd.q_inconsistency >= 0.0);
        assert!((dd.q_overall - dd.q_heterogeneity - dd.q_inconsistency).abs() < 1e-9);
    }

    #[test]
    fn netsplit_runs() {
        let d = triangle();
        let fit = fit_iv_at(&d, 0.0);
        let ns = netsplit(&d, &fit);
        assert_eq!(ns.len(), 3);
        // Triangle is closed, so direct + indirect both exist.
        for r in &ns {
            assert!(r.direct.is_some());
            assert!(r.indirect.is_some(), "{} indirect None", r.comparison);
        }
    }
}
