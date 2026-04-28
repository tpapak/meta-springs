//! Gaussian (continuous-outcome) Grand Canonical evaluator.
//!
//! Direct port of the Gaussian arm of `springGrandCanonicalAdaptive` from
//! `src/Data/Meta/RandomEffects.hs:1308-1465`. Unlike the binomial path this
//! is a *linear* Laplace integral: at each τ² the mode is the solution of one
//! `(k+T-1) × (k+T-1)` linear system, no IRLS.
//!
//! For arm `i` with sample mean `y_i` and sampling variance `v_i = sd_i²/n_i`:
//!
//!   * effective spring stiffness  k_eff_i = 1 / (v_i + τ²_arm)
//!   * τ²_arm = τ²_contrast / 2  (same convention as the binomial path)
//!
//! The reduced (`x`-only) system is then
//!     A · x = b
//! where
//!     A[s,s] += k_eff,  A[t,t] += k_eff,  A[s,t] -= k_eff,  A[t,s] -= k_eff
//!     b[s]  -= k_eff · y_i,  b[t]  += k_eff · y_i
//! per arm (with one treatment pinned at 0; `t` index is `None` for the
//! pinned branch).
//!
//! Once the mode is at hand,
//!     E*    = ½ Σ_i k_eff_i · (x_t - x_s - y_i)²
//!     logZ  = -E*  -  ½ Σ_i log(v_i + τ²_arm)  -  ½ log|det A|.

use crate::data::Dataset;
use crate::solver::{GridPoint, Topology};
use nalgebra::{DMatrix, DVector};

/// Per-arm continuous payload: (sample mean, sampling variance).
#[derive(Debug, Clone)]
pub struct ContArm {
    pub y: f64,
    pub v: f64,
}

/// Pre-compute per-arm `(y, v)` in the same order as `topo.arms`. Caller is
/// expected to pass the same `Dataset` that `Topology::build` consumed.
pub fn cont_arms(ds: &Dataset) -> Vec<ContArm> {
    let mut out = Vec::with_capacity(ds.studies.iter().map(|s| s.arms.len()).sum());
    for st in &ds.studies {
        for arm in &st.arms {
            let n_ = arm.n as f64;
            // Guard against zero-sample studies; v=0 is fine numerically when
            // τ²_arm > 0, but we keep a tiny floor for robustness.
            let v = if n_ > 0.0 {
                (arm.sd * arm.sd) / n_
            } else {
                0.0
            };
            out.push(ContArm { y: arm.mean, v: v.max(1e-12) });
        }
    }
    out
}

/// Single Laplace evaluation at a fixed `τ²_contrast`.
///
/// Returns a [`GridPoint`] so the existing posterior / prior infrastructure
/// in `gc.rs` works without modification — we reuse the same shape (`tau2`,
/// `log_z`, per-treatment `effects`, per-treatment `eff_vars`) that the
/// binomial path produces.
pub fn eval_xi_cont_at(
    topo: &Topology,
    arms: &[ContArm],
    tau2_contrast: f64,
) -> GridPoint {
    let tau2c = tau2_contrast.max(1e-10);
    let tau2arm = tau2c / 2.0;
    let n_free = topo.n_free;

    let mut a_mat = DMatrix::<f64>::zeros(n_free, n_free);
    let mut b_vec = DVector::<f64>::zeros(n_free);
    let mut keff_per_arm = vec![0.0_f64; topo.arms.len()];
    let mut v_per_arm = vec![0.0_f64; topo.arms.len()];

    for (i, arec) in topo.arms.iter().enumerate() {
        let v_i = arms[i].v;
        let y_i = arms[i].y;
        let k_eff = 1.0 / (v_i + tau2arm);
        keff_per_arm[i] = k_eff;
        v_per_arm[i] = v_i;

        let s = arec.s_idx;
        match arec.t_idx {
            None => {
                // pinned treatment branch: x_t == 0
                a_mat[(s, s)] += k_eff;
                b_vec[s] += -k_eff * y_i;
            }
            Some(t) => {
                a_mat[(s, s)] += k_eff;
                a_mat[(t, t)] += k_eff;
                a_mat[(s, t)] += -k_eff;
                a_mat[(t, s)] += -k_eff;
                b_vec[s] += -k_eff * y_i;
                b_vec[t] += k_eff * y_i;
            }
        }
    }

    // Solve A · x = b via LU. The reduced system is symmetric positive
    // definite for τ²_arm > 0 in a connected network; LU is enough for the
    // grid sizes we care about.
    let lu = a_mat.clone().lu();
    let x_free = match lu.solve(&b_vec) {
        Some(x) => x,
        None => panic!(
            "continuous reduced system singular at tau2c={} (disconnected network?)",
            tau2c
        ),
    };

    // Energy at the mode.
    let mut e_star = 0.0_f64;
    for (i, arec) in topo.arms.iter().enumerate() {
        let x_s = x_free[arec.s_idx];
        let x_t = match arec.t_idx {
            None => 0.0,
            Some(t) => x_free[t],
        };
        let r = x_t - x_s - arms[i].y;
        e_star += 0.5 * keff_per_arm[i] * r * r;
    }

    // Arm prefactor: -½ Σ log(v_i + τ²_arm) = +½ Σ log(k_eff_i).
    let mut arm_pref = 0.0_f64;
    for i in 0..topo.arms.len() {
        arm_pref += (v_per_arm[i] + tau2arm).ln();
    }
    arm_pref *= -0.5;

    // log|det A| via LU diagonal of U.
    let log_det_a = lu_log_det_abs(&lu, n_free);

    let log_z = arm_pref - e_star - 0.5 * log_det_a;

    // Per-treatment effects vs the pinned ref. Variance from A^{-1}[idx,idx].
    let n_studies = topo.n_studies;
    let n_t = topo.n_treats;
    let mut effects = vec![0.0_f64; n_t];
    let mut eff_vars = vec![0.0_f64; n_t];

    let a_inv = match a_mat.clone().try_inverse() {
        Some(m) => m,
        None => panic!("continuous A matrix singular at tau2={}", tau2c),
    };

    for t in 0..n_t {
        if t == 0 {
            // pinned treatment
            effects[t] = 0.0;
            eff_vars[t] = 0.0;
        } else {
            let idx = n_studies + t - 1;
            effects[t] = x_free[idx];
            eff_vars[t] = a_inv[(idx, idx)];
        }
    }

    GridPoint {
        tau2: tau2c,
        log_z,
        effects,
        eff_vars,
        // No iteration in the Gaussian path; report 1.
        iters: 1,
    }
}

/// log|det| of an `n x n` matrix from its LU factorisation. Reads the diagonal
/// of U.
fn lu_log_det_abs(
    lu: &nalgebra::LU<f64, nalgebra::Dyn, nalgebra::Dyn>,
    n: usize,
) -> f64 {
    let u = lu.u();
    let mut s = 0.0;
    for i in 0..n {
        let d = u[(i, i)].abs();
        s += d.ln();
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::data::{Arm, Dataset, Mode, Study};

    fn tiny_cont_dataset() -> Dataset {
        let mk_arm = |t: &str, mean: f64, sd: f64, n: i64| Arm {
            treatment: t.into(),
            events: 0,
            n,
            mean,
            sd,
        };
        let s1 = Study {
            study_id: 1,
            arms: vec![mk_arm("A", 0.0, 1.0, 50), mk_arm("B", 0.5, 1.0, 50)],
        };
        let s2 = Study {
            study_id: 2,
            arms: vec![mk_arm("A", 0.1, 1.0, 100), mk_arm("B", 0.7, 1.0, 100)],
        };
        let s3 = Study {
            study_id: 3,
            arms: vec![mk_arm("A", -0.05, 1.0, 40), mk_arm("B", 0.4, 1.0, 40)],
        };
        Dataset {
            studies: vec![s1, s2, s3],
            treatments: vec!["A".into(), "B".into()],
            mode: Mode::Continuous,
        }
    }

    #[test]
    fn cont_log_z_finite() {
        let ds = tiny_cont_dataset();
        let topo = Topology::build(&ds);
        let arms = cont_arms(&ds);
        let g = eval_xi_cont_at(&topo, &arms, 0.05);
        assert!(g.log_z.is_finite(), "logZ not finite: {}", g.log_z);
        // Effect of B vs A should be near 0.5.
        assert!(
            (g.effects[1] - 0.5).abs() < 0.2,
            "effect B-A out of range: {}",
            g.effects[1]
        );
    }
}
