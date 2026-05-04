//! Bivariate fixed-effect NMA — joint K=2 spring solve.
//!
//! Per `slmm/docs/spring_multivariate.html`: each treatment is a point
//! in R^K (K=2 here), each study creates a spring with matrix-valued
//! stiffness `V_i⁻¹` (inverse within-study covariance) coupling the K
//! outcome dimensions. Equilibrium effects β satisfy
//!
//!     (Xᵀ W X) β = Xᵀ W y
//!
//! where:
//!   - β ∈ R^{(T−1)·K} is the stacked vector of treatment-vs-reference
//!     effects (treatment 0 pinned at 0 ∈ R^K).
//!   - X is the contrast design matrix, rows-one-per-study, ±1 entries.
//!   - W is block-diagonal with blocks `V_i⁻¹` (K×K), one per study.
//!   - The normal-equation matrix has block-Laplacian structure:
//!     `Σ_i (e_{t2_i} − e_{t1_i})(e_{t2_i} − e_{t1_i})ᵀ ⊗ V_i⁻¹`.
//!
//! This is solved as **one joint Cholesky factorisation** — not K
//! separate scalar solves. Per `spring_tilted_axes.html`, the off-
//! diagonal of `V_i` (or equivalently of `V_i⁻¹`) is the angular tilt
//! between outcome axes — `cos(angle) = ρ`.
//!
//! Validation:
//!   - Compare to two independent univariate NMAs when ρ = 0 (must match).
//!   - With ρ ≠ 0 the joint estimate differs from the univariate
//!     estimates — that's the "borrowing strength" effect.

use nalgebra::{Cholesky, DMatrix, DVector};

/// One bivariate study: contrast t1 → t2 with observed effects (y1, y2)
/// on the two outcomes, within-study variances v1, v2, and within-
/// study correlation between outcomes ρ.
#[derive(Clone, Debug)]
struct BivStudy {
    t1: usize,
    t2: usize,
    y: [f64; 2],
    v1: f64,
    v2: f64,
    rho: f64,
}

impl BivStudy {
    /// 2×2 within-study covariance V.
    fn covariance(&self) -> [[f64; 2]; 2] {
        let cov = self.rho * (self.v1 * self.v2).sqrt();
        [[self.v1, cov], [cov, self.v2]]
    }
    /// 2×2 within-study precision V⁻¹.
    fn precision(&self) -> [[f64; 2]; 2] {
        let v = self.covariance();
        let det = v[0][0] * v[1][1] - v[0][1] * v[1][0];
        [
            [ v[1][1] / det, -v[0][1] / det],
            [-v[1][0] / det,  v[0][0] / det],
        ]
    }
}

/// Bivariate NMA: solve for β_t ∈ R² for all t ≠ 0 (treatment 0 pinned).
fn fit_bivariate(t: usize, studies: &[BivStudy]) -> Vec<[f64; 2]> {
    let k = 2;
    let dim = (t - 1) * k;
    let mut a = DMatrix::<f64>::zeros(dim, dim);
    let mut b = DVector::<f64>::zeros(dim);

    // Map treatment index (1..t) to row index in stacked β.
    let row_of = |treat: usize, outcome: usize| -> Option<usize> {
        if treat == 0 {
            None
        } else {
            Some((treat - 1) * k + outcome)
        }
    };

    // Each study contributes (e_{t2} − e_{t1})(e_{t2} − e_{t1})ᵀ ⊗ V⁻¹
    // to A and (e_{t2} − e_{t1}) ⊗ (V⁻¹ y) to b.
    for s in studies {
        let p = s.precision();
        // Compute contribution to A: scalar (e_{t2}_t1 − e_{t1}_t1)·(e_{t2}_t2 − e_{t1}_t2)
        // for each pair of treatments (t_a, t_b) with non-zero entries.
        // Since contrast vector is (±1 at t1, ∓1 at t2, 0 elsewhere), the
        // outer product has nonzero entries at (t1,t1), (t1,t2), (t2,t1), (t2,t2).
        for (sign_a, t_a) in [(-1.0_f64, s.t1), (1.0, s.t2)] {
            if t_a == 0 {
                continue;
            }
            for (sign_b, t_b) in [(-1.0_f64, s.t1), (1.0, s.t2)] {
                if t_b == 0 {
                    continue;
                }
                let scalar = sign_a * sign_b;
                for i in 0..k {
                    for j in 0..k {
                        let ra = row_of(t_a, i).unwrap();
                        let rb = row_of(t_b, j).unwrap();
                        a[(ra, rb)] += scalar * p[i][j];
                    }
                }
            }
        }
        // RHS: b += (e_{t2} - e_{t1}) ⊗ (V⁻¹ y)
        let py = [
            p[0][0] * s.y[0] + p[0][1] * s.y[1],
            p[1][0] * s.y[0] + p[1][1] * s.y[1],
        ];
        for (sign, t_) in [(-1.0_f64, s.t1), (1.0, s.t2)] {
            if t_ == 0 {
                continue;
            }
            for i in 0..k {
                let r = row_of(t_, i).unwrap();
                b[r] += sign * py[i];
            }
        }
    }

    // Solve via dense Cholesky.
    let chol = Cholesky::new(a).expect("matrix should be SPD");
    let beta = chol.solve(&b);

    // Unpack into per-treatment R^k vectors.
    let mut out: Vec<[f64; 2]> = vec![[0.0, 0.0]; t];
    for treat in 1..t {
        out[treat] = [beta[(treat - 1) * k], beta[(treat - 1) * k + 1]];
    }
    out
}

/// For ρ = 0, the joint solve must equal two independent univariate
/// solves. This computes one outcome's univariate NMA: each study
/// contributes 1/v_outcome to (X'WX) and y_outcome/v_outcome to X'Wy.
fn fit_univariate(t: usize, studies: &[BivStudy], outcome: usize) -> Vec<f64> {
    let dim = t - 1;
    let mut a = DMatrix::<f64>::zeros(dim, dim);
    let mut b = DVector::<f64>::zeros(dim);
    for s in studies {
        let v = if outcome == 0 { s.v1 } else { s.v2 };
        let y = s.y[outcome];
        let w = 1.0 / v;
        for (sa, ta) in [(-1.0_f64, s.t1), (1.0, s.t2)] {
            if ta == 0 { continue; }
            for (sb, tb) in [(-1.0_f64, s.t1), (1.0, s.t2)] {
                if tb == 0 { continue; }
                a[(ta - 1, tb - 1)] += sa * sb * w;
            }
            b[ta - 1] += sa * w * y;
        }
    }
    let chol = Cholesky::new(a).expect("should be SPD");
    let beta = chol.solve(&b);
    (0..t).map(|i| if i == 0 { 0.0 } else { beta[i - 1] }).collect()
}

fn main() {
    // 3-treatment bivariate test: A=0, B=1, C=2. Two outcomes per study.
    // Six studies covering A-B, A-C, B-C contrasts.
    let studies = vec![
        BivStudy { t1: 0, t2: 1, y: [2.0, 1.5], v1: 0.4, v2: 0.3, rho: 0.0 },
        BivStudy { t1: 0, t2: 1, y: [1.7, 1.2], v1: 0.5, v2: 0.4, rho: 0.0 },
        BivStudy { t1: 0, t2: 2, y: [3.1, 2.0], v1: 0.6, v2: 0.5, rho: 0.0 },
        BivStudy { t1: 0, t2: 2, y: [2.9, 1.8], v1: 0.5, v2: 0.4, rho: 0.0 },
        BivStudy { t1: 1, t2: 2, y: [1.0, 0.8], v1: 0.4, v2: 0.3, rho: 0.0 },
        BivStudy { t1: 1, t2: 2, y: [1.2, 0.9], v1: 0.5, v2: 0.4, rho: 0.0 },
    ];

    println!("==== Test 1: ρ = 0 (joint should equal two univariate) ====");
    let mut s0 = studies.clone();
    for s in &mut s0 {
        s.rho = 0.0;
    }
    let beta_joint = fit_bivariate(3, &s0);
    let beta_uni0 = fit_univariate(3, &s0, 0);
    let beta_uni1 = fit_univariate(3, &s0, 1);
    println!("Joint:");
    for t in 1..3 {
        println!("  β_{} = ({:.6}, {:.6})", t, beta_joint[t][0], beta_joint[t][1]);
    }
    println!("Univariate outcome 0: β_1 = {:.6}, β_2 = {:.6}", beta_uni0[1], beta_uni0[2]);
    println!("Univariate outcome 1: β_1 = {:.6}, β_2 = {:.6}", beta_uni1[1], beta_uni1[2]);

    let mut max_diff = 0.0_f64;
    for t in 1..3 {
        max_diff = max_diff.max((beta_joint[t][0] - beta_uni0[t]).abs());
        max_diff = max_diff.max((beta_joint[t][1] - beta_uni1[t]).abs());
    }
    println!("Max |joint - univariate| = {:.2e} (must be ~0 at ρ=0)", max_diff);
    assert!(max_diff < 1e-12, "ρ=0 case must match exactly");
    println!("✓ ρ = 0 case matches univariate");

    println!();
    println!("==== Test 2: ρ = 0.6 (joint diverges from univariate via borrowing) ====");
    let mut s_corr = studies.clone();
    for s in &mut s_corr {
        s.rho = 0.6;
    }
    let beta_joint_c = fit_bivariate(3, &s_corr);
    let beta_uni0_c = fit_univariate(3, &s_corr, 0);
    let beta_uni1_c = fit_univariate(3, &s_corr, 1);
    println!("Joint (ρ=0.6):");
    for t in 1..3 {
        println!("  β_{} = ({:.6}, {:.6})", t, beta_joint_c[t][0], beta_joint_c[t][1]);
    }
    println!("Univariate outcome 0: β_1 = {:.6}, β_2 = {:.6}", beta_uni0_c[1], beta_uni0_c[2]);
    println!("Univariate outcome 1: β_1 = {:.6}, β_2 = {:.6}", beta_uni1_c[1], beta_uni1_c[2]);
    let mut max_diff = 0.0_f64;
    for t in 1..3 {
        max_diff = max_diff.max((beta_joint_c[t][0] - beta_uni0_c[t]).abs());
        max_diff = max_diff.max((beta_joint_c[t][1] - beta_uni1_c[t]).abs());
    }
    println!("Max |joint - univariate| = {:.4} (should be > 0; this is the borrowing-strength effect)", max_diff);
    assert!(max_diff > 1e-6, "ρ≠0 should produce different joint estimate");

    println!();
    println!("==== Test 3: missing-outcome study (R^p borrowing-strength demo) ====");
    // Same dataset, but study 5 only measures outcome 0 (set v_2 = ∞).
    let mut s_miss = studies.clone();
    for s in &mut s_miss {
        s.rho = 0.7;
    }
    s_miss[5].v2 = 1e10; // effectively missing outcome 1 in this study
    let beta_joint_m = fit_bivariate(3, &s_miss);
    let beta_uni1_m = fit_univariate(3, &s_miss, 1);
    println!("Joint (study 5 missing y2, ρ=0.7):");
    for t in 1..3 {
        println!("  β_{} = ({:.6}, {:.6})", t, beta_joint_m[t][0], beta_joint_m[t][1]);
    }
    println!("Univariate outcome 1 only: β_1 = {:.6}, β_2 = {:.6}", beta_uni1_m[1], beta_uni1_m[2]);
    println!("(Joint outcome-1 estimates use information from study 5's outcome 0 via correlation;");
    println!(" univariate ignores study 5 entirely on outcome 1 because v2 = ∞.)");

    println!();
    println!("==== Tilted-axes geometric check ====");
    // Per spring_tilted_axes.html: angle = arccos(ρ).
    for &rho in &[-0.9_f64, -0.5, 0.0, 0.3, 0.6, 0.9] {
        let angle_rad = rho.acos();
        let angle_deg = angle_rad.to_degrees();
        println!("  ρ = {:+.2}  →  axes angle = {:.2}°", rho, angle_deg);
    }
}
