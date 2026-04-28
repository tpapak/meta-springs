//! Reduced-Schur Newton solver for the binomial spring network at a fixed τ²,
//! plus the Laplace `logZ(τ²)` evaluation.
//!
//! Direct port of `newtonSolveReducedBin` and the `evalLogZFrom` lambda from
//! `src/Data/Meta/RandomEffects.hs`. The reduced system has dimension
//! `nFree = nStudies + nTreatments - 1` (one treatment is pinned at 0).

use crate::data::Dataset;
use nalgebra::{DMatrix, DVector};
use std::collections::BTreeMap;

/// Per-arm static topology in terms of reduced-vertex indices.
#[derive(Debug, Clone)]
pub struct ArmRec {
    /// Index of the study vertex in the reduced system.
    pub s_idx: usize,
    /// Index of the treatment vertex in the reduced system, or `None` if pinned.
    pub t_idx: Option<usize>,
    /// Events / sample size for this arm (binomial).
    pub events: i64,
    pub n: i64,
}

/// Topological description of the reduced spring network: which study/treatment
/// vertex each arm touches and the dimension of the reduced system.
#[derive(Debug, Clone)]
pub struct Topology {
    pub n_free: usize,
    /// `studies[i]` -> reduced index in `0..nStudies`. Same order as
    /// `dataset.studies`.
    pub study_idx: Vec<usize>,
    /// Treatment id -> reduced index. None means pinned (lex-first treatment).
    pub treat_idx: BTreeMap<String, Option<usize>>,
    /// Per-arm records, flattened in `(study, arm)` order.
    pub arms: Vec<ArmRec>,
    /// Number of studies and treatments.
    pub n_studies: usize,
    pub n_treats: usize,
    /// Sorted treatment id list. The first one is the pinned (`ref`) treatment.
    pub treatment_order: Vec<String>,
}

impl Topology {
    pub fn build(ds: &Dataset) -> Self {
        // Treatments sorted lex; first one is pinned.
        let treatment_order: Vec<String> = ds.treatments.clone();
        let n_studies = ds.studies.len();
        let n_treats = treatment_order.len();
        let n_free = n_studies + n_treats - 1;

        // study_idx: studies are reduced vertices 0..n_studies.
        let study_idx: Vec<usize> = (0..n_studies).collect();

        // treat_idx: pinned treatment (treatment_order[0]) -> None,
        // others -> n_studies, n_studies+1, ...
        let mut treat_idx = BTreeMap::new();
        for (i, t) in treatment_order.iter().enumerate() {
            if i == 0 {
                treat_idx.insert(t.clone(), None);
            } else {
                treat_idx.insert(t.clone(), Some(n_studies + i - 1));
            }
        }

        let mut arms = Vec::new();
        for (s_pos, st) in ds.studies.iter().enumerate() {
            for arm in &st.arms {
                let s_idx = study_idx[s_pos];
                let t_idx = *treat_idx
                    .get(&arm.treatment)
                    .expect("treatment id missing from topology");
                arms.push(ArmRec { s_idx, t_idx, events: arm.events, n: arm.n });
            }
        }

        Topology {
            n_free,
            study_idx,
            treat_idx,
            arms,
            n_studies,
            n_treats,
            treatment_order,
        }
    }
}

/// Per-arm state tracked across Newton iterations: positions of the study,
/// treatment, and τ-aux node for the corresponding arm. Treatment position is
/// 0 for the pinned treatment.
#[derive(Debug, Clone)]
struct ArmState {
    x_s: f64,
    x_t: f64,
    x_tau: f64,
    /// Length `l = x_T - x_τ` at the current iterate. Used for convergence.
    l: f64,
}

#[inline]
fn sigmoid(x: f64) -> f64 {
    if x >= 0.0 {
        let e = (-x).exp();
        1.0 / (1.0 + e)
    } else {
        let e = x.exp();
        e / (1.0 + e)
    }
}

#[inline]
fn clamp(lo: f64, hi: f64, x: f64) -> f64 {
    x.max(lo).min(hi)
}

/// Result of the reduced Newton solve at a single τ². Carries everything the
/// Laplace evaluator needs (Fisher weights, mode positions, A matrix factor).
#[derive(Debug, Clone)]
pub struct SolveOut {
    /// `nFree x nFree` reduced Hessian at the converged mode (linearised).
    pub a_mat: DMatrix<f64>,
    /// Solution vector (length `nFree`): study positions then free-treatment
    /// positions, in `Topology` order.
    pub x_free: DVector<f64>,
    /// Tau-aux positions per arm, in arm order.
    pub x_tau_per_arm: Vec<f64>,
    /// `l = x_T - x_τ` per arm, after convergence.
    pub l_per_arm: Vec<f64>,
    /// `u = x_τ - x_S` per arm, after convergence.
    pub u_per_arm: Vec<f64>,
    /// Fisher weight `w_ij = n σ(l)(1-σ(l))` per arm at convergence.
    pub w_per_arm: Vec<f64>,
    /// `kEff_per_arm[i] = 1/(1/w_i + τ²_arm)`.
    pub keff_per_arm: Vec<f64>,
    pub iters: usize,
    pub tau2_contrast: f64,
    pub tau2_arm: f64,
}

/// Exact spring-network energy at the current per-arm state. Used by the
/// Armijo line-search guard. The mode minimises this energy.
///
///   E = Σ [n log(1 + exp(l)) - e · l]   (binomial)
///     + Σ u² / (2 τ²_arm)               (study↔τ-aux spring)
fn energy_at(arms: &[ArmRec], state: &[ArmState], tau2arm: f64) -> f64 {
    let mut e = 0.0_f64;
    for (i, arm) in arms.iter().enumerate() {
        let st = &state[i];
        let l = st.x_t - st.x_tau;
        let u = st.x_tau - st.x_s;
        let n_ = arm.n as f64;
        let ev = arm.events as f64;
        e += n_ * log1p_exp(l) - ev * l;
        e += u * u / (2.0 * tau2arm);
    }
    e
}

/// Recover x_τ analytically per arm given x_S, x_T and the linearised Fisher
/// weight w. Same closed form used inside the Newton update.
#[inline]
fn recover_xtau(x_s: f64, x_t: f64, w: f64, y_l: f64, tau2arm: f64) -> f64 {
    (x_s / tau2arm + w * (x_t - y_l)) / (1.0 / tau2arm + w)
}

/// Run reduced-Schur Newton at a fixed τ²_contrast. Returns the converged
/// linearisation. Mirrors `newtonSolveReducedBin` in
/// `src/Data/Meta/RandomEffects.hs:564`, with an Armijo-style line-search
/// guard around the candidate step (matches the safety damping the Haskell
/// solver picked up for rare-events networks where the un-damped Newton
/// could oscillate for thousands of iterations).
pub fn newton_solve_reduced_bin(
    topo: &Topology,
    tau2_contrast: f64,
    max_iter: usize,
    eps: f64,
) -> SolveOut {
    let tau2c = tau2_contrast.max(1e-10);
    let tau2arm = tau2c / 2.0;
    let n_free = topo.n_free;
    let n_arms = topo.arms.len();

    // Initial positions: zero everywhere.
    let mut arm_state: Vec<ArmState> = (0..n_arms)
        .map(|_| ArmState { x_s: 0.0, x_t: 0.0, x_tau: 0.0, l: 0.0 })
        .collect();

    // Per-iteration scratch space.
    let mut a_mat = DMatrix::<f64>::zeros(n_free, n_free);
    let mut b_vec = DVector::<f64>::zeros(n_free);

    // Per-arm fields recomputed each iteration: (w, y_lin, kEff)
    let mut arm_w = vec![0.0_f64; n_arms];
    let mut arm_y = vec![0.0_f64; n_arms];
    let mut arm_keff = vec![0.0_f64; n_arms];

    let mut iters = 0_usize;
    // Track energy across iterations for an additional convergence test.
    // Some rare-events networks oscillate around the mode with `max_dl`
    // never quite hitting `eps`, but the *energy* stalls — which is what we
    // actually need for an accurate Laplace logZ.
    let mut prev_energy: f64 = f64::INFINITY;

    // Maximum number of step halvings inside the line-search before we give
    // up and accept whatever we have.
    const MAX_BACKTRACKS: usize = 6;
    // Apply Armijo-style line-search damping ONLY after the un-damped Newton
    // has failed to converge for `ARMIJO_KICKIN` iterations. This keeps the
    // well-behaved networks (diabetes, the T=30 sweep) bit-identical to the
    // pre-damping baseline while still rescuing the rare-events case where
    // un-damped Newton oscillates for thousands of iterations.
    const ARMIJO_KICKIN: usize = 30;
    // Backdoor switch: GCRUST_NO_ARMIJO=1 disables damping entirely.
    let no_armijo =
        std::env::var("GCRUST_NO_ARMIJO").map(|v| v == "1").unwrap_or(false);
    // GCRUST_FORCE_ARMIJO=1 enables damping from iteration 0 (used to debug
    // that the line search is wired up correctly).
    let force_armijo =
        std::env::var("GCRUST_FORCE_ARMIJO").map(|v| v == "1").unwrap_or(false);

    for it in 0..max_iter {
        iters = it + 1;
        // 1) Per-arm linearisation at current positions.
        for (i, arm) in topo.arms.iter().enumerate() {
            let st = &arm_state[i];
            let l = st.x_t - st.x_tau; // spring length convention
            let p = sigmoid(l);
            let n_ = arm.n as f64;
            let k_min = (1e-6_f64).max(n_ * 1e-10);
            let w = (n_ * p * (1.0 - p)).max(k_min);
            let resid = arm.events as f64 - n_ * p;
            let y_lin = l + clamp(-5.0, 5.0, resid / w);
            let k_eff = 1.0 / (tau2arm + 1.0 / w);
            arm_w[i] = w;
            arm_y[i] = y_lin;
            arm_keff[i] = k_eff;
        }

        // 2) Build reduced linear system A x = b.
        a_mat.fill(0.0);
        b_vec.fill(0.0);
        for (i, arm) in topo.arms.iter().enumerate() {
            let s = arm.s_idx;
            let k_eff = arm_keff[i];
            let y_l = arm_y[i];
            match arm.t_idx {
                None => {
                    // pinned treatment branch
                    a_mat[(s, s)] += k_eff;
                    b_vec[s] += -k_eff * y_l;
                }
                Some(t) => {
                    a_mat[(s, s)] += k_eff;
                    a_mat[(t, t)] += k_eff;
                    a_mat[(s, t)] += -k_eff;
                    a_mat[(t, s)] += -k_eff;
                    b_vec[s] += -k_eff * y_l;
                    b_vec[t] += k_eff * y_l;
                }
            }
        }

        // 3) Solve via LU.
        let lu = a_mat.clone().lu();
        let sol = match lu.solve(&b_vec) {
            Some(x) => x,
            None => panic!("reduced system singular at tau2c={tau2c}"),
        };

        // 4) Armijo-style line-search guard. Try the full Newton step first;
        //    if the energy increases, halve the step size repeatedly until it
        //    decreases (or we exhaust the backtracking budget — at which point
        //    we accept the smallest step we tried). We only compute the
        //    current energy if we plan to actually use it below.
        // Cache the full-step targets (α = 1) for x_S and x_T per arm.
        let mut full_xs = vec![0.0_f64; n_arms];
        let mut full_xt = vec![0.0_f64; n_arms];
        let mut cur_xs = vec![0.0_f64; n_arms];
        let mut cur_xt = vec![0.0_f64; n_arms];
        for (i, arm) in topo.arms.iter().enumerate() {
            full_xs[i] = sol[arm.s_idx];
            full_xt[i] = match arm.t_idx {
                None => 0.0,
                Some(t) => sol[t],
            };
            cur_xs[i] = arm_state[i].x_s;
            cur_xt[i] = arm_state[i].x_t;
        }

        // Decide whether to engage the Armijo line search this iteration.
        // - `no_armijo` forces plain Newton.
        // - `force_armijo` engages damping from the first iteration (debug).
        // - Otherwise we only kick in once `it >= ARMIJO_KICKIN` to keep the
        //   well-behaved cases bit-identical to plain Newton.
        let use_armijo = !no_armijo && (force_armijo || it >= ARMIJO_KICKIN);
        let cur_energy = if use_armijo {
            energy_at(&topo.arms, &arm_state, tau2arm)
        } else {
            0.0
        };

        let mut alpha = 1.0_f64;
        let mut new_state: Vec<ArmState> = Vec::with_capacity(n_arms);
        let max_bt = if use_armijo { MAX_BACKTRACKS } else { 0 };
        for _ in 0..=max_bt {
            new_state.clear();
            for i in 0..n_arms {
                let new_xs = cur_xs[i] + alpha * (full_xs[i] - cur_xs[i]);
                let new_xt = cur_xt[i] + alpha * (full_xt[i] - cur_xt[i]);
                let w = arm_w[i];
                let y_l = arm_y[i];
                let new_xtau = recover_xtau(new_xs, new_xt, w, y_l, tau2arm);
                let new_l = new_xt - new_xtau;
                new_state.push(ArmState {
                    x_s: new_xs,
                    x_t: new_xt,
                    x_tau: new_xtau,
                    l: new_l,
                });
            }
            if !use_armijo {
                break;
            }
            let new_energy = energy_at(&topo.arms, &new_state, tau2arm);
            if new_energy.is_finite() && new_energy <= cur_energy {
                break;
            }
            alpha *= 0.5;
        }

        let mut max_dl = 0.0_f64;
        for i in 0..n_arms {
            let dl = (new_state[i].l - arm_state[i].l).abs();
            if dl > max_dl {
                max_dl = dl;
            }
        }
        arm_state = new_state;

        // We don't track `sol` directly any more — `x_free` is rebuilt from
        // the final arm-state below so it always reflects the position the
        // line-search actually accepted.
        let _ = &sol;
        if max_dl < eps {
            break;
        }
        // Energy-stagnation fallback: if the energy is no longer changing
        // (relative to its magnitude), the Laplace mode has been reached even
        // if the per-arm length-deltas are still rattling around. This
        // matters for rare-events networks where the un-damped oscillation
        // never quite settles below `eps`.
        if it >= ARMIJO_KICKIN {
            let cur_e = energy_at(&topo.arms, &arm_state, tau2arm);
            if (cur_e - prev_energy).abs() / cur_e.abs().max(1.0) < 1e-12 {
                break;
            }
            prev_energy = cur_e;
        }
    }

    // After convergence reflect the actual mode positions (which can lag
    // behind the last `sol` if the last step was damped). Rebuild from the
    // final `arm_state`.
    let mut converged_x = DVector::<f64>::zeros(n_free);
    let mut writes_per_idx = vec![0u32; n_free];
    let mut sums_per_idx = vec![0.0_f64; n_free];
    for (i, arm) in topo.arms.iter().enumerate() {
        let st = &arm_state[i];
        sums_per_idx[arm.s_idx] += st.x_s;
        writes_per_idx[arm.s_idx] += 1;
        if let Some(t) = arm.t_idx {
            sums_per_idx[t] += st.x_t;
            writes_per_idx[t] += 1;
        }
    }
    for k in 0..n_free {
        if writes_per_idx[k] > 0 {
            converged_x[k] = sums_per_idx[k] / writes_per_idx[k] as f64;
        }
    }

    // Final pass: rebuild A at convergence and assemble result.
    let mut a_final = DMatrix::<f64>::zeros(n_free, n_free);
    let mut x_tau_per_arm = vec![0.0_f64; n_arms];
    let mut l_per_arm = vec![0.0_f64; n_arms];
    let mut u_per_arm = vec![0.0_f64; n_arms];
    let mut w_per_arm = vec![0.0_f64; n_arms];
    let mut keff_per_arm = vec![0.0_f64; n_arms];

    for (i, arm) in topo.arms.iter().enumerate() {
        let st = &arm_state[i];
        let l = st.x_t - st.x_tau;
        let p = sigmoid(l);
        let n_ = arm.n as f64;
        let k_min = (1e-6_f64).max(n_ * 1e-10);
        let w = (n_ * p * (1.0 - p)).max(k_min);
        let k_eff = 1.0 / (tau2arm + 1.0 / w);

        x_tau_per_arm[i] = st.x_tau;
        l_per_arm[i] = l;
        u_per_arm[i] = st.x_tau - st.x_s;
        w_per_arm[i] = w;
        keff_per_arm[i] = k_eff;

        let s = arm.s_idx;
        match arm.t_idx {
            None => {
                a_final[(s, s)] += k_eff;
            }
            Some(t) => {
                a_final[(s, s)] += k_eff;
                a_final[(t, t)] += k_eff;
                a_final[(s, t)] += -k_eff;
                a_final[(t, s)] += -k_eff;
            }
        }
    }

    SolveOut {
        a_mat: a_final,
        x_free: converged_x,
        x_tau_per_arm,
        l_per_arm,
        u_per_arm,
        w_per_arm,
        keff_per_arm,
        iters,
        tau2_contrast: tau2c,
        tau2_arm: tau2arm,
    }
}

/// Stable `log(1 + exp(l))`.
#[inline]
fn log1p_exp(l: f64) -> f64 {
    if l >= 0.0 {
        l + (-l).exp().ln_1p()
    } else {
        l.exp().ln_1p()
    }
}

/// Compute log Z(τ²_contrast) and the network treatment-effect estimates and
/// variances at the converged mode. Mirrors `evalLogZFrom` from
/// `src/Data/Meta/RandomEffects.hs:1658`.
#[derive(Debug, Clone)]
pub struct GridPoint {
    pub tau2: f64,
    pub log_z: f64,
    /// Treatment effects vs the pinned treatment (= treatment_order[0]).
    /// `effects[t]` is the effect of `treatment_order[t]` relative to the ref.
    /// `effects[0] = 0` by convention.
    pub effects: Vec<f64>,
    /// Variance of (treatment_order[t] - ref) under Laplace.
    pub eff_vars: Vec<f64>,
    pub iters: usize,
}

pub fn eval_xi_at(topo: &Topology, tau2_contrast: f64) -> GridPoint {
    // 200 iterations is enough for the rare-events networks (rep 4 of the
    // T=50 sweep) once the Armijo guard is in play; well-behaved networks
    // hit `eps` in 5-10 iterations regardless.
    let solve = newton_solve_reduced_bin(topo, tau2_contrast, 200, 1e-8);

    // ----- Laplace energy -----
    let mut e_star = 0.0;
    for (i, arm) in topo.arms.iter().enumerate() {
        let n_ = arm.n as f64;
        let e_ = arm.events as f64;
        let l = solve.l_per_arm[i];
        e_star += n_ * log1p_exp(l) - e_ * l;
    }
    for i in 0..topo.arms.len() {
        let u = solve.u_per_arm[i];
        e_star += u * u / (2.0 * solve.tau2_arm);
    }

    // log|det A| via LU
    let lu = solve.a_mat.clone().lu();
    let det_sign_log = lu_log_det_abs(&lu, topo.n_free);
    let log_det_a = det_sign_log;

    // arm prefactor: -0.5 Σ log(1 + w_ij τ²_arm)
    let mut arm_pref = 0.0_f64;
    for i in 0..topo.arms.len() {
        let v = 1.0 / solve.w_per_arm[i];
        arm_pref += (1.0 + solve.tau2_arm / v).ln();
    }
    arm_pref *= -0.5;

    let log_z = arm_pref - e_star - 0.5 * log_det_a;

    // ----- treatment effects vs the pinned treatment -----
    // effect[t] = position of treatment t (= 0 for pinned, else x_free[idx])
    // var[t] = a_inv[idx, idx]  (Schur identity collapses for the pinned arm)
    let n_studies = topo.n_studies;
    let n_t = topo.n_treats;
    let mut effects = vec![0.0_f64; n_t];
    let mut eff_vars = vec![0.0_f64; n_t];

    // Compute aInv. n_free is small (k+T-1 ~ 27 for diabetes); explicit
    // inverse is fine and matches the Haskell `Mat.inv finalAMat`.
    let a_inv = match solve.a_mat.clone().try_inverse() {
        Some(m) => m,
        None => panic!("A matrix singular at tau2={}", solve.tau2_contrast),
    };

    for (t, _tid) in topo.treatment_order.iter().enumerate() {
        if t == 0 {
            effects[t] = 0.0;
            eff_vars[t] = 0.0;
        } else {
            let idx = n_studies + t - 1;
            effects[t] = solve.x_free[idx];
            eff_vars[t] = a_inv[(idx, idx)];
        }
    }

    GridPoint {
        tau2: solve.tau2_contrast,
        log_z,
        effects,
        eff_vars,
        iters: solve.iters,
    }
}

/// log|det| of an `n x n` matrix from its LU factorisation. Reads the diagonal
/// of U.
fn lu_log_det_abs(lu: &nalgebra::LU<f64, nalgebra::Dyn, nalgebra::Dyn>, n: usize) -> f64 {
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

    fn tiny_dataset() -> Dataset {
        use crate::data::{Mode, Study};
        let mk = |t: &str, e: i64, n: i64| crate::data::Arm {
            treatment: t.into(),
            events: e,
            n,
            mean: 0.0,
            sd: 0.0,
        };
        // 2 treatments A, B. 3 studies, all 2-arm.
        let s1 = Study { study_id: 1, arms: vec![mk("A", 5, 50), mk("B", 12, 50)] };
        let s2 = Study {
            study_id: 2,
            arms: vec![mk("A", 10, 100), mk("B", 20, 100)],
        };
        let s3 = Study { study_id: 3, arms: vec![mk("A", 4, 40), mk("B", 9, 40)] };
        Dataset {
            studies: vec![s1, s2, s3],
            treatments: vec!["A".into(), "B".into()],
            mode: Mode::Binary,
        }
    }

    #[test]
    fn newton_converges_quickly() {
        let ds = tiny_dataset();
        let topo = Topology::build(&ds);
        let s = newton_solve_reduced_bin(&topo, 0.01, 50, 1e-8);
        assert!(s.iters <= 20, "did not converge: {} iters", s.iters);
    }

    #[test]
    fn log_z_finite_at_small_tau2() {
        let ds = tiny_dataset();
        let topo = Topology::build(&ds);
        let g = eval_xi_at(&topo, 0.01);
        assert!(g.log_z.is_finite());
    }
}
