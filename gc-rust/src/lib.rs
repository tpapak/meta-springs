//! Rust port of the Grand Canonical NMA pipeline.
//!
//! Mirrors the Haskell reference in `src/Data/Meta/RandomEffects.hs`:
//!   - newtonSolveReducedBin (lines 564-815)        -> [`solver::newton_solve_reduced_bin`]
//!   - springGrandCanonicalBinAdaptive (1591)        -> [`gc::run_adaptive`]
//!   - evalXiBinAt                    (1612)         -> [`solver::eval_xi_at`]
//!   - springGrandCanonicalAdaptive   (1308-1465)    -> [`gc::run_adaptive_cont`]
//!     (Gaussian / mean-difference path; one LU solve per τ², no IRLS).

pub mod data;
pub mod gc;
pub mod output;
pub mod solver;
pub mod solver_cont;
