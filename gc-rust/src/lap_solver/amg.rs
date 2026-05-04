//! Two-level smoothed-aggregation AMG preconditioner for graph Laplacians.
//!
//! Realistic-scope "Koutis-style" / lean AMG:
//!
//!   1. **Coarsen** by greedy heavy-edge matching. Pair unmatched
//!      vertices via the heaviest unmatched edge; leftovers become
//!      singletons. Aggregates ↔ coarse vertices.
//!   2. **Galerkin** coarse Laplacian `L_c = P^T L P` where `P` is the
//!      0/1 aggregation indicator. For Laplacians this preserves the
//!      symmetric PSD structure. Implemented directly as an edge-merge
//!      on canonical undirected pairs.
//!   3. **V-cycle** as `apply`: pre-smooth `n_smooth` Jacobi sweeps,
//!      restrict residual, coarse solve via Jacobi-PCG, prolong, post-
//!      smooth.
//!
//! Real Koutis CMG uses low-stretch trees, multi-level recursion, and
//! W-cycles. This is the simplest two-level approximation that has a
//! shot at breaking the PCG-conditioning blowup on heterogeneous /
//! deep-cut residual graphs.

use std::cell::RefCell;

use super::csr::CsrLap;
use super::pcg::pcg_solve;
use super::precond::{Jacobi, Preconditioner};

pub struct TwoLevelAmg {
    fine: CsrLap,
    fine_diag_inv: Vec<f64>,
    coarse: CsrLap,
    /// `agg[i]` is the coarse vertex that fine vertex `i` belongs to.
    agg: Vec<usize>,
    n_coarse: usize,
    n_smooth: usize,
    coarse_max_iter: usize,
    coarse_tol: f64,
    eps: f64,
    /// Reusable scratch buffers (interior-mutable so `apply(&self, ...)`
    /// doesn't need a `&mut self`).
    scratch: RefCell<Scratch>,
}

struct Scratch {
    lx: Vec<f64>,
    rhat: Vec<f64>,
    r_c: Vec<f64>,
}

/// Greedy heavy-edge matching. Visit vertices in order; for each
/// unmatched vertex, find its unmatched neighbour connected by the
/// heaviest edge and pair them. Unmatched vertices at the end become
/// singletons. Returns `(agg, n_coarse)`.
fn heavy_edge_aggregation(a: &CsrLap) -> (Vec<usize>, usize) {
    let n = a.n();
    let mut agg = vec![usize::MAX; n];
    let mut next_id = 0_usize;
    for u in 0..n {
        if agg[u] != usize::MAX {
            continue;
        }
        let mut best_v: Option<usize> = None;
        let mut best_w: f32 = -1.0;
        for k in a.row_ptr[u]..a.row_ptr[u + 1] {
            let v = a.nbr_v[k] as usize;
            if agg[v] != usize::MAX {
                continue;
            }
            let w = a.nbr_w[k];
            if w > best_w {
                best_w = w;
                best_v = Some(v);
            }
        }
        agg[u] = next_id;
        if let Some(v) = best_v {
            agg[v] = next_id;
        }
        next_id += 1;
    }
    (agg, next_id)
}

/// Build coarse Laplacian via Galerkin `P^T L P`. For an aggregation
/// indicator `P`, this contracts each original edge `(u, v)` into the
/// edge between aggregates `(agg[u], agg[v])`, dropping intra-aggregate
/// edges (they become coarse self-loops, which we don't materialise).
fn galerkin_coarse(fine: &CsrLap, agg: &[usize], n_coarse: usize) -> CsrLap {
    use std::collections::HashMap;
    let mut acc: HashMap<(u32, u32), f64> = HashMap::new();
    let n = fine.n();
    for u in 0..n {
        for k in fine.row_ptr[u]..fine.row_ptr[u + 1] {
            let v = fine.nbr_v[k] as usize;
            // Each undirected fine edge appears twice in CSR; only count once.
            if u >= v {
                continue;
            }
            let au = agg[u];
            let av = agg[v];
            if au == av {
                continue;
            }
            let (a, b) = if au < av {
                (au as u32, av as u32)
            } else {
                (av as u32, au as u32)
            };
            *acc.entry((a, b)).or_insert(0.0) += fine.nbr_w[k] as f64;
        }
    }
    let canonical: Vec<(u32, u32, f64)> =
        acc.into_iter().map(|((a, b), w)| (a, b, w)).collect();
    CsrLap::from_canonical_weights(&canonical, n_coarse)
}

impl TwoLevelAmg {
    pub fn new(fine: &CsrLap, eps: f64, n_smooth: usize) -> Self {
        let (agg, n_coarse) = heavy_edge_aggregation(fine);
        let coarse = galerkin_coarse(fine, &agg, n_coarse);
        let fine_diag = fine.diag(eps);
        let fine_diag_inv: Vec<f64> = fine_diag
            .iter()
            .map(|d| if *d > 0.0 { 1.0 / d } else { 0.0 })
            .collect();
        let n = fine.n();
        Self {
            fine: fine.clone(),
            fine_diag_inv,
            coarse,
            agg,
            n_coarse,
            n_smooth,
            coarse_max_iter: 200,
            coarse_tol: 1e-8,
            eps,
            scratch: RefCell::new(Scratch {
                lx: vec![0.0; n],
                rhat: vec![0.0; n],
                r_c: vec![0.0; n_coarse],
            }),
        }
    }

    /// Damped Jacobi sweeps: `x ← x + ω D⁻¹ (b − L x)` with `ω = 2/3`,
    /// the standard MG-smoothing damping for Jacobi.
    fn smooth(&self, b: &[f64], x: &mut [f64], scratch_lx: &mut [f64]) {
        const OMEGA: f64 = 2.0 / 3.0;
        for _ in 0..self.n_smooth {
            self.fine.apply(self.eps, x, scratch_lx);
            for i in 0..x.len() {
                x[i] += OMEGA * self.fine_diag_inv[i] * (b[i] - scratch_lx[i]);
            }
        }
    }
}

impl Preconditioner for TwoLevelAmg {
    fn apply(&self, r: &[f64], z: &mut [f64]) {
        let n = r.len();
        for v in z.iter_mut() {
            *v = 0.0;
        }
        // Move scratch buffers out so we can hold independent &mut refs.
        let (mut lx, mut rhat, mut r_c) = {
            let mut sc = self.scratch.borrow_mut();
            (
                std::mem::take(&mut sc.lx),
                std::mem::take(&mut sc.rhat),
                std::mem::take(&mut sc.r_c),
            )
        };

        // Pre-smooth on z = 0 with rhs r.
        self.smooth(r, z, &mut lx);

        // Compute residual rhat = r − L z at fine level.
        self.fine.apply(self.eps, z, &mut lx);
        for i in 0..n {
            rhat[i] = r[i] - lx[i];
        }

        // Restrict: r_c[a] = sum over fine i with agg[i] = a of rhat[i].
        for v in r_c.iter_mut() {
            *v = 0.0;
        }
        for i in 0..n {
            r_c[self.agg[i]] += rhat[i];
        }

        // Coarse solve via Jacobi-PCG on (L_c + εI).
        let coarse_prec = Jacobi::new(&self.coarse, self.eps);
        let coarse_res = pcg_solve(
            &self.coarse,
            self.eps,
            &r_c,
            &coarse_prec,
            self.coarse_tol,
            self.coarse_max_iter,
        );

        // Prolong + correct: z[i] += x_c[agg[i]].
        for i in 0..n {
            z[i] += coarse_res.x[self.agg[i]];
        }

        // Post-smooth.
        self.smooth(r, z, &mut lx);

        // Return scratch buffers.
        let mut sc = self.scratch.borrow_mut();
        sc.lx = lx;
        sc.rhat = rhat;
        sc.r_c = r_c;
    }
}
