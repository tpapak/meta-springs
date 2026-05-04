//! Preconditioners for the sparse Laplacian + εI system.
//!
//! All preconditioners are SPD; `apply` computes `z = M⁻¹ r` for the
//! preconditioner's effective `M`. Three are provided:
//!
//! - [`Jacobi`] — diagonal scaling `M = diag(L + εI)`. Constant per
//!   iteration, weak on heterogeneous-weight graphs.
//! - [`Ruiz`] — iterative two-sided row/column equilibration (Ruiz
//!   2001). `M⁻¹ = D²` where `D` is the symmetric Ruiz scaling. Strong
//!   on heterogeneous weights at low setup cost.
//! - [`Ic0`] — incomplete Cholesky with zero fill. Strongest of the
//!   three; ~2× per-iteration cost, big setup cost (one symbolic +
//!   numeric factorisation).
//!
//! All pair with [`super::pcg::pcg_solve`].
use super::csr::CsrLap;

pub trait Preconditioner {
    fn apply(&self, r: &[f64], z: &mut [f64]);
}

// ─────────────────────────────────────────────────────────────────────
// Jacobi
// ─────────────────────────────────────────────────────────────────────

pub struct Jacobi {
    m_inv: Vec<f64>,
}

impl Jacobi {
    pub fn new(a: &CsrLap, eps: f64) -> Self {
        let diag = a.diag(eps);
        let m_inv = diag
            .iter()
            .map(|d| if *d > 0.0 { 1.0 / d } else { 0.0 })
            .collect();
        Self { m_inv }
    }
}

impl Preconditioner for Jacobi {
    fn apply(&self, r: &[f64], z: &mut [f64]) {
        for i in 0..r.len() {
            z[i] = r[i] * self.m_inv[i];
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Ruiz iterative scaling
// ─────────────────────────────────────────────────────────────────────

/// Iterative symmetric row/column equilibration (Ruiz, INRIA 2001 /
/// Knight & Ruiz 2013). For each iteration `k`:
///
///   r_i = max_j |D_i² · A_ij|        // current row max abs
///   D_i ← D_i / sqrt(r_i)
///
/// converging to a state where every scaled row has unit max-abs entry.
/// Effective preconditioner `M⁻¹ = D²`. On Laplacians where one Jacobi
/// step already balances the diagonal, subsequent iterations equilibrate
/// off-diagonal magnitudes — exactly the regime where heterogeneous-
/// weight Boykov-Kolmogorov segmentation graphs hurt Jacobi.
pub struct Ruiz {
    pub d_sq: Vec<f64>,
    pub iters_run: usize,
}

impl Ruiz {
    pub fn new(a: &CsrLap, eps: f64, max_iter: usize, tol: f64) -> Self {
        let n = a.n();
        let mut d = vec![1.0_f64; n];
        let row_ptr = &a.row_ptr;
        let nbr_v = &a.nbr_v;
        let nbr_w = &a.nbr_w;
        let mut iters_run = 0;
        for k in 0..max_iter {
            // Row max abs of D A D for each row i.
            // Diagonal entry (i,i): D_i² · (eps + Σ w_ij).
            // Off-diagonal (i,j): D_i · D_j · w_ij.
            let mut r_min = f64::INFINITY;
            let mut r_max = 0.0_f64;
            let mut r = vec![0.0_f64; n];
            for i in 0..n {
                let di = d[i];
                let mut row_sum_w = 0.0_f64;
                let mut max_off = 0.0_f64;
                for kk in row_ptr[i]..row_ptr[i + 1] {
                    let v = nbr_v[kk] as usize;
                    let w = nbr_w[kk] as f64;
                    row_sum_w += w;
                    let off = di * d[v] * w;
                    if off > max_off {
                        max_off = off;
                    }
                }
                let diag_entry = di * di * (eps + row_sum_w);
                let r_i = diag_entry.max(max_off);
                if r_i.is_finite() && r_i > 0.0 {
                    r[i] = r_i;
                    if r_i < r_min {
                        r_min = r_i;
                    }
                    if r_i > r_max {
                        r_max = r_i;
                    }
                } else {
                    r[i] = 1.0;
                }
            }
            iters_run = k + 1;
            // Convergence: spread of row max in log space.
            if r_max > 0.0 && r_min > 0.0 && (r_max / r_min).ln() < tol {
                break;
            }
            for i in 0..n {
                d[i] /= r[i].sqrt();
            }
        }
        let d_sq: Vec<f64> = d.iter().map(|x| x * x).collect();
        Self { d_sq, iters_run }
    }
}

impl Preconditioner for Ruiz {
    fn apply(&self, r: &[f64], z: &mut [f64]) {
        for i in 0..r.len() {
            z[i] = r[i] * self.d_sq[i];
        }
    }
}

// ─────────────────────────────────────────────────────────────────────
// Incomplete Cholesky IC(0)
// ─────────────────────────────────────────────────────────────────────

/// IC(0): incomplete Cholesky with no fill-in. The factor `L` has the
/// same nonzero pattern as the lower-triangular part of `A = L_full +
/// εI`. We store it in CSR with a separate `diag` for `L_ii`.
///
/// For SPD M-matrices like Laplacians, IC(0) typically converges in the
/// √n regime times a constant factor << 1, since it captures more than
/// just the diagonal preconditioning.
pub struct Ic0 {
    n: usize,
    /// Lower-triangular CSR of `L`. Off-diagonals only — diagonal stored
    /// separately. `lo_v[lo_ptr[i]..lo_ptr[i+1]]` are columns `j < i` for
    /// row `i` of `L`.
    lo_ptr: Vec<usize>,
    lo_v: Vec<u32>,
    lo_x: Vec<f64>,
    diag: Vec<f64>,
    /// Scratch vector reused across applies.
    /// Wrapped in interior mutability so trait `apply(&self, ...)` works.
    scratch: std::cell::RefCell<Vec<f64>>,
}

impl Ic0 {
    pub fn new(a: &CsrLap, eps: f64) -> Self {
        let n = a.n();
        // Build lower-triangular pattern from CsrLap (upper neighbours
        // skipped). Sort within each row — required for IC(0) recurrence.
        let mut lo_ptr = vec![0_usize; n + 1];
        let mut lo_pairs: Vec<Vec<(u32, f32)>> = vec![Vec::new(); n];
        for i in 0..n {
            for k in a.row_ptr[i]..a.row_ptr[i + 1] {
                let j = a.nbr_v[k] as usize;
                if j < i {
                    lo_pairs[i].push((j as u32, a.nbr_w[k]));
                }
            }
            lo_pairs[i].sort_unstable_by_key(|&(j, _)| j);
            lo_ptr[i + 1] = lo_ptr[i] + lo_pairs[i].len();
        }
        let total = lo_ptr[n];
        let mut lo_v = Vec::with_capacity(total);
        let mut lo_x = vec![0.0_f64; total];
        for row in &lo_pairs {
            for &(j, _) in row {
                lo_v.push(j);
            }
        }
        // Initialise off-diagonal `L_ij = -w_ij` (Laplacian off-diag
        // negative) and diagonal `D_ii = Σ w_ij + ε`.
        let mut diag = a.diag(eps);
        let mut cursor = lo_ptr.clone();
        for (i, row) in lo_pairs.iter().enumerate() {
            for &(_, w) in row {
                let pos = cursor[i];
                lo_x[pos] = -(w as f64);
                cursor[i] = pos + 1;
            }
        }

        // IC(0) factorisation. For each row i in order:
        //   L_ij' = (A_ij - Σ_{k<j, k∈pat(i)∩pat(j)} L_ik L_jk) / L_jj
        //   L_ii  = sqrt(A_ii - Σ_{k<i, k∈pat(i)} L_ik²)
        // Lookup is O(deg_i + deg_j) per pair using a sparse-vector merge.
        //
        // We need an inverted lookup: for each (i, j) pair in lo, what's
        // L_ij? Maintain a "current row scratch" that gets indexed by j.
        let mut row_dense = vec![0.0_f64; n];
        for i in 0..n {
            // Gather lo row i into dense.
            for k in lo_ptr[i]..lo_ptr[i + 1] {
                row_dense[lo_v[k] as usize] = lo_x[k];
            }
            // Process columns j < i in increasing order.
            for k in lo_ptr[i]..lo_ptr[i + 1] {
                let j = lo_v[k] as usize;
                // Subtract Σ_{m<j, m∈pat(i)∩pat(j)} L_im L_jm.
                let mut s = 0.0_f64;
                for kk in lo_ptr[j]..lo_ptr[j + 1] {
                    let m = lo_v[kk] as usize;
                    if m >= j {
                        break;
                    }
                    let lim = row_dense[m];
                    if lim != 0.0 {
                        s += lim * lo_x[kk];
                    }
                }
                let val = (row_dense[j] - s) / diag[j];
                lo_x[k] = val;
                row_dense[j] = val;
            }
            // Diagonal: A_ii - Σ L_ik²
            let mut s = 0.0_f64;
            for k in lo_ptr[i]..lo_ptr[i + 1] {
                s += lo_x[k] * lo_x[k];
            }
            let dval = diag[i] - s;
            // Numerical safety: if non-positive (factorisation breakdown),
            // fall back to the original diagonal so apply doesn't NaN.
            diag[i] = if dval > 1e-30 { dval.sqrt() } else { diag[i].sqrt().max(1e-15) };
            // Scatter row back to dense → restore zero state for next iter.
            for k in lo_ptr[i]..lo_ptr[i + 1] {
                row_dense[lo_v[k] as usize] = 0.0;
            }
            row_dense[i] = 0.0;
        }

        Self {
            n,
            lo_ptr,
            lo_v,
            lo_x,
            diag,
            scratch: std::cell::RefCell::new(vec![0.0_f64; n]),
        }
    }
}

impl Preconditioner for Ic0 {
    fn apply(&self, r: &[f64], z: &mut [f64]) {
        // Solve L L^T z = r via forward + back substitution.
        let n = self.n;
        let mut tmp = self.scratch.borrow_mut();
        // Forward: L y = r.
        for i in 0..n {
            let mut s = r[i];
            for k in self.lo_ptr[i]..self.lo_ptr[i + 1] {
                s -= self.lo_x[k] * tmp[self.lo_v[k] as usize];
            }
            tmp[i] = s / self.diag[i];
        }
        // Back: L^T z = y. Iterate rows i = n−1..0; for each i, z_i =
        // (y_i - Σ_{k>i, j s.t. (k,i)∈lo} L_ki z_k) / L_ii. We reuse
        // CSR-by-row of L (lower-tri) but need column access. Trick:
        // walk rows in reverse and subtract our own contributions to
        // *earlier* indices on the way down.
        for i in (0..n).rev() {
            let zi = tmp[i] / self.diag[i];
            z[i] = zi;
            // Subtract zi · L_ij from y_j for each j < i in row i's
            // pattern. (i.e. propagate the back-substitution.)
            for k in self.lo_ptr[i]..self.lo_ptr[i + 1] {
                let j = self.lo_v[k] as usize;
                tmp[j] -= self.lo_x[k] * zi;
            }
        }
        // Reset scratch for next apply.
        for v in tmp.iter_mut() {
            *v = 0.0;
        }
    }
}
