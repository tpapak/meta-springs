//! Sparse Laplacian solver for big graphs.
//!
//! Three pieces, each in its own file:
//!
//! - [`csr`] — symmetric CSR adjacency for the weighted Laplacian
//!   `L = D − W` with optional `εI` regularisation.
//! - [`precond`] — preconditioners that pair with the CSR Laplacian:
//!   Jacobi (diagonal), Ruiz (iterative row-equilibration), IC0
//!   (incomplete Cholesky with zero fill).
//! - [`pcg`] — preconditioned conjugate gradient driver, generic over
//!   the preconditioner.

pub mod amg;
pub mod csr;
pub mod pcg;
pub mod precond;

pub use amg::TwoLevelAmg;
pub use csr::CsrLap;
pub use pcg::{pcg_solve, PcgResult};
pub use precond::{Ic0, Jacobi, Preconditioner, Ruiz};
