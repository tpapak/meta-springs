//! Preconditioned Conjugate Gradient solver for `(L + εI) x = b`.
//!
//! Vector ops parallelise via rayon for `n ≥ PAR_THRESHOLD`; the SpMV
//! parallelism lives inside [`super::csr::CsrLap::apply`].

use rayon::prelude::*;

use super::csr::CsrLap;
use super::precond::Preconditioner;

const PAR_THRESHOLD: usize = 200_000;

#[derive(Debug, Clone)]
pub struct PcgResult {
    pub x: Vec<f64>,
    pub iters: usize,
    pub residual: f64,
}

#[inline]
fn dot(a: &[f64], b: &[f64]) -> f64 {
    if a.len() < PAR_THRESHOLD {
        a.iter().zip(b.iter()).map(|(x, y)| x * y).sum()
    } else {
        a.par_iter().zip(b.par_iter()).map(|(x, y)| x * y).sum()
    }
}

#[inline]
fn norm_sq(a: &[f64]) -> f64 {
    if a.len() < PAR_THRESHOLD {
        a.iter().map(|v| v * v).sum()
    } else {
        a.par_iter().map(|v| v * v).sum()
    }
}

/// `x ← x + α p; r ← r − α Ap` fused.
#[inline]
fn step_xr(x: &mut [f64], r: &mut [f64], p: &[f64], ap: &[f64], alpha: f64) {
    if x.len() < PAR_THRESHOLD {
        for i in 0..x.len() {
            x[i] += alpha * p[i];
            r[i] -= alpha * ap[i];
        }
    } else {
        x.par_iter_mut()
            .zip(r.par_iter_mut())
            .zip(p.par_iter().zip(ap.par_iter()))
            .for_each(|((xi, ri), (pi, api))| {
                *xi += alpha * pi;
                *ri -= alpha * api;
            });
    }
}

/// `p ← z + β p`
#[inline]
fn step_p(p: &mut [f64], z: &[f64], beta: f64) {
    if p.len() < PAR_THRESHOLD {
        for i in 0..p.len() {
            p[i] = z[i] + beta * p[i];
        }
    } else {
        p.par_iter_mut().zip(z.par_iter()).for_each(|(pi, zi)| {
            *pi = *zi + beta * *pi;
        });
    }
}

pub fn pcg_solve<P: Preconditioner>(
    a: &CsrLap,
    eps: f64,
    b: &[f64],
    precond: &P,
    tol: f64,
    max_iter: usize,
) -> PcgResult {
    let n = a.n();
    let mut x = vec![0.0_f64; n];
    let mut r = b.to_vec();
    let mut z = vec![0.0_f64; n];
    precond.apply(&r, &mut z);
    let mut p = z.clone();
    let mut ap = vec![0.0_f64; n];
    let mut rz_old: f64 = dot(&r, &z);
    let b_norm = norm_sq(b).sqrt().max(1.0);

    for it in 0..max_iter {
        a.apply(eps, &p, &mut ap);
        let p_ap = dot(&p, &ap);
        if p_ap.abs() < 1e-30 {
            let rel = norm_sq(&r).sqrt() / b_norm;
            return PcgResult { x, iters: it, residual: rel };
        }
        let alpha = rz_old / p_ap;
        step_xr(&mut x, &mut r, &p, &ap, alpha);
        let r_norm = norm_sq(&r).sqrt();
        let rel = r_norm / b_norm;
        if rel < tol {
            return PcgResult { x, iters: it + 1, residual: rel };
        }
        precond.apply(&r, &mut z);
        let rz_new = dot(&r, &z);
        let beta = rz_new / rz_old;
        step_p(&mut p, &z, beta);
        rz_old = rz_new;
    }
    let rel = norm_sq(&r).sqrt() / b_norm;
    PcgResult { x, iters: max_iter, residual: rel }
}
