//! IPD + AgD integration: multilevel network meta-regression (ML-NMR) as a
//! spring network.
//!
//! Every **unit** of evidence is a spring acting on the parameter vector θ:
//!
//! * an **IPD patient** is one Bernoulli spring, linear predictor η = zᵀθ
//!   (study intercept, treatment effect, covariates, interactions);
//! * an **AgD arm** is ONE spring whose force law is the population AVERAGE of
//!   the individual force laws: p̄(θ) = (1/Q) Σ_q g⁻¹(z_qᵀθ) over Q quadrature
//!   (integration) points drawn from the arm's reported covariate distribution.
//!   Under a non-linear link p̄ ≠ g⁻¹(z̄ᵀθ) — that gap is the aggregation bias
//!   ML-NMR corrects, here evaluated deterministically.
//!
//! The equilibrium (net force = score = 0) is the posterior mode, found by
//! damped Fisher scoring (relaxation). The curvature of the energy well (the
//! OBSERVED information) gives the Laplace covariance. Random effects δ are
//! extra springs of stiffness τ⁻²K (K = inverse of the multi-arm correlation
//! ½(I+11ᵀ)); the grand-canonical sum over a τ² grid of the partition function
//!
//!   log Z(τ²) = L(θ̂_τ) − (m/2) log τ² − ½ log det H(τ²)        (Laplace)
//!
//! integrates heterogeneity without sampling, and any τ prior is a reweighting
//! of the same grid.
//!
//! The likelihood for an AgD arm (r events of n) is either
//! * `Binomial`:  r ~ Bin(n, p̄)                       (multinma "binomial"), or
//! * `Binomial2`: the two-parameter (mean- and variance-matched) binomial
//!   approximation to the Poisson-binomial sum, r ~ Bin(n*, p*) with
//!   p* = mean(p²)/p̄ and n* = n p̄²/mean(p²)        (multinma "binomial2").
//!
//! IPD units (q = 1, n = 1) are exact Bernoulli under either choice.

use nalgebra::{Cholesky, DMatrix, DVector};
use rayon::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Link {
    Logit,
    Probit,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum AgdLik {
    Binomial,
    Binomial2,
}

/// One spring: `q` design rows (row-major, `q × p`), r events out of n
/// (binomial), or a Gaussian spring (`g`).
#[derive(Debug, Clone)]
pub struct Unit {
    pub r: f64,
    pub n: f64,
    pub q: usize,
    pub x: Vec<f64>,
    /// Gaussian (identity-link) spring; `None` = binomial unit.
    pub g: Option<Box<Gauss>>,
}

/// Gaussian springs: linear force laws (Hooke). `sig` is the column holding
/// log σ (residual SD of that study's patients).
#[derive(Debug, Clone)]
pub enum Gauss {
    /// one patient: y ~ N(xᵀθ, σ²)
    Ipd { y: f64, sig: usize },
    /// one AgD arm: ȳ ~ N(mean_q x_qᵀθ, se²). With a linear force law the
    /// averaged spring IS the spring at the mean covariates: no aggregation bias.
    Arm { y: f64, se: f64 },
    /// a site's patients replaced by its equivalent spring (sufficient
    /// statistics) — exactly the same energy: ½σ⁻²(yᵀy − 2θᵀXᵀy + θᵀXᵀXθ) + n log σ
    Suff { xtx: Vec<f64>, xty: Vec<f64>, yty: f64, n: f64, sig: usize },
}

impl Unit {
    pub fn binom(r: f64, n: f64, q: usize, x: Vec<f64>) -> Self {
        Unit { r, n, q, x, g: None }
    }
}

const HALF_LN_2PI: f64 = 0.918_938_533_204_672_7;

/// Gaussian unit: (ll, grad, h_obs, h_fis) added in place.
fn gauss_accumulate(p: usize, u: &Unit, gs: &Gauss, theta: &[f64], acc: Option<&mut Accum>) -> f64 {
    match gs {
        Gauss::Ipd { y, sig } => {
            let z = &u.x[..p];
            let s = theta[*sig];
            let e = (-2.0 * s).exp();
            let r = y - dot(z, theta);
            let ll = -0.5 * r * r * e - s - HALF_LN_2PI;
            if let Some(acc) = acc {
                acc.ll += ll;
                for i in 0..p { acc.grad[i] += r * e * z[i]; }
                acc.grad[*sig] += r * r * e - 1.0;
                add_outer(&mut acc.h_obs, e, z, z);
                add_outer(&mut acc.h_fis, e, z, z);
                for i in 0..p {
                    acc.h_obs[(i, *sig)] += 2.0 * r * e * z[i];
                    acc.h_obs[(*sig, i)] += 2.0 * r * e * z[i];
                }
                acc.h_obs[(*sig, *sig)] += 2.0 * r * r * e;
                acc.h_fis[(*sig, *sig)] += 2.0;
            }
            ll
        }
        Gauss::Arm { y, se } => {
            let qf = u.q as f64;
            let mut zb = vec![0.0; p];
            for k in 0..u.q { for i in 0..p { zb[i] += u.x[k * p + i] / qf; } }
            let w = 1.0 / (se * se);
            let r = y - dot(&zb, theta);
            let ll = -0.5 * r * r * w - se.ln() - HALF_LN_2PI;
            if let Some(acc) = acc {
                acc.ll += ll;
                for i in 0..p { acc.grad[i] += r * w * zb[i]; }
                add_outer(&mut acc.h_obs, w, &zb, &zb);
                add_outer(&mut acc.h_fis, w, &zb, &zb);
            }
            ll
        }
        Gauss::Suff { xtx, xty, yty, n, sig } => {
            let s = theta[*sig];
            let e = (-2.0 * s).exp();
            // RSS(θ) = yᵀy − 2θᵀXᵀy + θᵀXᵀXθ ; ∇RSS = 2(XᵀXθ − Xᵀy)
            let mut xb = vec![0.0; p];
            for i in 0..p { for j in 0..p { xb[i] += xtx[i * p + j] * theta[j]; } }
            let rss = yty - 2.0 * dot(theta, xty) + dot(theta, &xb);
            let ll = -0.5 * rss * e - n * s - n * HALF_LN_2PI;
            if let Some(acc) = acc {
                acc.ll += ll;
                for i in 0..p {
                    let gi = xty[i] - xb[i];               // −½∇RSS
                    acc.grad[i] += e * gi;
                    acc.h_obs[(i, *sig)] += 2.0 * e * gi;
                    acc.h_obs[(*sig, i)] += 2.0 * e * gi;
                    for j in 0..p {
                        acc.h_obs[(i, j)] += e * xtx[i * p + j];
                        acc.h_fis[(i, j)] += e * xtx[i * p + j];
                    }
                }
                acc.grad[*sig] += rss * e - n;
                acc.h_obs[(*sig, *sig)] += 2.0 * rss * e;
                acc.h_fis[(*sig, *sig)] += 2.0 * n;
            }
            ll
        }
    }
}

#[derive(Debug, Clone)]
pub struct Problem {
    pub p: usize,
    pub link: Link,
    pub agd_lik: AgdLik,
    pub units: Vec<Unit>,
    /// Prior spring per parameter. Ignored for RE columns (those get τ⁻²K).
    pub priors: Vec<ParPrior>,
    /// Random-effect groups: column indices of the non-baseline arms of one study.
    pub re_groups: Vec<Vec<usize>>,
}

/// A prior on one fixed parameter, i.e. one more spring tied to `loc`.
///
/// Normal: a Hooke spring (stiffness 1/scale²). Student-t (Cauchy = df 1): a
/// spring that softens with stretch, so a far-away likelihood can overrule it.
/// Densities are unnormalised (constants cancel in every ratio we form).
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ParPrior {
    Flat,
    Normal { loc: f64, scale: f64 },
    StudentT { df: f64, loc: f64, scale: f64 },
    /// half-normal(scale) on σ, for a parameter s = log σ (Jacobian included)
    HalfNormalLog { scale: f64 },
}

impl ParPrior {
    /// N(0, 1/prec); prec = 0 ⇒ flat.
    pub fn from_prec(prec: f64) -> Self {
        if prec > 0.0 { ParPrior::Normal { loc: 0.0, scale: prec.powf(-0.5) } } else { ParPrior::Flat }
    }
    /// (log density, d/dθ, −d²/dθ² observed, expected curvature ≥ 0)
    pub fn terms(&self, x: f64) -> (f64, f64, f64, f64) {
        match *self {
            ParPrior::Flat => (0.0, 0.0, 0.0, 0.0),
            ParPrior::Normal { loc, scale } => {
                let k = 1.0 / (scale * scale);
                let r = x - loc;
                (-0.5 * k * r * r, -k * r, k, k)
            }
            ParPrior::HalfNormalLog { scale } => {
                let v = (2.0 * x).exp() / (scale * scale);
                (-0.5 * v + x, -v + 1.0, 2.0 * v, 2.0 * v)
            }
            ParPrior::StudentT { df, loc, scale } => {
                let r = x - loc;
                let a = df * scale * scale;
                let d = a + r * r;
                (-0.5 * (df + 1.0) * (d / a).ln(), -(df + 1.0) * r / d,
                 (df + 1.0) * (a - r * r) / (d * d), (df + 1.0) / ((df + 3.0) * scale * scale))
            }
        }
    }
}

impl Problem {
    pub fn n_re(&self) -> usize {
        self.re_groups.iter().map(|g| g.len()).sum()
    }
    pub fn is_re_col(&self) -> Vec<bool> {
        let mut v = vec![false; self.p];
        for g in &self.re_groups {
            for &c in g {
                v[c] = true;
            }
        }
        v
    }
}

// ---------------------------------------------------------------------------
// special functions
// ---------------------------------------------------------------------------

const INV_SQRT_2PI: f64 = 0.398_942_280_401_432_7;

/// (p, 1−p, dp/dη, d²p/dη²), with 1−p computed without cancellation.
#[inline]
pub fn inv_link(link: Link, eta: f64) -> (f64, f64, f64, f64) {
    match link {
        Link::Logit => {
            let (p, s) = if eta >= 0.0 {
                let e = (-eta).exp();
                (1.0 / (1.0 + e), e / (1.0 + e))
            } else {
                let e = eta.exp();
                (e / (1.0 + e), 1.0 / (1.0 + e))
            };
            let d1 = p * s;
            (p, s, d1, d1 * (s - p))
        }
        Link::Probit => {
            let p = 0.5 * libm::erfc(-eta / std::f64::consts::SQRT_2);
            let s = 0.5 * libm::erfc(eta / std::f64::consts::SQRT_2);
            let phi = INV_SQRT_2PI * (-0.5 * eta * eta).exp();
            (p, s, phi, -eta * phi)
        }
    }
}

pub fn digamma(mut x: f64) -> f64 {
    let mut r = 0.0;
    while x < 10.0 {
        r -= 1.0 / x;
        x += 1.0;
    }
    let f = 1.0 / (x * x);
    r + x.ln() - 0.5 / x
        - f * (1.0 / 12.0 - f * (1.0 / 120.0 - f * (1.0 / 252.0 - f * (1.0 / 240.0 - f / 132.0))))
}

pub fn trigamma(mut x: f64) -> f64 {
    let mut r = 0.0;
    while x < 10.0 {
        r += 1.0 / (x * x);
        x += 1.0;
    }
    let f = 1.0 / (x * x);
    r + 1.0 / x + f / 2.0
        + f / x * (1.0 / 6.0 - f * (1.0 / 30.0 - f * (1.0 / 42.0 - f * (1.0 / 30.0 - f * 5.0 / 66.0))))
}

fn lgamma(x: f64) -> f64 {
    libm::lgamma(x)
}

// ---------------------------------------------------------------------------
// one unit: log-likelihood, score, observed and expected information
// ---------------------------------------------------------------------------

/// Accumulators for the whole network.
pub struct Accum {
    pub ll: f64,
    pub grad: DVector<f64>,
    /// observed information −∇²ℓ
    pub h_obs: DMatrix<f64>,
    /// expected (Fisher) information, PSD — used for the relaxation steps
    pub h_fis: DMatrix<f64>,
}

impl Accum {
    pub fn new(p: usize) -> Self {
        Accum {
            ll: 0.0,
            grad: DVector::zeros(p),
            h_obs: DMatrix::zeros(p, p),
            h_fis: DMatrix::zeros(p, p),
        }
    }
}

#[inline]
fn row<'a>(u: &'a Unit, p: usize, k: usize) -> &'a [f64] {
    &u.x[k * p..(k + 1) * p]
}

#[inline]
fn dot(a: &[f64], b: &[f64]) -> f64 {
    a.iter().zip(b).map(|(x, y)| x * y).sum()
}

/// rank-one update  M += c · a bᵀ  (dense, upper and lower)
#[inline]
fn add_outer(m: &mut DMatrix<f64>, c: f64, a: &[f64], b: &[f64]) {
    if c == 0.0 {
        return;
    }
    let p = a.len();
    let data = m.as_mut_slice(); // column-major
    for j in 0..p {
        let cb = c * b[j];
        if cb == 0.0 {
            continue;
        }
        let col = &mut data[j * p..(j + 1) * p];
        for i in 0..p {
            col[i] += a[i] * cb;
        }
    }
}

/// log-likelihood of one unit only (line searches). `None` if infeasible.
pub fn unit_ll(prob: &Problem, u: &Unit, theta: &[f64]) -> Option<f64> {
    let p = prob.p;
    if let Some(gs) = &u.g {
        let ll = gauss_accumulate(p, u, gs, theta, None);
        return if ll.is_finite() { Some(ll) } else { None };
    }
    let qf = u.q as f64;
    let (mut pb, mut sb, mut m2, mut v) = (0.0, 0.0, 0.0, 0.0);
    for k in 0..u.q {
        let (pk, sk, _, _) = inv_link(prob.link, dot(row(u, p, k), theta));
        pb += pk;
        sb += sk;
        m2 += pk * pk;
        v += pk * sk;
    }
    pb /= qf;
    sb /= qf;
    m2 /= qf;
    v /= qf;
    let (r, n) = (u.r, u.n);
    if u.q == 1 || prob.agd_lik == AgdLik::Binomial {
        let ll = xlogy(r, pb) + xlogy(n - r, sb);
        return if ll.is_finite() { Some(ll + lchoose(n, r)) } else { None };
    }
    let ps = m2 / pb;
    let ns = n * pb * pb / m2;
    if !(ns - r + 1.0 > 0.0) || !(ps > 0.0) {
        return None;
    }
    let ll = lgamma(ns + 1.0) - lgamma(ns - r + 1.0) - lgamma(r + 1.0)
        + xlogy(r, ps)
        + xlogy(ns - r, v / pb);
    if ll.is_finite() { Some(ll) } else { None }
}

#[inline]
fn xlogy(x: f64, y: f64) -> f64 {
    if x == 0.0 { 0.0 } else { x * y.ln() }
}

fn lchoose(n: f64, r: f64) -> f64 {
    if n == 1.0 { 0.0 } else { lgamma(n + 1.0) - lgamma(r + 1.0) - lgamma(n - r + 1.0) }
}

/// Full contribution of one unit to the accumulators.
pub fn unit_accumulate(prob: &Problem, u: &Unit, theta: &[f64], acc: &mut Accum, scratch: &mut Scratch) {
    let p = prob.p;
    if let Some(gs) = &u.g {
        gauss_accumulate(p, u, gs, theta, Some(acc));
        return;
    }
    let (r, n) = (u.r, u.n);

    // ---- fast path: one design row (an IPD patient, or a covariate-free arm) ----
    if u.q == 1 {
        let z = row(u, p, 0);
        let (pk, sk, d1, d2) = inv_link(prob.link, dot(z, theta));
        acc.ll += xlogy(r, pk) + xlogy(n - r, sk) + lchoose(n, r);
        let a = r / pk - (n - r) / sk; // dℓ/dp
        let b = r / (pk * pk) + (n - r) / (sk * sk); // −d²ℓ/dp²
        for i in 0..p {
            acc.grad[i] += a * d1 * z[i];
        }
        add_outer(&mut acc.h_obs, b * d1 * d1 - a * d2, z, z);
        add_outer(&mut acc.h_fis, n * d1 * d1 / (pk * sk), z, z);
        return;
    }

    // ---- an aggregate arm: the averaged force law ----
    let qf = u.q as f64;
    let g1 = &mut scratch.g1; // ∂p̄
    let g2 = &mut scratch.g2; // ∂ mean(p²)
    g1.iter_mut().for_each(|x| *x = 0.0);
    g2.iter_mut().for_each(|x| *x = 0.0);
    scratch.pts.clear();
    let (mut pb, mut sb, mut m2, mut v) = (0.0, 0.0, 0.0, 0.0);
    for k in 0..u.q {
        let z = row(u, p, k);
        let lp = inv_link(prob.link, dot(z, theta));
        scratch.pts.push(lp);
        let (pk, sk, d1, _) = lp;
        pb += pk;
        sb += sk;
        m2 += pk * pk;
        v += pk * sk;
        for i in 0..p {
            g1[i] += d1 * z[i];
            g2[i] += 2.0 * pk * d1 * z[i];
        }
    }
    pb /= qf;
    sb /= qf;
    m2 /= qf;
    v /= qf;
    g1.iter_mut().for_each(|x| *x /= qf);
    g2.iter_mut().for_each(|x| *x /= qf);

    // Fisher (expected) information of a binomial in p̄ — PSD stepping matrix.
    add_outer(&mut acc.h_fis, n / (pb * sb), g1, g1);

    // coefficients: ∇ℓ = fa ∂p̄ + fb ∂m2 ;  −∇²ℓ = −[faa g1g1 + fab(g1g2+g2g1) + fbb g2g2
    //                                              + fa ∂²p̄ + fb ∂²m2]
    let (fa, fb, faa, fab, fbb);
    if prob.agd_lik == AgdLik::Binomial {
        acc.ll += xlogy(r, pb) + xlogy(n - r, sb) + lchoose(n, r);
        fa = r / pb - (n - r) / sb;
        fb = 0.0;
        faa = -(r / (pb * pb) + (n - r) / (sb * sb));
        fab = 0.0;
        fbb = 0.0;
    } else {
        let (a, b) = (pb, m2);
        let ps = b / a; // p*
        let qs = v / a; // 1 − p*
        let ns = n * a * a / b; // n*
        acc.ll += lgamma(ns + 1.0) - lgamma(ns - r + 1.0) - lgamma(r + 1.0)
            + xlogy(r, ps)
            + xlogy(ns - r, qs);
        let f_n = digamma(ns + 1.0) - digamma(ns - r + 1.0) + qs.ln();
        let f_p = r / ps - (ns - r) / qs;
        let f_nn = trigamma(ns + 1.0) - trigamma(ns - r + 1.0);
        let f_np = -1.0 / qs;
        let f_pp = -r / (ps * ps) - (ns - r) / (qs * qs);
        let (pa, pbb) = (-b / (a * a), 1.0 / a);
        let (paa, pab) = (2.0 * b / (a * a * a), -1.0 / (a * a));
        let (na, nb) = (2.0 * n * a / b, -n * a * a / (b * b));
        let (naa, nab, nbb) = (2.0 * n / b, -2.0 * n * a / (b * b), 2.0 * n * a * a / (b * b * b));
        fa = f_n * na + f_p * pa;
        fb = f_n * nb + f_p * pbb;
        faa = f_nn * na * na + 2.0 * f_np * na * pa + f_pp * pa * pa + f_n * naa + f_p * paa;
        fab = f_nn * na * nb + f_np * (na * pbb + nb * pa) + f_pp * pa * pbb + f_n * nab + f_p * pab;
        fbb = f_nn * nb * nb + 2.0 * f_np * nb * pbb + f_pp * pbb * pbb + f_n * nbb;
    }
    for i in 0..p {
        acc.grad[i] += fa * g1[i] + fb * g2[i];
    }
    add_outer(&mut acc.h_obs, -faa, g1, g1);
    add_outer(&mut acc.h_obs, -fab, g1, g2);
    add_outer(&mut acc.h_obs, -fab, g2, g1);
    add_outer(&mut acc.h_obs, -fbb, g2, g2);
    // second-derivative terms: Σ_q [fa d2 + fb·2(d1² + p d2)]/Q · z zᵀ
    for k in 0..u.q {
        let (pk, _, d1, d2) = scratch.pts[k];
        let c = (fa * d2 + fb * 2.0 * (d1 * d1 + pk * d2)) / qf;
        let z = row(u, p, k);
        add_outer(&mut acc.h_obs, -c, z, z);
    }
}

pub struct Scratch {
    g1: Vec<f64>,
    g2: Vec<f64>,
    pts: Vec<(f64, f64, f64, f64)>,
}

impl Scratch {
    pub fn new(p: usize) -> Self {
        Scratch { g1: vec![0.0; p], g2: vec![0.0; p], pts: Vec::new() }
    }
}

// ---------------------------------------------------------------------------
// the energy: likelihood + prior springs + random-effect springs
// ---------------------------------------------------------------------------

/// Block-diagonal RE precision structure K (τ² = 1): per study 2(I − 11ᵀ/(m+1)).
fn re_penalty(prob: &Problem) -> DMatrix<f64> {
    let mut k = DMatrix::zeros(prob.p, prob.p);
    for g in &prob.re_groups {
        let m = g.len() as f64;
        for &i in g {
            for &j in g {
                k[(i, j)] = 2.0 * ((i == j) as u8 as f64 - 1.0 / (m + 1.0));
            }
        }
    }
    k
}

pub struct Energy<'a> {
    pub prob: &'a Problem,
    /// τ⁻²K (random-effect springs)
    pub pen: DMatrix<f64>,
    /// prior springs on the fixed columns
    pub fixed_priors: Vec<(usize, ParPrior)>,
}

impl<'a> Energy<'a> {
    pub fn new(prob: &'a Problem, tau2: Option<f64>) -> Self {
        let is_re = prob.is_re_col();
        let pen = match tau2 {
            Some(t2) if !prob.re_groups.is_empty() => re_penalty(prob) / t2,
            _ => DMatrix::zeros(prob.p, prob.p),
        };
        let fixed_priors = (0..prob.p).filter(|&i| !is_re[i] && prob.priors[i] != ParPrior::Flat)
            .map(|i| (i, prob.priors[i])).collect();
        Energy { prob, pen, fixed_priors }
    }

    fn prior_ll(&self, theta: &[f64]) -> f64 {
        self.fixed_priors.iter().map(|(i, pr)| pr.terms(theta[*i]).0).sum()
    }

    /// log posterior (up to a τ-independent constant), or None if infeasible.
    pub fn value(&self, theta: &[f64]) -> Option<f64> {
        let mut ll = 0.0;
        for u in &self.prob.units {
            ll += unit_ll(self.prob, u, theta)?;
        }
        let t = DVector::from_column_slice(theta);
        Some(ll + self.prior_ll(theta) - 0.5 * (t.transpose() * &self.pen * &t)[(0, 0)])
    }

    pub fn accumulate(&self, theta: &[f64]) -> Accum {
        let p = self.prob.p;
        let mut acc = Accum::new(p);
        let mut sc = Scratch::new(p);
        for u in &self.prob.units {
            unit_accumulate(self.prob, u, theta, &mut acc, &mut sc);
        }
        let t = DVector::from_column_slice(theta);
        acc.ll -= 0.5 * (t.transpose() * &self.pen * &t)[(0, 0)];
        acc.grad -= &self.pen * &t;
        acc.h_obs += &self.pen;
        acc.h_fis += &self.pen;
        for (i, pr) in &self.fixed_priors {
            let (l, g, ho, hf) = pr.terms(theta[*i]);
            acc.ll += l;
            acc.grad[*i] += g;
            acc.h_obs[(*i, *i)] += ho;
            acc.h_fis[(*i, *i)] += hf;
        }
        // symmetrise
        let ht = acc.h_obs.transpose();
        acc.h_obs = (&acc.h_obs + ht) * 0.5;
        acc
    }
}

// ---------------------------------------------------------------------------
// relaxation to equilibrium + Laplace
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct ModeFit {
    pub theta: Vec<f64>,
    /// log posterior at the mode (incl. prior/RE springs)
    pub log_post: f64,
    /// log-likelihood at the mode (data springs only)
    pub log_lik: f64,
    /// curvature used for the Laplace step (observed info, PD-guarded)
    pub h: DMatrix<f64>,
    pub cov: DMatrix<f64>,
    pub log_det_h: f64,
    pub iters: usize,
    pub converged: bool,
    pub used_observed: bool,
}

fn chol_logdet(h: &DMatrix<f64>) -> Option<(Cholesky<f64, nalgebra::Dyn>, f64)> {
    let c = Cholesky::new(h.clone())?;
    let ld = 2.0 * c.l().diagonal().iter().map(|x| x.ln()).sum::<f64>();
    Some((c, ld))
}

pub fn fit_mode(prob: &Problem, tau2: Option<f64>, init: &[f64], max_iter: usize) -> ModeFit {
    let en = Energy::new(prob, tau2);
    let mut theta = init.to_vec();
    let mut f = en.value(&theta).unwrap_or(f64::NEG_INFINITY);
    let mut iters = 0;
    let mut converged = false;
    while iters < max_iter {
        iters += 1;
        let acc = en.accumulate(&theta);
        // Newton on the observed information when it is PD, else Fisher scoring.
        let step = match Cholesky::new(acc.h_obs.clone()) {
            Some(c) if iters > 2 => c.solve(&acc.grad),
            _ => match Cholesky::new(acc.h_fis.clone()) {
                Some(c) => c.solve(&acc.grad),
                None => {
                    let mut hj = acc.h_fis.clone();
                    for i in 0..prob.p {
                        hj[(i, i)] += 1e-8 + 1e-6 * hj[(i, i)].abs();
                    }
                    Cholesky::new(hj).expect("information not PD").solve(&acc.grad)
                }
            },
        };
        let mut alpha = 1.0;
        let mut accepted = false;
        for _ in 0..40 {
            let cand: Vec<f64> = theta.iter().zip(step.iter()).map(|(t, s)| t + alpha * s).collect();
            if let Some(fc) = en.value(&cand) {
                if fc >= f - 1e-10 * f.abs().max(1.0) {
                    theta = cand;
                    let df = fc - f;
                    f = fc;
                    accepted = true;
                    let smax = step.iter().fold(0.0f64, |m, s| m.max(s.abs())) * alpha;
                    if smax < 1e-9 || (df.abs() < 1e-12 * f.abs().max(1.0) && alpha == 1.0) {
                        converged = true;
                    }
                    break;
                }
            }
            alpha *= 0.5;
        }
        if !accepted || converged {
            converged = converged || !accepted && step.iter().all(|s| s.abs() < 1e-7);
            break;
        }
    }
    let acc = en.accumulate(&theta);
    let ll_data = prob.units.iter().map(|u| unit_ll(prob, u, &theta).unwrap_or(f64::NAN)).sum();
    let (h, used_observed) = if Cholesky::new(acc.h_obs.clone()).is_some() {
        (acc.h_obs.clone(), true)
    } else {
        (acc.h_fis.clone(), false)
    };
    let (c, log_det_h) = chol_logdet(&h).expect("curvature not PD at the mode");
    let cov = c.inverse();
    ModeFit { theta, log_post: f, log_lik: ll_data, h, cov, log_det_h, iters, converged, used_observed }
}

/// Grand-canonical grid point: the Laplace partition function at τ².
#[derive(Debug, Clone)]
pub struct GcPoint {
    pub tau2: f64,
    pub log_z: f64,
    pub fit: ModeFit,
}

pub fn gc_point(prob: &Problem, tau2: f64, init: &[f64]) -> GcPoint {
    let fit = fit_mode(prob, Some(tau2), init, 200);
    let m = prob.n_re() as f64;
    let log_z = fit.log_post - 0.5 * m * tau2.ln() - 0.5 * fit.log_det_h;
    GcPoint { tau2, log_z, fit }
}

/// Evaluate the partition function on a log-spaced τ² grid, warm-starting each
/// relaxation from its neighbour. Points are returned in ascending τ².
pub fn gc_grid(prob: &Problem, taus2: &[f64], init: &[f64]) -> Vec<GcPoint> {
    let mut t2: Vec<f64> = taus2.to_vec();
    t2.sort_by(|a, b| a.partial_cmp(b).unwrap());
    // start in the middle (best conditioned), sweep outwards both ways
    let mid = t2.len() / 2;
    let mut out: Vec<Option<GcPoint>> = vec![None; t2.len()];
    let first = gc_point(prob, t2[mid], init);
    let start = first.fit.theta.clone();
    out[mid] = Some(first);
    let mut prev = start.clone();
    for i in (mid + 1)..t2.len() {
        let g = gc_point(prob, t2[i], &prev);
        prev = g.fit.theta.clone();
        out[i] = Some(g);
    }
    prev = start;
    for i in (0..mid).rev() {
        let g = gc_point(prob, t2[i], &prev);
        prev = g.fit.theta.clone();
        out[i] = Some(g);
    }
    out.into_iter().map(|g| g.unwrap()).collect()
}

// ---------------------------------------------------------------------------
// importance correction of the Laplace spring solution
// ---------------------------------------------------------------------------
//
// The equilibrium + curvature define a proposal q = multivariate-t_ν(θ̂, H⁻¹).
// Reweighting by the exact energy, w = exp(F(θ)) / q(θ), gives
//   * exact posterior moments/quantiles (to Monte Carlo error), removing the
//     mode-vs-mean bias of a skewed posterior (sparse events, probit tails);
//   * the exact partition function  ∫ exp F = E_q[w]  (Laplace is its
//     leading-order approximation), so the GC τ² weights are corrected too;
//   * a diagnostic: the effective sample size of w measures how far the
//     posterior is from the harmonic (Gaussian) spring picture.
// Draws are iid — no chains, no convergence monitoring.

/// splitmix64 + Box–Muller.
pub struct Rng(pub u64);
impl Rng {
    pub fn seeded(seed: u64) -> Self {
        Rng(seed.wrapping_mul(0x9E3779B97F4A7C15).wrapping_add(1))
    }
    fn next(&mut self) -> u64 {
        self.0 = self.0.wrapping_add(0x9E3779B97F4A7C15);
        let mut z = self.0;
        z = (z ^ (z >> 30)).wrapping_mul(0xBF58476D1CE4E5B9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94D049BB133111EB);
        z ^ (z >> 31)
    }
    pub fn unif(&mut self) -> f64 {
        ((self.next() >> 11) as f64 + 0.5) / (1u64 << 53) as f64
    }
    pub fn normal(&mut self) -> f64 {
        let (u1, u2) = (self.unif(), self.unif());
        (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos()
    }
}

/// A weighted sample from one equilibrium (one τ², or FE).
pub struct IsPool {
    pub theta: Vec<Vec<f64>>,
    /// log importance weights log exp(F)/q (unnormalised)
    pub log_w: Vec<f64>,
    /// log ∫ exp(F) dθ  (the exact partition function, MC estimate)
    pub log_int: f64,
    /// Laplace value of the same integral, for comparison
    pub log_int_laplace: f64,
    pub ess: f64,
}

pub fn is_pool(prob: &Problem, tau2: Option<f64>, fit: &ModeFit, n: usize, nu: f64, seed: u64) -> IsPool {
    let p = prob.p;
    let en = Energy::new(prob, tau2);
    let l = Cholesky::new(fit.cov.clone()).expect("cov not PD").l();
    let pf = p as f64;
    let log_c = libm::lgamma(0.5 * (nu + pf)) - libm::lgamma(0.5 * nu)
        - 0.5 * pf * (nu * std::f64::consts::PI).ln() + 0.5 * fit.log_det_h;
    // independent RNG streams per chunk ⇒ reproducible and parallel
    let chunk = 256usize;
    let n_chunks = n.div_ceil(chunk);
    let parts: Vec<(Vec<Vec<f64>>, Vec<f64>)> = (0..n_chunks)
        .into_par_iter()
        .map(|c| {
            let mut rng = Rng::seeded(seed.wrapping_mul(1_000_003).wrapping_add(c as u64));
            let k = chunk.min(n - c * chunk);
            let mut th_c = Vec::with_capacity(k);
            let mut lw_c = Vec::with_capacity(k);
            for _ in 0..k {
                let z0 = DVector::from_fn(p, |_, _| rng.normal());
                let chi: f64 = (0..nu as usize).map(|_| rng.normal().powi(2)).sum();
                let sc = (nu / chi).sqrt();
                let zz = z0.dot(&z0) * sc * sc;
                let th: Vec<f64> = (&l * &z0).iter().zip(&fit.theta).map(|(d, m)| m + d * sc).collect();
                let lq = log_c - 0.5 * (nu + pf) * (1.0 + zz / nu).ln();
                lw_c.push(match en.value(&th) {
                    Some(f) => f - lq,
                    None => f64::NEG_INFINITY,
                });
                th_c.push(th);
            }
            (th_c, lw_c)
        })
        .collect();
    let mut theta = Vec::with_capacity(n);
    let mut log_w = Vec::with_capacity(n);
    for (t, w) in parts {
        theta.extend(t);
        log_w.extend(w);
    }
    let m = log_w.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
    let w: Vec<f64> = log_w.iter().map(|x| (x - m).exp()).collect();
    let sw: f64 = w.iter().sum();
    let sw2: f64 = w.iter().map(|x| x * x).sum();
    IsPool {
        theta,
        log_int: m + (sw / n as f64).ln(),
        log_int_laplace: fit.log_post + 0.5 * pf * (2.0 * std::f64::consts::PI).ln() - 0.5 * fit.log_det_h,
        ess: sw * sw / sw2,
        log_w,
    }
}

pub fn log_grid(lo: f64, hi: f64, n: usize) -> Vec<f64> {
    (0..n)
        .map(|i| (lo.ln() + (hi.ln() - lo.ln()) * i as f64 / (n as f64 - 1.0)).exp())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn toy(link: Link, lik: AgdLik) -> Problem {
        // 2 parameters; an IPD unit and an aggregate unit with 3 points
        let units = vec![
            Unit { r: 1.0, n: 1.0, q: 1, x: vec![1.0, 0.3], g: None },
            Unit { r: 0.0, n: 1.0, q: 1, x: vec![1.0, -1.2], g: None },
            Unit { r: 37.0, n: 90.0, q: 3, x: vec![1.0, 0.5, 1.0, -0.7, 1.0, 1.9], g: None },
            Unit { r: 12.0, n: 50.0, q: 4, x: vec![1.0, 0.1, 1.0, 2.5, 1.0, -2.0, 1.0, 0.0], g: None },
        ];
        Problem { p: 2, link, agd_lik: lik, units, priors: vec![ParPrior::from_prec(0.01), ParPrior::StudentT { df: 3.0, loc: 0.2, scale: 0.7 }], re_groups: vec![] }
    }

    #[test]
    fn special_functions() {
        assert!((digamma(1.0) + 0.577_215_664_901_532_9).abs() < 1e-12);
        assert!((trigamma(1.0) - std::f64::consts::PI.powi(2) / 6.0).abs() < 1e-12);
        assert!((digamma(0.37) - (-2.795_301_410_890_564)).abs() < 1e-9);
    }

    #[test]
    fn gradient_and_hessian_match_finite_differences() {
        for link in [Link::Logit, Link::Probit] {
            for lik in [AgdLik::Binomial, AgdLik::Binomial2] {
                let prob = toy(link, lik);
                let en = Energy::new(&prob, None);
                let th = [0.2, -0.4];
                let acc = en.accumulate(&th);
                let h = 1e-5;
                for i in 0..2 {
                    let mut a = th;
                    let mut b = th;
                    a[i] += h;
                    b[i] -= h;
                    let g_fd = (en.value(&a).unwrap() - en.value(&b).unwrap()) / (2.0 * h);
                    assert!((g_fd - acc.grad[i]).abs() < 1e-6 * (1.0 + g_fd.abs()),
                            "{link:?} {lik:?} grad {i}: fd {g_fd} vs {}", acc.grad[i]);
                    let ga = en.accumulate(&a).grad;
                    let gb = en.accumulate(&b).grad;
                    for j in 0..2 {
                        let h_fd = -(ga[j] - gb[j]) / (2.0 * h);
                        assert!((h_fd - acc.h_obs[(j, i)]).abs() < 1e-5 * (1.0 + h_fd.abs()),
                                "{link:?} {lik:?} hess {j},{i}: fd {h_fd} vs {}", acc.h_obs[(j, i)]);
                    }
                }
            }
        }
    }

    #[test]
    fn binomial2_reduces_to_binomial_without_covariate_spread() {
        // all integration points identical ⇒ p* = p̄, n* = n ⇒ same log-likelihood
        let u = Unit { r: 7.0, n: 20.0, q: 3, x: vec![1.0, 0.4, 1.0, 0.4, 1.0, 0.4], g: None };
        let mk = |lik| Problem { p: 2, link: Link::Logit, agd_lik: lik, units: vec![u.clone()],
                                 priors: vec![ParPrior::Flat; 2], re_groups: vec![] };
        let (a, b) = (mk(AgdLik::Binomial), mk(AgdLik::Binomial2));
        let th = [0.1, -0.3];
        let la = unit_ll(&a, &a.units[0], &th).unwrap();
        let lb = unit_ll(&b, &b.units[0], &th).unwrap();
        assert!((la - lb).abs() < 1e-9, "{la} vs {lb}");
    }

    #[test]
    fn importance_recovers_laplace_for_gaussian_like_posterior() {
        // large counts ⇒ near-Gaussian posterior ⇒ IS integral ≈ Laplace, ESS high
        let units = (0..40)
            .map(|i| {
                let x = (i as f64 - 20.0) / 10.0;
                Unit { r: (200.0 / (1.0 + (-0.3 - 0.8 * x).exp())).round(), n: 200.0, q: 1, x: vec![1.0, x], g: None }
            })
            .collect();
        let prob = Problem { p: 2, link: Link::Logit, agd_lik: AgdLik::Binomial, units,
                             priors: vec![ParPrior::Flat; 2], re_groups: vec![] };
        let fit = fit_mode(&prob, None, &[0.0, 0.0], 100);
        let pool = is_pool(&prob, None, &fit, 20000, 8.0, 7);
        assert!((pool.log_int - pool.log_int_laplace).abs() < 0.01, "{} vs {}", pool.log_int, pool.log_int_laplace);
        assert!(pool.ess > 0.6 * 20000.0, "ess {}", pool.ess);
    }

    #[test]
    fn mode_is_force_balance() {
        let prob = toy(Link::Probit, AgdLik::Binomial2);
        let fit = fit_mode(&prob, None, &[0.0, 0.0], 100);
        assert!(fit.converged);
        let g = Energy::new(&prob, None).accumulate(&fit.theta).grad;
        assert!(g.iter().all(|x| x.abs() < 1e-7), "net force {g:?}");
    }

    #[test]
    fn reweighting_a_pool_equals_refitting_under_the_new_prior() {
        // pool under a vague prior, reweighted by π_new/π_old  ==  pool under π_new
        let units: Vec<Unit> = (0..12)
            .map(|i| {
                let x = (i as f64 - 6.0) / 4.0;
                Unit { r: (30.0 / (1.0 + (0.8 - 0.6 * x).exp())).round(), n: 30.0, q: 1, x: vec![1.0, x], g: None }
            })
            .collect();
        let old = [ParPrior::Normal { loc: 0.0, scale: 10.0 }; 2];
        let new = [ParPrior::Normal { loc: 0.0, scale: 10.0 }, ParPrior::StudentT { df: 1.0, loc: 0.0, scale: 0.3 }];
        let mk = |pr: [ParPrior; 2]| Problem { p: 2, link: Link::Probit, agd_lik: AgdLik::Binomial,
                                              units: units.clone(), priors: pr.to_vec(), re_groups: vec![] };
        let (po, pn) = (mk(old), mk(new));
        let pool = is_pool(&po, None, &fit_mode(&po, None, &[0.0, 0.0], 100), 200_000, 8.0, 3);
        let lw: Vec<f64> = pool.theta.iter().zip(&pool.log_w)
            .map(|(t, w)| w + new[1].terms(t[1]).0 - old[1].terms(t[1]).0).collect();
        let direct = is_pool(&pn, None, &fit_mode(&pn, None, &[0.0, 0.0], 100), 200_000, 8.0, 4);
        let mean = |th: &[Vec<f64>], lw: &[f64], j: usize| {
            let m = lw.iter().cloned().fold(f64::NEG_INFINITY, f64::max);
            let (mut a, mut b) = (0.0, 0.0);
            for (t, l) in th.iter().zip(lw) { let w = (l - m).exp(); a += w * t[j]; b += w; }
            a / b
        };
        for j in 0..2 {
            let (r, d) = (mean(&pool.theta, &lw, j), mean(&direct.theta, &direct.log_w, j));
            assert!((r - d).abs() < 0.01, "param {j}: reweighted {r} vs direct {d}");
        }
        // the Cauchy prior must actually have moved the slope
        assert!((mean(&pool.theta, &pool.log_w, 1) - mean(&direct.theta, &direct.log_w, 1)).abs() > 0.02);
    }

    fn gauss_toy() -> (Problem, Vec<Unit>) {
        // columns: intercept, slope, log σ  (σ column has x = 0)
        let pts = [(0.3, 1.2), (-1.0, -0.4), (0.8, 2.1), (1.5, 2.9), (-0.2, 0.5)];
        let ipd: Vec<Unit> = pts.iter().map(|&(x, y)| Unit { r: 0.0, n: 1.0, q: 1, x: vec![1.0, x, 0.0],
            g: Some(Box::new(Gauss::Ipd { y, sig: 2 })) }).collect();
        let mut units = ipd.clone();
        units.push(Unit { r: 0.0, n: 40.0, q: 3, x: vec![1.0, 0.2, 0.0, 1.0, 0.9, 0.0, 1.0, -0.5, 0.0],
                          g: Some(Box::new(Gauss::Arm { y: 0.7, se: 0.15 })) });
        let pr = vec![ParPrior::Normal { loc: 0.0, scale: 10.0 }, ParPrior::Flat, ParPrior::HalfNormalLog { scale: 5.0 }];
        (Problem { p: 3, link: Link::Logit, agd_lik: AgdLik::Binomial, units, priors: pr, re_groups: vec![] }, ipd)
    }

    #[test]
    fn gaussian_springs_gradient_and_hessian() {
        let (prob, _) = gauss_toy();
        let en = Energy::new(&prob, None);
        let th = [0.4, 0.9, -0.3];
        let acc = en.accumulate(&th);
        let h = 1e-5;
        for i in 0..3 {
            let (mut a, mut b) = (th, th);
            a[i] += h; b[i] -= h;
            let g_fd = (en.value(&a).unwrap() - en.value(&b).unwrap()) / (2.0 * h);
            assert!((g_fd - acc.grad[i]).abs() < 1e-6 * (1.0 + g_fd.abs()), "grad {i}: {g_fd} vs {}", acc.grad[i]);
            let (ga, gb) = (en.accumulate(&a).grad, en.accumulate(&b).grad);
            for j in 0..3 {
                let h_fd = -(ga[j] - gb[j]) / (2.0 * h);
                assert!((h_fd - acc.h_obs[(j, i)]).abs() < 1e-5 * (1.0 + h_fd.abs()), "hess {j},{i}: {h_fd} vs {}", acc.h_obs[(j, i)]);
            }
        }
    }

    #[test]
    fn equivalent_spring_is_exact_for_gaussian_patients() {
        // a site's patients and its sufficient statistics give the same energy everywhere
        let (_, ipd) = gauss_toy();
        let p = 3;
        let (mut xtx, mut xty, mut yty) = (vec![0.0; p * p], vec![0.0; p], 0.0);
        for u in &ipd {
            if let Some(g) = &u.g { if let Gauss::Ipd { y, .. } = **g {
                for i in 0..p { xty[i] += u.x[i] * y; for j in 0..p { xtx[i * p + j] += u.x[i] * u.x[j]; } }
                yty += y * y;
            }}
        }
        let suff = Unit { r: 0.0, n: ipd.len() as f64, q: 1, x: vec![0.0; p],
                          g: Some(Box::new(Gauss::Suff { xtx, xty, yty, n: ipd.len() as f64, sig: 2 })) };
        let mk = |units: Vec<Unit>| Problem { p, link: Link::Logit, agd_lik: AgdLik::Binomial, units,
                                              priors: vec![ParPrior::Flat; p], re_groups: vec![] };
        let (a, b) = (mk(ipd.clone()), mk(vec![suff]));
        for th in [[0.0, 0.0, 0.0], [0.4, 0.9, -0.3], [-1.2, 2.0, 0.7]] {
            let (ea, eb) = (Energy::new(&a, None).accumulate(&th), Energy::new(&b, None).accumulate(&th));
            assert!((ea.ll - eb.ll).abs() < 1e-10, "ll {} vs {}", ea.ll, eb.ll);
            for i in 0..p {
                assert!((ea.grad[i] - eb.grad[i]).abs() < 1e-10);
                for j in 0..p { assert!((ea.h_obs[(i, j)] - eb.h_obs[(i, j)]).abs() < 1e-9); }
            }
        }
    }
}
