// Random-effects + Grand-Canonical network meta-regression in Rust (nalgebra).
//   RE  : REML/EM for (beta, tau2)        -- analogue of nmaRegressionRE
//   GC  : joint (beta, tau2[, sigmaB2]) posterior by Gaussian marginalisation
//         (shared and exchangeable beta)  -- analogue of nmaRegressionPosterior(B)
// Usage: nmr_re_gc <data.json> <cov.json> [exch]
use nalgebra::{DMatrix, DVector, Cholesky};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct Arm { study: String, treatment: String, mean: f64, sd: f64, n: f64 }

struct Design { rows: Vec<Vec<f64>>, ys: Vec<f64>, vs: Vec<f64>, np: usize, ns: usize,
                beta_col: usize, common_col: usize, nonref: Vec<String>, reff: String,
                tidx: BTreeMap<String, usize> }

fn norm_cdf(x: f64) -> f64 {
    let s = x.signum(); let az = (x / 2f64.sqrt()).abs();
    let t = 1.0 / (1.0 + 0.3275911 * az);
    let y = 1.0 - (((((1.061405429*t - 1.453152027)*t) + 1.421413741)*t - 0.284496736)*t + 0.254829592)*t*(-az*az).exp();
    0.5 * (1.0 + s * y)
}

fn build(arms: &[Arm], cov: &BTreeMap<String,f64>, exch: bool) -> Design {
    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String>  = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff = treats[0].clone();
    let nonref: Vec<String> = treats.iter().filter(|t| **t != reff).cloned().collect();
    let ns = studies.len(); let nt = nonref.len();
    let nbeta = if exch { nt + 1 } else { 1 };
    let np = ns + nt + nbeta; let beta_base = ns + nt;
    let sidx: BTreeMap<&str,usize> = studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx: BTreeMap<String,usize> = nonref.iter().enumerate().map(|(i,t)|(t.clone(),i)).collect();
    let (mut rows, mut ys, mut vs) = (vec![], vec![], vec![]);
    for a in arms {
        let mut row = vec![0.0; np];
        row[sidx[a.study.as_str()]] = -1.0;
        if a.treatment != reff {
            let bc = if exch { beta_base + tidx[&a.treatment] } else { beta_base };
            row[ns + tidx[&a.treatment]] = 1.0;
            row[bc] = *cov.get(&a.study).unwrap_or(&0.0);
        }
        rows.push(row); ys.push(a.mean); vs.push(a.sd*a.sd/a.n);
    }
    Design { rows, ys, vs, np, ns, beta_col: beta_base, common_col: beta_base + nt, nonref, reff, tidx }
}

// assemble A=X'WX(+K), b=X'Wy with weight 1/(v+t2arm); return (theta, cov, logdet)
fn solve(d: &Design, t2arm: f64, kadd: Option<&DMatrix<f64>>) -> (DVector<f64>, DMatrix<f64>, f64) {
    let np = d.np;
    let mut a = DMatrix::<f64>::zeros(np, np);
    let mut b = DVector::<f64>::zeros(np);
    for ((row, &y), &v) in d.rows.iter().zip(&d.ys).zip(&d.vs) {
        let w = 1.0 / (v + t2arm);
        for i in 0..np { if row[i]!=0.0 { b[i]+=w*row[i]*y;
            for j in 0..np { if row[j]!=0.0 { a[(i,j)]+=w*row[i]*row[j]; } } } }
    }
    if let Some(k) = kadd { a += k; }
    let chol = Cholesky::new(a).expect("not SPD");
    let theta = chol.solve(&b);
    let cov = chol.inverse();
    let logdet = 2.0 * chol.l().diagonal().iter().map(|x| x.ln()).sum::<f64>();
    (theta, cov, logdet)
}
fn quad(row: &[f64], cov: &DMatrix<f64>) -> f64 {
    let mut s = 0.0;
    for i in 0..row.len() { if row[i]!=0.0 { for j in 0..row.len() { if row[j]!=0.0 { s += row[i]*cov[(i,j)]*row[j]; } } } }
    s
}

fn re(d: &Design) -> (f64, f64) {        // (beta, tau2) by REML/EM
    let n = d.rows.len() as f64;
    let mut tau2 = 0.1;
    for _ in 0..300 {
        let t2arm = tau2 / 2.0;
        let (theta, cov, _) = solve(d, t2arm, None);
        let (mut sd2, mut svd) = (0.0, 0.0);
        for ((row, &y), &v) in d.rows.iter().zip(&d.ys).zip(&d.vs) {
            let fitted: f64 = row.iter().zip(theta.iter()).map(|(r,t)| r*t).sum();
            let resid = y - fitted; let b = (1.0/v)*t2arm/(1.0+(1.0/v)*t2arm);
            sd2 += (b*resid).powi(2);
            svd += (1.0-b)*t2arm + b*b*quad(row, &cov);
        }
        let t2new = (2.0*(sd2/n + svd/n)).max(1e-8);
        if (t2new - tau2).abs() < 1e-9 { tau2 = t2new; break; }
        tau2 = t2new;
    }
    let (theta, _, _) = solve(d, tau2/2.0, None);
    (theta[d.beta_col], tau2)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let exch = args.get(3).map(|s| s == "exch").unwrap_or(false);
    let arms: Vec<Arm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let cov: BTreeMap<String,f64> = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
    let d = build(&arms, &cov, exch);
    println!("rust RE+GC: studies={} np={} exch={}", d.ns, d.np, exch);

    // ---- RE ----
    let t0 = Instant::now();
    let (beta_re, tau2_re) = re(&d);
    println!("RE  : beta={:.5} tau2={:.5}   [{:.2} ms]", beta_re, tau2_re, t0.elapsed().as_secs_f64()*1000.0);

    // ---- GC (shared) ----
    let t1 = Instant::now();
    // tau2 grid scaled from the RE mode
    let tau2hi = (tau2_re*12.0).max(0.02); let ngrid = 200usize; let dt = tau2hi/ngrid as f64;
    let ts: Vec<f64> = (1..=ngrid).map(|i| tau2hi*(i as f64-0.5)/ngrid as f64).collect();
    let (mut logzs, mut bh, mut bv) = (vec![], vec![], vec![]);
    for &t2 in &ts {
        let t2arm = t2/2.0; let (theta, cov, logdet) = solve(&d, t2arm, None);
        let sumlogv: f64 = d.vs.iter().map(|v| (v+t2arm).ln()).sum();
        let ywy: f64 = d.ys.iter().zip(&d.vs).map(|(y,v)| y*y/(v+t2arm)).sum();
        let mut bty = 0.0; for ((row,&y),&v) in d.rows.iter().zip(&d.ys).zip(&d.vs) {
            let w=1.0/(v+t2arm); for i in 0..d.np { if row[i]!=0.0 { bty += w*row[i]*y*theta[i]; } } }
        logzs.push(-0.5*(sumlogv + logdet + (ywy - bty)));
        bh.push(theta[d.beta_col]); bv.push(cov[(d.beta_col,d.beta_col)]);
    }
    let mx = logzs.iter().cloned().fold(f64::MIN, f64::max);
    let ws: Vec<f64> = logzs.iter().map(|l| (l-mx).exp()).collect();
    let tot: f64 = ws.iter().sum(); let ws: Vec<f64> = ws.iter().map(|w| w/tot).collect();
    let beta_mean: f64 = ws.iter().zip(&bh).map(|(w,b)| w*b).sum();
    let beta_var: f64 = ws.iter().zip(bh.iter()).zip(&bv).map(|((w,b),v)| w*(v+b*b)).sum::<f64>() - beta_mean*beta_mean;
    let tau2_mean: f64 = ws.iter().zip(&ts).map(|(w,t)| w*t).sum();
    // tau2 mode (grid argmax)
    let mode_i = logzs.iter().enumerate().fold((0,f64::MIN),|(mi,mv),(i,&l)| if l>mv {(i,l)} else {(mi,mv)}).0;
    // beta mixture 95% CI by bisection
    let bsd = beta_var.max(1e-12).sqrt();
    let cdf = |x: f64| -> f64 { ws.iter().zip(bh.iter()).zip(&bv).map(|((w,m),v)| w*norm_cdf((x-m)/v.max(1e-300).sqrt())).sum() };
    let q = |p: f64| -> f64 { let (mut lo, mut hi)=(beta_mean-12.0*bsd, beta_mean+12.0*bsd);
        for _ in 0..60 { let m=(lo+hi)/2.0; if cdf(m)<p {lo=m} else {hi=m} } (lo+hi)/2.0 };
    println!("GC  : beta mean={:.5} sd={:.5} 95%=[{:.4},{:.4}]   tau2 mean={:.5} mode={:.5}   [{:.2} ms, exch={}]",
             beta_mean, bsd, q(0.025), q(0.975), tau2_mean, ts[mode_i], t1.elapsed().as_secs_f64()*1000.0, exch);
    let _ = dt;
}
