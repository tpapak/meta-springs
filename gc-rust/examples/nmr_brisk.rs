// Baseline-risk binary network meta-regression in Rust (Achana/bnma model), the analogue of
// the Haskell `nmaRegressionBaselineRiskBin`. The regressor is the latent study baseline eta_i:
//   logit(p_ik) = eta_i + d_t + B*(eta_i - mx),   mx = mean cc control log-odds (fixed).
// The bilinear B*eta_i term => damped Fisher scoring (Gauss-Newton). Observed-information SEs.
// Usage: nmr_brisk <data.json (study,treatment,events,n)> <ref-treatment>
use nalgebra::{DMatrix, DVector};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct BinArm { study: String, treatment: String, events: f64, n: f64 }

fn sigmoid(x: f64) -> f64 { if x >= 0.0 { 1.0/(1.0+(-x).exp()) } else { let e=x.exp(); e/(1.0+e) } }

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let arms: Vec<BinArm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let reff = args[2].clone();

    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats:  Vec<String> = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    assert!(treats.contains(&reff), "reference '{}' not present", reff);
    let nonref: Vec<String> = treats.iter().filter(|t| **t != reff).cloned().collect();
    let ns = studies.len(); let nt = nonref.len(); let np = ns + nt + 1; let beta_col = ns + nt;
    let sidx: BTreeMap<&str,usize> = studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx: BTreeMap<&str,usize> = nonref.iter().enumerate().map(|(i,t)|(t.as_str(),i)).collect();
    let dcol = |t: usize| ns + t;

    // arm meta: (study idx, Some(treat idx)|None for ref, events, n)
    let meta: Vec<(usize, Option<usize>, f64, f64)> = arms.iter().map(|a|
        (sidx[a.study.as_str()],
         if a.treatment == reff { None } else { Some(tidx[a.treatment.as_str()]) },
         a.events, a.n)).collect();

    // mx = mean continuity-corrected control-arm log-odds; theta0 baselines = those log-odds
    let mut clo = vec![0.0f64; ns];
    for a in &arms {
        if a.treatment == reff {
            let (mut r, mut n) = (a.events, a.n);
            if r == 0.0 { r = 0.5; n = a.n + 1.0; }
            let p = r / n; clo[sidx[a.study.as_str()]] = (p/(1.0-p)).ln();
        }
    }
    let mx: f64 = clo.iter().sum::<f64>() / ns as f64;

    let eta_of = |th: &DVector<f64>, si: usize, ot: Option<usize>| -> f64 {
        match ot { None => th[si], Some(t) => th[si] + th[dcol(t)] + th[beta_col]*(th[si]-mx) }
    };
    let loglik = |th: &DVector<f64>| -> f64 {
        let mut ll = 0.0;
        for &(si, ot, e, n) in &meta {
            let p = sigmoid(eta_of(th, si, ot)).clamp(1e-12, 1.0-1e-12);
            ll += e*p.ln() + (n-e)*(1.0-p).ln();
        }
        ll
    };
    // Fisher system A = J'WJ, b = J'W z (working response z = J*theta + (e-n p)/w)
    let build = |th: &DVector<f64>| -> (DMatrix<f64>, DVector<f64>) {
        let mut a = DMatrix::<f64>::zeros(np, np);
        let mut b = DVector::<f64>::zeros(np);
        for &(si, ot, e, n) in &meta {
            let bb = th[beta_col];
            let (eta, jac): (f64, Vec<(usize,f64)>) = match ot {
                None    => (th[si], vec![(si,1.0)]),
                Some(t) => (th[si] + th[dcol(t)] + bb*(th[si]-mx),
                            vec![(si,1.0+bb),(dcol(t),1.0),(beta_col,th[si]-mx)]),
            };
            let p = sigmoid(eta); let w = (n*p*(1.0-p)).max(1e-9);
            let jtheta: f64 = jac.iter().map(|(i,v)| v*th[*i]).sum();
            let z = jtheta + (e - n*p)/w;
            for &(i,vi) in &jac { b[i] += w*vi*z; for &(j,vj) in &jac { a[(i,j)] += w*vi*vj; } }
        }
        (a, b)
    };

    let t0 = Instant::now();
    let mut theta = DVector::<f64>::zeros(np);
    for i in 0..ns { theta[i] = clo[i]; }
    for _ in 0..200 {
        let (a, b) = build(&theta);
        let newton = a.lu().solve(&b).expect("singular");
        let dir = &newton - &theta;
        let ll0 = loglik(&theta);
        // backtracking line search on the binomial log-likelihood (bilinear term overshoots)
        let mut alpha = 1.0; let mut cand = theta.clone();
        for _ in 0..40 {
            cand = &theta + &dir*alpha;
            let llc = loglik(&cand);
            if llc.is_finite() && llc >= ll0 - 1e-9 { break; }
            alpha *= 0.5;
        }
        let step = (&cand - &theta).amax();
        theta = cand;
        if step < 1e-12 { break; }
    }
    let secs = t0.elapsed().as_secs_f64();

    // observed information: A_fisher - sum_active (e - n p) at (si, beta_col) & (beta_col, si)
    let (mut iobs, _) = build(&theta);
    for &(si, ot, e, n) in &meta {
        if ot.is_some() {
            let r = e - n*sigmoid(eta_of(&theta, si, ot));
            iobs[(si, beta_col)] -= r; iobs[(beta_col, si)] -= r;
        }
    }
    let cov = iobs.try_inverse().expect("singular observed info");
    let se = |i: usize| cov[(i,i)].max(0.0).sqrt();

    println!("=== Rust baseline-risk (Achana/bnma): {}  ref={} ===", args[1], reff);
    println!("mx = {:.5}   ({} studies, {} non-ref treatments, np={})", mx, ns, nt, np);
    println!("B  = {:.5} (se {:.5})", theta[beta_col], se(beta_col));
    for t in &nonref {
        let c = dcol(tidx[t.as_str()]);
        println!("d[{:<11}] = {:8.5} (se {:.5})", t, theta[c], se(c));
    }
    println!("solve time: {:.3} ms", secs*1000.0);
}
