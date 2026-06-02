// Fixed-effect network meta-regression (shared beta) by weighted least squares.
// Rust analogue of Data.Meta.NMA.nmaRegression. Reads arm-level continuous data
// (study, treatment, mean, sd, n) + a study covariate, solves the WLS with nalgebra.
// Usage: nmr_fe <data.json> <cov.json>
use nalgebra::{DMatrix, DVector};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct Arm { study: String, treatment: String, mean: f64, sd: f64, n: f64 }

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let arms: Vec<Arm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let cov: BTreeMap<String, f64> = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();

    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String>  = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff = treats[0].clone();
    let nonref: Vec<String> = treats.iter().filter(|t| **t != reff).cloned().collect();
    let ns = studies.len(); let nt = nonref.len(); let np = ns + nt + 1; let beta_col = ns + nt;
    let sidx: BTreeMap<&str, usize> = studies.iter().enumerate().map(|(i, s)| (s.as_str(), i)).collect();
    let tidx: BTreeMap<&str, usize> = nonref.iter().enumerate().map(|(i, t)| (t.as_str(), i)).collect();

    let t0 = Instant::now();
    let mut a = DMatrix::<f64>::zeros(np, np);
    let mut b = DVector::<f64>::zeros(np);
    for arm in &arms {
        let w = arm.n / (arm.sd * arm.sd);            // 1 / (sd^2/n)
        let mut row = vec![0.0; np];
        row[sidx[arm.study.as_str()]] = -1.0;
        if arm.treatment != reff {
            row[ns + tidx[arm.treatment.as_str()]] = 1.0;
            row[beta_col] = *cov.get(&arm.study).unwrap_or(&0.0);
        }
        for i in 0..np {
            if row[i] == 0.0 { continue; }
            b[i] += w * row[i] * arm.mean;
            for j in 0..np { if row[j] != 0.0 { a[(i, j)] += w * row[i] * row[j]; } }
        }
    }
    let theta = a.clone().lu().solve(&b).expect("singular");
    let elapsed = t0.elapsed();

    let beta = theta[beta_col];
    println!("rust nmr_fe: T={} studies={} arms={} np={}", treats.len(), ns, arms.len(), np);
    println!("beta = {:.5}", beta);
    for t in nonref.iter().take(3) { println!("  {}->{} = {:.5}", reff, t, theta[ns + tidx[t.as_str()]]); }
    println!("build+solve time = {:.3} ms", elapsed.as_secs_f64() * 1000.0);
}
