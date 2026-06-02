// BINARY (logit) network meta-regression in Rust: IRLS linearisation (binomial ->
// Gaussian working response) + RE (REML/EM) and GC (joint posterior), shared beta.
// Rust analogue of linearizeBinaryRegression + nmaRegressionBinRE / BinPosteriorB.
// Usage: nmr_bin <data.json (study,treatment,events,n)> <cov.json>
use nalgebra::{DMatrix, DVector, Cholesky};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct BinArm { study: String, treatment: String, events: f64, n: f64 }

fn sigmoid(x: f64) -> f64 { if x >= 0.0 { 1.0/(1.0+(-x).exp()) } else { let e=x.exp(); e/(1.0+e) } }
fn norm_cdf(x: f64) -> f64 {
    let s=x.signum(); let az=(x/2f64.sqrt()).abs(); let t=1.0/(1.0+0.3275911*az);
    let y=1.0-(((((1.061405429*t-1.453152027)*t)+1.421413741)*t-0.284496736)*t+0.254829592)*t*(-az*az).exp();
    0.5*(1.0+s*y)
}
// shared-beta design rows over (mu_s, d_t, beta); returns (rows, np, beta_col)
fn rows_shared(meta: &[(usize,Option<usize>,f64)], ns: usize, nt: usize) -> (Vec<Vec<f64>>, usize, usize) {
    let np = ns + nt + 1; let bc = ns + nt;
    let rows = meta.iter().map(|&(si,ti,x)| {
        let mut r = vec![0.0; np]; r[si] = -1.0;
        if let Some(t) = ti { r[ns+t] = 1.0; r[bc] = x; }
        r
    }).collect();
    (rows, np, bc)
}
fn solve(rows: &[Vec<f64>], ys: &[f64], w: &[f64], np: usize) -> (DVector<f64>, DMatrix<f64>, f64) {
    let mut a = DMatrix::<f64>::zeros(np, np); let mut b = DVector::<f64>::zeros(np);
    for ((row,&y),&wi) in rows.iter().zip(ys).zip(w) {
        for i in 0..np { if row[i]!=0.0 { b[i]+=wi*row[i]*y;
            for j in 0..np { if row[j]!=0.0 { a[(i,j)]+=wi*row[i]*row[j]; } } } }
    }
    let ch = Cholesky::new(a).expect("not SPD");
    (ch.solve(&b), ch.inverse(), 2.0*ch.l().diagonal().iter().map(|x| x.ln()).sum::<f64>())
}
fn quad(row: &[f64], cov: &DMatrix<f64>) -> f64 {
    let mut s=0.0; for i in 0..row.len() { if row[i]!=0.0 { for j in 0..row.len() { if row[j]!=0.0 { s+=row[i]*cov[(i,j)]*row[j]; } } } } s
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let arms: Vec<BinArm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let cov: BTreeMap<String,f64> = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String> = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff = treats[0].clone(); let nonref: Vec<String> = treats.iter().filter(|t| **t!=reff).cloned().collect();
    let ns=studies.len(); let nt=nonref.len();
    let sidx: BTreeMap<&str,usize> = studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx: BTreeMap<&str,usize> = nonref.iter().enumerate().map(|(i,t)|(t.as_str(),i)).collect();
    // arm meta: (study idx, Some(treat idx) or None for ref, covariate), plus (events, n)
    let meta: Vec<(usize,Option<usize>,f64)> = arms.iter().map(|a| {
        (sidx[a.study.as_str()], if a.treatment==reff {None} else {Some(tidx[a.treatment.as_str()])},
         *cov.get(&a.study).unwrap_or(&0.0)) }).collect();
    let en: Vec<(f64,f64)> = arms.iter().map(|a| (a.events, a.n)).collect();
    let (rows, np, bcol) = rows_shared(&meta, ns, nt);

    // ---- IRLS: linearise binomial arms at the shared-beta fit ----
    let t0 = Instant::now();
    let mut theta = DVector::<f64>::zeros(np);
    for _ in 0..100 {
        let (mut zs, mut ws) = (vec![], vec![]);
        for (row,&(e,n)) in rows.iter().zip(&en) {
            let eta: f64 = row.iter().zip(theta.iter()).map(|(r,t)| r*t).sum();
            let p=sigmoid(eta); let w=(n*p*(1.0-p)).max(1e-9); zs.push(eta+(e-n*p)/w); ws.push(w);
        }
        let (th,_,_) = solve(&rows, &zs, &ws, np);
        if (&th-&theta).amax() < 1e-10 { theta=th; break; } theta=th;
    }
    // frozen working response (z, working variance 1/w)
    let (mut z, mut v) = (vec![], vec![]);
    for (row,&(e,n)) in rows.iter().zip(&en) {
        let eta: f64 = row.iter().zip(theta.iter()).map(|(r,t)| r*t).sum();
        let p=sigmoid(eta); let w=(n*p*(1.0-p)).max(1e-9); z.push(eta+(e-n*p)/w); v.push(1.0/w);
    }
    println!("rust binary NMR (logit, IRLS+spring): studies={} np={}  [linearise {:.2} ms]", ns, np, t0.elapsed().as_secs_f64()*1000.0);

    // ---- RE on the working response (REML/EM) ----
    let t1=Instant::now(); let nf=rows.len() as f64; let mut tau2=0.1;
    for _ in 0..300 {
        let t2a=tau2/2.0; let w: Vec<f64>=v.iter().map(|vi| 1.0/(vi+t2a)).collect();
        let (th,cov,_)=solve(&rows,&z,&w,np);
        let (mut sd2,mut svd)=(0.0,0.0);
        for ((row,&y),&vi) in rows.iter().zip(&z).zip(&v) {
            let fitted: f64=row.iter().zip(th.iter()).map(|(r,t)|r*t).sum(); let resid=y-fitted;
            let b=(1.0/vi)*t2a/(1.0+(1.0/vi)*t2a); sd2+=(b*resid).powi(2); svd+=(1.0-b)*t2a+b*b*quad(row,&cov);
        }
        let t2n=(2.0*(sd2/nf+svd/nf)).max(1e-8); if (t2n-tau2).abs()<1e-9 {tau2=t2n;break;} tau2=t2n;
    }
    let beta_re={ let t2a=tau2/2.0; let w:Vec<f64>=v.iter().map(|vi|1.0/(vi+t2a)).collect(); solve(&rows,&z,&w,np).0[bcol] };
    println!("RE : beta={:.5} tau2={:.5}  [{:.2} ms]", beta_re, tau2, t1.elapsed().as_secs_f64()*1000.0);

    // ---- GC (shared) joint posterior on the working response ----
    let t2=Instant::now(); let tau2hi=(tau2*12.0).max(0.02); let ng=200usize;
    let ts: Vec<f64>=(1..=ng).map(|i| tau2hi*(i as f64-0.5)/ng as f64).collect();
    let (mut lz,mut bh,mut bv)=(vec![],vec![],vec![]);
    for &tt in &ts {
        let t2a=tt/2.0; let w:Vec<f64>=v.iter().map(|vi|1.0/(vi+t2a)).collect();
        let (th,cov,logdet)=solve(&rows,&z,&w,np);
        let sumlogv:f64=v.iter().map(|vi|(vi+t2a).ln()).sum();
        let ywy:f64=z.iter().zip(&v).map(|(y,vi)|y*y/(vi+t2a)).sum();
        let mut bty=0.0; for ((row,&y),&vi) in rows.iter().zip(&z).zip(&v){ let wi=1.0/(vi+t2a); for i in 0..np { if row[i]!=0.0 {bty+=wi*row[i]*y*th[i];}}}
        lz.push(-0.5*(sumlogv+logdet+(ywy-bty))); bh.push(th[bcol]); bv.push(cov[(bcol,bcol)]);
    }
    let mx=lz.iter().cloned().fold(f64::MIN,f64::max);
    let ws:Vec<f64>=lz.iter().map(|l|(l-mx).exp()).collect(); let tot:f64=ws.iter().sum(); let ws:Vec<f64>=ws.iter().map(|w|w/tot).collect();
    let bmean:f64=ws.iter().zip(&bh).map(|(w,b)|w*b).sum();
    let bvar:f64=ws.iter().zip(bh.iter()).zip(&bv).map(|((w,b),vv)|w*(vv+b*b)).sum::<f64>()-bmean*bmean;
    let t2mean:f64=ws.iter().zip(&ts).map(|(w,t)|w*t).sum();
    let bsd=bvar.max(1e-12).sqrt();
    let cdf=|x:f64| ws.iter().zip(bh.iter()).zip(&bv).map(|((w,m),vv)| w*norm_cdf((x-m)/vv.max(1e-300).sqrt())).sum::<f64>();
    let q=|p:f64|{let(mut lo,mut hi)=(bmean-12.0*bsd,bmean+12.0*bsd); for _ in 0..60{let m=(lo+hi)/2.0; if cdf(m)<p{lo=m}else{hi=m}} (lo+hi)/2.0};
    println!("GC : beta mean={:.5} sd={:.5} 95%=[{:.4},{:.4}]  tau2 mean={:.5}  [{:.2} ms]",
             bmean,bsd,q(0.025),q(0.975),t2mean,t2.elapsed().as_secs_f64()*1000.0);
}
