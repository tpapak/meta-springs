// EXCHANGEABLE-beta BINARY network meta-regression in Rust: IRLS linearisation +
// exchangeable GC (2-D grid over tau2 and sigmaB2, slope-springs beta_t<->B).
// Rust analogue of nmaRegressionBinPosteriorB ExchangeableB.
// Usage: nmr_bin_exch <data.json (study,treatment,events,n)> <cov.json> [sigmaSD_bound]
use nalgebra::{DMatrix, DVector, Cholesky};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};

#[derive(Deserialize)]
struct BinArm { study: String, treatment: String, events: f64, n: f64 }
fn sigmoid(x: f64) -> f64 { if x>=0.0 {1.0/(1.0+(-x).exp())} else {let e=x.exp(); e/(1.0+e)} }

fn solve(rows: &[Vec<f64>], ys: &[f64], w: &[f64], np: usize, kadd: Option<&DMatrix<f64>>)
    -> (DVector<f64>, DMatrix<f64>, f64) {
    let mut a = DMatrix::<f64>::zeros(np,np); let mut b = DVector::<f64>::zeros(np);
    for ((row,&y),&wi) in rows.iter().zip(ys).zip(w) {
        for i in 0..np { if row[i]!=0.0 { b[i]+=wi*row[i]*y;
            for j in 0..np { if row[j]!=0.0 { a[(i,j)]+=wi*row[i]*row[j]; } } } } }
    if let Some(k)=kadd { a += k; }
    let ch = Cholesky::new(a).expect("not SPD");
    (ch.solve(&b), ch.inverse(), 2.0*ch.l().diagonal().iter().map(|x| x.ln()).sum::<f64>())
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let sig_bound: f64 = args.get(3).and_then(|s| s.parse().ok()).unwrap_or(0.9612);
    let arms: Vec<BinArm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let cov: BTreeMap<String,f64> = serde_json::from_str(&std::fs::read_to_string(&args[2]).unwrap()).unwrap();
    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats: Vec<String> = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff = treats[0].clone(); let nonref: Vec<String> = treats.iter().filter(|t| **t!=reff).cloned().collect();
    let ns=studies.len(); let nt=nonref.len();
    let sidx: BTreeMap<&str,usize> = studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx: BTreeMap<&str,usize> = nonref.iter().enumerate().map(|(i,t)|(t.as_str(),i)).collect();
    let meta: Vec<(usize,Option<usize>,f64,f64,f64)> = arms.iter().map(|a|
        (sidx[a.study.as_str()], if a.treatment==reff {None} else {Some(tidx[a.treatment.as_str()])},
         *cov.get(&a.study).unwrap_or(&0.0), a.events, a.n)).collect();

    // shared-beta rows for the IRLS linearisation
    let np_s = ns + nt + 1; let bc_s = ns + nt;
    let rows_s: Vec<Vec<f64>> = meta.iter().map(|&(si,ti,x,_,_)| {
        let mut r=vec![0.0;np_s]; r[si]=-1.0; if let Some(t)=ti {r[ns+t]=1.0; r[bc_s]=x;} r }).collect();
    let mut theta = DVector::<f64>::zeros(np_s);
    for _ in 0..100 {
        let (mut z,mut w)=(vec![],vec![]);
        for (row,&(_,_,_,e,n)) in rows_s.iter().zip(&meta) {
            let eta:f64=row.iter().zip(theta.iter()).map(|(r,t)|r*t).sum();
            let p=sigmoid(eta); let wi=(n*p*(1.0-p)).max(1e-9); z.push(eta+(e-n*p)/wi); w.push(wi); }
        let (th,_,_)=solve(&rows_s,&z,&w,np_s,None); if (&th-&theta).amax()<1e-10 {theta=th;break;} theta=th;
    }
    let (mut zs,mut vs)=(vec![],vec![]);
    for (row,&(_,_,_,e,n)) in rows_s.iter().zip(&meta) {
        let eta:f64=row.iter().zip(theta.iter()).map(|(r,t)|r*t).sum();
        let p=sigmoid(eta); let wi=(n*p*(1.0-p)).max(1e-9); zs.push(eta+(e-n*p)/wi); vs.push(1.0/wi); }

    // exchangeable design: mu(ns), d_t(nt), beta_t(nt), B(1)
    let np = ns + 2*nt + 1; let bbase = ns+nt; let bcol = |t:usize| bbase+t; let bigb = ns+2*nt;
    let rows: Vec<Vec<f64>> = meta.iter().map(|&(si,ti,x,_,_)| {
        let mut r=vec![0.0;np]; r[si]=-1.0; if let Some(t)=ti {r[ns+t]=1.0; r[bcol(t)]=x;} r }).collect();
    // coupling pattern K0 = sum_t (e_bt - e_B)(e_bt - e_B)^T  (scaled by 1/sigmaB2)
    let mut k0 = DMatrix::<f64>::zeros(np,np);
    for t in 0..nt { let (bt,bb)=(bcol(t),bigb); k0[(bt,bt)]+=1.0; k0[(bb,bb)]+=1.0; k0[(bt,bb)]-=1.0; k0[(bb,bt)]-=1.0; }

    // 2-D grid: tau2 (RE-ish scale) x sigmaB2 (log-spaced), priors HN(0.5) on tau, U(0,bound) on sigma_B
    let tau2hi=0.8; let ngt=160usize;
    let ts:Vec<f64>=(1..=ngt).map(|i| tau2hi*(i as f64-0.5)/ngt as f64).collect();
    let sighi=sig_bound*sig_bound; let ngs=40usize; let slo=sighi/1e4;
    let ss:Vec<f64>=(1..=ngs).map(|i| slo*(sighi/slo).powf((i as f64-0.5)/ngs as f64)).collect();
    let lp_tau=|t:f64| -t/(2.0*0.25) - 0.5*t.ln();              // HalfNormal(0.5) on tau, on tau2 scale
    let lp_sig=|s:f64| if s<sighi {-0.5*s.ln()} else {f64::MIN/4.0};  // Uniform(0,bound) on sigma_B

    let mut cells: Vec<(f64,f64,DVector<f64>,DMatrix<f64>)> = vec![];   // (tau2, sigma2, theta, cov)
    let mut lps: Vec<f64> = vec![];
    for &t2 in &ts { let t2a=t2/2.0; let w:Vec<f64>=vs.iter().map(|v| 1.0/(v+t2a)).collect();
        let sumlogv:f64=vs.iter().map(|v|(v+t2a).ln()).sum();
        let ywy:f64=zs.iter().zip(&vs).map(|(y,v)| y*y/(v+t2a)).sum();
        for &s in &ss {
            let k = &k0 * (1.0/s);
            let (th,cov,logdet)=solve(&rows,&zs,&w,np,Some(&k));
            let mut bty=0.0; for ((row,&y),&v) in rows.iter().zip(&zs).zip(&vs){let wi=1.0/(v+t2a); for i in 0..np{if row[i]!=0.0{bty+=wi*row[i]*y*th[i];}}}
            let logz = -0.5*(sumlogv+logdet+(ywy-bty)) - 0.5*(nt as f64)*s.ln();
            lps.push(logz + lp_tau(t2) + lp_sig(s) + s.ln());      // +ln s : log-grid Jacobian
            cells.push((t2,s,th,cov));
        }
    }
    let mx=lps.iter().cloned().fold(f64::MIN,f64::max);
    let ws:Vec<f64>=lps.iter().map(|l|(l-mx).exp()).collect(); let tot:f64=ws.iter().sum(); let ws:Vec<f64>=ws.iter().map(|w|w/tot).collect();
    let mean=|col:usize| ws.iter().zip(&cells).map(|(w,(_,_,th,_))| w*th[col]).sum::<f64>();
    let sd=|col:usize| { let m=mean(col); (ws.iter().zip(&cells).map(|(w,(_,_,th,cov))| w*(cov[(col,col)]+th[col]*th[col])).sum::<f64>()-m*m).max(0.0).sqrt() };
    println!("rust EXCHANGEABLE binary GC: studies={} treats={} np={} (sigmaSD bound={})", ns, treats.len(), np, sig_bound);
    println!("common B : {:.4} (sd {:.4})", mean(bigb), sd(bigb));
    for t in &nonref { let c=bcol(tidx[t.as_str()]); println!("  beta[{}] : {:.4} (sd {:.4})", t, mean(c), sd(c)); }
    let t2mean:f64=ws.iter().zip(&cells).map(|(w,(t,_,_,_))| w*t).sum();
    let s2mean:f64=ws.iter().zip(&cells).map(|(w,(_,s,_,_))| w*s).sum();
    println!("tau2 mean = {:.4} ; sigmaB2 mean = {:.4}", t2mean, s2mean);
}
