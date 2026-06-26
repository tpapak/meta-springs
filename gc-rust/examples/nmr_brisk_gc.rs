// 2-D grand-canonical exchangeable baseline-risk in Rust: integrate (tau2, sigmaB2) instead
// of maximising. Analogue of Haskell `nmaRegressionBaselineRiskBinExchGC`. Fit the exchangeable
// mode (Fisher scoring + EM for tau2/sigmaB2), linearise once, then sum the exact Gaussian
// partition function over the (tau2, sigmaB2) grid. Lifts sigmaB2 off the REML boundary.
// Usage: nmr_brisk_gc <data.json (study,treatment,events,n)> <ref-treatment>
use nalgebra::{DMatrix, DVector, Cholesky};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::time::Instant;

#[derive(Deserialize)]
struct BinArm { study: String, treatment: String, events: f64, n: f64 }
fn sigmoid(x: f64) -> f64 { if x>=0.0 {1.0/(1.0+(-x).exp())} else {let e=x.exp(); e/(1.0+e)} }
fn lphn(s: f64, t: f64) -> f64 { if t<=0.0 { -1e300 } else { -t/(2.0*s*s) - 0.5*t.ln() } }

struct GcOut { sig_em: f64, gc_mode: f64, gc_mean: f64, gc_tau: f64, gc_b: f64,
               // per non-ref treatment: (name, EM slope, EM sd, GC slope mean, GC slope sd)
               bt: Vec<(String, f64, f64, f64, f64)>, ms: f64 }

fn main() {
    let args: Vec<String> = std::env::args().collect();
    // batch mode: nmr_brisk_gc --batch <dir> <ref> <nreps>  -> one line "em mode mean" per rep
    if args.len() >= 5 && args[1] == "--batch" {
        let (dir, reff, n) = (&args[2], &args[3], args[4].parse::<usize>().unwrap());
        for i in 1..=n {
            let path = format!("{}/rep_{:03}.json", dir, i);
            let arms: Vec<BinArm> = match std::fs::read_to_string(&path) {
                Ok(s) => serde_json::from_str(&s).unwrap(), Err(_) => continue };
            let r = fit_gc(&arms, reff);
            // per rep: sig(em,mode,mean) tau b  then for each treatment: em_slope em_sd gc_slope gc_sd
            let mut line = format!("{:.6} {:.6} {:.6} {:.6} {:.6}", r.sig_em, r.gc_mode, r.gc_mean, r.gc_tau, r.gc_b);
            for (_t, em, emsd, gc, gcsd) in &r.bt { line.push_str(&format!(" {:.6} {:.6} {:.6} {:.6}", em, emsd, gc, gcsd)); }
            println!("{}", line);
        }
        return;
    }
    let arms: Vec<BinArm> = serde_json::from_str(&std::fs::read_to_string(&args[1]).unwrap()).unwrap();
    let reff = args[2].clone();
    let r = fit_gc(&arms, &reff);
    println!("=== Rust 2-D GC exchangeable baseline-risk: {}  ref={} ===", args[1], reff);
    println!("sigmaB2:  EM(REML) = {:.5} (boundary)   GC mode = {:.5}   GC mean = {:.5}", r.sig_em, r.gc_mode, r.gc_mean);
    println!("tau2:     GC mean = {:.5}", r.gc_tau);
    println!("common B: GC mean = {:.5}", r.gc_b);
    println!("  {:<13} {:>9} {:>8}  | {:>9} {:>8}", "treatment", "EM slope", "EM sd", "GC slope", "GC sd");
    for (t, em, emsd, gc, gcsd) in &r.bt {
        println!("  b[{:<11}] {:+9.5} {:8.5}  | {:+9.5} {:8.5}", t, em, emsd, gc, gcsd); }
    println!("GC time: {:.1} ms", r.ms);
}

fn fit_gc(arms: &[BinArm], reff: &str) -> GcOut {
    let reff = reff.to_string();
    let studies: Vec<String> = arms.iter().map(|a| a.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats:  Vec<String> = arms.iter().map(|a| a.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let nonref: Vec<String> = treats.iter().filter(|t| **t!=reff).cloned().collect();
    let ns=studies.len(); let nt=nonref.len(); let np=ns+2*nt+1;
    let sidx: BTreeMap<&str,usize> = studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx: BTreeMap<&str,usize> = nonref.iter().enumerate().map(|(i,t)|(t.as_str(),i)).collect();
    let dcol=|t:usize| ns+t; let bcol=|t:usize| ns+nt+t; let ccol=ns+2*nt;
    let meta: Vec<(usize,Option<usize>,f64,f64)> = arms.iter().map(|a|
        (sidx[a.study.as_str()], if a.treatment==reff {None} else {Some(tidx[a.treatment.as_str()])}, a.events, a.n)).collect();
    let mut clo=vec![0.0;ns];
    for a in arms { if a.treatment==reff { let (mut r,mut n)=(a.events,a.n); if r==0.0 {r=0.5;n=a.n+1.0;} clo[sidx[a.study.as_str()]]=((r/n)/(1.0-r/n)).ln(); } }
    let mx: f64 = clo.iter().sum::<f64>()/ns as f64;

    // jac/eta for an arm at theta
    let jac_of = |th:&DVector<f64>, si:usize, ot:Option<usize>| -> (f64, Vec<(usize,f64)>) {
        match ot { None=>(th[si], vec![(si,1.0)]),
            Some(t)=>{let bb=th[bcol(t)]; (th[si]+th[dcol(t)]+bb*(th[si]-mx), vec![(si,1.0+bb),(dcol(t),1.0),(bcol(t),th[si]-mx)])} }
    };
    // assemble A=J'W_eff J (+ridge 1/sig2), b=J'W_eff z  for given (working z,w) and t2,sig2
    let assemble = |jacs:&Vec<(Vec<(usize,f64)>,f64,f64)>, t2:f64, sig2:f64| -> (DMatrix<f64>, DVector<f64>, f64, f64) {
        let mut a=DMatrix::<f64>::zeros(np,np); let mut b=DVector::<f64>::zeros(np);
        let (mut slv, mut zwz)=(0.0,0.0);
        for (jac,z,w) in jacs { let we=1.0/(1.0/w+t2/2.0); slv+=(1.0/w+t2/2.0).ln(); zwz+=we*z*z;
            for &(i,vi) in jac { b[i]+=we*vi*z; for &(j,vj) in jac { a[(i,j)]+=we*vi*vj; } } }
        let inv=1.0/sig2;
        for t in 0..nt { a[(bcol(t),bcol(t))]+=inv; a[(ccol,ccol)]+=inv; a[(bcol(t),ccol)]-=inv; a[(ccol,bcol(t))]-=inv; }
        (a,b,slv,zwz)
    };
    let loglik=|th:&DVector<f64>, sig2:f64| -> f64 {
        let mut ll=0.0;
        for &(si,ot,e,n) in &meta { let (eta,_)=jac_of(th,si,ot); let p=sigmoid(eta).clamp(1e-12,1.0-1e-12); ll+=e*p.ln()+(n-e)*(1.0-p).ln(); }
        if nt>0 { let bb=th[ccol]; for t in 0..nt { let d=th[bcol(t)]-bb; ll-=0.5/sig2*d*d; } }
        ll
    };

    // ---- exchangeable mode: damped Fisher scoring + EM(tau2, sigmaB2) ----
    let t0=Instant::now();
    let mut th=DVector::<f64>::zeros(np); for i in 0..ns { th[i]=clo[i]; }
    let (mut t2, mut sig2)=(0.1_f64, 0.1_f64);
    for _ in 0..200 {
        // current binomial linearisation
        let jacs: Vec<(Vec<(usize,f64)>,f64,f64)> = meta.iter().map(|&(si,ot,e,n)|{
            let (eta,jac)=jac_of(&th,si,ot); let p=sigmoid(eta); let w=(n*p*(1.0-p)).max(1e-9);
            let jt:f64=jac.iter().map(|(i,v)| v*th[*i]).sum(); (jac, jt+(e-n*p)/w, w)}).collect();
        let (a,b,_,_)=assemble(&jacs,t2,sig2);
        let chol=Cholesky::new(a).expect("spd"); let newton=chol.solve(&b);
        let dir=&newton-&th; let ll0=loglik(&th,sig2);
        let mut al=1.0; let mut cand=th.clone();
        for _ in 0..40 { cand=&th+&dir*al; let lc=loglik(&cand,sig2); if lc.is_finite()&&lc>=ll0-1e-9 {break;} al*=0.5; }
        let step=(&cand-&th).amax(); th=cand;
        // EM at the new point
        let jacs2: Vec<(Vec<(usize,f64)>,f64,f64)> = meta.iter().map(|&(si,ot,e,n)|{
            let (eta,jac)=jac_of(&th,si,ot); let p=sigmoid(eta); let w=(n*p*(1.0-p)).max(1e-9);
            let jt:f64=jac.iter().map(|(i,v)| v*th[*i]).sum(); (jac, jt+(e-n*p)/w, w)}).collect();
        let (a2,_,_,_)=assemble(&jacs2,t2,sig2); let cov=Cholesky::new(a2).unwrap().inverse();
        let t2arm=t2/2.0; let (mut s1,mut s2)=(0.0,0.0); let na=meta.len() as f64;
        for (jac,z,w) in &jacs2 { let fit:f64=jac.iter().map(|(i,v)| v*th[*i]).sum(); let r=z-fit;
            let bsh=w*t2arm/(1.0+w*t2arm); let mut qf=0.0; for &(i,vi) in jac { for &(j,vj) in jac { qf+=vi*cov[(i,j)]*vj; } }
            s1+=(bsh*r).powi(2); s2+=(1.0-bsh)*t2arm+bsh*bsh*qf; }
        let t2n=(2.0*(s1/na+s2/na)).max(1e-8);
        let mut sg=0.0; for t in 0..nt { let m=th[bcol(t)]-th[ccol]; sg+=m*m+cov[(bcol(t),bcol(t))]+cov[(ccol,ccol)]-2.0*cov[(bcol(t),ccol)]; }
        let sgn=(sg/nt as f64).max(1e-8);
        let conv= step<1e-9 && (t2-t2n).abs()<1e-9 && (sig2-sgn).abs()<1e-9; t2=t2n; sig2=sgn;
        if conv {break;}
    }
    let sig_em=sig2;
    // freeze the linearisation at the mode
    let jacs: Vec<(Vec<(usize,f64)>,f64,f64)> = meta.iter().map(|&(si,ot,e,n)|{
        let (eta,jac)=jac_of(&th,si,ot); let p=sigmoid(eta); let w=(n*p*(1.0-p)).max(1e-9);
        let jt:f64=jac.iter().map(|(i,v)| v*th[*i]).sum(); (jac, jt+(e-n*p)/w, w)}).collect();
    // EM/grouped-REML slope point + SE: cov = (Fisher + ridge 1/sigmaB2)^-1 at the mode.
    // When sigmaB2 collapses to the boundary the ridge -> inf and these SEs are artificially tiny.
    let (a_em,_,_,_)=assemble(&jacs, t2, sig_em);
    let cov_em=Cholesky::new(a_em).unwrap().inverse();
    let em_slope: Vec<(f64,f64)> = (0..nt).map(|t| (th[bcol(t)], cov_em[(bcol(t),bcol(t))].max(0.0).sqrt())).collect();

    // ---- 2-D grand canonical grid ----
    let ngrid=26usize; let nsig=26usize;
    let tau2hi=(t2*12.0).max(0.02);
    let ts: Vec<f64>=(1..=ngrid).map(|i| tau2hi*(i as f64-0.5)/ngrid as f64).collect();
    let (sighi,slo)=(1.0_f64,1.0_f64/1e4);
    let ss: Vec<f64>=(1..=nsig).map(|j| slo*(sighi/slo).powf((j as f64-0.5)/nsig as f64)).collect();
    let mut lp=vec![vec![0.0;nsig];ngrid]; let mut thc=vec![vec![DVector::<f64>::zeros(np);nsig];ngrid];
    let mut svar=vec![vec![vec![0.0;nt];nsig];ngrid];   // per-cell slope-column variances cov[bcol,bcol]
    let mut mxlp=-1e300;
    for i in 0..ngrid { for j in 0..nsig {
        let (a,b,slv,zwz)=assemble(&jacs,ts[i],ss[j]);
        let chol=Cholesky::new(a).expect("spd"); let theta=chol.solve(&b);
        let logdet=2.0*chol.l().diagonal().iter().map(|x| x.ln()).sum::<f64>();
        let bx:f64=b.iter().zip(theta.iter()).map(|(bi,ti)| bi*ti).sum();
        let lz=-0.5*(slv+logdet+(zwz-bx));
        lp[i][j]=lz - 0.5*nt as f64*ss[j].ln() + lphn(0.5,ts[i]) + lphn(0.5,ss[j]) + ss[j].ln();
        let cov=chol.inverse(); for t in 0..nt { svar[i][j][t]=cov[(bcol(t),bcol(t))]; }
        thc[i][j]=theta; if lp[i][j]>mxlp {mxlp=lp[i][j];}
    }}
    let mut tot=0.0; let mut w=vec![vec![0.0;nsig];ngrid];
    for i in 0..ngrid { for j in 0..nsig { w[i][j]=(lp[i][j]-mxlp).exp(); tot+=w[i][j]; } }
    let (mut gc_sig,mut gc_tau,mut gc_b)=(0.0,0.0,0.0);
    let mut gc_bt=vec![0.0;nt]; let mut gc_m2=vec![0.0;nt];   // mixture mean and E[var+mean^2]
    let mut mars=vec![0.0;nsig]; let mut mart=vec![0.0;ngrid];
    for i in 0..ngrid { for j in 0..nsig { let p=w[i][j]/tot;
        gc_sig+=p*ss[j]; gc_tau+=p*ts[i]; gc_b+=p*thc[i][j][ccol];
        for t in 0..nt { let m=thc[i][j][bcol(t)]; gc_bt[t]+=p*m; gc_m2[t]+=p*(svar[i][j][t]+m*m); }
        mars[j]+=w[i][j]; mart[i]+=w[i][j]; } }
    // GC slope posterior SD = sqrt(mixture variance)
    let gc_sd: Vec<f64> = (0..nt).map(|t| (gc_m2[t]-gc_bt[t]*gc_bt[t]).max(0.0).sqrt()).collect();
    let smode=ss[(0..nsig).max_by(|&a,&b| mars[a].partial_cmp(&mars[b]).unwrap()).unwrap()];
    let secs=t0.elapsed().as_secs_f64();

    GcOut { sig_em, gc_mode: smode, gc_mean: gc_sig, gc_tau, gc_b,
            bt: nonref.iter().map(|t| { let k=tidx[t.as_str()];
                    (t.clone(), em_slope[k].0, em_slope[k].1, gc_bt[k], gc_sd[k]) }).collect(),
            ms: secs*1000.0 }
}
