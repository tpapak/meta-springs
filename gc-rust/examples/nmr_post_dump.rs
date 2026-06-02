// Dump the Rust GC (shared-beta) posterior densities to JSON for the HTML overlay.
// Usage: nmr_post_dump <data.json> <cov.json> <out.json>
use nalgebra::{DMatrix, DVector, Cholesky};
use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
#[derive(Deserialize)]
struct Arm { study: String, treatment: String, mean: f64, sd: f64, n: f64 }
fn norm_pdf(m:f64,v:f64,x:f64)->f64{ (-(x-m).powi(2)/(2.0*v)).exp()/(2.0*std::f64::consts::PI*v).sqrt() }
fn norm_cdf(x:f64)->f64{ let s=x.signum(); let az=(x/2f64.sqrt()).abs(); let t=1.0/(1.0+0.3275911*az);
    let y=1.0-(((((1.061405429*t-1.453152027)*t)+1.421413741)*t-0.284496736)*t+0.254829592)*t*(-az*az).exp(); 0.5*(1.0+s*y) }
fn main(){
    let a:Vec<String>=std::env::args().collect();
    let arms:Vec<Arm>=serde_json::from_str(&std::fs::read_to_string(&a[1]).unwrap()).unwrap();
    let cov:BTreeMap<String,f64>=serde_json::from_str(&std::fs::read_to_string(&a[2]).unwrap()).unwrap();
    let studies:Vec<String>=arms.iter().map(|x|x.study.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let treats:Vec<String>=arms.iter().map(|x|x.treatment.clone()).collect::<BTreeSet<_>>().into_iter().collect();
    let reff=treats[0].clone(); let nonref:Vec<String>=treats.iter().filter(|t|**t!=reff).cloned().collect();
    let ns=studies.len(); let nt=nonref.len(); let np=ns+nt+1; let bc=ns+nt;
    let sidx:BTreeMap<&str,usize>=studies.iter().enumerate().map(|(i,s)|(s.as_str(),i)).collect();
    let tidx:BTreeMap<&str,usize>=nonref.iter().enumerate().map(|(i,t)|(t.as_str(),i)).collect();
    let (mut rows,mut ys,mut vs)=(vec![],vec![],vec![]);
    for x in &arms { let mut r=vec![0.0;np]; r[sidx[x.study.as_str()]]=-1.0;
        if x.treatment!=reff { r[ns+tidx[x.treatment.as_str()]]=1.0; r[bc]=*cov.get(&x.study).unwrap_or(&0.0); }
        rows.push(r); ys.push(x.mean); vs.push(x.sd*x.sd/x.n); }
    // RE mode tau2 for grid scale
    let nf=rows.len() as f64; let mut tau2=0.1;
    let solve=|t2a:f64|->(DVector<f64>,DMatrix<f64>,f64){ let mut m=DMatrix::zeros(np,np); let mut b=DVector::zeros(np);
        for ((row,&y),&v) in rows.iter().zip(&ys).zip(&vs){ let w=1.0/(v+t2a); for i in 0..np{ if row[i]!=0.0 {b[i]+=w*row[i]*y; for j in 0..np{if row[j]!=0.0{m[(i,j)]+=w*row[i]*row[j];}}}}}
        let ch=Cholesky::new(m).unwrap(); (ch.solve(&b),ch.inverse(),2.0*ch.l().diagonal().iter().map(|x|x.ln()).sum::<f64>()) };
    for _ in 0..300 { let t2a=tau2/2.0; let (th,cov,_)=solve(t2a); let (mut s2,mut sv)=(0.0,0.0);
        for ((row,&y),&v) in rows.iter().zip(&ys).zip(&vs){ let f:f64=row.iter().zip(th.iter()).map(|(r,t)|r*t).sum(); let rr=y-f; let b=(1.0/v)*t2a/(1.0+(1.0/v)*t2a);
            s2+=(b*rr).powi(2); let mut q=0.0; for i in 0..np{if row[i]!=0.0{for j in 0..np{if row[j]!=0.0{q+=row[i]*cov[(i,j)]*row[j];}}}} sv+=(1.0-b)*t2a+b*b*q; }
        let t2n=(2.0*(s2/nf+sv/nf)).max(1e-8); if (t2n-tau2).abs()<1e-9{tau2=t2n;break;} tau2=t2n; }
    // GC grid (flat prior, matching nmaRegressionPosterior FlatT2)
    let tau2hi=(tau2*12.0).max(0.02); let ng=240usize; let dt=tau2hi/ng as f64;
    let ts:Vec<f64>=(1..=ng).map(|i|tau2hi*(i as f64-0.5)/ng as f64).collect();
    let (mut lz,mut bh,mut bv)=(vec![],vec![],vec![]);
    for &t2 in &ts{ let t2a=t2/2.0; let (th,cov,ld)=solve(t2a);
        let slv:f64=vs.iter().map(|v|(v+t2a).ln()).sum(); let ywy:f64=ys.iter().zip(&vs).map(|(y,v)|y*y/(v+t2a)).sum();
        let mut bty=0.0; for ((row,&y),&v) in rows.iter().zip(&ys).zip(&vs){let w=1.0/(v+t2a); for i in 0..np{if row[i]!=0.0{bty+=w*row[i]*y*th[i];}}}
        lz.push(-0.5*(slv+ld+(ywy-bty)) + (-2.0*t2 - 0.5*t2.ln())); bh.push(th[bc]); bv.push(cov[(bc,bc)]); }  // HalfNormal(0.5) tau prior (matches Haskell dump)
    let mx=lz.iter().cloned().fold(f64::MIN,f64::max); let ws:Vec<f64>=lz.iter().map(|l|(l-mx).exp()).collect();
    let tot:f64=ws.iter().sum(); let ws:Vec<f64>=ws.iter().map(|w|w/tot).collect();
    let bmean:f64=ws.iter().zip(&bh).map(|(w,b)|w*b).sum();
    let bvar:f64=ws.iter().zip(bh.iter()).zip(&bv).map(|((w,b),v)|w*(v+b*b)).sum::<f64>()-bmean*bmean; let bsd=bvar.max(1e-12).sqrt();
    let cdf=|x:f64| ws.iter().zip(bh.iter()).zip(&bv).map(|((w,m),v)|w*norm_cdf((x-m)/v.max(1e-300).sqrt())).sum::<f64>();
    let q=|p:f64|{let(mut lo,mut hi)=(bmean-12.0*bsd,bmean+12.0*bsd); for _ in 0..60{let m=(lo+hi)/2.0; if cdf(m)<p{lo=m}else{hi=m}}(lo+hi)/2.0};
    let t2mean:f64=ws.iter().zip(&ts).map(|(w,t)|w*t).sum();
    let modei=lz.iter().enumerate().fold((0,f64::MIN),|(mi,mv),(i,&l)|if l>mv{(i,l)}else{(mi,mv)}).0;
    // beta density on a grid
    let bgrid:Vec<f64>=(0..=200).map(|i| bmean-4.5*bsd+9.0*bsd*i as f64/200.0).collect();
    let bdens:Vec<f64>=bgrid.iter().map(|&x| ws.iter().zip(bh.iter()).zip(&bv).map(|((w,m),v)|w*norm_pdf(*m,*v,x)).sum()).collect();
    let t2dens:Vec<f64>=ws.iter().map(|w|w/dt).collect();
    let out=serde_json::json!({
        "beta":{"mean":bmean,"sd":bsd,"lo":q(0.025),"hi":q(0.975)},
        "tau2":{"mode":ts[modei],"mean":t2mean,"lo":0.0,"hi":0.0},
        "betaGrid":bgrid,"betaDens":bdens,"tau2Grid":ts,"tau2Dens":t2dens});
    std::fs::write(&a[3], serde_json::to_string(&out).unwrap()).unwrap();
    println!("wrote {} : beta mean={:.5} tau2 mode={:.5}", a[3], bmean, ts[modei]);
}
