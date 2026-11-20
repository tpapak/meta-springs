//! One site of a federated ML-NMR: evaluates ONLY its own data springs.
//!
//! A site never sends patients. It answers two kinds of query about its own
//! energy (log-likelihood; no prior: priors are applied once, by whoever
//! reweights at the end):
//!
//! * `"force"`: at one configuration θ, its total log-likelihood, force
//!   (gradient, P numbers) and stiffness (observed information, P×P);
//!   used to relax the joint network to equilibrium.
//! * `"energy"`: at N configurations θ_1..θ_N (regenerated locally from a
//!   shared seed, or sent), N numbers: log p(site data | θ_i); used for the
//!   exact (importance-corrected) posterior.
//!
//! Usage:  mlnmr_site --data site.json --query q.json --output a.json
//!
//! site.json:  { "link", "agd_likelihood", "p", "units": [{r,n,q,x}, ...] }
//! q.json:     { "mode": "force", "theta": [..P..] }
//!          |  { "mode": "energy", "thetas": [[..P..], ...] }

use clap::Parser;
use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::fs;

use gc_rust::ipd::{AgdLik, Energy, Gauss, Link, ParPrior, Problem, Unit};
use std::collections::BTreeMap;

#[derive(Parser, Debug)]
#[command(name = "mlnmr_site", about = "Federated ML-NMR: one site's data springs")]
struct Args {
    #[arg(long)]
    data: String,
    #[arg(long)]
    query: String,
    #[arg(long)]
    output: String,
}

#[derive(Deserialize)]
struct UnitIn {
    #[serde(default)]
    kind: String,
    #[serde(default)]
    r: f64,
    #[serde(default = "d_onef")]
    n: f64,
    #[serde(default = "d_one_usize")]
    q: usize,
    #[serde(default)]
    x: Vec<f64>,
    #[serde(default)]
    y: f64,
    #[serde(default)]
    se: f64,
    #[serde(default)]
    sig: usize,
}
fn d_onef() -> f64 { 1.0 }
fn d_one_usize() -> usize { 1 }

/// A site's equivalent spring for its Gaussian patients sharing one σ column:
/// the sufficient statistics (XᵀX, Xᵀy, yᵀy, n). Exact: same energy everywhere.
#[derive(Serialize)]
struct SuffOut {
    sig: usize,
    n: f64,
    xtx: Vec<f64>,
    xty: Vec<f64>,
    yty: f64,
}

#[derive(Serialize)]
struct EquivOut {
    /// one per σ column (study) among the site's patients
    suff: Vec<SuffOut>,
    /// number of non-patient units (AgD arms) the site holds; they are already
    /// aggregate and are shared as published
    other_units: usize,
}

#[derive(Deserialize)]
struct Site {
    link: String,
    #[serde(default = "d_lik")]
    agd_likelihood: String,
    p: usize,
    units: Vec<UnitIn>,
}
fn d_lik() -> String { "binomial".into() }

#[derive(Deserialize)]
struct Query {
    mode: String,
    #[serde(default)]
    theta: Vec<f64>,
    #[serde(default)]
    thetas: Vec<Vec<f64>>,
}

#[derive(Serialize)]
struct ForceOut {
    ll: f64,
    grad: Vec<f64>,
    hess: Vec<Vec<f64>>,
}

#[derive(Serialize)]
struct EnergyOut {
    ll: Vec<f64>,
}

fn main() {
    let a = Args::parse();
    let site: Site = serde_json::from_str(&fs::read_to_string(&a.data).expect("read site data")).expect("site JSON");
    let q: Query = serde_json::from_str(&fs::read_to_string(&a.query).expect("read query")).expect("query JSON");
    let p = site.p;
    let link = match site.link.as_str() {
        "logit" => Link::Logit,
        "probit" => Link::Probit,
        "identity" => Link::Logit, // Gaussian units ignore the link
        l => panic!("unsupported link '{l}'"),
    };
    let agd_lik = match site.agd_likelihood.as_str() {
        "binomial" | "bernoulli" => AgdLik::Binomial,
        "binomial2" | "bernoulli2" => AgdLik::Binomial2,
        l => panic!("unsupported AgD likelihood '{l}'"),
    };
    let units = site
        .units
        .into_iter()
        .map(|u| {
            assert_eq!(u.x.len(), u.q * p, "unit design size mismatch");
            let g = match u.kind.as_str() {
                "" | "binomial" => None,
                "ipd_normal" => Some(Box::new(Gauss::Ipd { y: u.y, sig: u.sig })),
                "arm_normal" => Some(Box::new(Gauss::Arm { y: u.y, se: u.se })),
                k => panic!("unknown unit kind '{k}'"),
            };
            Unit { r: u.r, n: u.n, q: u.q, x: u.x, g }
        })
        .collect::<Vec<Unit>>();
    // data springs only: flat priors, no random-effect springs
    let prob = Problem { p, link, agd_lik, units, priors: vec![ParPrior::Flat; p], re_groups: vec![] };
    let en = Energy::new(&prob, None);
    let out = match q.mode.as_str() {
        "force" => {
            assert_eq!(q.theta.len(), p, "theta length");
            let acc = en.accumulate(&q.theta);
            serde_json::to_string(&ForceOut {
                ll: acc.ll,
                grad: acc.grad.iter().copied().collect(),
                hess: (0..p).map(|i| (0..p).map(|j| acc.h_obs[(i, j)]).collect()).collect(),
            })
        }
        "energy" => {
            let ll: Vec<f64> = q.thetas.par_iter().map(|t| en.value(t).unwrap_or(f64::NEG_INFINITY)).collect();
            serde_json::to_string(&EnergyOut { ll })
        }
        "equivalent_spring" => {
            let mut by_sig: BTreeMap<usize, SuffOut> = BTreeMap::new();
            let mut other = 0usize;
            for u in &prob.units {
                match u.g.as_deref() {
                    Some(Gauss::Ipd { y, sig }) => {
                        let e = by_sig.entry(*sig).or_insert_with(|| SuffOut { sig: *sig, n: 0.0,
                            xtx: vec![0.0; p * p], xty: vec![0.0; p], yty: 0.0 });
                        let z = &u.x[..p];
                        for i in 0..p {
                            e.xty[i] += z[i] * y;
                            for j in 0..p { e.xtx[i * p + j] += z[i] * z[j]; }
                        }
                        e.yty += y * y;
                        e.n += 1.0;
                    }
                    _ => other += 1,
                }
            }
            serde_json::to_string(&EquivOut { suff: by_sig.into_values().collect(), other_units: other })
        }
        m => panic!("unknown mode '{m}' (force | energy | equivalent_spring)"),
    }
    .unwrap();
    fs::write(&a.output, out).expect("write output");
}
