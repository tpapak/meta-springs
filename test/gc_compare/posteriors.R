#!/usr/bin/env Rscript
# Produce posterior-overlay plots:
#  (A) Binary NMA (synth_041): tau and d_2 — spring GC vs multinma MCMC
#  (B) Pairwise binary (test/binary.json): tau and mu — spring GC vs bayesmeta

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
  library(bayesmeta)
  library(dplyr)
  library(htmltools)
})
options(mc.cores = parallel::detectCores())
set.seed(42)

# ---------------- (A) Binary NMA: synth_041 ----------------
cat("=== (A) synth_041 binary NMA: multinma MCMC with effect samples ===\n")
d <- fromJSON("test/synth_bin/synth_041.json")
d$study <- factor(d$study); d$treatment <- factor(d$treatment)
net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n,
                   trt_ref = levels(d$treatment)[1])
fit <- nma(net, trt_effects = "random",
           likelihood = "binomial", link = "logit",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1),
           iter = 4000, warmup = 1000, chains = 4, refresh = 0, seed = 42)

tau_samples <- as.data.frame(fit, pars = "tau")[[1]]
re <- relative_effects(fit, trt_ref = levels(d$treatment)[1])
d2_samples <- as.vector(re$sims[,,"d[2]"])

cat(sprintf("multinma samples: tau n=%d, d[2] n=%d\n",
            length(tau_samples), length(d2_samples)))
cat(sprintf("  tau: mean=%.4f median=%.4f\n", mean(tau_samples), median(tau_samples)))
cat(sprintf("  d[2]: mean=%+.4f median=%+.4f sd=%.4f\n",
            mean(d2_samples), median(d2_samples), sd(d2_samples)))

# GC posterior from gc_results.json
gc_all  <- fromJSON("test/gc_compare/gc_results.json", simplifyVector = FALSE)
gc_syn  <- Filter(function(x) x$name == "synth_041", gc_all$datasets)[[1]]
grid    <- do.call(rbind, lapply(gc_syn$grid_logZ_flat, function(r)
                         data.frame(tau2 = r$tau2, logZ = r$logZ)))
# Half-Normal(0,1) on tau -> log pi(tau2) = -(1/2)log(tau2) - tau2/2 + const
grid$lp <- grid$logZ - 0.5 * log(pmax(grid$tau2, 1e-12)) - 0.5 * grid$tau2
grid <- grid[grid$tau2 > 0, ]
grid$pr_t2 <- exp(grid$lp - max(grid$lp))
grid$pr_t2 <- grid$pr_t2 / sum(grid$pr_t2 * diff(c(0, grid$tau2)))  # density
# Transform to tau space: p_tau(tau) = p_tau2(tau^2) * 2*tau
grid$tau   <- sqrt(grid$tau2)
grid$pr_t  <- grid$pr_t2 * 2 * grid$tau

# Effect d_2 at GC mode: pick from effects_at_gc_mode (from=1, to=2).
# Approximate marginal as Gaussian(mean, sd) at the GC τ² mode.
eff_at_mode <- Filter(function(e) e$from == "1" && e$to == "2",
                      gc_syn$effects_at_gc_mode)[[1]]
gc_d2_mean <- eff_at_mode$mean
gc_d2_sd   <- sqrt(eff_at_mode$var)
cat(sprintf("GC d_2 (Gaussian approx at τ² mode): mean=%+.4f sd=%.4f\n",
            gc_d2_mean, gc_d2_sd))

# ---------------- (B) Pairwise: test/binary.json ----------------
cat("\n=== (B) test/binary.json pairwise binary: bayesmeta ===\n")
dp <- fromJSON("test/binary.json")
cc <- 0.5
hasZero <- (dp$eventsA == 0 | dp$eventsB == 0 | dp$eventsA == dp$nA | dp$eventsB == dp$nB)
eA <- ifelse(hasZero, dp$eventsA + cc, dp$eventsA)
eB <- ifelse(hasZero, dp$eventsB + cc, dp$eventsB)
nA <- ifelse(hasZero, dp$nA + 2*cc, dp$nA)
nB <- ifelse(hasZero, dp$nB + 2*cc, dp$nB)
pA <- eA / nA; pB <- eB / nB
logOR <- log((pB/(1-pB)) / (pA/(1-pA)))
se    <- sqrt(1/eA + 1/(nA - eA) + 1/eB + 1/(nB - eB))

bm <- bayesmeta(y = logOR, sigma = se, labels = dp$study,
                mu.prior.mean = 0, mu.prior.sd = 100,
                tau.prior = function(t) 2 * t)   # flat-in-tau2

# bayesmeta posterior densities on a grid
bm_tau_grid <- seq(1e-3, 4, length.out = 1000)
bm_tau_den  <- bm$dposterior(tau = bm_tau_grid)
bm_mu_grid  <- seq(-3, 4, length.out = 800)
bm_mu_den   <- bm$dposterior(mu = bm_mu_grid)

cat(sprintf("bayesmeta:\n"))
cat(sprintf("  tau mode=%.4f mean=%.4f\n",
            bm$summary["mode","tau"], bm$summary["mean","tau"]))
cat(sprintf("  mu  mean=%+.4f 95%% CI (%+.4f, %+.4f)\n",
            bm$summary["mean","mu"],
            bm$summary["95% lower","mu"], bm$summary["95% upper","mu"]))

# GC for the pairwise: run BayesmetaCompare-equivalent via the Haskell runner's
# output — but we already have gc_results.json only for the NMAs.
# Write a quick Haskell dump for the pairwise posterior, then read it back.
# (Simplest path: produce it inline with bayesmeta_binary_compare.R results + GC JSON.)
# Here we re-compute the GC posterior from the grid that we can produce via a
# tiny wrapper: reuse the Haskell BayesmetaCompare output to dump a small JSON
# (see helper section below). For now expect test/gc_compare/pairwise_gc.json.
gc_pair_path <- "test/gc_compare/pairwise_gc.json"
if (!file.exists(gc_pair_path)) {
  cat(sprintf("\n!! %s not found; run the helper first:\n", gc_pair_path))
  cat("   stack runghc --package meta-analysis -- test/gc_compare/RunPairwiseGC.hs\n")
  quit(status = 1)
}
gpair <- fromJSON(gc_pair_path, simplifyVector = FALSE)
pgrid <- do.call(rbind, lapply(gpair$grid, function(r)
                       data.frame(tau2 = r$tau2, logZ = r$logZ)))
# Bayesmeta uses a flat-in-tau² prior (density 2τ in τ-space) by our choice.
# Match that for GC (no reweight).
pgrid$pr_t2 <- exp(pgrid$logZ - max(pgrid$logZ))
pgrid$pr_t2 <- pgrid$pr_t2 / sum(pgrid$pr_t2 * diff(c(0, pgrid$tau2)))
pgrid$tau   <- sqrt(pgrid$tau2)
pgrid$pr_t  <- pgrid$pr_t2 * 2 * pgrid$tau

# GC pairwise mu at τ² mode
gc_mu_mean <- gpair$mu_at_mode$mean
gc_mu_sd   <- sqrt(gpair$mu_at_mode$var)

# ---------------- HTML output ----------------
as_js <- function(x) toJSON(x, auto_unbox = TRUE, na = "null", digits = 6)

payload <- list(
  nma = list(
    name        = "synth_041",
    tau_samples = tau_samples,
    d2_samples  = d2_samples,
    gc_tau_grid = grid$tau, gc_tau_dens = grid$pr_t,
    gc_d2_mean  = gc_d2_mean, gc_d2_sd = gc_d2_sd
  ),
  pair = list(
    name        = "test/binary.json",
    bm_tau_grid = bm_tau_grid, bm_tau_dens = bm_tau_den,
    bm_mu_grid  = bm_mu_grid,  bm_mu_dens  = bm_mu_den,
    bm_mu_mean  = as.numeric(bm$summary["mean","mu"]),
    bm_mu_sd    = as.numeric(bm$summary["sd","mu"]),
    gc_tau_grid = pgrid$tau, gc_tau_dens = pgrid$pr_t,
    gc_mu_mean  = gc_mu_mean, gc_mu_sd = gc_mu_sd
  )
)

html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Posterior overlays — GC vs multinma (NMA) and bayesmeta (pairwise)</title>
<style>
  body { font: 14px/1.5 -apple-system, Helvetica, Arial, sans-serif;
         max-width: 1080px; margin: 2rem auto; padding: 0 1rem; }
  h1, h2 { line-height: 1.2; }
  h2 { margin-top: 1.6rem; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  .row { display: flex; gap: 20px; flex-wrap: wrap; }
  .plot { flex: 1 1 420px; min-height: 380px; background: #fbfbfd;
          border: 1px solid #e4e6ea; border-radius: 6px; padding: 10px; }
  .note { color: #666; font-size: 0.9em; }
</style>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
</head><body>
<h1>Posterior distribution overlays</h1>

<h2>(A) Binary NMA: synth_041 (k=20, T=6). GC vs multinma MCMC</h2>
<p class="note">Both with half-Normal(0,1) prior on τ. GC τ shown as posterior density from the grand-canonical grid; GC d<sub>2</sub> as Gaussian approximation at the GC τ² mode. multinma as kernel density of MCMC samples.</p>
<div class="row">
  <div id="p_tau_nma" class="plot"></div>
  <div id="p_d2_nma"  class="plot"></div>
</div>

<h2>(B) Pairwise binary: test/binary.json. GC vs bayesmeta</h2>
<p class="note">Both with flat-in-τ² prior. bayesmeta uses normal approximation to logOR; GC uses exact binomial likelihood. GC μ shown as Gaussian approximation at the GC τ² mode.</p>
<div class="row">
  <div id="p_tau_pair" class="plot"></div>
  <div id="p_mu_pair"  class="plot"></div>
</div>

<script>
const data = ', as_js(payload), ';

function normalDensity(x, mean, sd) {
  const z = (x - mean) / sd;
  return Math.exp(-0.5 * z * z) / (sd * Math.sqrt(2 * Math.PI));
}
function gridFromRange(lo, hi, n) {
  const g = []; for (let i = 0; i < n; i++) g.push(lo + (hi-lo) * i / (n-1));
  return g;
}
function kdeSimple(samples, bandwidth, xs) {
  // Gaussian KDE with fixed bandwidth
  return xs.map(x => {
    let s = 0;
    for (const v of samples) s += normalDensity(x, v, bandwidth);
    return s / samples.length;
  });
}

// ---------- (A) NMA plots ----------
(function(){
  const tau_s = data.nma.tau_samples;
  const xs = gridFromRange(0, Math.max(3, Math.max(...tau_s)*1.05), 250);
  const bw = 1.06 * d3sd(tau_s) * Math.pow(tau_s.length, -1/5);
  function d3sd(arr){const m=arr.reduce((a,b)=>a+b)/arr.length;
    return Math.sqrt(arr.reduce((a,b)=>a+(b-m)*(b-m),0)/(arr.length-1));}
  const kde = kdeSimple(tau_s, bw, xs);
  const traces = [
    { x: xs, y: kde, mode: "lines", name: "multinma (MCMC)",
      line: {color: "#d73a49", width: 2}},
    { x: data.nma.gc_tau_grid, y: data.nma.gc_tau_dens, mode: "lines",
      name: "spring GC",
      line: {color: "#1f77b4", width: 2, dash: "solid"}}
  ];
  Plotly.newPlot("p_tau_nma", traces,
    { title: "τ posterior (synth_041)", xaxis:{title:"τ"}, yaxis:{title:"density"},
      margin:{t:40,b:40,l:50,r:10}, hovermode:"closest"},
    {displayModeBar:false});
})();

(function(){
  const d2_s = data.nma.d2_samples;
  const lo = Math.min(...d2_s, data.nma.gc_d2_mean - 4*data.nma.gc_d2_sd);
  const hi = Math.max(...d2_s, data.nma.gc_d2_mean + 4*data.nma.gc_d2_sd);
  const xs = gridFromRange(lo, hi, 400);
  function d3sd(arr){const m=arr.reduce((a,b)=>a+b)/arr.length;
    return Math.sqrt(arr.reduce((a,b)=>a+(b-m)*(b-m),0)/(arr.length-1));}
  const bw = 1.06 * d3sd(d2_s) * Math.pow(d2_s.length, -1/5);
  const kde = kdeSimple(d2_s, bw, xs);
  const gcD = xs.map(x => normalDensity(x, data.nma.gc_d2_mean, data.nma.gc_d2_sd));
  Plotly.newPlot("p_d2_nma", [
    { x: xs, y: kde, mode: "lines", name: "multinma (MCMC)",
      line: {color: "#d73a49", width: 2}},
    { x: xs, y: gcD, mode: "lines", name: "spring GC (Gaussian @ τ² mode)",
      line: {color: "#1f77b4", width: 2}}
  ],
  { title: "d_2 posterior (synth_041, vs ref treatment 1)",
    xaxis:{title:"d_2"}, yaxis:{title:"density"},
    margin:{t:40,b:40,l:50,r:10}, hovermode:"closest"},
  {displayModeBar:false});
})();

// ---------- (B) Pairwise plots ----------
Plotly.newPlot("p_tau_pair", [
  { x: data.pair.bm_tau_grid, y: data.pair.bm_tau_dens, mode: "lines",
    name: "bayesmeta (normal-approx)", line:{color:"#d73a49", width: 2}},
  { x: data.pair.gc_tau_grid, y: data.pair.gc_tau_dens, mode: "lines",
    name: "spring GC (exact binomial)", line:{color:"#1f77b4", width: 2}}
  ],
  { title: "τ posterior (binary.json)",
    xaxis:{title:"τ"}, yaxis:{title:"density"},
    margin:{t:40,b:40,l:50,r:10}, hovermode:"closest"},
  {displayModeBar:false});

(function(){
  const gcD = data.pair.bm_mu_grid.map(x =>
     normalDensity(x, data.pair.gc_mu_mean, data.pair.gc_mu_sd));
  Plotly.newPlot("p_mu_pair", [
    { x: data.pair.bm_mu_grid, y: data.pair.bm_mu_dens, mode: "lines",
      name: "bayesmeta", line: {color: "#d73a49", width: 2}},
    { x: data.pair.bm_mu_grid, y: gcD, mode: "lines",
      name: "spring GC (Gaussian @ τ² mode)",
      line: {color: "#1f77b4", width: 2}}
  ],
  { title: "μ posterior (binary.json, logOR of B vs A)",
    xaxis:{title:"μ"}, yaxis:{title:"density"},
    margin:{t:40,b:40,l:50,r:10}, hovermode:"closest"},
  {displayModeBar:false});
})();
</script>
</body></html>')

writeLines(html, "test/gc_compare/posteriors.html")
cat("\nWrote: test/gc_compare/posteriors.html\n")
