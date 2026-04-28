#!/usr/bin/env Rscript
# Real binary NMA posterior overlay: nmadb_479650 (27 studies, 6 treatments).
# multinma MCMC vs spring GC (τ and all 5 effects vs reference).

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
  library(htmltools)
})
options(mc.cores = parallel::detectCores())
set.seed(42)

ds <- "test/nmadb/nmadb_479650.json"
cat(sprintf("=== %s: multinma MCMC ===\n", ds))
d <- fromJSON(ds)
# handle zero-event rows — keep them; multinma binomial handles zeros natively
d$study     <- factor(d$study)
d$treatment <- factor(d$treatment)
cat(sprintf("%d rows, %d studies, %d treatments\n",
            nrow(d), nlevels(d$study), nlevels(d$treatment)))

net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n,
                   trt_ref = levels(d$treatment)[1])

t0 <- Sys.time()
fit <- nma(net, trt_effects = "random",
           likelihood = "binomial", link = "logit",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1),
           iter = 6000, warmup = 1500, chains = 4, refresh = 0, seed = 42)
t1 <- Sys.time()
cat(sprintf("multinma wall: %.1f s\n",
            as.numeric(difftime(t1, t0, units = "secs"))))

tau_samples <- as.data.frame(fit, pars = "tau")[[1]]
re <- relative_effects(fit, trt_ref = levels(d$treatment)[1])
eff_names <- dimnames(re$sims)[[3]]
eff_samples <- setNames(
  lapply(eff_names, function(p) as.vector(re$sims[,,p])),
  eff_names
)
cat(sprintf("τ: mean=%.4f median=%.4f 95%% CI (%.4f, %.4f)\n",
            mean(tau_samples), median(tau_samples),
            quantile(tau_samples, 0.025), quantile(tau_samples, 0.975)))
for (p in eff_names) {
  v <- eff_samples[[p]]
  cat(sprintf("  %-6s: mean=%+.4f median=%+.4f sd=%.4f\n",
              p, mean(v), median(v), sd(v)))
}

# Spring GC — already dumped to real_bin_gc.json
gc <- fromJSON("test/gc_compare/real_bin_gc.json", simplifyVector = FALSE)
grid <- do.call(rbind, lapply(gc$grid, function(r)
                         data.frame(tau2 = r$tau2, logZ = r$logZ)))
# half-Normal(0,1) on tau -> τ² log prior = -(1/2) log(τ²) - τ²/2 + C
grid$lp    <- grid$logZ - 0.5 * log(pmax(grid$tau2, 1e-12)) - 0.5 * grid$tau2
grid       <- grid[grid$tau2 > 0, ]
grid$pr_t2 <- exp(grid$lp - max(grid$lp))
grid$pr_t2 <- grid$pr_t2 / sum(grid$pr_t2 * diff(c(0, grid$tau2)))
grid$tau   <- sqrt(grid$tau2)
grid$pr_t  <- grid$pr_t2 * 2 * grid$tau

# Effects at GC mode — all contrasts with from == "1"
gc_effs <- Filter(function(e) e$from == "1", gc$effect_pairs)
gc_map <- setNames(
  lapply(gc_effs, function(e) list(mean = e$mean, sd = sqrt(e$var))),
  sapply(gc_effs, function(e) e$to)
)

cat(sprintf("\nGC τ² mode = %.5f\n", gc$mode_tau2))
for (to in names(gc_map)) {
  e <- gc_map[[to]]
  cat(sprintf("  d_%s: mean=%+.4f sd=%.4f\n", to, e$mean, e$sd))
}

# ---------- HTML ----------
as_js <- function(x) toJSON(x, auto_unbox = TRUE, na = "null", digits = 6)

payload <- list(
  dataset = ds,
  n_studies = nlevels(d$study),
  n_trts    = nlevels(d$treatment),
  tau_samples = tau_samples,
  gc_tau_grid = grid$tau, gc_tau_dens = grid$pr_t,
  effects = lapply(eff_names, function(p) {
    tid <- sub("^d\\[(.*)\\]$", "\\1", p)
    gcE <- gc_map[[tid]]
    list(
      param = p, tid = tid,
      mn_samples = eff_samples[[p]],
      gc_mean    = if (!is.null(gcE)) gcE$mean else NA,
      gc_sd      = if (!is.null(gcE)) gcE$sd   else NA
    )
  })
)

html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Real NMA posteriors — nmadb_479650 (27 studies, 6 trts)</title>
<style>
  body { font: 14px/1.5 -apple-system, Helvetica, Arial, sans-serif;
         max-width: 1200px; margin: 2rem auto; padding: 0 1rem; }
  h1 { line-height: 1.2; font-size: 1.5rem; }
  h2 { margin-top: 1.6rem; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  .row { display: flex; gap: 18px; flex-wrap: wrap; }
  .plot { flex: 1 1 420px; min-height: 340px; background: #fbfbfd;
          border: 1px solid #e4e6ea; border-radius: 6px; padding: 8px; }
  .note { color: #666; font-size: 0.9em; }
</style>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
</head><body>
<h1>Real binary NMA — <code>nmadb_479650</code></h1>
<p class="note">27 studies, 6 treatments, 26 two-arm + 1 three-arm. multinma MCMC (half-Normal(0,1) on τ, 4 chains × 4 500 post-warmup) vs spring GC with full-Hessian 1-loop correction and half-Normal(0,1) prior on τ. GC effect posteriors shown as Gaussian approximation at the GC τ² posterior mode. All effects are d<sub>j</sub> vs reference treatment 1 (log-odds scale).</p>

<h2>τ posterior</h2>
<div class="row"><div id="p_tau" class="plot"></div></div>

<h2>Treatment effects (d<sub>j</sub> vs d<sub>1</sub>)</h2>
<div class="row" id="effects_row"></div>

<script>
const data = ', as_js(payload), ';

function normalDensity(x, m, s) {
  const z = (x - m) / s;
  return Math.exp(-0.5*z*z) / (s * Math.sqrt(2*Math.PI));
}
function sd(arr) { const m = arr.reduce((a,b)=>a+b)/arr.length;
  return Math.sqrt(arr.reduce((a,b)=>a+(b-m)*(b-m),0)/(arr.length-1)); }
function grid(lo, hi, n) { const g=[]; for(let i=0;i<n;i++) g.push(lo+(hi-lo)*i/(n-1)); return g; }
function kde(samples, bw, xs) {
  return xs.map(x => { let s=0; for (const v of samples) s += normalDensity(x, v, bw); return s/samples.length; });
}

// τ plot
(function(){
  const tau_s = data.tau_samples;
  const xs = grid(0, Math.max(...tau_s) * 1.05, 300);
  const bw = 1.06 * sd(tau_s) * Math.pow(tau_s.length, -1/5);
  Plotly.newPlot("p_tau", [
    { x: xs, y: kde(tau_s, bw, xs), mode: "lines", name: "multinma (MCMC)",
      line: {color: "#d73a49", width: 2}},
    { x: data.gc_tau_grid, y: data.gc_tau_dens, mode: "lines",
      name: "spring GC", line: {color: "#1f77b4", width: 2}}
  ], { title: "τ posterior", xaxis: {title: "τ"}, yaxis: {title: "density"},
       margin: {t: 40, b: 40, l: 50, r: 10}}, {displayModeBar: false});
})();

// One plot per effect
const row = document.getElementById("effects_row");
data.effects.forEach(eff => {
  const div = document.createElement("div");
  div.className = "plot"; div.id = "p_" + eff.param.replace(/[^a-zA-Z0-9]/g,"_");
  row.appendChild(div);
  const samples = eff.mn_samples;
  const lo_ = Math.min(...samples, (eff.gc_mean||0) - 4*(eff.gc_sd||1));
  const hi_ = Math.max(...samples, (eff.gc_mean||0) + 4*(eff.gc_sd||1));
  const xs  = grid(lo_, hi_, 300);
  const bw  = 1.06 * sd(samples) * Math.pow(samples.length, -1/5);
  const traces = [
    { x: xs, y: kde(samples, bw, xs), mode:"lines", name:"multinma",
      line: {color:"#d73a49", width:2}}
  ];
  if (eff.gc_mean != null && isFinite(eff.gc_sd)) {
    traces.push({
      x: xs, y: xs.map(x => normalDensity(x, eff.gc_mean, eff.gc_sd)),
      mode:"lines", name:"spring GC (Gaussian)",
      line: {color:"#1f77b4", width:2}
    });
  }
  Plotly.newPlot(div.id, traces,
    { title: eff.param, xaxis:{title:"log-odds"}, yaxis:{title:"density"},
      margin:{t:40,b:40,l:50,r:10}},
    {displayModeBar:false});
});
</script>
</body></html>')

writeLines(html, "test/gc_compare/real_posteriors.html")
cat("\nWrote: test/gc_compare/real_posteriors.html\n")
