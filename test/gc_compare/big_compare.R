#!/usr/bin/env Rscript
# Compare spring GC vs multinma MCMC on nmadb_501194 (99 studies × 17 trts).
# Overlay τ posterior and all effect posteriors (d_j vs reference d_1).

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
})
options(mc.cores = parallel::detectCores())
set.seed(42)

ds <- "test/nmadb/nmadb_501194.json"
cat(sprintf("=== %s ===\n", ds))

# -- multinma --
d <- fromJSON(ds)
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
           iter = 4000, warmup = 1000, chains = 4, refresh = 0, seed = 42)
t1 <- Sys.time()
mn_secs <- as.numeric(difftime(t1, t0, units = "secs"))
cat(sprintf("multinma wall: %.1f s\n", mn_secs))

tau_samples <- as.data.frame(fit, pars = "tau")[[1]]
re <- relative_effects(fit, trt_ref = levels(d$treatment)[1])
eff_names <- dimnames(re$sims)[[3]]
eff_samples <- setNames(
  lapply(eff_names, function(p) as.vector(re$sims[,,p])),
  eff_names)
cat(sprintf("τ: mean=%.4f median=%.4f 95%% CI (%.4f, %.4f)\n",
            mean(tau_samples), median(tau_samples),
            quantile(tau_samples, 0.025), quantile(tau_samples, 0.975)))

# -- spring GC (already computed) --
gc <- fromJSON("test/gc_compare/biggest_nmadb_gc.json", simplifyVector = FALSE)
flat <- do.call(rbind, lapply(gc$grid, function(g) {
  do.call(rbind, lapply(g$effects, function(e) {
    data.frame(tau2 = g$tau2, logZ = g$logZ,
               from = e$from, to = e$to,
               mean = e$mean, var = e$var,
               stringsAsFactors = FALSE)
  }))
}))
tau2_grid <- unique(flat$tau2)

prior_logπ <- function(t2)
  ifelse(t2 > 0, -0.5 * log(t2) - 0.5 * t2, -Inf)

tau_df <- data.frame(tau2 = tau2_grid,
                     logZ = sapply(gc$grid, `[[`, "logZ"))
tau_df$lp <- tau_df$logZ + prior_logπ(tau_df$tau2)
tau_df    <- tau_df[tau_df$tau2 > 0, ]
t2s       <- tau_df$tau2
widths    <- c((t2s[2] - t2s[1]) / 2,
               (t2s[3:length(t2s)] - t2s[1:(length(t2s)-2)]) / 2,
               (t2s[length(t2s)] - t2s[length(t2s)-1]) / 2)
tau_df$w   <- widths
tau_df$pp  <- exp(tau_df$lp - max(tau_df$lp))
nrm        <- sum(tau_df$pp * tau_df$w)
tau_df$pr_t2  <- tau_df$pp / nrm
tau_df$tau    <- sqrt(tau_df$tau2)
tau_df$pr_tau <- tau_df$pr_t2 * 2 * tau_df$tau
tau_df$wmix   <- tau_df$pp * tau_df$w / nrm

cat(sprintf("GC τ² posterior: mean=%.4f median=%.4f\n",
            sum(tau_df$tau2 * tau_df$wmix),
            tau_df$tau2[min(which(cumsum(tau_df$wmix) >= 0.5))]))

# Per-effect mixture of Gaussians from GC, paired with multinma KDE
effects_list <- split(flat, paste(flat$from, flat$to, sep = "_"))
ref_to_effs  <- Filter(function(df) df$from[1] == "1", effects_list)
panel_data <- lapply(ref_to_effs, function(ef) {
  ef <- ef[ef$tau2 > 0 & is.finite(ef$var) & ef$var > 0, ]
  if (nrow(ef) == 0) return(NULL)
  ef <- merge(ef, tau_df[, c("tau2", "wmix")], by = "tau2")
  to <- ef$to[1]
  tid_str <- paste0("d[", to, "]")
  mn_samples <- eff_samples[[tid_str]]
  if (is.null(mn_samples)) return(NULL)
  max_sd <- max(sqrt(ef$var))
  grand_mean <- sum(ef$mean * ef$wmix)
  grand_sd  <- sqrt(sum(ef$wmix * (ef$var + (ef$mean - grand_mean)^2)))
  lo <- min(grand_mean - 5 * max_sd, min(mn_samples))
  hi <- max(grand_mean + 5 * max_sd, max(mn_samples))
  xs <- seq(lo, hi, length.out = 400)
  gc_dens <- sapply(xs, function(x)
                      sum(ef$wmix * dnorm(x, ef$mean, sqrt(ef$var))))
  list(to = to, xs = xs,
       gc_dens = gc_dens,
       mn_samples = mn_samples,
       gc_mean = grand_mean, gc_sd = grand_sd,
       mn_mean = mean(mn_samples), mn_sd = sd(mn_samples))
})
panel_data <- Filter(Negate(is.null), panel_data)
cat(sprintf("Panels to render: %d\n", length(panel_data)))

# --- HTML ---
payload <- list(
  dataset   = ds,
  gc_secs   = gc$gc_wall_sec,
  mn_secs   = mn_secs,
  mode_tau2 = gc$mode_tau2,
  tau_samples_mn = tau_samples,
  gc_tau_grid = tau_df$tau,
  gc_tau_dens = tau_df$pr_tau,
  effects = lapply(panel_data, function(p)
    list(label = paste0("d[", p$to, "]"),
         xs = p$xs,
         gc_dens = p$gc_dens,
         mn_samples = p$mn_samples,
         gc_mean = p$gc_mean, gc_sd = p$gc_sd,
         mn_mean = p$mn_mean, mn_sd = p$mn_sd))
)

as_js <- function(x) toJSON(x, auto_unbox = TRUE, na = "null", digits = 6)
html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Big NMA comparison — GC vs multinma (nmadb_501194)</title>
<style>
  body { font: 13.5px/1.5 -apple-system, Helvetica, Arial, sans-serif;
         max-width: 1280px; margin: 2rem auto; padding: 0 1rem; }
  h1 { font-size: 1.4rem; }
  h2 { margin-top: 1.4rem; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  .row { display: flex; gap: 14px; flex-wrap: wrap; }
  .plot { flex: 1 1 360px; min-height: 280px; background: #fbfbfd;
          border: 1px solid #e4e6ea; border-radius: 6px; padding: 8px; }
  .note { color: #666; font-size: 0.9em; }
</style>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
</head><body>
<h1><code>nmadb_501194</code> — 99 studies × 17 treatments</h1>
<p class="note">Real binary NMA. Both tools use half-Normal(0,1) prior on τ and exact binomial likelihood.</p>

<h2>τ posterior</h2>
<div class="row"><div id="p_tau" class="plot"></div></div>

<h2>All non-reference effects (d<sub>j</sub> − d<sub>1</sub>)</h2>
<div class="row" id="effects_row"></div>

<script>
const data = ', as_js(payload), ';

function normalDensity(x, m, s) {
  const z = (x - m) / s; return Math.exp(-0.5*z*z) / (s*Math.sqrt(2*Math.PI));
}
function sd(arr) { const m = arr.reduce((a,b)=>a+b)/arr.length;
  return Math.sqrt(arr.reduce((a,b)=>a+(b-m)*(b-m),0)/(arr.length-1)); }
function kde(samples, bw, xs) {
  return xs.map(x => { let s=0; for (const v of samples) s += normalDensity(x, v, bw); return s/samples.length; });
}
function grid(lo, hi, n) { const g=[]; for(let i=0;i<n;i++) g.push(lo+(hi-lo)*i/(n-1)); return g; }

// τ plot
(function(){
  const tau_s = data.tau_samples_mn;
  const xs = grid(0, Math.max(...tau_s) * 1.05, 300);
  const bw = 1.06 * sd(tau_s) * Math.pow(tau_s.length, -1/5);
  Plotly.newPlot("p_tau", [
    { x: xs, y: kde(tau_s, bw, xs), mode:"lines", name:"multinma (MCMC)",
      line:{color:"#d73a49", width:2}},
    { x: data.gc_tau_grid, y: data.gc_tau_dens, mode:"lines", name:"spring GC",
      line:{color:"#1f77b4", width:2}}
  ], { title: "τ posterior", xaxis:{title:"τ"}, yaxis:{title:"density"},
       margin:{t:40,b:40,l:50,r:10}}, {displayModeBar:false});
})();

// Effect panels
const row = document.getElementById("effects_row");
data.effects.forEach(eff => {
  const div = document.createElement("div");
  div.className = "plot"; div.id = "p_" + eff.label.replace(/[^a-zA-Z0-9]/g,"_");
  row.appendChild(div);
  const bw = 1.06 * sd(eff.mn_samples) * Math.pow(eff.mn_samples.length, -1/5);
  const mn_dens = kde(eff.mn_samples, bw, eff.xs);
  Plotly.newPlot(div.id, [
    { x: eff.xs, y: mn_dens, mode:"lines", name:"multinma",
      line:{color:"#d73a49", width:2}},
    { x: eff.xs, y: eff.gc_dens, mode:"lines", name:"spring GC",
      line:{color:"#1f77b4", width:2}}
  ],
  { title: eff.label + "<br>GC μ=" + eff.gc_mean.toFixed(3) + " σ=" + eff.gc_sd.toFixed(3)
           + "<br>MN μ=" + eff.mn_mean.toFixed(3) + " σ=" + eff.mn_sd.toFixed(3),
    xaxis:{title:"log-odds"}, yaxis:{title:"density"},
    margin:{t:60,b:40,l:50,r:10}},
  {displayModeBar:false});
});

document.title += "  — GC " + data.gc_secs.toFixed(1) + "s, MN " + data.mn_secs.toFixed(1) + "s";
</script>
</body></html>')

writeLines(html, "test/gc_compare/big_compare.html")
cat(sprintf("\nTimings: GC %.1fs vs multinma %.1fs\n", gc$gc_wall_sec, mn_secs))
cat("Wrote: test/gc_compare/big_compare.html\n")

# Summary table
cat("\n=== τ² posterior ===\n")
cat(sprintf("GC mean=%.4f median=%.4f\n",
            sum(tau_df$tau2 * tau_df$wmix),
            tau_df$tau2[min(which(cumsum(tau_df$wmix) >= 0.5))]))
cat(sprintf("MN tau mean=%.4f median=%.4f  (τ² mean=%.4f)\n",
            mean(tau_samples), median(tau_samples), mean(tau_samples^2)))

cat("\n=== Effects: posterior mean agreement ===\n")
diffs <- c()
for (p in panel_data) {
  cat(sprintf("  d[%s]: GC %+.3f±%.3f  MN %+.3f±%.3f  Δμ=%+.3f\n",
              p$to, p$gc_mean, p$gc_sd, p$mn_mean, p$mn_sd,
              p$gc_mean - p$mn_mean))
  diffs <- c(diffs, p$gc_mean - p$mn_mean)
}
cat(sprintf("\nEffect-mean differences (GC − MN): mean=%+.4f sd=%.4f max|Δ|=%.4f\n",
            mean(diffs), sd(diffs), max(abs(diffs))))
