#!/usr/bin/env Rscript
# Posterior overlay for nmadb_474842 (45 studies × 8 trts, real binary NMA).
# Full marginalised posteriors: τ from Ξ(τ²) grid, all effects as mixture
# of Gaussians across the τ² grid.

suppressPackageStartupMessages({ library(jsonlite) })

d <- fromJSON("test/gc_compare/medium_nmadb_gc.json", simplifyVector = FALSE)
cat(sprintf("Loaded %s, n_studies=%d, grid_nPts=%d, mode τ²=%.5f\n",
            d$dataset, d$n_studies, d$grid_nPts, d$mode_tau2))

# flatten grid
flat <- do.call(rbind, lapply(d$grid, function(g) {
  do.call(rbind, lapply(g$effects, function(e) {
    data.frame(tau2 = g$tau2, logZ = g$logZ,
               from = e$from, to = e$to,
               mean = e$mean, var = e$var,
               stringsAsFactors = FALSE)
  }))
}))
tau2_grid <- unique(flat$tau2)

# Half-Normal(0,1) on tau: log pi_τ²(τ²) = -(1/2) log τ² - τ²/2 + const
prior_logπ <- function(t2)
  ifelse(t2 > 0, -0.5 * log(t2) - 0.5 * t2, -Inf)

tau_df <- data.frame(tau2 = tau2_grid,
                     logZ = sapply(d$grid, `[[`, "logZ"))
tau_df$lp  <- tau_df$logZ + prior_logπ(tau_df$tau2)
tau_df <- tau_df[tau_df$tau2 > 0, ]

# For a non-uniform grid we need trapezoidal weights:
#   Δτ²_k = (τ²_{k+1} - τ²_{k-1})/2 (or the edge variants)
t2s <- tau_df$tau2
widths <- c((t2s[2] - t2s[1]) / 2,
            (t2s[3:length(t2s)] - t2s[1:(length(t2s)-2)]) / 2,
            (t2s[length(t2s)] - t2s[length(t2s)-1]) / 2)
tau_df$w  <- widths
tau_df$pp <- exp(tau_df$lp - max(tau_df$lp))
norm      <- sum(tau_df$pp * tau_df$w)
tau_df$pr_t2  <- tau_df$pp / norm                 # density on τ²
tau_df$tau    <- sqrt(tau_df$tau2)
tau_df$pr_tau <- tau_df$pr_t2 * 2 * tau_df$tau    # density on τ
tau_df$wmix   <- tau_df$pp * tau_df$w / norm       # discrete weights, summing to 1

cat(sprintf("τ² posterior: mean=%.5f median=%.5f\n",
            sum(tau_df$tau2 * tau_df$wmix),
            tau_df$tau2[min(which(cumsum(tau_df$wmix) >= 0.5))]))

# Per-effect mixture of Gaussians (keeping only 'from == "1"' for the
# 1-vs-others view; for 8 treatments this is 7 posteriors)
effects_list <- split(flat, paste(flat$from, flat$to, sep = "_"))
ref_to_effs  <- Filter(function(df) df$from[1] == "1", effects_list)
cat(sprintf("Effects panels (from reference 1): %d\n", length(ref_to_effs)))

panel_data <- lapply(ref_to_effs, function(ef) {
  ef <- ef[ef$tau2 > 0 & is.finite(ef$var) & ef$var > 0, ]
  if (nrow(ef) == 0) return(NULL)
  ef <- merge(ef, tau_df[, c("tau2", "wmix")], by = "tau2")
  max_sd <- max(sqrt(ef$var))
  grand_mean <- sum(ef$mean * ef$wmix)
  lo <- grand_mean - 5 * max_sd
  hi <- grand_mean + 5 * max_sd
  xs <- seq(lo, hi, length.out = 400)
  dens <- sapply(xs, function(x) sum(ef$wmix * dnorm(x, ef$mean, sqrt(ef$var))))
  list(to = ef$to[1], xs = xs, dens = dens,
       grand_mean = grand_mean,
       posterior_sd =
         sqrt(sum(ef$wmix * (ef$var + (ef$mean - grand_mean)^2))))
})
panel_data <- Filter(Negate(is.null), panel_data)

payload <- list(
  dataset = d$dataset,
  n_studies = d$n_studies,
  mode_tau2 = d$mode_tau2,
  tau = list(x = tau_df$tau, dens = tau_df$pr_tau),
  effects = lapply(panel_data, function(p)
    list(label = paste0("d[", p$to, "]"),
         xs = p$xs, dens = p$dens,
         mean = p$grand_mean, sd = p$posterior_sd))
)

as_js <- function(x) toJSON(x, auto_unbox = TRUE, na = "null", digits = 6)
html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Full posterior — nmadb_474842 (45 studies × 8 trts, spring GC)</title>
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
<h1>Full posterior — <code>nmadb_474842</code> (45 studies × 8 treatments)</h1>
<p class="note">Adaptive-grid spring GC with half-Normal(0,1) prior on τ. Full posteriors: τ from Ξ(τ²); every effect d<sub>j</sub> vs reference 1 as mixture Σ<sub>k</sub> p(τ²<sub>k</sub> | y) · N(d<sub>j</sub>; μ<sub>j</sub>(τ²<sub>k</sub>), σ²<sub>j</sub>(τ²<sub>k</sub>)).</p>
<h2>τ posterior</h2>
<div class="row"><div id="p_tau" class="plot"></div></div>
<h2>All non-reference effects (d<sub>j</sub> − d<sub>1</sub>, log-odds)</h2>
<div class="row" id="effects_row"></div>
<script>
const data = ', as_js(payload), ';

Plotly.newPlot("p_tau", [
  { x: data.tau.x, y: data.tau.dens, mode: "lines",
    line: {color: "#1f77b4", width: 2}, name: "spring GC" }
], { title: "τ posterior", xaxis: {title: "τ"}, yaxis: {title: "density"},
     margin: {t:40,b:40,l:50,r:10}}, {displayModeBar:false});

const row = document.getElementById("effects_row");
data.effects.forEach(eff => {
  const div = document.createElement("div");
  div.className = "plot"; div.id = "p_" + eff.label.replace(/[^a-zA-Z0-9]/g, "_");
  row.appendChild(div);
  Plotly.newPlot(div.id, [
    { x: eff.xs, y: eff.dens, mode: "lines",
      line: {color: "#1f77b4", width: 2}}
  ],
  { title: eff.label + "  (mean=" + eff.mean.toFixed(3)
           + ", sd=" + eff.sd.toFixed(3) + ")",
    xaxis: {title: "log-odds"}, yaxis: {title: "density"},
    margin: {t:40,b:40,l:50,r:10}, showlegend: false },
  {displayModeBar:false});
});
</script>
</body></html>')

writeLines(html, "test/gc_compare/medium_nmadb_plots.html")
cat("\nWrote: test/gc_compare/medium_nmadb_plots.html\n")
