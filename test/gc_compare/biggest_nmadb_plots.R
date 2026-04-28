#!/usr/bin/env Rscript
# Render full-posterior plots from biggest_nmadb_gc.json:
#   p(τ² | y)  ∝  exp(logZ) · π(τ²)          (grand-canonical sum)
#   p(d_j | y) = Σ_k p(τ²_k | y) · N(d_j; μ_j(τ²_k), σ²_j(τ²_k))   (mixture)
#
# Shows the full posterior of τ and every non-reference effect for
# nmadb_501194 (99 studies, 17 treatments). Reference treatment = 1.

suppressPackageStartupMessages({
  library(jsonlite)
})

if (!file.exists("test/gc_compare/biggest_nmadb_gc.json")) {
  stop("biggest_nmadb_gc.json not found yet — wait for the GC run.")
}

d <- fromJSON("test/gc_compare/biggest_nmadb_gc.json", simplifyVector = FALSE)
cat(sprintf("Loaded: %s\n", d$dataset))
cat(sprintf("  n_studies=%d, kTau=%g, nMax=%d, mode τ²=%.4f\n",
            d$n_studies, d$grid_kTau, d$grid_nMax, d$mode_tau2))

# Build a data frame: one row per (tau2, effect)
flat <- do.call(rbind, lapply(d$grid, function(g) {
  do.call(rbind, lapply(g$effects, function(e) {
    data.frame(tau2 = g$tau2, logZ = g$logZ,
               from = e$from, to = e$to,
               mean = e$mean, var = e$var,
               stringsAsFactors = FALSE)
  }))
}))
tau2_grid <- unique(flat$tau2)
cat(sprintf("Grid points: %d; effect pairs per point: %d\n",
            length(tau2_grid), nrow(flat) / length(tau2_grid)))

# Reweight log Z by the chosen prior on τ. Default: half-Normal(0, 1) on τ
# → log π(τ²) = -(1/2) log(τ²) - τ²/2 (plus const).
prior_logπ <- function(t2)
  ifelse(t2 > 0, -0.5 * log(t2) - 0.5 * t2, -Inf)

# Weights p(τ²_k | y) on the grid
tau_df <- data.frame(tau2 = tau2_grid,
                     logZ = sapply(d$grid, `[[`, "logZ"))
tau_df$lp  <- tau_df$logZ + prior_logπ(tau_df$tau2)
tau_df <- tau_df[tau_df$tau2 > 0, ]
tau_df$w   <- exp(tau_df$lp - max(tau_df$lp))
tau_df$w   <- tau_df$w / sum(tau_df$w * diff(c(0, tau_df$tau2)))  # density
# marginal density on τ: p_τ(τ) = 2 τ · p_{τ²}(τ²)
tau_df$tau <- sqrt(tau_df$tau2)
tau_df$pr_tau <- 2 * tau_df$tau * tau_df$w
# also normalised weight to form mixture (equal Δτ²)
tau_df$wmix <- exp(tau_df$lp - max(tau_df$lp))
tau_df$wmix <- tau_df$wmix / sum(tau_df$wmix)

cat(sprintf("Post-prior τ² posterior: mean=%.4f median=%.4f\n",
            sum(tau_df$tau2 * tau_df$wmix),
            tau_df$tau2[min(which(cumsum(tau_df$wmix) >= 0.5))]))

# For each non-reference effect, compute the marginal density over a d-axis
# as the mixture Σ_k w_k · N(d; μ_k, σ²_k).
# Build per-effect panels: one per (from="1", to in {2..T})
effects_list <- split(flat, paste(flat$from, flat$to, sep = "_"))
ref_to_effs <- Filter(function(df) df$from[1] == "1", effects_list)
cat(sprintf("Effects from reference (from='1'): %d\n", length(ref_to_effs)))

# For each effect, find a sensible d-axis: go ±4 SD of the widest component
panel_data <- lapply(ref_to_effs, function(ef) {
  ef <- ef[ef$tau2 > 0 & is.finite(ef$var) & ef$var > 0, ]
  if (nrow(ef) == 0) return(NULL)
  # Attach mixture weights
  ef <- merge(ef, tau_df[, c("tau2", "wmix")], by = "tau2")
  max_sd <- max(sqrt(ef$var))
  grand_mean <- sum(ef$mean * ef$wmix)
  lo <- grand_mean - 5 * max_sd
  hi <- grand_mean + 5 * max_sd
  xs <- seq(lo, hi, length.out = 400)
  dens <- sapply(xs, function(x) {
    sum(ef$wmix * dnorm(x, ef$mean, sqrt(ef$var)))
  })
  list(to = ef$to[1], xs = xs, dens = dens,
       grand_mean = grand_mean, posterior_sd =
         sqrt(sum(ef$wmix * (ef$var + (ef$mean - grand_mean)^2))))
})
panel_data <- Filter(Negate(is.null), panel_data)

# --- HTML ---
payload <- list(
  dataset = d$dataset,
  n_studies = d$n_studies,
  gc_wall_sec = d$gc_wall_sec,
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
<title>Full posterior — nmadb_501194 (99 studies × 17 trts, spring GC)</title>
<style>
  body { font: 13.5px/1.5 -apple-system, Helvetica, Arial, sans-serif;
         max-width: 1280px; margin: 2rem auto; padding: 0 1rem; }
  h1 { font-size: 1.4rem; }
  h2 { margin-top: 1.5rem; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  .row { display: flex; gap: 14px; flex-wrap: wrap; }
  .plot { flex: 1 1 360px; min-height: 280px; background: #fbfbfd;
          border: 1px solid #e4e6ea; border-radius: 6px; padding: 8px; }
  .note { color: #666; font-size: 0.9em; }
</style>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
</head><body>
<h1>Full posterior — <code>nmadb_501194</code></h1>
<p class="note">99 studies × 17 treatments, real binary NMA. Spring GC with half-Normal(0, 1) prior on τ. <b>Full posteriors</b>: τ from the Ξ(τ²) grid; every effect d<sub>j</sub> vs reference as the mixture Σ<sub>k</sub> p(τ²<sub>k</sub> | y) · N(d<sub>j</sub>; μ<sub>j</sub>(τ²<sub>k</sub>), σ²<sub>j</sub>(τ²<sub>k</sub>)), marginalising over the τ² posterior.</p>

<h2>τ posterior</h2>
<div class="row"><div id="p_tau" class="plot"></div></div>

<h2>All non-reference effects (d<sub>j</sub> − d<sub>1</sub>, log-odds scale)</h2>
<div class="row" id="effects_row"></div>

<script>
const data = ', as_js(payload), ';

Plotly.newPlot("p_tau", [
  { x: data.tau.x, y: data.tau.dens, mode: "lines",
    line:{color:"#1f77b4", width: 2}, name: "spring GC"}
], { title: "τ posterior", xaxis: {title: "τ"}, yaxis: {title: "density"},
     margin: {t: 40, b: 40, l: 50, r: 10}}, {displayModeBar: false});

const row = document.getElementById("effects_row");
data.effects.forEach(eff => {
  const div = document.createElement("div");
  div.className = "plot"; div.id = "p_" + eff.label.replace(/[^a-zA-Z0-9]/g, "_");
  row.appendChild(div);
  Plotly.newPlot(div.id, [
    { x: eff.xs, y: eff.dens, mode: "lines",
      line:{color:"#1f77b4", width: 2}}
  ],
  { title: eff.label + "  (mean=" + eff.mean.toFixed(3)
           + ", sd=" + eff.sd.toFixed(3) + ")",
    xaxis: {title: "log-odds"}, yaxis: {title: "density"},
    margin: {t: 40, b: 40, l: 50, r: 10}, showlegend: false },
   {displayModeBar:false});
});
</script>
</body></html>')

writeLines(html, "test/gc_compare/biggest_nmadb_plots.html")
cat("\nWrote: test/gc_compare/biggest_nmadb_plots.html\n")
