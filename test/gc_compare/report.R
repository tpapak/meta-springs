#!/usr/bin/env Rscript
# Build an HTML comparison report from gc_results.json + multinma_results.json.
# Two scatter plots + per-dataset table.

suppressPackageStartupMessages({
  library(jsonlite)
  library(htmltools)
})

gc <- fromJSON("test/gc_compare/gc_results.json", simplifyVector = FALSE)
mn <- fromJSON("test/gc_compare/multinma_results.json", simplifyVector = FALSE)

gc_by <- setNames(gc$datasets, sapply(gc$datasets, `[[`, "name"))

# Per-dataset summary rows
rows <- list()
effect_points <- list()
tau2_points   <- list()
for (nm in names(mn)) {
  g <- gc_by[[nm]]
  if (is.null(g)) next
  m <- mn[[nm]]
  # tau2 stats
  gh <- g$tau2_halfnormal
  mt <- m$tau2
  tau2_points[[length(tau2_points)+1]] <- list(
    name    = nm,
    lk      = g$likelihood,
    gc_med  = gh$median,   mn_med  = mt$median,
    gc_mean = gh$mean,     mn_mean = mt$mean,
    gc_lo   = gh$ci_lo,    mn_lo   = mt$ci_lo,
    gc_hi   = gh$ci_hi,    mn_hi   = mt$ci_hi,
    tau2_true = g$tau2_true,
    gc_mode_tau2_half = gh$mode_tau2_via_tau_space
  )
  rows[[length(rows)+1]] <- list(
    name=nm, lk=g$likelihood, k=g$n_studies, tau2_true=g$tau2_true,
    gc_reml=g$spring_reml_tau2, gc_flat_mode=g$tau2_flat$mode,
    gc_hn_med=gh$median, mn_med=mt$median,
    gc_hn_lo=gh$ci_lo, gc_hn_hi=gh$ci_hi,
    mn_lo=mt$ci_lo,    mn_hi=mt$ci_hi
  )

  # Effect comparison: multinma gives d[trt] vs ref=trt1; GC effects list has
  # from/to contrasts for every pair present. Match on ref=trt 1.
  gc_effs <- g$effects_at_gc_mode
  # Keep only rows with from == "1" (ref treatment)
  gc_ref <- Filter(function(e) e$from == "1", gc_effs)
  gc_map <- setNames(
    lapply(gc_ref, function(e) list(mean=e$mean, sd=sqrt(e$var))),
    sapply(gc_ref, function(e) e$to)
  )
  # multinma effect labels are d[B] etc. for levels, here treatments are integers
  # so labels should be like d[2], d[3], ..., d[6]
  for (p in names(m$effects)) {
    # extract the treatment id from "d[2]" form
    tid <- sub("^d\\[(.*)\\]$", "\\1", p)
    if (tid == p) next  # not in expected form
    gc_ent <- gc_map[[tid]]
    if (is.null(gc_ent)) next
    # Drop effectively inestimable contrasts (GC sentinel ~1000, or very
    # large posterior SD from multinma MCMC) — only 2 such points in this
    # sweep, both from isolated zero-event single-study comparisons.
    gc_sd_val <- gc_ent$sd
    mn_sd_val <- m$effects[[p]]$sd
    inestimable <- gc_sd_val > 50 || mn_sd_val > 10
    effect_points[[length(effect_points)+1]] <- list(
      name=nm, lk=g$likelihood, tid=tid,
      gc_mean=gc_ent$mean, gc_sd=gc_sd_val,
      mn_mean=m$effects[[p]]$mean, mn_sd=mn_sd_val,
      mn_lo=m$effects[[p]]$ci_lo, mn_hi=m$effects[[p]]$ci_hi,
      inestimable = inestimable
    )
  }
}

# Helpers for inline JS data
as_js <- function(x) toJSON(x, auto_unbox = TRUE, na = "null", digits = 6)

html <- paste0('<!DOCTYPE html>
<html><head><meta charset="utf-8">
<title>Spring GC vs multinma — 20 T=6 NMA datasets</title>
<style>
  body { font: 14px/1.5 -apple-system, Helvetica, Arial, sans-serif;
         max-width: 1080px; margin: 2rem auto; padding: 0 1rem; color: #222; }
  h1,h2 { line-height:1.2; }
  h1 { font-size: 1.5rem; }
  h2 { margin-top: 2rem; border-bottom: 1px solid #ddd; padding-bottom: 4px; }
  .row { display: flex; gap: 20px; flex-wrap: wrap; }
  .plot { flex: 1 1 420px; min-height: 420px; background: #fbfbfd;
          border: 1px solid #e4e6ea; border-radius: 6px; padding: 10px; }
  table { border-collapse: collapse; margin-top: 8px; font-size: 13px; }
  th, td { border: 1px solid #ccc; padding: 3px 8px; text-align: right; }
  th { background: #eee; }
  td:first-child, th:first-child { text-align: left; }
  .bin { background: #fff7ed; }
  .cont { background: #eff6ff; }
  .note { color: #666; font-size: 0.9em; }
</style>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
</head><body>

<h1>Spring&nbsp;Ξ(τ²) vs multinma MCMC &mdash; T=6 NMAs</h1>

<p class="note">10 binary (exact binomial, orange) + 10 continuous (Gaussian, blue) networks,
6 treatments each, k &isin; [5, 23]. Spring-GC uses a half-Normal(0,&nbsp;1) prior on
τ to match multinma&rsquo;s default. 95% intervals are posterior CIs (multinma:
quantile; GC: grid CDF). Treatment effects are posterior means of d<sub>j</sub> vs
reference treatment 1 at the GC posterior mode / multinma posterior mean.</p>

<h2>τ² comparison (median, mean, 95% CI)</h2>
<div class="row">
  <div id="plot_tau2_med" class="plot"></div>
  <div id="plot_tau2_ci"  class="plot"></div>
</div>

<h2>Treatment-effect comparison (d<sub>j</sub> vs d<sub>1</sub>)</h2>
<div class="row">
  <div id="plot_eff_mean" class="plot"></div>
  <div id="plot_eff_sd"   class="plot"></div>
</div>

<h2>Per-dataset summary</h2>
<div id="tbl"></div>

<script>
const tau2_points  = ', as_js(tau2_points), ';
const effect_points = ', as_js(effect_points), ';
const rows = ', as_js(rows), ';

function byLk(arr, lk) { return arr.filter(p => p.lk === lk && !p.inestimable); }
const n_inest = effect_points.filter(p => p.inestimable).length;
document.write(`<p class="note">${n_inest} effect contrast(s) dropped as inestimable `
  + `(isolated zero-event single-study comparisons). `
  + `Full list in the per-dataset table.</p>`);

function lineY_eq_X(min, max) {
  return {x:[min,max], y:[min,max], mode:"lines", type:"scatter",
          line:{dash:"dot", color:"#888", width:1}, showlegend:false,
          hoverinfo:"skip"};
}

function scatter(div, arr, xkey, ykey, title, xlab, ylab, logAxes, errx, erry) {
  const bin  = byLk(arr, "Binomial");
  const cont = byLk(arr, "Gaussian");
  const traces = [
    {x: bin.map(d=>d[xkey]),  y: bin.map(d=>d[ykey]),
     error_x: errx ? {type:"data", array: bin.map(d=>d[errx]),  visible:true, thickness:1, width:3, color:"#e67e22"} : undefined,
     error_y: erry ? {type:"data", array: bin.map(d=>d[erry]),  visible:true, thickness:1, width:3, color:"#e67e22"} : undefined,
     mode:"markers", type:"scatter", name:"binary",
     marker:{color:"#e67e22", size:9, line:{color:"#000",width:0.5}},
     text: bin.map(d=>d.name)},
    {x: cont.map(d=>d[xkey]), y: cont.map(d=>d[ykey]),
     error_x: errx ? {type:"data", array: cont.map(d=>d[errx]), visible:true, thickness:1, width:3, color:"#2980b9"} : undefined,
     error_y: erry ? {type:"data", array: cont.map(d=>d[erry]), visible:true, thickness:1, width:3, color:"#2980b9"} : undefined,
     mode:"markers", type:"scatter", name:"continuous",
     marker:{color:"#2980b9", size:9, line:{color:"#000",width:0.5}},
     text: cont.map(d=>d.name)}
  ];
  const allX = arr.map(d=>d[xkey]).filter(v=>isFinite(v));
  const allY = arr.map(d=>d[ykey]).filter(v=>isFinite(v));
  const mn = Math.min(...allX, ...allY);
  const mx = Math.max(...allX, ...allY);
  traces.push(lineY_eq_X(mn, mx));
  const layout = {title, margin:{t:40,l:55,r:10,b:50},
    xaxis:{title:xlab, type: logAxes ? "log" : "linear"},
    yaxis:{title:ylab, type: logAxes ? "log" : "linear"},
    legend:{x:0.02,y:0.98}, hovermode:"closest"};
  Plotly.newPlot(div, traces, layout, {displayModeBar:false});
}

// τ² plots
scatter("plot_tau2_med", tau2_points, "mn_med", "gc_med",
        "τ² median: spring GC vs multinma", "multinma median", "spring GC median", true);
// For the CI plot we show median (marker) and CI as error bars in linear scale
(function() {
  const bin  = byLk(tau2_points, "Binomial");
  const cont = byLk(tau2_points, "Gaussian");
  function errAsym(arr, lo, hi, m) {
    return arr.map((d,i) => d[hi] - d[m]);
  }
  function errAsymMinus(arr, lo, hi, m) {
    return arr.map((d,i) => d[m] - d[lo]);
  }
  const traces = [
    {x: bin.map(d=>d.mn_med),  y: bin.map(d=>d.gc_med),
     error_x: {type:"data", array: errAsym(bin,"mn_lo","mn_hi","mn_med"),
                arrayminus: errAsymMinus(bin,"mn_lo","mn_hi","mn_med"),
                symmetric:false, visible:true, thickness:1, color:"#e67e22"},
     error_y: {type:"data", array: errAsym(bin,"gc_lo","gc_hi","gc_med"),
                arrayminus: errAsymMinus(bin,"gc_lo","gc_hi","gc_med"),
                symmetric:false, visible:true, thickness:1, color:"#e67e22"},
     mode:"markers", type:"scatter", name:"binary",
     marker:{color:"#e67e22", size:8},
     text: bin.map(d=>d.name)},
    {x: cont.map(d=>d.mn_med), y: cont.map(d=>d.gc_med),
     error_x: {type:"data", array: errAsym(cont,"mn_lo","mn_hi","mn_med"),
                arrayminus: errAsymMinus(cont,"mn_lo","mn_hi","mn_med"),
                symmetric:false, visible:true, thickness:1, color:"#2980b9"},
     error_y: {type:"data", array: errAsym(cont,"gc_lo","gc_hi","gc_med"),
                arrayminus: errAsymMinus(cont,"gc_lo","gc_hi","gc_med"),
                symmetric:false, visible:true, thickness:1, color:"#2980b9"},
     mode:"markers", type:"scatter", name:"continuous",
     marker:{color:"#2980b9", size:8},
     text: cont.map(d=>d.name)}
  ];
  const allX = tau2_points.flatMap(d=>[d.mn_lo,d.mn_hi]).filter(isFinite);
  const allY = tau2_points.flatMap(d=>[d.gc_lo,d.gc_hi]).filter(isFinite);
  const mn2 = Math.min(...allX,...allY); const mx2 = Math.max(...allX,...allY);
  traces.push(lineY_eq_X(mn2, mx2));
  Plotly.newPlot("plot_tau2_ci", traces,
    {title:"τ² median &plusmn; 95% CI", margin:{t:40,l:55,r:10,b:50},
     xaxis:{title:"multinma"}, yaxis:{title:"spring GC"}, hovermode:"closest"},
    {displayModeBar:false});
})();

// Effect plots
scatter("plot_eff_mean", effect_points, "mn_mean", "gc_mean",
        "d<sub>j</sub> posterior mean", "multinma mean", "spring GC mean", false,
        "mn_sd", "gc_sd");
scatter("plot_eff_sd", effect_points, "mn_sd", "gc_sd",
        "d<sub>j</sub> posterior SD", "multinma SD", "spring GC SD", false);

// Table
(function(){
  const header = `<tr>
    <th>Dataset</th><th>lk</th><th>k</th><th>τ²_true</th>
    <th>REML</th><th>GC flat mode</th>
    <th>GC HN median</th><th>multinma median</th>
    <th>GC 95% CI</th><th>multinma 95% CI</th></tr>`;
  const body = rows.map(r => {
    const cls = r.lk === "Binomial" ? "bin" : "cont";
    const f = (x, n=3) => x == null ? "—" : Number(x).toFixed(n);
    return `<tr class="${cls}"><td>${r.name}</td><td>${r.lk}</td>
      <td>${r.k}</td><td>${f(r.tau2_true)}</td>
      <td>${f(r.gc_reml)}</td><td>${f(r.gc_flat_mode)}</td>
      <td>${f(r.gc_hn_med)}</td><td>${f(r.mn_med)}</td>
      <td>(${f(r.gc_hn_lo)}, ${f(r.gc_hn_hi)})</td>
      <td>(${f(r.mn_lo)}, ${f(r.mn_hi)})</td></tr>`;
  }).join("");
  document.getElementById("tbl").innerHTML = "<table>" + header + body + "</table>";
})();
</script>

</body></html>')

writeLines(html, "test/gc_compare/report.html")
cat("Wrote: test/gc_compare/report.html\n")
