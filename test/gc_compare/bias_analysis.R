#!/usr/bin/env Rscript
# Bias of GC (full-Hessian corrected) vs multinma vs generating truth,
# on both τ² and treatment effects.

suppressPackageStartupMessages({
  library(jsonlite)
})

gc <- fromJSON("test/gc_compare/gc_results.json",        simplifyVector = FALSE)
mn <- fromJSON("test/gc_compare/multinma_results.json",  simplifyVector = FALSE)
bm <- fromJSON("test/synth_bin/synth_meta.json",         simplifyVector = FALSE)
cm <- fromJSON("test/gc_compare/continuous_manifest.json",simplifyVector = FALSE)
manifest <- c(bm, cm)

gc_by <- setNames(gc$datasets, sapply(gc$datasets, `[[`, "name"))
truth <- setNames(manifest, sapply(manifest, `[[`, "name"))

# ----- τ² bias -----
tau_rows <- list()
for (nm in names(mn)) {
  g <- gc_by[[nm]]; m <- mn[[nm]]; t <- truth[[nm]]
  if (is.null(g) || is.null(m) || is.null(t)) next
  t2 <- if (!is.null(t$tau2_true)) t$tau2_true else t$tau2
  tau_rows[[length(tau_rows)+1]] <- data.frame(
    name = nm, lk = g$likelihood, k = g$n_studies, tau2_true = t2,
    gc_med = g$tau2_halfnormal$median, gc_mean = g$tau2_halfnormal$mean,
    mn_med = m$tau2$median,            mn_mean = m$tau2$mean,
    stringsAsFactors = FALSE)
}
td <- do.call(rbind, tau_rows)
# absolute (not percent) errors in τ²
td$gc_med_err <- td$gc_med - td$tau2_true
td$mn_med_err <- td$mn_med - td$tau2_true
td$gc_mean_err<- td$gc_mean - td$tau2_true
td$mn_mean_err<- td$mn_mean - td$tau2_true

cat("============  τ² bias: GC  vs  multinma  vs  truth  ============\n\n")
cat(sprintf("%-12s %-6s %3s %8s   %+8s %+8s   %+8s %+8s\n",
            "name","lk","k","true",
            "GC-med-err","MN-med-err","GC-mn-err","MN-mn-err"))
cat(strrep("-", 80), "\n", sep="")
for (i in seq_len(nrow(td))) {
  r <- td[i,]
  cat(sprintf("%-12s %-6s %3d %8.4f   %+8.4f %+8.4f   %+8.4f %+8.4f\n",
              r$name, substr(r$lk,1,6), r$k, r$tau2_true,
              r$gc_med_err, r$mn_med_err,
              r$gc_mean_err, r$mn_mean_err))
}

cat("\n----- τ² aggregate (absolute error in τ² units, by likelihood) -----\n\n")
for (lk_ in unique(td$lk)) {
  sub <- td[td$lk == lk_, ]
  cat(sprintf("%-10s  (n=%d)\n", lk_, nrow(sub)))
  cat(sprintf("             %10s %10s %10s %10s\n",
              "RMSE","MAE","mean-bias","med-bias"))
  for (col_lbl in list(c("GC med   ","gc_med_err"),
                       c("MN med   ","mn_med_err"),
                       c("GC mean  ","gc_mean_err"),
                       c("MN mean  ","mn_mean_err"))) {
    e <- sub[[col_lbl[2]]]
    cat(sprintf("  %-10s %10.4f %10.4f %+10.4f %+10.4f\n",
                col_lbl[1],
                sqrt(mean(e*e)), mean(abs(e)), mean(e), median(e)))
  }
  cat("\n")
}

# ----- effects bias (against d_true) -----
cat("\n============  Effect bias vs d_true (posterior means only)  ===========\n\n")

effect_rows <- list()
for (nm in names(mn)) {
  g <- gc_by[[nm]]; m <- mn[[nm]]; t <- truth[[nm]]
  if (is.null(g) || is.null(m) || is.null(t) || is.null(t$d_true)) next
  d_true <- t$d_true
  gc_effs <- g$effects_at_gc_mode
  # ref=trt1
  gc_ref <- Filter(function(e) e$from == "1", gc_effs)
  gc_map <- setNames(
    sapply(gc_ref, function(e) e$mean),
    sapply(gc_ref, function(e) e$to)
  )
  for (p in names(m$effects)) {
    tid <- sub("^d\\[(.*)\\]$", "\\1", p)
    if (tid == p) next
    tidi <- as.integer(tid)
    if (is.na(tidi) || tidi > length(d_true)) next
    dj_true <- d_true[[tidi]] - d_true[[1]]
    gc_mean <- as.numeric(gc_map[[tid]]); if (is.null(gc_mean)) next
    mn_mean <- m$effects[[p]]$mean
    effect_rows[[length(effect_rows)+1]] <- data.frame(
      name = nm, lk = g$likelihood, tid = tidi,
      d_true = dj_true, gc = gc_mean, mn = mn_mean,
      gc_err = gc_mean - dj_true, mn_err = mn_mean - dj_true,
      # filter out inestimable (sentinel sd)
      inestimable = sqrt(m$effects[[p]]$sd^2) > 10,
      stringsAsFactors = FALSE
    )
  }
}
ed <- do.call(rbind, effect_rows)
ed <- ed[!ed$inestimable, ]   # drop the 2 zero-event single-study outliers

cat(sprintf("%d estimable contrasts across %d datasets\n\n",
            nrow(ed), length(unique(ed$name))))
for (lk_ in unique(ed$lk)) {
  sub <- ed[ed$lk == lk_, ]
  cat(sprintf("%-10s  (n=%d contrasts)\n", lk_, nrow(sub)))
  cat(sprintf("             %10s %10s %10s %10s\n",
              "RMSE","MAE","mean-bias","med-bias"))
  for (col_lbl in list(c("GC effect","gc_err"), c("MN effect","mn_err"))) {
    e <- sub[[col_lbl[2]]]
    cat(sprintf("  %-10s %10.4f %10.4f %+10.4f %+10.4f\n",
                col_lbl[1],
                sqrt(mean(e*e)), mean(abs(e)), mean(e), median(e)))
  }
  cat("\n")
}

write.csv(td, "test/gc_compare/bias_tau2.csv",   row.names = FALSE)
write.csv(ed, "test/gc_compare/bias_effects.csv",row.names = FALSE)
cat("Saved: test/gc_compare/bias_tau2.csv, bias_effects.csv\n")
