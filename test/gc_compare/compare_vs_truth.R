#!/usr/bin/env Rscript
# Compare both GC (Laplace, post-correction) and multinma against the
# generating τ²_true, to see whether the GC vs multinma gap is Laplace bias
# or multinma prior/MCMC bias.

suppressPackageStartupMessages(library(jsonlite))

g  <- fromJSON("test/gc_compare/gc_results.json",        simplifyVector = FALSE)
mn <- fromJSON("test/gc_compare/multinma_results.json",  simplifyVector = FALSE)
gc_by <- setNames(g$datasets, sapply(g$datasets, `[[`, "name"))

rows <- list()
for (nm in names(mn)) {
  gg <- gc_by[[nm]]; m <- mn[[nm]]
  if (is.null(gg) || is.null(gg$tau2_true)) next
  rows[[length(rows)+1]] <- data.frame(
    name      = nm,
    lk        = gg$likelihood,
    k         = gg$n_studies,
    tau2_true = gg$tau2_true,
    gc_med    = gg$tau2_halfnormal$median,
    gc_mean   = gg$tau2_halfnormal$mean,
    mn_med    = m$tau2$median,
    mn_mean   = m$tau2$mean,
    stringsAsFactors = FALSE
  )
}
df <- do.call(rbind, rows)

# signed relative error vs truth
df$gc_med_err  <- 100 * (df$gc_med  - df$tau2_true) / pmax(df$tau2_true, 1e-4)
df$mn_med_err  <- 100 * (df$mn_med  - df$tau2_true) / pmax(df$tau2_true, 1e-4)
df$gc_mean_err <- 100 * (df$gc_mean - df$tau2_true) / pmax(df$tau2_true, 1e-4)
df$mn_mean_err <- 100 * (df$mn_mean - df$tau2_true) / pmax(df$tau2_true, 1e-4)

cat("=== Error vs generating τ²_true ===\n")
cat("(all relative to τ²_true, floor at 1e-4; +% means overestimate)\n\n")
cat(sprintf("%-12s %-6s %3s %8s  %+7s %+7s   %+7s %+7s\n",
            "name","lk","k","τ²_true","GC med", "MN med","GC mean","MN mean"))
cat(strrep("-", 78), "\n", sep = "")
for (i in seq_len(nrow(df))) {
  r <- df[i,]
  cat(sprintf("%-12s %-6s %3d %8.4f  %+6.0f%% %+6.0f%%   %+6.0f%% %+6.0f%%\n",
              r$name, substr(r$lk,1,6), r$k, r$tau2_true,
              r$gc_med_err, r$mn_med_err, r$gc_mean_err, r$mn_mean_err))
}

cat("\n=== Aggregate |err%| vs truth (by likelihood) ===\n\n")
for (lk_ in unique(df$lk)) {
  sub <- df[df$lk == lk_, ]
  cat(sprintf("%-10s n=%d\n", lk_, nrow(sub)))
  cat(sprintf("  median |err%%|  GC  : %5.0f%%   multinma : %5.0f%%   (tau2 median)\n",
              median(abs(sub$gc_med_err)),  median(abs(sub$mn_med_err))))
  cat(sprintf("  median |err%%|  GC  : %5.0f%%   multinma : %5.0f%%   (tau2 mean  )\n",
              median(abs(sub$gc_mean_err)), median(abs(sub$mn_mean_err))))
  cat(sprintf("  mean   |err%%|  GC  : %5.0f%%   multinma : %5.0f%%   (tau2 median)\n",
              mean  (abs(sub$gc_med_err)),  mean  (abs(sub$mn_med_err))))
  cat(sprintf("  mean   |err%%|  GC  : %5.0f%%   multinma : %5.0f%%   (tau2 mean  )\n\n",
              mean  (abs(sub$gc_mean_err)), mean  (abs(sub$mn_mean_err))))
}

# bias direction: +% means both overestimate?
cat("=== Signed mean err (direction of bias vs truth) ===\n\n")
for (lk_ in unique(df$lk)) {
  sub <- df[df$lk == lk_, ]
  cat(sprintf("%-10s  GC med bias: %+6.0f%%   MN med bias: %+6.0f%%\n",
              lk_, mean(sub$gc_med_err), mean(sub$mn_med_err)))
}

write.csv(df, "test/gc_compare/truth_comparison.csv", row.names = FALSE)
cat("\nSaved: test/gc_compare/truth_comparison.csv\n")
