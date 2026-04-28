#!/usr/bin/env Rscript
# Compare: Laplace-only, per-arm-local correction, and full-Hessian correction
# against multinma AND against τ²_true. Tabulate per-dataset and aggregate.

suppressPackageStartupMessages(library(jsonlite))

g_la <- fromJSON("test/gc_compare/gc_results_laplace.json",    simplifyVector = FALSE)
g_lc <- fromJSON("test/gc_compare/gc_results_local_corr.json", simplifyVector = FALSE)
g_hf <- fromJSON("test/gc_compare/gc_results.json",            simplifyVector = FALSE)
mn   <- fromJSON("test/gc_compare/multinma_results.json",      simplifyVector = FALSE)

by_la <- setNames(g_la$datasets, sapply(g_la$datasets, `[[`, "name"))
by_lc <- setNames(g_lc$datasets, sapply(g_lc$datasets, `[[`, "name"))
by_hf <- setNames(g_hf$datasets, sapply(g_hf$datasets, `[[`, "name"))

rows <- list()
for (nm in names(mn)) {
  la <- by_la[[nm]]; lc <- by_lc[[nm]]; hf <- by_hf[[nm]]; m <- mn[[nm]]
  if (is.null(la) || is.null(lc) || is.null(hf) || is.null(m)) next
  rows[[length(rows)+1]] <- data.frame(
    name       = nm,
    lk         = la$likelihood,
    k          = la$n_studies,
    tau2_true  = la$tau2_true,
    la_med     = la$tau2_halfnormal$median,
    lc_med     = lc$tau2_halfnormal$median,
    hf_med     = hf$tau2_halfnormal$median,
    mn_med     = m$tau2$median,
    la_mean    = la$tau2_halfnormal$mean,
    lc_mean    = lc$tau2_halfnormal$mean,
    hf_mean    = hf$tau2_halfnormal$mean,
    mn_mean    = m$tau2$mean,
    stringsAsFactors = FALSE
  )
}
df <- do.call(rbind, rows)

relerr <- function(x, ref) 100 * (x - ref) / pmax(abs(ref), 1e-6)

cat("=== τ² median: three GC variants vs multinma (as reference) ===\n\n")
cat(sprintf("%-12s %-6s %3s   %7s %7s %7s %7s     %7s %7s %7s\n",
            "name","lk","k",
            "MN med","Laplace","Local","FullH",
            "La%","Lc%","Hf%"))
cat(strrep("-", 96), "\n", sep = "")
for (i in seq_len(nrow(df))) {
  r <- df[i,]
  cat(sprintf("%-12s %-6s %3d   %7.4f %7.4f %7.4f %7.4f     %+6.1f%% %+6.1f%% %+6.1f%%\n",
              r$name, substr(r$lk,1,6), r$k,
              r$mn_med, r$la_med, r$lc_med, r$hf_med,
              relerr(r$la_med, r$mn_med),
              relerr(r$lc_med, r$mn_med),
              relerr(r$hf_med, r$mn_med)))
}

cat("\n=== Aggregate median |err%| vs multinma ===\n\n")
for (lk_ in unique(df$lk)) {
  sub <- df[df$lk == lk_, ]
  cat(sprintf("%-10s  n=%d\n", lk_, nrow(sub)))
  for (lbl_col in list(c("Laplace","la_med"), c("Local","lc_med"), c("FullH","hf_med"))) {
    e <- relerr(sub[[lbl_col[2]]], sub$mn_med)
    cat(sprintf("  %-10s  τ² median |err%%| vs multinma: median %5.1f%%  mean %5.1f%%\n",
                lbl_col[1], median(abs(e)), mean(abs(e))))
  }
}

cat("\n=== |err%| vs τ²_true (by method) ===\n\n")
for (lk_ in unique(df$lk)) {
  sub <- df[df$lk == lk_, ]
  cat(sprintf("%-10s  n=%d\n", lk_, nrow(sub)))
  for (lbl_col in list(c("Laplace ","la_med"), c("Local   ","lc_med"),
                       c("FullH   ","hf_med"), c("multinma","mn_med"))) {
    e <- relerr(sub[[lbl_col[2]]], sub$tau2_true)
    cat(sprintf("  %s  τ² median |err%%| vs truth: median %5.0f%%  mean %5.0f%%\n",
                lbl_col[1], median(abs(e)), mean(abs(e))))
  }
  cat("\n")
}

write.csv(df, "test/gc_compare/three_way_comparison.csv", row.names = FALSE)
cat("Saved: test/gc_compare/three_way_comparison.csv\n")
