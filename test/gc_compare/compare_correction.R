#!/usr/bin/env Rscript
# Tabulate pre- vs post-correction τ² against multinma, per dataset.

suppressPackageStartupMessages(library(jsonlite))

g_la <- fromJSON("test/gc_compare/gc_results_laplace.json",  simplifyVector = FALSE)
g_co <- fromJSON("test/gc_compare/gc_results.json",          simplifyVector = FALSE)
mn   <- fromJSON("test/gc_compare/multinma_results.json",    simplifyVector = FALSE)

by_name_la <- setNames(g_la$datasets, sapply(g_la$datasets, `[[`, "name"))
by_name_co <- setNames(g_co$datasets, sapply(g_co$datasets, `[[`, "name"))

rows <- list()
for (nm in names(mn)) {
  la <- by_name_la[[nm]]; co <- by_name_co[[nm]]; m <- mn[[nm]]
  if (is.null(la) || is.null(co) || is.null(m)) next
  rows[[length(rows)+1]] <- data.frame(
    name     = nm,
    lk       = la$likelihood,
    k        = la$n_studies,
    tau2_true = ifelse(is.null(la$tau2_true), NA, la$tau2_true),
    mn_med   = m$tau2$median,
    la_med   = la$tau2_halfnormal$median,
    co_med   = co$tau2_halfnormal$median,
    mn_mean  = m$tau2$mean,
    la_mean  = la$tau2_halfnormal$mean,
    co_mean  = co$tau2_halfnormal$mean,
    stringsAsFactors = FALSE
  )
}
df <- do.call(rbind, rows)
df$d_la_med  <- 100 * (df$la_med  - df$mn_med)  / pmax(df$mn_med, 1e-6)
df$d_co_med  <- 100 * (df$co_med  - df$mn_med)  / pmax(df$mn_med, 1e-6)
df$d_la_mean <- 100 * (df$la_mean - df$mn_mean) / pmax(df$mn_mean, 1e-6)
df$d_co_mean <- 100 * (df$co_mean - df$mn_mean) / pmax(df$mn_mean, 1e-6)

cat("=== τ² median vs multinma (before/after 1-loop correction) ===\n\n")
cat(sprintf("%-12s %-5s %3s %7s  %7s %7s %7s  %+7s %+7s\n",
            "name","lk","k","true","MNmed","LaMed","CoMed","LaPct","CoPct"))
cat(strrep("-", 82), "\n", sep = "")
for (i in seq_len(nrow(df))) {
  r <- df[i,]
  cat(sprintf("%-12s %-5s %3d %7.4f  %7.4f %7.4f %7.4f  %+6.1f%% %+6.1f%%\n",
              r$name, substr(r$lk,1,5), r$k, r$tau2_true,
              r$mn_med, r$la_med, r$co_med, r$d_la_med, r$d_co_med))
}

cat("\n\n=== τ² mean vs multinma ===\n\n")
cat(sprintf("%-12s %-5s %3s  %7s %7s %7s  %+7s %+7s\n",
            "name","lk","k","MNmean","LaMean","CoMean","LaPct","CoPct"))
cat(strrep("-", 76), "\n", sep = "")
for (i in seq_len(nrow(df))) {
  r <- df[i,]
  cat(sprintf("%-12s %-5s %3d  %7.4f %7.4f %7.4f  %+6.1f%% %+6.1f%%\n",
              r$name, substr(r$lk,1,5), r$k,
              r$mn_mean, r$la_mean, r$co_mean, r$d_la_mean, r$d_co_mean))
}

cat("\n\n=== Bias summary (median |err%|) ===\n")
for (lk_ in unique(df$lk)) {
  sub <- df[df$lk == lk_, ]
  cat(sprintf("%-10s  (n=%d)  median |err%%| on τ² median : Laplace %5.1f%%  Corrected %5.1f%%\n",
              lk_, nrow(sub),
              median(abs(sub$d_la_med)), median(abs(sub$d_co_med))))
  cat(sprintf("%-10s           median |err%%| on τ² mean   : Laplace %5.1f%%  Corrected %5.1f%%\n",
              "", median(abs(sub$d_la_mean)), median(abs(sub$d_co_mean))))
}

write.csv(df, "test/gc_compare/correction_comparison.csv", row.names = FALSE)
cat("\nSaved: test/gc_compare/correction_comparison.csv\n")
