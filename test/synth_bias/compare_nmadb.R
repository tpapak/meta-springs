#!/usr/bin/env Rscript
library(jsonlite)

spring_raw <- fromJSON("test/synth_bias/nmadb_spring_results.json")
bayes_list <- fromJSON("test/synth_bias/nmadb_bayes_results.json")

sp <- spring_raw
names(sp)[names(sp) == "tau2"] <- "sp_tau2"

bayes <- do.call(rbind, lapply(bayes_list, function(x) {
  if (!is.null(x$error)) return(NULL)
  data.frame(name = x$name, by_mode = x$tau2_mode,
             by_med = x$tau2_median, by_mean = x$tau2_mean,
             stringsAsFactors = FALSE)
}))

df <- merge(sp, bayes, by = "name")

cat("\n=== nmadb: Spring vs multinma Bayesian ===\n\n")
cat(sprintf("%-20s %8s %8s %8s %8s | %8s\n",
    "Dataset", "spring", "by_mode", "by_med", "by_mean", "sp/byMode"))
cat(strrep("-", 76), "\n")
for (i in seq_len(nrow(df))) {
  ratio <- if (df$by_mode[i] > 0.001) df$sp_tau2[i] / df$by_mode[i] else NA
  cat(sprintf("%-20s %8.4f %8.4f %8.4f %8.4f | %8.4f\n",
      df$name[i], df$sp_tau2[i],
      df$by_mode[i], df$by_med[i], df$by_mean[i], ratio))
}

valid <- df[df$by_mode > 0.001 & df$sp_tau2 > 0.001, ]
cat(sprintf("\nMedian spring/bayes_mode: %.4f  (n=%d)\n",
    median(valid$sp_tau2 / valid$by_mode), nrow(valid)))
cat(sprintf("Mean   spring/bayes_mode: %.4f\n",
    mean(valid$sp_tau2 / valid$by_mode)))
