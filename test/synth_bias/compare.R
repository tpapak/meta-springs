#!/usr/bin/env Rscript
library(jsonlite)

spring_raw <- fromJSON("test/synth_bias/spring_results.json")
bayes_list <- fromJSON("test/synth_bias/multinma_results.json")
manifest   <- fromJSON("test/synth_bias/manifest.json")

sp <- spring_raw
names(sp)[names(sp) == "tau2"] <- "sp_tau2"

bayes <- do.call(rbind, lapply(bayes_list, function(x) {
  if (!is.null(x$error)) return(NULL)
  data.frame(name = x$name,
             by_mode = x$tau2_mode, by_med = x$tau2_median, by_mean = x$tau2_mean,
             stringsAsFactors = FALSE)
}))

df <- merge(merge(sp, bayes, by = "name"), manifest, by = "name")
df <- df[order(df$treats), ]

cat("\n=== Dias TSD 2 NMA: Spring vs multinma (true τ² = 0.3) ===\n\n")
cat(sprintf("%-16s %4s %4s | %8s %8s %8s %8s | %8s %8s %8s\n",
    "Name", "T", "k", "spring", "by_mode", "by_med", "by_mean",
    "sp/tr", "byM/tr", "sp/byM"))
cat(strrep("-", 100), "\n")
for (i in seq_len(nrow(df))) {
  cat(sprintf("%-16s %4d %4d | %8.4f %8.4f %8.4f %8.4f | %8.4f %8.4f %8.4f\n",
      df$name[i], df$treats[i], df$k[i],
      df$sp_tau2[i], df$by_mode[i], df$by_med[i], df$by_mean[i],
      df$sp_tau2[i] / df$tau2[i],
      df$by_mode[i] / df$tau2[i],
      if (df$by_mode[i] > 1e-4) df$sp_tau2[i] / df$by_mode[i] else NA))
}

cat("\n=== Overall ===\n")
valid <- df[df$by_mode > 1e-4, ]
cat(sprintf("Median spring/true:       %.4f\n", median(df$sp_tau2 / df$tau2)))
cat(sprintf("Median bayes_mode/true:   %.4f\n", median(valid$by_mode / valid$tau2)))
cat(sprintf("Median bayes_median/true: %.4f\n", median(df$by_med / df$tau2)))
cat(sprintf("Median spring/bayes_mode: %.4f\n", median(valid$sp_tau2 / valid$by_mode)))
