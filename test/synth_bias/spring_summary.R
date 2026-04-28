#!/usr/bin/env Rscript
library(jsonlite)

spring   <- fromJSON("test/synth_bias/spring_results.json")
manifest <- fromJSON("test/synth_bias/manifest.json")
names(spring)[names(spring) == "tau2"] <- "tau2_est"
df <- merge(spring, manifest, by = "name")

for (block in c("pairwise", "nma")) {
  sub_all <- df[df$type == block, ]
  cat(sprintf("\n=== %s (t=%d) : springREMLBin vs true tau2 ===\n\n",
      toupper(block), sub_all$treats[1]))
  cat(sprintf("%-14s %5s | %8s %8s %8s | %8s %8s\n",
      "scenario", "n", "true", "sp_mean", "sp_med", "ratio_mn", "ratio_md"))
  cat(strrep("-", 72), "\n")

  for (t2 in sort(unique(sub_all$tau2))) {
    for (mu0 in sort(unique(sub_all$mu0))) {
      sub <- sub_all[sub_all$tau2 == t2 & sub_all$mu0 == mu0, ]
      if (nrow(sub) == 0) next
      sp_mean <- mean(sub$tau2_est)
      sp_med  <- median(sub$tau2_est)
      label <- sprintf("tau%.1f_mu%+.0f", t2, mu0)
      cat(sprintf("%-14s %5d | %8.4f %8.4f %8.4f | %8.4f %8.4f\n",
          label, nrow(sub), t2, sp_mean, sp_med, sp_mean/t2, sp_med/t2))
    }
  }
  cat(sprintf("  Overall median ratio: %.4f\n",
      median(sub_all$tau2_est / sub_all$tau2)))
}
