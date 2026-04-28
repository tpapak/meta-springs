#!/usr/bin/env Rscript
# netmeta REML benchmark on continuous (Gaussian) NMA: T=30 rep 1 + T=30
# rep 5 + diabetes-like.  Reads arm-level mean/sd/n JSON.
suppressPackageStartupMessages({
  library(jsonlite); library(meta); library(netmeta)
})

arm_to_pairwise_cont <- function(d_raw) {
  d <- data.frame(
    study = as.character(d_raw$study),
    treatment = as.character(d_raw$treatment),
    mean = as.numeric(d_raw$mean),
    sd = as.numeric(d_raw$sd),
    n = as.integer(d_raw$n)
  )
  pairwise(treat = treatment, mean = mean, sd = sd, n = n,
           studlab = study, data = d, sm = "MD")
}

run_one <- function(label, path) {
  d_raw <- fromJSON(path)
  pw <- arm_to_pairwise_cont(d_raw)
  ref <- sort(unique(c(pw$treat1, pw$treat2)))[1]
  t0 <- Sys.time()
  fit <- netmeta(TE = pw$TE, seTE = pw$seTE,
                 treat1 = pw$treat1, treat2 = pw$treat2,
                 studlab = pw$studlab, sm = "MD",
                 reference.group = ref,
                 random = TRUE, common = FALSE,
                 method.tau = "REML")
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))
  cat(sprintf("%-32s  T=%-3d  k=%-3d  wall=%6.3fs   τ̂² (REML) = %.4f\n",
              label, length(unique(c(pw$treat1, pw$treat2))),
              length(unique(pw$studlab)), wall, fit$tau2))
  invisible(list(label = label, wall_sec = wall, tau2 = fit$tau2,
                 n_treats = length(unique(c(pw$treat1, pw$treat2))),
                 n_studies = length(unique(pw$studlab))))
}

cat("netmeta REML benchmark — continuous (mean difference)\n")
cat(strrep("-", 78), "\n", sep = "")
results <- list()
# T=30 continuous reps from earlier study
for (r in c(1, 5, 15, 30)) {
  path <- sprintf("/tmp/t30_reps_cont/rep_%02d.json", r)
  results[[path]] <- run_one(sprintf("t30_cont rep %02d", r), path)
}
cat(strrep("-", 78), "\n", sep = "")

write_json(results, "/tmp/netmeta_cont_bench.json", auto_unbox = TRUE, digits = 6)
cat("Wrote /tmp/netmeta_cont_bench.json\n")
