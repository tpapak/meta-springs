#!/usr/bin/env Rscript
# netmeta wall-time benchmark on the same datasets used for GC and multinma.
# netmeta is frequentist NMA: REML τ̂² + Wald CIs per contrast (no Bayesian
# posterior).  We just measure wall and report the point estimates.
suppressPackageStartupMessages({
  library(jsonlite); library(meta); library(netmeta)
})

# Helper: arm-level binary JSON → pairwise contrast frame for netmeta.
arm_to_pairwise <- function(d_raw) {
  d <- data.frame(
    study = as.character(d_raw$study),
    treatment = as.character(d_raw$treatment),
    events = as.integer(d_raw$events),
    n = as.integer(d_raw$n)
  )
  pairwise(treat = treatment, event = events, n = n, studlab = study,
           data = d, sm = "OR", allstudies = TRUE)
}

run_one <- function(label, path) {
  d_raw <- fromJSON(path)
  pw <- arm_to_pairwise(d_raw)
  ref <- sort(unique(c(pw$treat1, pw$treat2)))[1]
  t0 <- Sys.time()
  fit <- netmeta(TE = pw$TE, seTE = pw$seTE,
                 treat1 = pw$treat1, treat2 = pw$treat2,
                 studlab = pw$studlab,
                 sm = "OR", reference.group = ref,
                 random = TRUE, common = FALSE,
                 method.tau = "REML")    # default is DL — force REML
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))
  tau2_hat <- fit$tau2
  cat(sprintf("%-30s  T=%-3d  k=%-3d  wall=%6.3fs   τ̂² (REML) = %.4f\n",
              label, length(unique(c(pw$treat1, pw$treat2))),
              length(unique(pw$studlab)), wall, tau2_hat))
  invisible(list(label = label, wall_sec = wall, tau2 = tau2_hat,
                 n_treats = length(unique(c(pw$treat1, pw$treat2))),
                 n_studies = length(unique(pw$studlab))))
}

cat("netmeta REML benchmark (single fit, contrast model)\n")
cat(strrep("-", 78), "\n", sep = "")
results <- list()
for (path in c("test/diabetes.json",
               "/tmp/t30_reps_bin/rep_01.json",
               "/tmp/t50_rare_bin/rep_01.json",
               "/tmp/t100_reps_bin/rep_01.json")) {
  results[[path]] <- run_one(path, path)
}
cat(strrep("-", 78), "\n", sep = "")
cat("Note: netmeta gives a point estimate of τ² (no posterior).\n")
cat("Rust GC gives 4 prior-conditional posteriors via 1 fit + reweights.\n")

write_json(results, "/tmp/netmeta_bench.json", auto_unbox = TRUE, digits = 6)
cat("Wrote /tmp/netmeta_bench.json\n")
