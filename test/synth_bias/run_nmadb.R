#!/usr/bin/env Rscript
# Run multinma (with mode) on the 20 nmadb binary datasets.
library(multinma)
library(jsonlite)

datasets <- c(
  "nmadb_501212", "nmadb_482472", "nmadb_481147", "nmadb_479806",
  "nmadb_482521", "nmadb_476033",
  "nmadb_501226", "nmadb_501412", "nmadb_480430", "nmadb_481731",
  "nmadb_501311", "nmadb_501263",
  "nmadb_501324", "nmadb_482440", "nmadb_501367", "nmadb_481216",
  "nmadb_479574", "nmadb_501340",
  "nmadb_473269", "nmadb_473552"
)

results <- list()

for (ds in datasets) {
  cat(sprintf("\n=== %s ===\n", ds))
  fn <- sprintf("test/nmadb/%s.json", ds)
  if (!file.exists(fn)) { cat("  File not found\n"); next }
  d <- fromJSON(fn)

  # Exclude all-zero studies
  study_ids <- unique(d$study)
  for (sid in study_ids) {
    s <- d[d$study == sid, ]
    if (all(s$events == 0)) d <- d[d$study != sid, ]
  }
  d$study     <- factor(d$study)
  d$treatment <- factor(d$treatment)

  tryCatch({
    net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n)
    ref_trt <- levels(d$treatment)[1]

    fit <- nma(net,
      trt_effects = "random",
      likelihood  = "binomial", link = "logit",
      prior_intercept = normal(scale = 100),
      prior_trt       = normal(scale = 100),
      prior_het       = half_normal(scale = 5),
      prior_het_type  = "var",
      iter = 5000, warmup = 2000, chains = 4,
      seed = 42, refresh = 0)

    tau_sims   <- as.data.frame(fit, pars = "tau")[[1]]
    tau2_samps <- tau_sims^2
    tau2_mean   <- mean(tau2_samps)
    tau2_median <- median(tau2_samps)
    dens <- density(tau2_samps, from = 0, bw = "SJ")
    tau2_mode <- dens$x[which.max(dens$y)]

    cat(sprintf("  tau2: mode=%.4f median=%.4f mean=%.4f\n",
                tau2_mode, tau2_median, tau2_mean))

    results[[ds]] <- list(
      name        = ds,
      tau2_mode   = tau2_mode,
      tau2_median = tau2_median,
      tau2_mean   = tau2_mean)

  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

write_json(results, "test/synth_bias/nmadb_bayes_results.json",
           pretty = TRUE, auto_unbox = TRUE)
cat("\nDone.\n")
