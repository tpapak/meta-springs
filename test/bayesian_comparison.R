#!/usr/bin/env Rscript
# Compare multinma Bayesian RE vs binomial spring RE on 20 nmadb binary datasets.
# Outputs JSON with tau2 and treatment effects for each dataset.

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

  # Load data
  fn <- sprintf("test/nmadb/%s.json", ds)
  if (!file.exists(fn)) { cat("  File not found, skipping\n"); next }
  d <- fromJSON(fn)

  # Check for zero-zero studies
  study_ids <- unique(d$study)
  zero_studies <- c()
  for (sid in study_ids) {
    s <- d[d$study == sid, ]
    if (all(s$events == 0)) zero_studies <- c(zero_studies, sid)
  }
  if (length(zero_studies) > 0) {
    cat("  Excluding 0/0 studies:", zero_studies, "\n")
    d <- d[!d$study %in% zero_studies, ]
  }

  d$study <- factor(d$study)
  d$treatment <- factor(d$treatment)

  tryCatch({
    net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n)
    ref_trt <- levels(d$treatment)[1]

    # Bayesian RE
    fit <- nma(net,
      trt_effects = "random",
      likelihood = "binomial", link = "logit",
      prior_intercept = normal(scale = 100),
      prior_trt = normal(scale = 100),
      prior_het = half_normal(scale = 1),
      iter = 4000, warmup = 1000, chains = 4,
      seed = 42, refresh = 0)

    # Extract tau
    tau_sims <- as.data.frame(fit, pars = "tau")
    tau2_mean <- mean(tau_sims[[1]]^2)
    tau2_median <- median(tau_sims[[1]]^2)

    # Extract effects vs reference
    re <- relative_effects(fit, trt_ref = ref_trt)
    params <- dimnames(re$sims)[[3]]
    effects <- list()
    for (p in params) {
      vals <- as.vector(re$sims[,,p])
      effects[[p]] <- list(mean = mean(vals), sd = sd(vals))
    }

    cat(sprintf("  tau2: mean=%.4f median=%.4f\n", tau2_mean, tau2_median))
    for (p in params) {
      cat(sprintf("  %-20s mean=%.4f\n", p, effects[[p]]$mean))
    }

    results[[ds]] <- list(
      tau2_mean = tau2_mean,
      tau2_median = tau2_median,
      ref = ref_trt,
      effects = effects
    )
  }, error = function(e) {
    cat(sprintf("  ERROR: %s\n", e$message))
  })
}

# Save results
write_json(results, "test/nmadb/bayesian_reference.json", pretty = TRUE, auto_unbox = TRUE)
cat("\nDone. Results saved to test/nmadb/bayesian_reference.json\n")
