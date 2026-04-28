#!/usr/bin/env Rscript
# Run multinma on a subset (10 reps per cell = 160 datasets)
library(multinma)
library(jsonlite)

manifest <- fromJSON("test/synth_bias/manifest.json")

# Subset: first 10 reps per (T, tau2) cell
keep <- character(0)
for (T in unique(manifest$treats)) {
  for (t2 in unique(manifest$tau2)) {
    matches <- manifest[manifest$treats == T & manifest$tau2 == t2, ]
    keep <- c(keep, matches$name[1:min(10, nrow(matches))])
  }
}
manifest <- manifest[manifest$name %in% keep, ]
cat(sprintf("Running multinma on %d datasets\n", nrow(manifest)))

results <- list()
for (i in seq_len(nrow(manifest))) {
  name <- manifest$name[i]
  fn <- sprintf("test/synth_bias/data/%s.json", name)
  cat(sprintf("[%d/%d] %s ... ", i, nrow(manifest), name))

  d <- fromJSON(fn)
  d$study     <- factor(d$study)
  d$treatment <- factor(d$treatment)

  tryCatch({
    net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n)
    fit <- nma(net,
      trt_effects = "random",
      likelihood  = "binomial", link = "logit",
      prior_intercept = normal(scale = 100),
      prior_trt       = normal(scale = 100),
      prior_het       = half_normal(scale = 5),
      prior_het_type  = "var",
      iter = 3000, warmup = 1000, chains = 2,
      seed = 42, refresh = 0)

    tau_sims   <- as.data.frame(fit, pars = "tau")[[1]]
    tau2_samps <- tau_sims^2
    tau2_ci    <- quantile(tau2_samps, c(0.025, 0.975))
    dens <- density(tau2_samps, from = 0, bw = "SJ")
    tau2_mode <- dens$x[which.max(dens$y)]

    re <- relative_effects(fit, trt_ref = "1")
    params <- dimnames(re$sims)[[3]]
    effects <- list()
    for (p in params) {
      vals <- as.vector(re$sims[,,p])
      ci <- quantile(vals, c(0.025, 0.975))
      effects[[p]] <- list(mean = mean(vals), median = median(vals),
                           lo = as.numeric(ci[1]), hi = as.numeric(ci[2]))
    }

    cat(sprintf("tau2 mode=%.4f median=%.4f\n", tau2_mode, median(tau2_samps)))

    results[[name]] <- list(
      name = name,
      tau2_true = manifest$tau2[i], treats = manifest$treats[i],
      tau2_mode = tau2_mode, tau2_mean = mean(tau2_samps),
      tau2_median = median(tau2_samps),
      tau2_lo = as.numeric(tau2_ci[1]), tau2_hi = as.numeric(tau2_ci[2]),
      effects = effects)
  }, error = function(e) cat(sprintf("ERROR: %s\n", e$message)))
}

write_json(results, "test/synth_bias/multinma_results.json",
           pretty = TRUE, auto_unbox = TRUE)
cat("\nDone.\n")
