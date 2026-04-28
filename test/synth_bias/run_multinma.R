#!/usr/bin/env Rscript
# Run multinma on Dias TSD 2 synthetic datasets (test/synth_bias/data/).
library(multinma)
library(jsonlite)

manifest <- fromJSON("test/synth_bias/manifest.json")
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
      iter = 5000, warmup = 2000, chains = 4,
      seed = 42, refresh = 0)

    tau_sims   <- as.data.frame(fit, pars = "tau")[[1]]
    tau2_samps <- tau_sims^2
    tau2_mean   <- mean(tau2_samps)
    tau2_median <- median(tau2_samps)
    dens <- density(tau2_samps, from = 0, bw = "SJ")
    tau2_mode <- dens$x[which.max(dens$y)]

    # tau2 95% credible interval
    tau2_ci <- quantile(tau2_samps, c(0.025, 0.975))

    # All treatment effects vs reference (treatment 1)
    re <- relative_effects(fit, trt_ref = "1")
    params <- dimnames(re$sims)[[3]]
    effects <- list()
    for (p in params) {
      vals <- as.vector(re$sims[,,p])
      ci <- quantile(vals, c(0.025, 0.975))
      effects[[p]] <- list(mean = mean(vals), median = median(vals),
                           sd = sd(vals),
                           lo = as.numeric(ci[1]), hi = as.numeric(ci[2]))
    }

    cat(sprintf("tau2: mode=%.4f median=%.4f mean=%.4f\n",
                tau2_mode, tau2_median, tau2_mean))

    results[[name]] <- list(
      name = name,
      tau2_true = manifest$tau2[i],
      treats    = manifest$treats[i],
      tau2_mode = tau2_mode,
      tau2_median = tau2_median,
      tau2_mean = tau2_mean,
      tau2_lo   = as.numeric(tau2_ci[1]),
      tau2_hi   = as.numeric(tau2_ci[2]),
      effects   = effects)

  }, error = function(e) {
    cat(sprintf("ERROR: %s\n", e$message))
  })
}

write_json(results, "test/synth_bias/multinma_results.json",
           pretty = TRUE, auto_unbox = TRUE)
cat("\nDone.\n")
