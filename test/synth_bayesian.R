#!/usr/bin/env Rscript
# Run multinma Bayesian RE on 100 synthetic binary NMA datasets.
# Prior on tau^2 (variance) to match REML.
library(multinma)
library(jsonlite)

for (idx in 1:100) {
  name <- sprintf("synth_%03d", idx)
  fn <- sprintf("test/synth_bin/%s.json", name)
  d <- fromJSON(fn)

  # Exclude 0/0 studies
  study_ids <- unique(d$study)
  for (sid in study_ids) {
    s <- d[d$study == sid, ]
    if (all(s$events == 0)) d <- d[d$study != sid, ]
  }
  d$study <- factor(d$study)
  d$treatment <- factor(d$treatment)

  tryCatch({
    net <- set_agd_arm(d, study = study, trt = treatment, r = events, n = n)
    ref_trt <- levels(d$treatment)[1]

    fit <- nma(net,
      trt_effects = "random",
      likelihood = "binomial", link = "logit",
      prior_intercept = normal(scale = 100),
      prior_trt = normal(scale = 100),
      prior_het = half_normal(scale = 1),
      prior_het_type = "var",
      iter = 3000, warmup = 1000, chains = 2,
      seed = 42, refresh = 0)

    # tau2
    tau_sims <- as.data.frame(fit, pars = "tau")[[1]]
    tau2_samps <- tau_sims^2
    tau2_mean <- mean(tau2_samps)
    dens <- density(tau2_samps, from = 0, bw = "SJ")
    tau2_mode <- dens$x[which.max(dens$y)]

    # Effects vs reference
    re <- relative_effects(fit, trt_ref = ref_trt)
    params <- dimnames(re$sims)[[3]]
    effs <- sapply(params, function(p) mean(as.vector(re$sims[,,p])))

    eff_str <- paste(round(effs, 4), collapse = ",")
    cat(sprintf("%s %.6f %.6f %s\n", name, tau2_mode, tau2_mean, eff_str))
  }, error = function(e) {
    cat(sprintf("%s ERROR %s\n", name, e$message))
  })
}
