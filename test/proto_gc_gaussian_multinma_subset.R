#!/usr/bin/env Rscript
# Quick multinma comparison on a few Gaussian multi-arm datasets.
# Half-normal(0,1) prior on τ; we report the posterior MODE of τ²
# (KDE on the posterior of τ²) so it is comparable to the GC posterior
# mode and to REML.
#
# Output: test/gc_compare/gaussian_multiarm_multinma.json

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
})
options(mc.cores = parallel::detectCores())

datasets <- list(
  list(name = "senn2013",  file = "test/nma_senn2013.json"),
  list(name = "parkinson", file = "test/nma_parkinson.json"),
  list(name = "stowe2010", file = "test/nma_stowe2010.json"),
  list(name = "synth_c1",  file = "test/nma_synth_c1.json"),
  list(name = "synth_c4",  file = "test/nma_synth_c4.json"),
  list(name = "synth_c6",  file = "test/nma_synth_c6.json")
)

fit_one <- function(ds) {
  cat(sprintf("\n=== %s ===\n", ds$name)); flush.console()
  d <- fromJSON(ds$file, simplifyDataFrame = TRUE)
  d$study     <- factor(d$study)
  d$treatment <- factor(d$treatment)
  net <- set_agd_arm(d, study = study, trt = treatment,
                     y = mean, se = sd / sqrt(n),
                     trt_ref = levels(d$treatment)[1])
  fit <- nma(net, trt_effects = "random",
             likelihood = "normal",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = half_normal(scale = 1),
             iter = 2000, warmup = 1000, chains = 4,
             refresh = 0, seed = 42)
  tau  <- as.data.frame(fit, pars = "tau")[[1]]
  tau2 <- tau^2
  dens <- density(tau2, from = 0, to = max(tau2), n = 4096)
  list(name        = ds$name,
       k           = length(unique(d$study)),
       tau2_mode   = dens$x[which.max(dens$y)],
       tau2_median = median(tau2),
       tau2_mean   = mean(tau2),
       tau2_ci_lo  = as.numeric(quantile(tau2, 0.025)),
       tau2_ci_hi  = as.numeric(quantile(tau2, 0.975)))
}

res <- lapply(datasets, fit_one)
write_json(list(results = res),
           path = "test/gc_compare/gaussian_multiarm_multinma.json",
           auto_unbox = TRUE, pretty = TRUE)
cat("\nwrote test/gc_compare/gaussian_multiarm_multinma.json\n")
for (r in res) cat(sprintf("%-12s τ²: mode=%.5f median=%.5f mean=%.5f\n",
                           r$name, r$tau2_mode, r$tau2_median, r$tau2_mean))
