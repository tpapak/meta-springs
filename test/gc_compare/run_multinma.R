#!/usr/bin/env Rscript
# Reference multinma NMA fits on the 20 datasets listed in datasets.json.
# For each: fit RE NMA with exact likelihood (binomial-logit or normal-identity),
# save tau2 posterior samples (thinned) and treatment-effect posterior summaries.

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
  library(dplyr)
})

options(mc.cores = parallel::detectCores())
set.seed(42)

dsets <- fromJSON("test/gc_compare/datasets.json", simplifyDataFrame = FALSE)

# MCMC settings: small-ish for a fast 20-dataset sweep.
MCMC <- list(iter = 3000, warmup = 1000, chains = 4, refresh = 0, seed = 42)

summarize_tau2 <- function(samples) {
  tau2 <- samples^2
  dens <- density(tau2, from = 0, to = max(tau2), n = 2048)
  list(
    mode   = dens$x[which.max(dens$y)],
    median = median(tau2),
    mean   = mean(tau2),
    ci_lo  = as.numeric(quantile(tau2, 0.025)),
    ci_hi  = as.numeric(quantile(tau2, 0.975))
  )
}

fit_one <- function(name, lk, path) {
  cat(sprintf("\n=== %s (%s) ===\n", name, lk))
  d <- fromJSON(path)
  if (is.data.frame(d)) d <- as.data.frame(d)
  d$study     <- factor(d$study)
  d$treatment <- factor(d$treatment)

  if (lk == "Binomial") {
    net <- set_agd_arm(d, study = study, trt = treatment,
                       r = events, n = n,
                       trt_ref = levels(d$treatment)[1])
    fit <- nma(net, trt_effects = "random",
               likelihood = "binomial", link = "logit",
               prior_intercept = normal(scale = 100),
               prior_trt       = normal(scale = 100),
               prior_het       = half_normal(scale = 1),
               iter = MCMC$iter, warmup = MCMC$warmup,
               chains = MCMC$chains, refresh = MCMC$refresh,
               seed = MCMC$seed)
  } else {
    # multinma uses y, se columns for pre-aggregated continuous outcomes,
    # or mean / sd / n at the arm level.
    net <- set_agd_arm(d, study = study, trt = treatment,
                       y = mean, se = sd / sqrt(n),
                       trt_ref = levels(d$treatment)[1])
    fit <- nma(net, trt_effects = "random",
               likelihood = "normal",
               prior_intercept = normal(scale = 100),
               prior_trt       = normal(scale = 100),
               prior_het       = half_normal(scale = 1),
               iter = MCMC$iter, warmup = MCMC$warmup,
               chains = MCMC$chains, refresh = MCMC$refresh,
               seed = MCMC$seed)
  }

  tau_samples <- as.data.frame(fit, pars = "tau")[[1]]
  tsum <- summarize_tau2(tau_samples)

  re <- relative_effects(fit, trt_ref = levels(d$treatment)[1])
  pars <- dimnames(re$sims)[[3]]
  effs <- list()
  for (p in pars) {
    v <- as.vector(re$sims[,,p])
    effs[[p]] <- list(
      mean   = mean(v), median = median(v),
      sd     = sd(v),
      ci_lo  = as.numeric(quantile(v, 0.025)),
      ci_hi  = as.numeric(quantile(v, 0.975))
    )
  }

  cat(sprintf("  tau2 mode=%.4f median=%.4f 95%% CI (%.4f, %.4f)\n",
              tsum$mode, tsum$median, tsum$ci_lo, tsum$ci_hi))

  list(
    name       = name,
    likelihood = lk,
    tau2       = tsum,
    # thin tau samples to keep JSON small
    tau_samples_thinned = tau_samples[seq(1, length(tau_samples), by = 20)],
    effects    = effs
  )
}

results <- list()
t0 <- Sys.time()
for (d in dsets$binary)     results[[d$name]] <- fit_one(d$name, "Binomial", d$file)
for (d in dsets$continuous) results[[d$name]] <- fit_one(d$name, "Gaussian", d$file)
t1 <- Sys.time()

cat(sprintf("\nTotal multinma time: %.1f min\n",
            as.numeric(difftime(t1, t0, units = "mins"))))

write_json(results, "test/gc_compare/multinma_results.json",
           auto_unbox = TRUE, digits = 6)
cat("Wrote: test/gc_compare/multinma_results.json\n")
