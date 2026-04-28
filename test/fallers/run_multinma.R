#!/usr/bin/env Rscript
# multinma (Bayesian MCMC) on the fallers arm-level reconstruction.
# Same data as gc-rust and netmeta. HN(τ; 0.5) prior matching gc-rust's
# HN(τ; σ=0.5) so summaries are directly comparable.

suppressPackageStartupMessages({library(multinma); library(jsonlite)})
options(mc.cores = parallel::detectCores())

arms <- fromJSON("test/fallers/data/fallers_armlevel.json")
arms$treatment <- factor(as.character(arms$treatment))
arms$study     <- factor(arms$study)
arms$se        <- arms$sd / sqrt(arms$n)

cat(sprintf("studies=%d  treatments=%d  arm-rows=%d\n",
            length(unique(arms$study)), length(unique(arms$treatment)), nrow(arms)))

net <- set_agd_arm(arms, study = study, trt = treatment,
                   y = mean, se = se,
                   trt_ref = levels(arms$treatment)[1])
cat("\nnetwork:\n"); print(net)

t0 <- Sys.time()
fit <- nma(net, trt_effects = "random",
           likelihood = "normal",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 0.5),
           iter = 2000, warmup = 1000, chains = 4,
           refresh = 0, seed = 42,
           control = list(adapt_delta = 0.95))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\nmultinma fit time: %.2f s\n", elapsed))

tau_samples <- as.data.frame(fit, pars = "tau")[[1]]
tau2 <- tau_samples^2
dens <- density(tau2, from = 0, to = max(tau2), n = 4096)

result <- list(
  wall_sec     = elapsed,
  tau_mean     = mean(tau_samples),
  tau_median   = median(tau_samples),
  tau_sd       = sd(tau_samples),
  tau_ci_lo    = as.numeric(quantile(tau_samples, 0.025)),
  tau_ci_hi    = as.numeric(quantile(tau_samples, 0.975)),
  tau2_mean    = mean(tau2),
  tau2_median  = median(tau2),
  tau2_mode    = dens$x[which.max(dens$y)],
  tau2_ci_lo   = as.numeric(quantile(tau2, 0.025)),
  tau2_ci_hi   = as.numeric(quantile(tau2, 0.975)),
  n_studies    = length(unique(arms$study)),
  n_treatments = nlevels(arms$treatment),
  prior        = "half_normal(scale = 0.5)"
)
cat(sprintf("\nτ²: mean=%.4f median=%.4f mode=%.4f  CI=(%.4f, %.4f)\n",
            result$tau2_mean, result$tau2_median, result$tau2_mode,
            result$tau2_ci_lo, result$tau2_ci_hi))
write_json(result, "test/fallers/multinma_result.json",
           auto_unbox = TRUE, pretty = TRUE)
cat("wrote test/fallers/multinma_result.json\n")
