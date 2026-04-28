#!/usr/bin/env Rscript
# Direct INLA call (no nmaINLA wrapper) for the NMA random-effects
# model on the fallers arm-level reconstruction.  Implements the
# Sauter-Held (2015) consistency model: per-arm observation with
# treatment fixed effect + per-study random intercept.
#
# y_{ia} ~ N(mu_a + b_i, sigma_{ia}²)
# b_i    ~ N(0, tau²)        prior tau ~ uniform on [0, 5] per nmaINLA default
#
# Requires only INLA + jsonlite (no nmaINLA, no dplyr).

suppressPackageStartupMessages({library(INLA); library(jsonlite)})

# Read arm-level JSON
arms <- fromJSON("test/fallers/data/fallers_armlevel.json")
arms$treatment <- as.character(arms$treatment)
treats <- sort(unique(arms$treatment))
arms$tid <- match(arms$treatment, treats)
arms$prec <- 1 / (arms$sd^2 / arms$n)   # observation precision = n / sd²

cat(sprintf("studies=%d  treatments=%d  arm-rows=%d\n",
            length(unique(arms$study)), length(treats), nrow(arms)))

# Reference treatment = first one alphabetically (matches gc-rust convention)
ref_idx <- 1
arms$treat_factor <- factor(arms$tid)
arms$treat_factor <- relevel(arms$treat_factor, ref = as.character(ref_idx))

# Uniform prior on τ ∈ [0, 5] → on precision scale: U(1/25, ∞)
# Use loggamma prior on log-precision approximating uniform on tau.
# nmaINLA default is tau.prior="uniform", tau.par=c(0,5).
hyper.iid <- list(prec = list(prior = "logtnormal", param = c(0, 1/5^2)))

formula <- mean ~ -1 + treat_factor + f(study, model = "iid", hyper = hyper.iid)

cat("\n--- starting INLA fit ---\n"); flush.console()
t0 <- Sys.time()
fit <- inla(formula, family = "gaussian", data = arms,
            scale = arms$prec,                      # observation precision
            control.family = list(hyper = list(prec = list(initial = log(1e6),
                                                            fixed   = TRUE))),
            control.compute = list(config = TRUE))
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("\nINLA fit time: %.2f s\n", elapsed))

# Extract τ posterior
hyp <- fit$summary.hyperpar
print(hyp)

# τ = 1/sqrt(precision); transform marginal
tau_marg <- inla.tmarginal(function(prec) 1/sqrt(prec),
                            fit$marginals.hyperpar$`Precision for study`)
tau_summary <- inla.zmarginal(tau_marg, silent = TRUE)
cat(sprintf("\nτ posterior: mean=%.4f median=%.4f sd=%.4f 95%% CI=(%.4f, %.4f)\n",
            tau_summary$mean, tau_summary$quant0.5, tau_summary$sd,
            tau_summary$quant0.025, tau_summary$quant0.975))

# Save
out <- list(
  fit_sec     = elapsed,
  tau_mean    = tau_summary$mean,
  tau_median  = tau_summary$quant0.5,
  tau_sd      = tau_summary$sd,
  tau_lo      = tau_summary$quant0.025,
  tau_hi      = tau_summary$quant0.975,
  tau2_median = tau_summary$quant0.5^2,
  tau2_mean   = tau_summary$mean^2 + tau_summary$sd^2
)
write_json(out, "test/fallers/inla_direct_result.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nwrote test/fallers/inla_direct_result.json\n")
