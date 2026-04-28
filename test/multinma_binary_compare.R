#!/usr/bin/env Rscript
# Compare springGrandCanonicalBin against multinma on test/binary.json.
# multinma uses exact binomial likelihood with logit link -> fairer comparison
# than bayesmeta (which uses a normal approximation to logOR).

suppressPackageStartupMessages({
  library(jsonlite)
  library(multinma)
  library(dplyr)
})

dw <- fromJSON("test/binary.json")

# Wide -> long
long <- bind_rows(
  data.frame(study = dw$study, treatment = dw$treatmentA,
             events = dw$eventsA, n = dw$nA),
  data.frame(study = dw$study, treatment = dw$treatmentB,
             events = dw$eventsB, n = dw$nB)
)
long$study     <- factor(long$study, levels = dw$study)
long$treatment <- factor(long$treatment, levels = c("A", "B"))

cat("=== test/binary.json (A=reference, B=comparator) ===\n")
print(long)

net <- set_agd_arm(long, study = study, trt = treatment,
                   r = events, n = n, trt_ref = "A")

# Half-Normal(0, 1) on tau (multinma standard); use wide normal priors on
# fixed effects so the prior doesn't dominate.
fit <- nma(net,
           trt_effects    = "random",
           likelihood     = "binomial", link = "logit",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1),
           iter  = 8000, warmup = 2000, chains = 4,
           seed  = 42, refresh = 0)

print(fit)

# Extract tau^2 posterior
tau_sims  <- as.data.frame(fit, pars = "tau")[[1]]
tau2_sims <- tau_sims^2

mode_density <- function(x) {
  dens <- density(x, from = 0, to = max(x), n = 2048)
  dens$x[which.max(dens$y)]
}

tau2_mode   <- mode_density(tau2_sims)
tau2_median <- median(tau2_sims)
tau2_mean   <- mean(tau2_sims)
tau2_lo     <- quantile(tau2_sims, 0.025)
tau2_hi     <- quantile(tau2_sims, 0.975)

cat("\n--- multinma (exact binomial, half-Normal(0,1) on tau) ---\n")
cat(sprintf("  tau2 mode   : %.5f\n", tau2_mode))
cat(sprintf("  tau2 median : %.5f\n", tau2_median))
cat(sprintf("  tau2 mean   : %.5f\n", tau2_mean))
cat(sprintf("  tau2 95%% CI : (%.5f, %.5f)\n", tau2_lo, tau2_hi))

# Treatment effect d_BA
re <- relative_effects(fit, trt_ref = "A")
params <- dimnames(re$sims)[[3]]
for (p in params) {
  v <- as.vector(re$sims[,,p])
  cat(sprintf("  %-10s mean=%+.4f median=%+.4f 95%% CI (%+.4f, %+.4f)\n",
              p, mean(v), median(v),
              quantile(v, 0.025), quantile(v, 0.975)))
}

out <- list(
  dataset = "test/binary.json",
  prior   = "half-Normal(0, 1) on tau",
  model   = "multinma exact binomial, logit link",
  tau2    = list(mode = tau2_mode, median = tau2_median, mean = tau2_mean,
                 ci95 = as.numeric(c(tau2_lo, tau2_hi)))
)
write_json(out, "test/multinma_binary_result.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nSaved: test/multinma_binary_result.json\n")
