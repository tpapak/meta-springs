#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(jsonlite); library(multinma) })
options(mc.cores = parallel::detectCores())

d <- fromJSON("/tmp/truth.json")
d$study <- factor(d$study); d$treatment <- factor(d$treatment)
d$se    <- 1 / sqrt(d$n)
ref     <- levels(d$treatment)[1]
net <- set_agd_arm(d, study = study, trt = treatment,
                   y = mean, se = se, trt_ref = ref)

fit <- nma(net, trt_effects = "random", likelihood = "normal",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1),
           iter = 4000, warmup = 1000, chains = 4, refresh = 0, seed = 42)

tau   <- as.data.frame(fit, pars = "tau")[[1]]
tau2  <- tau^2
mode_density <- function(x) {
  d <- density(x, from=0, to=max(x), n=2048); d$x[which.max(d$y)]
}

write_json(list(
  tau_param   = "multinma's prior_het is on tau (the SD)",
  mode        = mode_density(tau2),
  median      = median(tau2),
  mean        = mean(tau2),
  ci_lo       = unname(quantile(tau2, 0.025)),
  ci_hi       = unname(quantile(tau2, 0.975)),
  tau_median  = median(tau),
  tau_mean    = mean(tau)
), "/tmp/truth_mn.json", auto_unbox=TRUE, digits=6)
cat(sprintf("multinma  τ²: mode=%.4f median=%.4f mean=%.4f CI=(%.4f, %.4f)\n",
            mode_density(tau2), median(tau2), mean(tau2),
            quantile(tau2, 0.025), quantile(tau2, 0.975)))
