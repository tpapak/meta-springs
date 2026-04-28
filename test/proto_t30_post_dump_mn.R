#!/usr/bin/env Rscript
# Same fit as proto_t30_reps_mn{,_cont}.R but for ONE replicate, dumping the
# raw HMC samples for τ² and every contrast 1→j.  Argv: <bin|cont> <repIdx>.
# Outputs /tmp/t30_post_<regime>_mn.json with arrays of MCMC draws.
suppressPackageStartupMessages({
  library(jsonlite); library(multinma)
})
options(mc.cores = parallel::detectCores())

args <- commandArgs(trailingOnly = TRUE)
regime <- args[1]
rep    <- as.integer(args[2])
data_path <- sprintf("/tmp/t30_reps_%s/rep_%02d.json", regime, rep)
out_path  <- sprintf("/tmp/t30_post_%s_mn.json", regime)

d_raw <- fromJSON(data_path)
d <- if (regime == "bin") {
  data.frame(study = factor(d_raw$study),
             treatment = factor(d_raw$treatment),
             events = as.integer(d_raw$events),
             n = as.integer(d_raw$n))
} else {
  data.frame(study = factor(d_raw$study),
             treatment = factor(d_raw$treatment),
             mean = as.numeric(d_raw$mean),
             sd = as.numeric(d_raw$sd),
             n = as.integer(d_raw$n))
}
ref <- levels(d$treatment)[1]

if (regime == "bin") {
  net <- set_agd_arm(d, study = study, trt = treatment,
                     r = events, n = n, trt_ref = ref)
  fit <- nma(net, trt_effects = "random",
             likelihood = "binomial", link = "logit",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = half_normal(scale = 1.0),
             iter = 3000, warmup = 1000, chains = 4,
             refresh = 0, seed = 42)
} else {
  d$se <- d$sd / sqrt(d$n)
  net <- set_agd_arm(d, study = study, trt = treatment,
                     y = mean, se = se, trt_ref = ref)
  fit <- nma(net, trt_effects = "random", likelihood = "normal",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = half_normal(scale = 1.0),
             iter = 3000, warmup = 1000, chains = 4,
             refresh = 0, seed = 42)
}

tau_samp <- as.data.frame(fit, pars = "tau")[[1]]
tau2     <- tau_samp ^ 2

re   <- relative_effects(fit, trt_ref = ref)
sims <- re$sims
pn   <- dimnames(sims)[[3]]
contrast_samples <- list()
for (j in seq_along(pn)) {
  x <- as.vector(sims[,,j])
  lbl <- pn[j]
  if (startsWith(lbl, "d[")) lbl <- substr(lbl, 3, nchar(lbl) - 1)
  contrast_samples[[length(contrast_samples) + 1L]] <- list(
    to     = lbl,
    samples = x
  )
}

out <- list(
  prior_label = "HalfNormal(τ; σ=1)",
  rep = rep,
  regime = regime,
  tau2_samples = tau2,
  contrasts = contrast_samples
)
write_json(out, out_path, auto_unbox = TRUE, digits = 6)
cat(sprintf("Wrote %s  (%d τ² samples, %d contrasts)\n",
            out_path, length(tau2), length(contrast_samples)))
