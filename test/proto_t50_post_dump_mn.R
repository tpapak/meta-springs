#!/usr/bin/env Rscript
# multinma posterior dump for one T=50 rare-events replicate, prior
# HalfNormal(τ; σ=1).  Dumps all HMC samples for τ² and every contrast.
# Argv: <repIdx>. Output /tmp/t50_post_mn.json.
suppressPackageStartupMessages({ library(jsonlite); library(multinma) })
options(mc.cores = parallel::detectCores())

args <- commandArgs(trailingOnly = TRUE)
rep  <- if (length(args) >= 1) as.integer(args[1]) else 4L
path <- sprintf("/tmp/t50_rare_bin/rep_%02d.json", rep)

d_raw <- fromJSON(path)
d <- data.frame(
  study     = factor(d_raw$study),
  treatment = factor(d_raw$treatment),
  events    = as.integer(d_raw$events),
  n         = as.integer(d_raw$n)
)
ref <- levels(d$treatment)[1]
net <- set_agd_arm(d, study = study, trt = treatment,
                   r = events, n = n, trt_ref = ref)

cat(sprintf("multinma T=50 rare dump: rep %d (%d arms)\n", rep, nrow(d)))
fit <- nma(net, trt_effects = "random",
           likelihood = "binomial", link = "logit",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1.0),
           iter = 4000, warmup = 1500, chains = 4,
           refresh = 0, seed = 42)

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
    to = lbl, mean = mean(x), sd = sd(x), samples = x
  )
}

out <- list(
  rep = rep, engine = "multinma", prior_label = "HalfNormal(τ; σ=1)",
  ref = ref, tau2_samples = tau2, contrasts = contrast_samples
)
write_json(out, "/tmp/t50_post_mn.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("Wrote /tmp/t50_post_mn.json (%d τ² samples, %d contrasts)\n",
            length(tau2), length(contrast_samples)))
