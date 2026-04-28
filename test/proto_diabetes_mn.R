#!/usr/bin/env Rscript
# Prior investigation on the Dias diabetes NMA via multinma.  Three priors:
# HN(τ;0.5), HN(τ;1), HC(τ;0.5).  Records full HMC samples for τ² and every
# contrast vs the reference (Diuretic).  Output /tmp/diabetes_mn.json.
suppressPackageStartupMessages({
  library(jsonlite); library(multinma)
})
options(mc.cores = parallel::detectCores())

# diabetes.json is in the long format that includes both two-arm and three-arm
# trials with rows per arm.
d_raw <- fromJSON("test/diabetes.json")
d <- data.frame(
  study     = factor(d_raw$study),
  treatment = factor(d_raw$treatment),
  events    = as.integer(d_raw$events),
  n         = as.integer(d_raw$n)
)
# Reference: Diuretic (alphabetical first if treatments are ACE..., not ideal —
# pick Diuretic explicitly, since that's the convention in Dias 2013).
d$treatment <- relevel(d$treatment, ref = "Diuretic")
ref <- "Diuretic"
net <- set_agd_arm(d, study = study, trt = treatment,
                   r = events, n = n, trt_ref = ref)

priors <- list(
  list(label = "HalfNormal(τ; σ=0.5)", p = half_normal(scale = 0.5)),
  list(label = "HalfNormal(τ; σ=1)",   p = half_normal(scale = 1.0)),
  list(label = "HalfCauchy(τ; σ=0.5)", p = half_cauchy(scale = 0.5))
)

mode_density <- function(x) {
  d <- density(x, from = 0, to = max(x), n = 2048); d$x[which.max(d$y)]
}

results <- list()
t0_total <- Sys.time()
for (pp in priors) {
  cat(sprintf("Fitting %s ...\n", pp$label))
  t0 <- Sys.time()
  fit <- nma(net, trt_effects = "random",
             likelihood = "binomial", link = "logit",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = pp$p,
             iter = 4000, warmup = 1500, chains = 4,
             refresh = 0, seed = 42)
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))
  tau_samp <- as.data.frame(fit, pars = "tau")[[1]]
  tau2 <- tau_samp ^ 2
  re   <- relative_effects(fit, trt_ref = ref)
  sims <- re$sims
  pn   <- dimnames(sims)[[3]]
  contrast_samples <- list()
  for (j in seq_along(pn)) {
    x <- as.vector(sims[,,j])
    lbl <- pn[j]
    if (startsWith(lbl, "d[")) lbl <- substr(lbl, 3, nchar(lbl) - 1)
    contrast_samples[[length(contrast_samples) + 1L]] <- list(
      to       = lbl,
      mean     = mean(x),
      sd       = sd(x),
      samples  = x
    )
  }
  results[[length(results) + 1L]] <- list(
    label = pp$label, wall_sec = wall,
    tau2 = list(
      mode   = mode_density(tau2),
      median = median(tau2),
      mean   = mean(tau2),
      ci_lo  = unname(quantile(tau2, 0.025)),
      ci_hi  = unname(quantile(tau2, 0.975))
    ),
    tau2_samples = tau2,
    contrasts = contrast_samples
  )
}
t1_total <- Sys.time()
total <- as.numeric(difftime(t1_total, t0_total, units = "secs"))
cat(sprintf("\nmultinma diabetes total: %.1fs (%d priors)\n",
            total, length(priors)))
write_json(list(total_wall_sec = total, ref = ref, results = results),
           "/tmp/diabetes_mn.json", auto_unbox = TRUE, digits = 6)
cat("Wrote /tmp/diabetes_mn.json\n")
