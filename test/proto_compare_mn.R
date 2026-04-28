#!/usr/bin/env Rscript
# Single multinma fit on synth_041 with HalfNormal(0,1) prior on τ.
# Writes /tmp/mn_compare.json with tau2 posterior summary + every pairwise
# contrast (mean + 95% CI) + wall time.

suppressPackageStartupMessages({
  library(jsonlite); library(multinma); library(dplyr)
})
options(mc.cores = parallel::detectCores())

d <- fromJSON("test/synth_bin/synth_041.json")
d$study     <- factor(d$study)
d$treatment <- factor(d$treatment)
ref         <- levels(d$treatment)[1]

net <- set_agd_arm(d, study = study, trt = treatment,
                   r = events, n = n, trt_ref = ref)

t0 <- Sys.time()
fit <- nma(net, trt_effects = "random",
           likelihood = "binomial", link = "logit",
           prior_intercept = normal(scale = 100),
           prior_trt       = normal(scale = 100),
           prior_het       = half_normal(scale = 1),
           iter = 3000, warmup = 1000, chains = 4,
           refresh = 0, seed = 42)
t1 <- Sys.time()
wall <- as.numeric(difftime(t1, t0, units = "secs"))

# τ² posterior
tau_samp <- as.data.frame(fit, pars = "tau")[[1]]
tau2     <- tau_samp ^ 2
mode_density <- function(x) {
  dens <- density(x, from = 0, to = max(x), n = 2048)
  dens$x[which.max(dens$y)]
}
tau2_summary <- list(
  mode   = mode_density(tau2),
  median = median(tau2),
  mean   = mean(tau2),
  ci_lo  = unname(quantile(tau2, 0.025)),
  ci_hi  = unname(quantile(tau2, 0.975))
)

# Contrast posteriors
re <- relative_effects(fit, trt_ref = ref)
sims <- re$sims
pn   <- dimnames(sims)[[3]]
mat  <- do.call(cbind, lapply(seq_along(pn), function(j) as.vector(sims[,,j])))
colnames(mat) <- pn
to_trt <- sub("^d\\[(.*)\\]$", "\\1", pn)

trt_levels <- levels(d$treatment)
zero_ref <- rep(0, nrow(mat))
effs <- list()
for (a in trt_levels) for (b in trt_levels) {
  if (a == b) next
  sa <- if (a == ref) zero_ref else mat[, which(to_trt == a)]
  sb <- if (b == ref) zero_ref else mat[, which(to_trt == b)]
  v <- sb - sa
  effs[[paste0(a, "->", b)]] <- list(
    from   = a, to = b,
    mean   = mean(v), median = median(v), sd = sd(v),
    ci_lo  = unname(quantile(v, 0.025)),
    ci_hi  = unname(quantile(v, 0.975))
  )
}

write_json(list(
  dataset = "test/synth_bin/synth_041.json",
  prior   = "HalfNormal(0,1) on tau; N(0,100) on intercept and trt",
  mcmc    = list(iter = 3000, warmup = 1000, chains = 4, seed = 42),
  wall_sec = wall,
  tau2     = tau2_summary,
  effects  = effs
), "/tmp/mn_compare.json", auto_unbox = TRUE, digits = 6)

cat(sprintf("multinma  wall=%.2fs  tau2 median=%.3f  mean=%.3f  CI=(%.3f,%.3f)\n",
            wall, tau2_summary$median, tau2_summary$mean,
            tau2_summary$ci_lo, tau2_summary$ci_hi))
