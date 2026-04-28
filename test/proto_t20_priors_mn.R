#!/usr/bin/env Rscript
# multinma prior sweep on /tmp/t20.json (synthetic binary NMA, T=20, k=30).
# 3 informative priors. multinma must run a separate fit per prior.
suppressPackageStartupMessages({ library(jsonlite); library(multinma) })
options(mc.cores = parallel::detectCores())

d_raw <- fromJSON("/tmp/t20.json")
d <- data.frame(
  study     = factor(d_raw$study),
  treatment = factor(d_raw$treatment),
  events    = as.integer(d_raw$events),
  n         = as.integer(d_raw$n)
)
ref <- levels(d$treatment)[1]
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
for (p in priors) {
  cat(sprintf("Fitting %s ...\n", p$label))
  t0 <- Sys.time()
  fit <- nma(net, trt_effects = "random",
             likelihood = "binomial", link = "logit",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = p$p,
             iter = 3000, warmup = 1000, chains = 4, refresh = 0, seed = 42)
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))
  tau  <- as.data.frame(fit, pars = "tau")[[1]]
  tau2 <- tau ^ 2
  results[[length(results) + 1L]] <- list(
    label = p$label, wall = wall,
    tau2_mode   = mode_density(tau2),
    tau2_median = median(tau2),
    tau2_mean   = mean(tau2),
    tau2_lo     = unname(quantile(tau2, 0.025)),
    tau2_hi     = unname(quantile(tau2, 0.975))
  )
}
t1_total <- Sys.time()
total <- as.numeric(difftime(t1_total, t0_total, units = "secs"))
write_json(list(total_wall_sec = total, results = results),
           "/tmp/t20_mn.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("\nmultinma total wall: %.2fs (%d priors)\n", total, length(priors)))
