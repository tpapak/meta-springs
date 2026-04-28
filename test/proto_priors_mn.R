#!/usr/bin/env Rscript
# multinma fit per prior, on /tmp/g50.json (Gaussian arm-level data).
# Writes /tmp/mn_priors.json.
suppressPackageStartupMessages({
  library(jsonlite); library(multinma); library(dplyr)
})
options(mc.cores = parallel::detectCores())

d <- fromJSON("/tmp/g50.json")
d$study     <- factor(d$study)
d$treatment <- factor(d$treatment)
d$se        <- 1 / sqrt(d$n)
ref         <- levels(d$treatment)[1]
net <- set_agd_arm(d, study = study, trt = treatment,
                   y = mean, se = se, trt_ref = ref)

priors <- list(
  list(label = "HalfN(0,1)",       p = half_normal(scale = 1.0)),
  list(label = "HalfN(0,0.5)",     p = half_normal(scale = 0.5)),
  list(label = "HalfN(0,2)",       p = half_normal(scale = 2.0)),
  list(label = "HalfCauchy(0,1)",  p = half_cauchy(scale = 1.0)),
  list(label = "HalfCauchy(0,0.25)", p = half_cauchy(scale = 0.25))
)

mode_density <- function(x) {
  dens <- density(x, from = 0, to = max(x), n = 2048)
  dens$x[which.max(dens$y)]
}

t_total_0 <- Sys.time()
results <- list()
for (pr in priors) {
  cat(sprintf("Fitting %s...\n", pr$label))
  t0 <- Sys.time()
  fit <- nma(net, trt_effects = "random", likelihood = "normal",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = pr$p,
             iter = 3000, warmup = 1000, chains = 4,
             refresh = 0, seed = 42)
  t1 <- Sys.time()
  wall <- as.numeric(difftime(t1, t0, units = "secs"))
  tau2 <- as.data.frame(fit, pars = "tau")[[1]] ^ 2
  results[[length(results) + 1L]] <- list(
    prior  = pr$label,
    wall   = wall,
    mode   = mode_density(tau2),
    median = median(tau2),
    mean   = mean(tau2),
    ci_lo  = unname(quantile(tau2, 0.025)),
    ci_hi  = unname(quantile(tau2, 0.975))
  )
}
t_total_1 <- Sys.time()
total_wall <- as.numeric(difftime(t_total_1, t_total_0, units = "secs"))

write_json(list(total_wall_sec = total_wall, summaries = results),
           "/tmp/mn_priors.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("\nmultinma total wall: %.2f s\n", total_wall))
