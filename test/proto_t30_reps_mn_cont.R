#!/usr/bin/env Rscript
# multinma sweep over /tmp/t30_reps_cont/rep_NN.json (Gaussian arm summaries:
# mean, sd, n).  Three priors per replicate: HalfNormal(τ; 0.5),
# HalfNormal(τ; 1), HalfCauchy(τ; 0.5).  Records τ² posterior summary +
# per-contrast (1→j) mean+sd.  Same output schema as the binary runner.
suppressPackageStartupMessages({
  library(jsonlite); library(multinma); library(dplyr)
})
options(mc.cores = parallel::detectCores())

priors <- list(
  list(label = "HalfNormal(τ; σ=0.5)", p = half_normal(scale = 0.5)),
  list(label = "HalfNormal(τ; σ=1)",   p = half_normal(scale = 1.0)),
  list(label = "HalfCauchy(τ; σ=0.5)", p = half_cauchy(scale = 0.5))
)

mode_density <- function(x) {
  d <- density(x, from = 0, to = max(x), n = 2048); d$x[which.max(d$y)]
}

nReps <- 30
results <- list()
t0_total <- Sys.time()
for (r in seq_len(nReps)) {
  path <- sprintf("/tmp/t30_reps_cont/rep_%02d.json", r)
  d_raw <- fromJSON(path)
  d <- data.frame(
    study     = factor(d_raw$study),
    treatment = factor(d_raw$treatment),
    mean      = as.numeric(d_raw$mean),
    sd        = as.numeric(d_raw$sd),
    n         = as.integer(d_raw$n)
  )
  d$se <- d$sd / sqrt(d$n)
  ref <- levels(d$treatment)[1]
  net <- set_agd_arm(d, study = study, trt = treatment,
                     y = mean, se = se, trt_ref = ref)

  rep_t0 <- Sys.time()
  prior_results <- list()
  for (pp in priors) {
    fit <- nma(net, trt_effects = "random", likelihood = "normal",
               prior_intercept = normal(scale = 100),
               prior_trt       = normal(scale = 100),
               prior_het       = pp$p,
               iter = 3000, warmup = 1000, chains = 4,
               refresh = 0, seed = 42)
    tau_samp <- as.data.frame(fit, pars = "tau")[[1]]
    tau2 <- tau_samp ^ 2
    re   <- relative_effects(fit, trt_ref = ref)
    sims <- re$sims
    pn   <- dimnames(sims)[[3]]
    eff_list <- list()
    for (j in seq_along(pn)) {
      x <- as.vector(sims[,,j])
      eff_list[[length(eff_list) + 1L]] <- list(
        to   = pn[j],
        mean = mean(x),
        sd   = sd(x)
      )
    }
    prior_results[[length(prior_results) + 1L]] <- list(
      label = pp$label,
      tau2  = list(
        mode   = mode_density(tau2),
        median = median(tau2),
        mean   = mean(tau2),
        ci_lo  = unname(quantile(tau2, 0.025)),
        ci_hi  = unname(quantile(tau2, 0.975))
      ),
      effects = eff_list
    )
  }
  rep_t1 <- Sys.time()
  rep_wall <- as.numeric(difftime(rep_t1, rep_t0, units = "secs"))
  cat(sprintf("rep %02d: %.1fs (3 priors)\n", r, rep_wall))
  results[[length(results) + 1L]] <- list(
    rep      = r,
    wall_sec = rep_wall,
    priors   = prior_results
  )
}
t1_total <- Sys.time()
total <- as.numeric(difftime(t1_total, t0_total, units = "secs"))
cat(sprintf("\nmultinma continuous total wall: %.1fs (%d reps × 3 priors)\n",
            total, nReps))

write_json(list(total_wall_sec = total, n_reps = nReps, results = results),
           "/tmp/t30_reps_cont_mn.json", auto_unbox = TRUE, digits = 6)
cat("Wrote /tmp/t30_reps_cont_mn.json\n")
