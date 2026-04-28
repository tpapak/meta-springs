#!/usr/bin/env Rscript
suppressPackageStartupMessages({ library(jsonlite); library(multinma) })
options(mc.cores = parallel::detectCores())

n_reps <- 20
mode_density <- function(x) {
  d <- density(x, from=0, to=max(x), n=2048); d$x[which.max(d$y)]
}
results <- list()
for (rep_i in 1:n_reps) {
  d <- fromJSON(sprintf("/tmp/reps/rep_%02d.json", rep_i))
  d$study <- factor(d$study); d$treatment <- factor(d$treatment)
  d$se    <- 1 / sqrt(d$n)
  ref     <- levels(d$treatment)[1]
  net <- set_agd_arm(d, study=study, trt=treatment, y=mean, se=se, trt_ref=ref)
  fit <- nma(net, trt_effects = "random", likelihood = "normal",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = half_normal(scale = 1),
             iter = 3000, warmup = 1000, chains = 4, refresh = 0, seed = 42)
  tau  <- as.data.frame(fit, pars="tau")[[1]]
  tau2 <- tau^2
  re   <- relative_effects(fit, trt_ref = ref)
  pn   <- dimnames(re$sims)[[3]]
  mat  <- do.call(cbind, lapply(seq_along(pn), function(j) as.vector(re$sims[,,j])))
  to_trt <- sub("^d\\[(.*)\\]$", "\\1", pn)
  trt_levels <- levels(d$treatment)
  zero_ref <- rep(0, nrow(mat))
  effs <- list()
  for (a in trt_levels) for (b in trt_levels) {
    if (a == b) next
    sa <- if (a == ref) zero_ref else mat[, which(to_trt == a)]
    sb <- if (b == ref) zero_ref else mat[, which(to_trt == b)]
    v <- sb - sa
    effs[[paste0(a, "->", b)]] <- list(label=paste0(a, "->", b), mean=mean(v))
  }
  results[[rep_i]] <- list(
    rep = rep_i,
    tau2_mode  = mode_density(tau2),
    tau2_median = median(tau2),
    tau2_mean  = mean(tau2),
    tau2_ci_lo = unname(quantile(tau2, 0.025)),
    tau2_ci_hi = unname(quantile(tau2, 0.975)),
    effects = unname(effs)
  )
  cat(sprintf("  rep %02d: τ² mean=%.4f\n", rep_i, mean(tau2)))
}
write_json(list(reps=results), "/tmp/reps_mn.json", auto_unbox=TRUE, digits=6)
cat("Wrote /tmp/reps_mn.json\n")
