#!/usr/bin/env Rscript
# multinma posterior dump for ONE replicate, ALL 3 priors, with samples
# and per-fit wall time.  Argv: <regime> <repIdx>  where regime is "t30" or
# "t50". Output /tmp/{t30,t50}_allpriors_mn.json.
suppressPackageStartupMessages({ library(jsonlite); library(multinma) })
options(mc.cores = parallel::detectCores())

args <- commandArgs(trailingOnly = TRUE)
regime <- args[1]
rep    <- as.integer(args[2])
in_dir <- if (regime == "t30") "/tmp/t30_reps_bin" else "/tmp/t50_rare_bin"
out_path <- sprintf("/tmp/%s_allpriors_mn.json", regime)
data_path <- sprintf("%s/rep_%02d.json", in_dir, rep)

d_raw <- fromJSON(data_path)
d <- data.frame(
  study     = factor(d_raw$study),
  treatment = factor(d_raw$treatment),
  events    = as.integer(d_raw$events),
  n         = as.integer(d_raw$n)
)
ref <- levels(d$treatment)[1]
net <- set_agd_arm(d, study = study, trt = treatment,
                   r = events, n = n, trt_ref = ref)
cat(sprintf("multinma %s rep %d: %d arms, ref %s\n",
            regime, rep, nrow(d), ref))

priors <- list(
  list(label = "HalfNormal(τ; σ=0.5)", p = half_normal(scale = 0.5)),
  list(label = "HalfNormal(τ; σ=1)",   p = half_normal(scale = 1.0)),
  list(label = "HalfCauchy(τ; σ=0.5)", p = half_cauchy(scale = 0.5))
)

results <- list()
t_total0 <- Sys.time()
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
      to = lbl, mean = mean(x), sd = sd(x), samples = x
    )
  }
  results[[length(results) + 1L]] <- list(
    label = pp$label, wall_sec = wall,
    tau2_samples = tau2, contrasts = contrast_samples
  )
}
t_total1 <- Sys.time()
total_wall <- as.numeric(difftime(t_total1, t_total0, units = "secs"))

write_json(list(
  regime = regime, rep = rep, ref = ref,
  total_wall_sec = total_wall,
  results = results
), out_path, auto_unbox = TRUE, digits = 6)
cat(sprintf("Wrote %s (%.1fs total, 3 priors)\n", out_path, total_wall))
