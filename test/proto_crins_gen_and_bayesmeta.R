#!/usr/bin/env Rscript
# Reproduce the prior-sensitivity example from Roever (2020, R Journal,
# bayesmeta package paper) using the Crins et al. 2014 dataset (6 RCTs of
# basiliximab in pediatric liver transplantation).
#
# Run bayesmeta under 4 τ-priors and tabulate the τ posterior summaries.
# Also write the dataset out in arm-level form so the GC pipeline can
# ingest it.

suppressPackageStartupMessages({ library(bayesmeta); library(jsonlite) })

data("CrinsEtAl2014")
d <- CrinsEtAl2014
print(d[, c("publication", "exp.AR.events", "exp.total",
            "cont.AR.events", "cont.total")])

# Compute log-OR + SE per study with continuity correction (0.5 added to
# zero cells, standard for normal-approx).
events <- function(e, n, ec) ifelse(e == 0 | e == n, e + 0.5, e)
log_or <- function(e1, n1, e2, n2) {
  e1c <- events(e1, n1); e2c <- events(e2, n2)
  log( (e1c / (n1 - e1c)) / (e2c / (n2 - e2c)) )
}
log_or_se <- function(e1, n1, e2, n2) {
  e1c <- events(e1, n1); e2c <- events(e2, n2)
  sqrt(1/e1c + 1/(n1-e1c) + 1/e2c + 1/(n2-e2c))
}

y  <- log_or(d$exp.AR.events, d$exp.total, d$cont.AR.events, d$cont.total)
sg <- log_or_se(d$exp.AR.events, d$exp.total, d$cont.AR.events, d$cont.total)

cat("\nStudy-level (logOR, SE):\n")
for (i in seq_along(y)) cat(sprintf("  %s: y=%+.3f  se=%.3f\n",
                                     d$publication[i], y[i], sg[i]))

# Run bayesmeta under 4 τ-priors -----------------------------------------
priors <- list(
  list(label = "Jeffreys",        tau.prior = "Jeffreys"),
  list(label = "HalfNormal(0,0.5)", tau.prior = function(t) dhalfnormal(t, scale = 0.5)),
  list(label = "HalfNormal(0,1)",   tau.prior = function(t) dhalfnormal(t, scale = 1.0)),
  list(label = "HalfCauchy(0,0.5)", tau.prior = function(t) dhalfcauchy(t, scale = 0.5))
)

bm_results <- list()
t0 <- Sys.time()
for (p in priors) {
  cat(sprintf("\nbayesmeta %s ...\n", p$label))
  fit <- bayesmeta(y = y, sigma = sg,
                   labels = d$publication,
                   tau.prior = p$tau.prior,
                   interval.type = "central")
  s <- fit$summary
  bm_results[[length(bm_results) + 1L]] <- list(
    label  = p$label,
    tau_mode    = unname(s["mode",   "tau"]),
    tau_median  = unname(s["median", "tau"]),
    tau_mean    = unname(s["mean",   "tau"]),
    tau_lo      = unname(s["95% lower", "tau"]),
    tau_hi      = unname(s["95% upper", "tau"]),
    mu_mean     = unname(s["mean",   "mu"]),
    mu_lo       = unname(s["95% lower", "mu"]),
    mu_hi       = unname(s["95% upper", "mu"])
  )
}
t1 <- Sys.time()
bm_total <- as.numeric(difftime(t1, t0, units = "secs"))

# Convert to per-arm pseudo-NMA so GC can ingest it ----------------------
# For each study: 2 arms (control=ref, exp=trt 2). Per-arm sampling
# variance set so that contrast variance = SE^2: v_arm = SE^2 / 2.
# Mean for control arm = 0; mean for exp arm = logOR.
arm_rows <- list()
for (i in seq_along(y)) {
  v_arm <- (sg[i] ^ 2) / 2
  n_arm <- 1 / v_arm                    # integer-ish; we'll use "n" as sample size; sd=1 ⇒ se=1/sqrt(n)
  arm_rows[[length(arm_rows) + 1L]] <- data.frame(
    study = i, treatment = 1L, mean = 0.0,    sd = 1.0, n = n_arm)
  arm_rows[[length(arm_rows) + 1L]] <- data.frame(
    study = i, treatment = 2L, mean = y[i],   sd = 1.0, n = n_arm)
}
write_json(do.call(rbind, arm_rows), "/tmp/crins.json", auto_unbox = TRUE)

write_json(list(
  bayesmeta_total_sec = bm_total,
  results = bm_results
), "/tmp/crins_bayesmeta.json", auto_unbox = TRUE, digits = 6)

cat(sprintf("\nbayesmeta total wall: %.2fs (4 priors)\n", bm_total))
cat("Wrote /tmp/crins.json (per-arm) and /tmp/crins_bayesmeta.json\n")
