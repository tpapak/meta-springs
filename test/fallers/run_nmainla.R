#!/usr/bin/env Rscript
# Run nmaINLA on the fallers arm-level reconstruction.
# Continuous likelihood (Gaussian on log-OR + se), uniform prior on τ
# matching netmeta's REML-target via the τ-grid implicitly.
#
# We need to pivot the long-format arm JSON into nmaINLA's wide format:
# one row per study with columns y1..yK, se1..seK, t1..tK, na.

suppressPackageStartupMessages({
  library(nmaINLA); library(INLA); library(jsonlite); library(dplyr); library(tidyr)
})

# --- read arm-level data (already reconstructed star-form for multi-arm) ---
arms <- fromJSON("test/fallers/data/fallers_armlevel.json")
arms$treatment <- as.character(arms$treatment)
treats <- sort(unique(arms$treatment))
treat_idx <- setNames(seq_along(treats), treats)
arms$tid <- treat_idx[arms$treatment]
arms$se  <- arms$sd / sqrt(arms$n)         # standard error of the per-arm mean

cat(sprintf("studies=%d  treatments=%d  arm-rows=%d\n",
            length(unique(arms$study)), length(treats), nrow(arms)))

# pivot to wide: y1..yK, se1..seK, t1..tK
arms_by_st <- split(arms, arms$study)
maxK <- max(sapply(arms_by_st, nrow))
cat(sprintf("max arms per study: %d\n", maxK))

rows <- lapply(arms_by_st, function(g) {
  g <- g[order(g$tid), ]
  K <- nrow(g)
  pad <- function(x, len) c(x, rep(NA, len - length(x)))
  c(setNames(as.list(pad(g$mean, maxK)), paste0("y",  1:maxK)),
    setNames(as.list(pad(g$se,   maxK)), paste0("se", 1:maxK)),
    setNames(as.list(pad(g$tid,  maxK)), paste0("t",  1:maxK)),
    list(na = K, des = K))
})
df <- as.data.frame(do.call(rbind, lapply(rows, unlist)))
# ensure numeric cols
df[] <- lapply(df, as.numeric)
cat(sprintf("wide data: %d rows × %d cols\n", nrow(df), ncol(df)))

# --- create INLA dataset ---
t0 <- Sys.time()
dINLA <- create_INLA_dat(dat = df,
                         armVars = c(treatment = "t", responders = "y",
                                     sampleSize = "se"),
                         nArmsVar = "na", design = "des")
t_prep <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
cat(sprintf("create_INLA_dat: %.2f s\n", t_prep))

# --- fit consistency model (continuous likelihood) ---
t1 <- Sys.time()
fit <- nma_inla(dINLA,
                likelihood   = "normal",
                fixed.par    = c(0, 1000),
                tau.prior    = "uniform",
                tau.par      = c(0, 5),
                type         = "consistency",
                inla.strategy = "simplified.laplace",
                verbose      = FALSE)
t_fit <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
cat(sprintf("\nnma_inla fit time: %.2f s\n", t_fit))

# --- extract τ posterior summaries ---
hyper <- fit$summary.hyperpar
cat("\nHyperparameters (precisions/SD):\n")
print(hyper)

# τ posterior is the SD of the random effect on consistency contrasts
if ("tau" %in% rownames(hyper)) {
  tau_post <- hyper["tau", ]
} else {
  # try variant name
  tau_post <- hyper[grep("tau|sd|stdev", rownames(hyper), ignore.case=TRUE)[1], ]
}
cat(sprintf("\nτ posterior: mean=%.5f, sd=%.5f, 95%% CI=(%.5f, %.5f)\n",
            tau_post[["mean"]], tau_post[["sd"]],
            tau_post[["0.025quant"]], tau_post[["0.975quant"]]))

result <- list(
  prep_sec   = t_prep,
  fit_sec    = t_fit,
  total_sec  = t_prep + t_fit,
  tau2_mean  = tau_post[["mean"]]^2 + tau_post[["sd"]]^2,
  tau2_median = tau_post[["0.5quant"]]^2,
  tau_summary = as.list(tau_post)
)
write_json(result, "test/fallers/nmainla_result.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nwrote test/fallers/nmainla_result.json\n")
