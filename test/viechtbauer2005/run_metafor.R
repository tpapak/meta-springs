#!/usr/bin/env Rscript
# Strict Viechtbauer (2005) reproduction using metafor.
#   - Same simulated data as gen.R (1000 reps × 6 conditions).
#   - All 5 estimators he compared: HS, HE, DL, ML, REML.
#   - Untruncated:  control = list(tau2.min = -Inf), so τ̂² is allowed
#     to go negative (matching his §6.1.2 stance: "Estimates of σθ²
#     were not truncated for calculating the bias, efficiency, and MSE.")
#
# We compute SE per study from the arm-level data (n_C = n_E, σ² = 10
# in the data-generating model, so vi = 2 σ² / n with σ² estimated from
# the within-study sd's).  Outputs a single CSV with one row per
# (condition, estimator).

suppressPackageStartupMessages({
  library(jsonlite); library(metafor)
})

ROOT <- "test/viechtbauer2005"
conds <- list(
  list(name = "nbar20_s2t0.125", nbar = 20, s2t = 0.125),
  list(name = "nbar20_s2t0.25",  nbar = 20, s2t = 0.25 ),
  list(name = "nbar20_s2t0.5",   nbar = 20, s2t = 0.5  ),
  list(name = "nbar40_s2t0.125", nbar = 40, s2t = 0.125),
  list(name = "nbar40_s2t0.25",  nbar = 40, s2t = 0.25 ),
  list(name = "nbar40_s2t0.5",   nbar = 40, s2t = 0.5  )
)

methods <- c("HS", "HE", "DL", "ML", "REML")
ctrl    <- list(tau2.min = -Inf)   # let τ̂² go negative — Viechtbauer's setup

build_pairwise <- function(arms_df) {
  # arms_df is a data.frame with columns study, treatment, mean, sd, n
  studies <- sort(unique(arms_df$study))
  do.call(rbind, lapply(studies, function(s) {
    sub <- arms_df[arms_df$study == s, , drop = FALSE]
    c1 <- sub[sub$treatment == 1, , drop = FALSE]
    c2 <- sub[sub$treatment == 2, , drop = FALSE]
    if (nrow(c1) != 1 || nrow(c2) != 1) return(NULL)
    yi <- c2$mean - c1$mean
    vi <- c1$sd^2 / c1$n + c2$sd^2 / c2$n
    data.frame(yi = yi, vi = vi)
  }))
}

results <- data.frame(
  condition = character(), s2t = numeric(), nbar = integer(),
  method = character(),
  mean = numeric(), bias = numeric(), sd = numeric(), mse = numeric(),
  n_neg = integer(), n_used = integer()
)

t0 <- Sys.time()
for (cond in conds) {
  cat(sprintf("\n=== %s (σθ² = %.3f, n̄ = %d) ===\n", cond$name, cond$s2t, cond$nbar))
  flush.console()
  files <- sort(Sys.glob(file.path(ROOT, "data", cond$name, "rep_*.json")))

  per_method <- setNames(vector("list", length(methods)), methods)
  for (m in methods) per_method[[m]] <- numeric(0)

  for (f in files) {
    arms <- fromJSON(f)            # default simplifyVector = TRUE → data.frame
    df   <- build_pairwise(arms)
    if (is.null(df) || any(df$vi <= 0) || any(!is.finite(df$vi))) next
    for (m in methods) {
      res <- tryCatch(
        suppressWarnings(rma(yi = df$yi, vi = df$vi,
                             method = m, control = ctrl)),
        error = function(e) NULL
      )
      if (!is.null(res)) per_method[[m]] <- c(per_method[[m]], res$tau2)
    }
  }

  for (m in methods) {
    xs <- per_method[[m]]
    if (length(xs) == 0) next
    err <- xs - cond$s2t
    results <- rbind(results, data.frame(
      condition = cond$name, s2t = cond$s2t, nbar = cond$nbar,
      method = m,
      mean = mean(xs), bias = mean(err),
      sd = sd(err), mse = mean(err^2),
      n_neg = sum(xs < 0), n_used = length(xs)
    ))
  }
  cat("  ", paste(sprintf("%-4s bias=%+.4f", results$method[results$condition==cond$name],
                          results$bias[results$condition==cond$name]), collapse = " | "), "\n")
}

cat(sprintf("\nTotal time: %.1fs\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

# Save
write.csv(results, file.path(ROOT, "results", "metafor_bias.csv"), row.names = FALSE)
cat("\nwrote test/viechtbauer2005/results/metafor_bias.csv\n")
print(results)
