#!/usr/bin/env Rscript
# 20 conditionally-independent replicates of the same NMA generating model.
# Fixed truth: T=4, k=20, τ²_contrast=0.25, d_true held constant. Only the
# arm-level Gaussian draws differ across reps.
suppressPackageStartupMessages({ library(jsonlite) })
set.seed(20260427)

T <- 4; k <- 20
tau2_contrast <- 0.25
tau_arm       <- sqrt(tau2_contrast / 2)
d_true        <- c(0, rnorm(T - 1, 0, 0.6))   # fixed across reps

dir.create("/tmp/reps", showWarnings = FALSE)
arms_list <- list()
idx <- 1
for (j in 2:T) for (rep in 1:5) {
  if (idx > k) break
  arms_list[[idx]] <- c(1L, j); idx <- idx + 1L
}
while (idx <= k) { arms_list[[idx]] <- sort(sample.int(T, 2)); idx <- idx + 1L }

N_OPTS <- c(40, 60, 80, 100, 150)
for (rep_i in 1:20) {
  set.seed(20260427 + rep_i)
  rows <- list()
  for (i in seq_len(k)) {
    trts <- arms_list[[i]]
    mu_i <- rnorm(1, 0, 0.5)
    for (j in trts) {
      u <- rnorm(1, 0, tau_arm)
      n <- sample(N_OPTS, 1); se <- 1 / sqrt(n)
      y <- rnorm(1, mu_i + d_true[j] + u, se)
      rows[[length(rows) + 1L]] <- data.frame(
        study = i, treatment = j, mean = y, sd = 1, n = n)
    }
  }
  fn <- sprintf("/tmp/reps/rep_%02d.json", rep_i)
  write_json(do.call(rbind, rows), fn, auto_unbox = TRUE)
}
write_json(list(tau2_contrast_true = tau2_contrast,
                d_true             = d_true,
                T = T, k = k,
                n_reps = 20,
                seed_base = 20260427),
           "/tmp/reps/truth.json", auto_unbox = TRUE, digits = 6)
cat("Wrote 20 replicates to /tmp/reps/.\n")
cat(sprintf("Truth: τ²_contrast=%.4f, d_true=[%s]\n",
            tau2_contrast, paste(round(d_true, 4), collapse = ", ")))
