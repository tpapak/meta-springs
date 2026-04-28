#!/usr/bin/env Rscript
# Gaussian-arm NMA simulation. T=50 treatments, k=200 studies, 2-arm trials.
# Reference treatment 1; each non-ref treatment in ≥ 4 studies (chain through ref).
# Truth: τ² = 0.10, d_true ~ N(0, 0.6²) for j ≠ 1, σ²_arm = 1/n_ij (varying n).
# Writes /tmp/g50.json (long format y/se per arm) + /tmp/g50_truth.json.

suppressPackageStartupMessages({ library(jsonlite) })
set.seed(20260427)

T <- 50; k <- 200; tau <- sqrt(0.10)
d_true <- c(0, rnorm(T - 1, 0, 0.6))

# Network: first 4·(T-1) = 196 studies are ref-vs-each (chain). Remaining
# k - 196 = 4 studies are random pairs to ensure k=200.
arms_list <- list()
idx <- 1
for (j in 2:T) for (rep in 1:4) {
  if (idx > k) break
  arms_list[[idx]] <- c(1L, j); idx <- idx + 1L
}
while (idx <= k) {
  arms_list[[idx]] <- sort(sample.int(T, 2)); idx <- idx + 1L
}

N_OPTS <- c(40, 60, 80, 100, 150)
rows <- list()
for (i in seq_len(k)) {
  trts <- arms_list[[i]]
  mu_i <- rnorm(1, 0, 0.5)        # study baseline
  for (j in trts) {
    u   <- rnorm(1, 0, tau)
    n   <- sample(N_OPTS, 1)
    se  <- 1 / sqrt(n)             # per-arm sampling SE
    y   <- rnorm(1, mu_i + d_true[j] + u, se)
    rows[[length(rows) + 1L]] <-
      data.frame(study = i, treatment = j, mean = y, sd = 1, n = n)
  }
}
dat <- do.call(rbind, rows)
write_json(dat, "/tmp/g50.json", auto_unbox = TRUE)
write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, n_arms = nrow(dat)),
           "/tmp/g50_truth.json", auto_unbox = TRUE, digits = 6)

cat(sprintf("Generated /tmp/g50.json: T=%d, k=%d, A=%d arms\n", T, k, nrow(dat)))
