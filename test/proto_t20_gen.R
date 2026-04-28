#!/usr/bin/env Rscript
# One synthetic 20-treatment binary NMA. k = 30 studies, 2-arm trials,
# treatment 1 = reference. Each non-reference treatment appears in ≥ 3
# studies. Truth: τ² = 0.30, d_true ∈ N(0, 0.8²) for j ≠ 1.
#
# Writes /tmp/t20.json (data) and /tmp/t20_truth.json (d_true, τ²_true).

suppressPackageStartupMessages({ library(jsonlite) })

set.seed(20260426)

T   <- 20
k   <- 30
tau <- sqrt(0.30)

# d_true: reference at 0, others ~ N(0, 0.8)
d_true <- c(0, rnorm(T - 1, 0, 0.8))

# Each non-ref treatment must appear in at least 3 studies → reserve 3 × (T-1) = 57
# arm slots. With 30 2-arm studies = 60 arms, that fits. Build deterministic
# chain first: each non-ref treatment paired with treatment 1 in 3 studies.
arms_list <- list()
idx <- 1
for (j in 2:T) {
  for (rep in 1:3) {
    arms_list[[idx]] <- c(1L, j); idx <- idx + 1L
    if (idx > k) break
  }
  if (idx > k) break
}
# Pad any remaining with random 2-arm pairs (not necessary here since 57 < 60).
while (idx <= k) {
  arms_list[[idx]] <- sort(sample.int(T, 2)); idx <- idx + 1L
}

# Generate arm-level data.
N_OPTS <- c(50, 80, 100, 150, 200)
rows <- list()
for (i in seq_len(k)) {
  trts <- arms_list[[i]]
  mu_i <- rnorm(1, -2.0, 1)
  for (j in trts) {
    u <- rnorm(1, 0, tau)
    p <- plogis(mu_i + d_true[j] + u)
    n <- sample(N_OPTS, 1)
    rows[[length(rows) + 1L]] <-
      data.frame(study = i, treatment = j, events = rbinom(1, n, p), n = n)
  }
}
dat <- do.call(rbind, rows)
write_json(dat, "/tmp/t20.json", auto_unbox = TRUE)
write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, n_arms = nrow(dat)),
           "/tmp/t20_truth.json", auto_unbox = TRUE, digits = 6)

cat(sprintf("Generated /tmp/t20.json: T=%d, k=%d, A=%d arms (rare<5: %d)\n",
            T, k, nrow(dat), sum(dat$events < 5)))
