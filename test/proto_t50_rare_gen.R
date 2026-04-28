#!/usr/bin/env Rscript
# T=50 binary NMA, k=100 multi-arm (80 2-arm + 15 3-arm + 5 4-arm), 5 reps,
# RARE EVENTS regime: baseline μ_i ~ N(-4, 1) → ref p ≈ 0.018 with substantial
# variation across studies. d_true ∈ N(0, 0.5²) for j ≠ 1, τ²_true = 0.20.
# Outputs /tmp/t50_rare_bin/rep_NN.json + /tmp/t50_rare_truth.json.
suppressPackageStartupMessages({ library(jsonlite) })

dir.create("/tmp/t50_rare_bin", showWarnings = FALSE, recursive = TRUE)

T   <- 50L
k   <- 100L
nReps <- 30L
tau   <- sqrt(0.20)
N_OPTS <- c(50, 80, 100, 150, 200)

set.seed(20260427)
d_true <- c(0, rnorm(T - 1, 0, 0.5))

# Topology: 80 anchored 2-arm via round-robin, 15 three-arm + 5 four-arm.
set.seed(20260427 + 11)
n_2arm <- 80L; n_3arm <- 15L; n_4arm <- 5L
stopifnot(n_2arm + n_3arm + n_4arm == k)
two_arm <- lapply(seq_len(n_2arm), function(i) {
  j <- 2L + ((i - 1L) %% (T - 1L))
  c(1L, j)
})
three_arm <- lapply(seq_len(n_3arm), function(i)
  sort(c(1L, sample(2:T, 2))))
four_arm <- lapply(seq_len(n_4arm), function(i)
  sort(c(1L, sample(2:T, 3))))
arms_list <- c(two_arm, three_arm, four_arm)
stopifnot(length(arms_list) == k)

mu_seed_base <- 5000
for (r in seq_len(nReps)) {
  set.seed(mu_seed_base + r)
  rows <- list()
  zero_events <- 0
  for (i in seq_len(k)) {
    trts <- arms_list[[i]]
    mu_i <- rnorm(1, -4.0, 1)
    for (j in trts) {
      u <- rnorm(1, 0, tau)
      p <- plogis(mu_i + d_true[j] + u)
      n <- sample(N_OPTS, 1)
      ev <- rbinom(1, n, p)
      if (ev == 0) zero_events <- zero_events + 1
      rows[[length(rows) + 1L]] <-
        data.frame(study = i, treatment = j, events = ev, n = n)
    }
  }
  dat <- do.call(rbind, rows)
  write_json(dat, sprintf("/tmp/t50_rare_bin/rep_%02d.json", r),
             auto_unbox = TRUE)
  cat(sprintf("rep %02d: %d arms, %d zero-event arms (%.0f%%)\n",
              r, nrow(dat), zero_events, 100 * zero_events / nrow(dat)))
}

write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, nReps = nReps,
                mu_loc = -4.0, mu_scale = 1.0),
           "/tmp/t50_rare_truth.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("Generated %d reps in /tmp/t50_rare_bin/\n", nReps))
