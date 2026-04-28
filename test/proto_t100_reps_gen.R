#!/usr/bin/env Rscript
# 30-rep bias study generator: T=100, k=200 binary NMA, well-behaved
# (baseline μ ≈ -1, no rare events).  Truth τ²=0.20, d_true ∈ N(0, 0.5²).
# Outputs /tmp/t100_reps_bin/rep_NN.json + /tmp/t100_reps_truth.json.
suppressPackageStartupMessages({ library(jsonlite) })

dir.create("/tmp/t100_reps_bin", showWarnings = FALSE, recursive = TRUE)

T <- 100L; k <- 200L; nReps <- 30L
tau <- sqrt(0.20)
N_OPTS <- c(80, 120, 200, 300)

set.seed(20260427 + 100)
d_true <- c(0, rnorm(T - 1, 0, 0.5))

set.seed(20260427 + 101)
n_2arm <- 160L; n_3arm <- 30L; n_4arm <- 10L
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

mu_seed_base <- 8000
for (r in seq_len(nReps)) {
  set.seed(mu_seed_base + r)
  rows <- list()
  for (i in seq_len(k)) {
    trts <- arms_list[[i]]
    mu_i <- rnorm(1, -1.0, 0.5)
    for (j in trts) {
      u <- rnorm(1, 0, tau)
      p <- plogis(mu_i + d_true[j] + u)
      n <- sample(N_OPTS, 1)
      rows[[length(rows) + 1L]] <-
        data.frame(study = i, treatment = j,
                   events = rbinom(1, n, p), n = n)
    }
  }
  write_json(do.call(rbind, rows),
             sprintf("/tmp/t100_reps_bin/rep_%02d.json", r),
             auto_unbox = TRUE)
}

write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, nReps = nReps),
           "/tmp/t100_reps_truth.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("Generated %d reps in /tmp/t100_reps_bin/ (T=%d, k=%d)\n",
            nReps, T, k))
