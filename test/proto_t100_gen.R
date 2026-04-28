#!/usr/bin/env Rscript
# T=100 binary NMA, single replicate.  k=200 studies (160 2-arm + 30 3-arm
# + 10 4-arm), well-connected via round-robin anchored allocation.  Baseline
# μ_i ~ N(-1, 0.5) → ref event probability ≈ 0.27 (no rare events).  Truth
# τ²=0.20, d_true ∈ N(0, 0.5²).  Output /tmp/t100.json + /tmp/t100_truth.json.
suppressPackageStartupMessages({ library(jsonlite) })

T   <- 100L
k   <- 200L
tau <- sqrt(0.20)
N_OPTS <- c(80, 120, 200, 300)

set.seed(20260427 + 100)
d_true <- c(0, rnorm(T - 1, 0, 0.5))

# Topology: 160 anchored 2-arm via round-robin (so each non-ref appears ≥1
# anchored study; with 160/99 ≈ 1.6 most appear 2×).  Then 30 three-arm
# + 10 four-arm with ref + random non-refs.
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

set.seed(20260427 + 102)
rows <- list()
zero_events <- 0
for (i in seq_len(k)) {
  trts <- arms_list[[i]]
  mu_i <- rnorm(1, -1.0, 0.5)
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
write_json(dat, "/tmp/t100.json", auto_unbox = TRUE)

write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, n_arms = nrow(dat),
                zero_event_arms = zero_events),
           "/tmp/t100_truth.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("Generated /tmp/t100.json: T=%d, k=%d, A=%d arms, zero-event=%d (%.0f%%)\n",
            T, k, nrow(dat), zero_events, 100*zero_events/nrow(dat)))
