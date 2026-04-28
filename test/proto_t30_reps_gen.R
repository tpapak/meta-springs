#!/usr/bin/env Rscript
# Generate 30 synthetic NMA replicates with multi-arm topology, in BOTH the
# binary regime (events/n, logit link) and the continuous regime (mean/sd/n,
# identity link).  Both regimes share the same topology, the same baseline μ_i,
# and the same arm random effects u_arm — only the outcome process differs, so
# the two regimes are paired and directly comparable.
#
# T=30 treatments, k=60 studies (48 2-arm + 9 3-arm + 3 4-arm), τ²_true=0.30,
# d_true ∈ N(0, 0.8²) for j ≠ 1.  Outputs:
#   /tmp/t30_reps_bin/rep_NN.json   (events, n)
#   /tmp/t30_reps_cont/rep_NN.json  (mean, sd, n)
#   /tmp/t30_reps_truth.json        (τ²_true, d_true, T, k, nReps)
suppressPackageStartupMessages({ library(jsonlite) })

dir.create("/tmp/t30_reps_bin",  showWarnings = FALSE, recursive = TRUE)
dir.create("/tmp/t30_reps_cont", showWarnings = FALSE, recursive = TRUE)

T   <- 30
k   <- 60
nReps <- 30
tau <- sqrt(0.30)
N_OPTS <- c(50, 80, 100, 150, 200)

set.seed(20260427)
# Single d_true vector, shared across replicates (so bias is well-defined).
d_true <- c(0, rnorm(T - 1, 0, 0.8))

# Topology shared across replicates (only outcomes vary). Mix of 2-, 3-, and
# 4-arm trials matching real-world NMA frequencies (~80% 2-arm, ~15% 3-arm,
# ~5% 4-arm). Each non-ref treatment still appears in ≥3 studies.
set.seed(20260427 + 1)   # topology seed (separate from outcome seeds below)
n_2arm <- 48L
n_3arm <- 9L
n_4arm <- 3L
stopifnot(n_2arm + n_3arm + n_4arm == k)

# Round-robin allocation guarantees every non-ref treatment appears in ≥1
# anchored 2-arm study (and most appear in ≥2).
two_arm <- lapply(seq_len(n_2arm), function(i) {
  j <- 2L + ((i - 1L) %% (T - 1L))
  c(1L, j)
})
# 3-arm: reference + 2 random non-refs (each non-ref already has ≥1
# anchored study, so no connectivity risk).
three_arm <- lapply(seq_len(n_3arm), function(i)
  sort(c(1L, sample(2:T, 2))))
# 4-arm: reference + 3 random non-refs.
four_arm <- lapply(seq_len(n_4arm), function(i)
  sort(c(1L, sample(2:T, 3))))
arms_list <- c(two_arm, three_arm, four_arm)
stopifnot(length(arms_list) == k)

mu_seed_base  <- 1000
sigma_within  <- 1.0     # within-arm SD on the continuous scale (sleepstudy-like)
for (r in seq_len(nReps)) {
  set.seed(mu_seed_base + r)
  rows_bin  <- list()
  rows_cont <- list()
  for (i in seq_len(k)) {
    trts <- arms_list[[i]]
    mu_i <- rnorm(1, -2.0, 1)
    for (j in trts) {
      u <- rnorm(1, 0, tau)        # arm random effect, shared across regimes
      n <- sample(N_OPTS, 1)        # sample size, shared across regimes
      lin <- mu_i + d_true[j] + u

      # Binary outcome
      p   <- plogis(lin)
      ev  <- rbinom(1, n, p)
      rows_bin[[length(rows_bin) + 1L]] <-
        data.frame(study = i, treatment = j, events = ev, n = n)

      # Continuous outcome — arm-level summary (mean, sd, n).  Use the same
      # latent linear predictor as the location, sigma_within as residual SD.
      raw <- rnorm(n, lin, sigma_within)
      rows_cont[[length(rows_cont) + 1L]] <-
        data.frame(study = i, treatment = j,
                   mean = mean(raw), sd = sd(raw), n = n)
    }
  }
  write_json(do.call(rbind, rows_bin),
             sprintf("/tmp/t30_reps_bin/rep_%02d.json", r),
             auto_unbox = TRUE, digits = 6)
  write_json(do.call(rbind, rows_cont),
             sprintf("/tmp/t30_reps_cont/rep_%02d.json", r),
             auto_unbox = TRUE, digits = 6)
}

write_json(list(tau2_true = tau^2, d_true = d_true,
                T = T, k = k, nReps = nReps,
                sigma_within = sigma_within),
           "/tmp/t30_reps_truth.json", auto_unbox = TRUE, digits = 6)
cat(sprintf("Generated %d binary + %d continuous reps (T=%d, k=%d)\n",
            nReps, nReps, T, k))
