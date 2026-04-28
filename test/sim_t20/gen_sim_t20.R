#!/usr/bin/env Rscript
# Simulation: T=20 treatments, S=100 studies/replica, 50 replicas at
# each of τ² ∈ {0.01, 0.1, 0.6}.  Lu-Ades / per-arm iid random
# effects (the model GC, spring-REML, and netmeta all target).
#
#   Per arm a in study s:  u_{s,a}  ~ N(0, τ²/2)   iid
#   Per arm:               y_{s,a}  ~ N(θ_a + u_{s,a}, σ²/n)
#
#   θ_a iid N(0, 1)       -- treatment effects (fixed across replicas)
#   σ = 1, n = 60         -- typical RCT
#
# Arm count distribution (≈40 % multi-arm trials, realistic for big NMAs):
#   2-arm 60 %, 3-arm 25 %, 4-arm 10 %, 5-arm 5 %.
#
# Treatments per study drawn uniformly without replacement from 1..T.
# We force the network to be connected by also requiring every
# treatment to appear in at least 2 studies (guaranteed in expectation
# at S=100 / T=20; we resample studies that violate it).
#
# Output: test/sim_t20/data/tau{IDX}_rep{REP}.json  (long-format arms)
#         test/sim_t20/data/manifest.json

suppressPackageStartupMessages({ library(jsonlite) })

set.seed(20260427L)

T_treats <- 20L
S_studies <- 100L
N_reps   <- 50L
tau2_grid <- c(0.01, 0.10, 0.60)

n_per_arm <- 60L
sigma     <- 1.0
arm_probs <- c("2" = 0.60, "3" = 0.25, "4" = 0.10, "5" = 0.05)

# Fixed treatment effects across all replicas — this isolates τ² as
# the only varying nuisance.
theta <- rnorm(T_treats, mean = 0, sd = 1)

gen_one_replica <- function(tau2, rep_id) {
  set.seed(rep_id * 10007L + as.integer(round(tau2 * 1e6)))

  # Pick number of arms per study and which treatments
  arm_counts <- sample(as.integer(names(arm_probs)),
                       size = S_studies, replace = TRUE,
                       prob = arm_probs)
  rows <- list()
  for (s in seq_len(S_studies)) {
    k <- arm_counts[s]
    treats <- sample.int(T_treats, size = k, replace = FALSE)
    # Per-arm random effects:  u ~ N(0, τ²/2)
    u <- rnorm(k, mean = 0, sd = sqrt(tau2 / 2))
    # Per-arm observed mean given σ²/n
    y <- theta[treats] + u + rnorm(k, mean = 0, sd = sigma / sqrt(n_per_arm))
    for (i in seq_len(k)) {
      rows[[length(rows) + 1L]] <- list(
        study     = as.integer(s),
        treatment = as.integer(treats[i]),
        mean      = unname(y[i]),
        sd        = sigma,
        n         = n_per_arm
      )
    }
  }
  rows
}

manifest <- list()
for (ti in seq_along(tau2_grid)) {
  tau2 <- tau2_grid[ti]
  cat(sprintf("τ²=%.2f  ", tau2)); flush.console()
  for (r in seq_len(N_reps)) {
    rows <- gen_one_replica(tau2, r)
    fname <- sprintf("test/sim_t20/data/tau%d_rep%02d.json", ti, r)
    write_json(rows, path = fname, auto_unbox = TRUE, digits = 10)
    multi <- sum(table(sapply(rows, `[[`, "study")) > 2)
    manifest[[length(manifest) + 1L]] <- list(
      tau2_idx = ti, tau2 = tau2, rep = r, file = fname,
      k_studies = length(unique(sapply(rows, `[[`, "study"))),
      multi_arm_k = multi
    )
  }
  cat("done\n")
}
write_json(manifest, "test/sim_t20/data/manifest.json",
           auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nGenerated %d datasets in test/sim_t20/data/\n", length(manifest)))
