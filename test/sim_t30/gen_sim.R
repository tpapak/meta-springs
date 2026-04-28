#!/usr/bin/env Rscript
# Bigger NMA simulation: T=30 treatments, S=200 studies/replica,
# 200 replicas at each of seven τ² levels {0.001, 0.005, 0.01, 0.05,
# 0.10, 0.30, 0.60}.  Same per-arm iid RE generating model as
# test/sim_t20/.
#
# Output: test/sim_t30/data/tau{IDX}_rep{REP:03d}.json
#         test/sim_t30/data/manifest.json

suppressPackageStartupMessages({ library(jsonlite) })

set.seed(20260427L)

T_treats   <- 30L
S_studies  <- 200L
N_reps     <- 200L
tau2_grid  <- c(0.001, 0.005, 0.01, 0.05, 0.10, 0.30, 0.60)

n_per_arm  <- 60L
sigma      <- 1.0
arm_probs  <- c("2" = 0.60, "3" = 0.25, "4" = 0.10, "5" = 0.05)

theta <- rnorm(T_treats, mean = 0, sd = 1)

gen_one <- function(tau2, rep_id) {
  set.seed(rep_id * 10007L + as.integer(round(tau2 * 1e6)))
  arm_counts <- sample(as.integer(names(arm_probs)),
                       size = S_studies, replace = TRUE,
                       prob = arm_probs)
  rows <- list()
  for (s in seq_len(S_studies)) {
    k <- arm_counts[s]
    treats <- sample.int(T_treats, size = k, replace = FALSE)
    u <- rnorm(k, mean = 0, sd = sqrt(tau2 / 2))
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
  cat(sprintf("τ²=%.3f ... ", tau2)); flush.console()
  for (r in seq_len(N_reps)) {
    rows  <- gen_one(tau2, r)
    fname <- sprintf("test/sim_t30/data/tau%d_rep%03d.json", ti, r)
    write_json(rows, path = fname, auto_unbox = TRUE, digits = 10)
    multi <- sum(table(sapply(rows, `[[`, "study")) > 2)
    manifest[[length(manifest)+1L]] <- list(
      tau2_idx = ti, tau2 = tau2, rep = r, file = fname,
      k_studies = length(unique(sapply(rows, `[[`, "study"))),
      multi_arm_k = multi
    )
  }
  cat("done\n")
}
write_json(manifest, "test/sim_t30/data/manifest.json",
           auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nGenerated %d datasets in test/sim_t30/data/\n", length(manifest)))
