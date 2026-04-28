#!/usr/bin/env Rscript
# Generate 10 binary NMA datasets with varying network size (4-20 trts)
# from the Dias TSD 2 RE model (Lu & Ades 2004; Dias et al. 2011).
#
# Model for study i with treatments {t_b, t_1, ..., t_m} (t_b = baseline):
#   r_ik ~ Binomial(p_ik, n_ik)
#   logit(p_i,t_b) = mu_i
#   logit(p_i,t_k) = mu_i + (d_{t_k} - d_{t_b}) + delta_{i,k}    k > b
#   delta_i ~ MVN(0, Sigma),  Sigma_jj = tau^2, Sigma_jk = tau^2/2

library(jsonlite)
set.seed(2026)

dir.create("test/synth_bias/data", showWarnings = FALSE, recursive = TRUE)

# Replicates at multiple network sizes and heterogeneity levels
T_levels    <- c(4, 10, 14, 20)
tau2_levels <- c(0.1, 0.3, 0.6, 1.0)
nreps       <- 100
mu0         <- -1                 # moderate event rate
sigma_mu    <- 1
n_arm       <- 100
# Full grid: T x tau2 x reps
grid <- expand.grid(T = T_levels, tau2 = tau2_levels, rep = 1:nreps)
T_grid    <- grid$T
tau2_grid <- grid$tau2

# Treatment effects: first is 0, others spread in [-1, 1].
# Use local RNG to avoid resetting the main stream.
gen_d <- function(T) {
  old <- .Random.seed
  set.seed(100 + T)
  d <- c(0, runif(T - 1, -1, 1))
  .Random.seed <<- old
  d
}

# Generate correlated REs for m non-baseline arms (cov tau^2/2 structure)
gen_deltas <- function(m, tau2) {
  if (m == 0) return(numeric(0))
  s <- sqrt(tau2 / 2)
  rnorm(1, 0, s) + rnorm(m, 0, s)
}

# Build a connected network: star around trt 1 + some chained comparisons.
# For T trts, we make ~2*T studies (more studies for larger networks).
gen_arms <- function(T, k) {
  arms <- list()
  # Star: each trt vs 1 at least once
  for (j in 2:T) arms[[length(arms) + 1]] <- c(1, j)
  # Chained pairs to connect trts
  for (j in 2:(T-1)) arms[[length(arms) + 1]] <- c(j, j + 1)
  # Multi-arm studies (3-arm) for some trts
  if (T >= 4) {
    for (j in seq(2, T - 2, by = 2)) {
      arms[[length(arms) + 1]] <- c(1, j, j + 1)
    }
  }
  # Pad with more pairwise head-to-head studies up to k
  while (length(arms) < k) {
    pair <- sort(sample(1:T, 2))
    arms[[length(arms) + 1]] <- pair
  }
  arms[1:k]
}

manifest <- list()
rep_counter <- list()
for (idx in seq_along(T_grid)) {
  T    <- T_grid[idx]
  tau2 <- tau2_grid[idx]
  k <- max(2 * T, 20)          # more studies for larger networks
  key <- sprintf("%d_%.1f", T, tau2)
  rep_counter[[key]] <- if (is.null(rep_counter[[key]])) 1 else rep_counter[[key]] + 1
  name <- sprintf("nma_T%02d_tau%.1f_r%02d", T, tau2, rep_counter[[key]])
  d_true <- gen_d(T)
  arms_list <- gen_arms(T, k)

  rows <- list()
  for (i in seq_along(arms_list)) {
    mu_i <- rnorm(1, mu0, sigma_mu)
    trts <- arms_list[[i]]
    baseline <- trts[1]
    non_base <- trts[-1]
    deltas <- gen_deltas(length(non_base), tau2)
    for (tt in seq_along(trts)) {
      j <- trts[tt]
      if (j == baseline) {
        logit_p <- mu_i
      } else {
        d_contrast <- d_true[j] - d_true[baseline]
        pos <- which(non_base == j)
        logit_p <- mu_i + d_contrast + deltas[pos]
      }
      p <- 1 / (1 + exp(-logit_p))
      e <- rbinom(1, n_arm, p)
      rows[[length(rows) + 1]] <- data.frame(
        study = i, treatment = j, events = e, n = n_arm)
    }
  }
  dat <- do.call(rbind, rows)
  write_json(dat, sprintf("test/synth_bias/data/%s.json", name), auto_unbox = TRUE)

  manifest[[idx]] <- list(
    name = name, tau2 = tau2, mu0 = mu0,
    d_true = d_true, treats = T, k = k, n = n_arm)
}

write_json(manifest, "test/synth_bias/manifest.json",
           pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Generated %d datasets (T: %s)\n",
    length(manifest), paste(T_grid, collapse = ", ")))
