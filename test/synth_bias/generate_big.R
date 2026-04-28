#!/usr/bin/env Rscript
# Generate big stress-test NMAs: 50 treatments, tau2=3
library(jsonlite)
set.seed(2027)

dir.create("test/synth_bias/data", showWarnings = FALSE, recursive = TRUE)

nreps    <- 5
T        <- 50
tau2     <- 3.0
mu0      <- -1
sigma_mu <- 1
n_arm    <- 100

gen_d <- function(T) {
  old <- .Random.seed
  set.seed(100 + T)
  d <- c(0, runif(T - 1, -1, 1))
  .Random.seed <<- old
  d
}

gen_deltas <- function(m, tau2) {
  if (m == 0) return(numeric(0))
  s <- sqrt(tau2 / 2)
  rnorm(1, 0, s) + rnorm(m, 0, s)
}

gen_arms <- function(T, k) {
  arms <- list()
  for (j in 2:T) arms[[length(arms) + 1]] <- c(1, j)
  for (j in 2:(T-1)) arms[[length(arms) + 1]] <- c(j, j + 1)
  if (T >= 4) {
    for (j in seq(2, T - 2, by = 2)) {
      arms[[length(arms) + 1]] <- c(1, j, j + 1)
    }
  }
  while (length(arms) < k) {
    pair <- sort(sample(1:T, 2))
    arms[[length(arms) + 1]] <- pair
  }
  arms[1:k]
}

# Load existing manifest to append
manifest <- fromJSON("test/synth_bias/manifest.json", simplifyVector = FALSE)

for (r in 1:nreps) {
  name <- sprintf("nma_T%02d_tau%.1f_r%02d", T, tau2, r)
  k <- 2 * T  # 100 studies
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
      if (j == baseline) logit_p <- mu_i
      else {
        pos <- which(non_base == j)
        logit_p <- mu_i + (d_true[j] - d_true[baseline]) + deltas[pos]
      }
      p <- 1 / (1 + exp(-logit_p))
      e <- rbinom(1, n_arm, p)
      rows[[length(rows) + 1]] <- data.frame(
        study = i, treatment = j, events = e, n = n_arm)
    }
  }
  dat <- do.call(rbind, rows)
  write_json(dat, sprintf("test/synth_bias/data/%s.json", name), auto_unbox = TRUE)

  manifest[[length(manifest) + 1]] <- list(
    name = name, tau2 = tau2, mu0 = mu0,
    d_true = d_true, treats = T, k = k, n = n_arm)
}

write_json(manifest, "test/synth_bias/manifest.json",
           pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Appended %d datasets (T=%d, tau2=%.1f). Total: %d\n",
    nreps, T, tau2, length(manifest)))
