#!/usr/bin/env Rscript
# Generate 10 continuous T=6 NMA datasets mirroring synth_bias/generate.R.
#
# Model (Lu & Ades / Dias TSD 2, continuous identity link):
#   y_ik | ...  ~ Normal(mu_i + (d_{t_k}-d_{t_b}) + delta_{i,k}, sigma_within^2 / n_ik)
#   delta_i    ~ MVN(0, Sigma), Sigma_jj = tau^2, Sigma_jk = tau^2/2  (k > j, same study)
# where b = baseline arm per study, t_k != b get their own correlated delta.

suppressPackageStartupMessages(library(jsonlite))
set.seed(2026)

dir.create("test/gc_compare/data", showWarnings = FALSE, recursive = TRUE)

TT <- 6L
# d_true for T=6: reference at 0 plus 5 effects spread in (-2, 2)
d_true <- c(0, 0.5, -0.8, 1.2, -0.3, 0.9)
mu0         <- 0
sigma_mu    <- 1
sigma_within<- 1           # residual SD (per-individual)
n_arm       <- 60          # mean per-arm sample size

# Same topology generator as the binary script (connected star + chain + some 3-arm)
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

gen_deltas <- function(m, tau2) {
  if (m == 0) return(numeric(0))
  s <- sqrt(tau2 / 2)
  rnorm(1, 0, s) + rnorm(m, 0, s)
}

specs <- list(
  list(name = "cont_t6_01", k = 8,  tau2 = 0.001),
  list(name = "cont_t6_02", k = 12, tau2 = 0.01),
  list(name = "cont_t6_03", k = 14, tau2 = 0.05),
  list(name = "cont_t6_04", k = 10, tau2 = 0.1),
  list(name = "cont_t6_05", k = 16, tau2 = 0.15),
  list(name = "cont_t6_06", k = 12, tau2 = 0.25),
  list(name = "cont_t6_07", k = 18, tau2 = 0.4),
  list(name = "cont_t6_08", k = 10, tau2 = 0.6),
  list(name = "cont_t6_09", k = 14, tau2 = 1.0),
  list(name = "cont_t6_10", k = 20, tau2 = 1.5)
)

manifest <- list()
for (s in specs) {
  arms_list <- gen_arms(TT, s$k)
  rows <- list()
  for (i in seq_along(arms_list)) {
    mu_i  <- rnorm(1, mu0, sigma_mu)
    trts  <- arms_list[[i]]
    baseline <- trts[1]
    non_base <- trts[-1]
    deltas   <- gen_deltas(length(non_base), s$tau2)
    for (tt in seq_along(trts)) {
      j <- trts[tt]
      if (j == baseline) {
        theta_true <- mu_i
      } else {
        pos  <- which(non_base == j)
        theta_true <- mu_i + (d_true[j] - d_true[baseline]) + deltas[pos]
      }
      # Arm-level sample mean and sd
      n_ik <- n_arm
      ys   <- rnorm(n_ik, theta_true, sigma_within)
      rows[[length(rows) + 1]] <- data.frame(
        study = i, treatment = j,
        mean = mean(ys), sd = sd(ys), n = n_ik)
    }
  }
  dat <- do.call(rbind, rows)
  write_json(dat, sprintf("test/gc_compare/data/%s.json", s$name),
             auto_unbox = TRUE, digits = 8)
  manifest[[length(manifest) + 1]] <- list(
    name = s$name, tau2_true = s$tau2, k = s$k, treats = TT,
    d_true = d_true, mu0 = mu0, sigma_within = sigma_within, n_arm = n_arm)
}

write_json(manifest, "test/gc_compare/continuous_manifest.json",
           pretty = TRUE, auto_unbox = TRUE)
cat(sprintf("Generated %d continuous T=6 datasets in test/gc_compare/data/\n",
            length(specs)))
