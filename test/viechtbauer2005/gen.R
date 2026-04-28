#!/usr/bin/env Rscript
# Reproduce a slice of Viechtbauer (2005) Table 1 / Figure 1 with the
# UMD outcome.  Following his §6.1 design verbatim:
#
#   X^C_ij ~ N(0,        σ²),  X^E_ij ~ N(μθ + θ_i,  σ²)
#   θ_i    ~ N(0, σ²θ),  σ² = 10  (his convention)
#   n_Ci = n_Ei = n_i,   n_i ~ N(n̄, n̄/3)  (rounded to int, ≥ 4)
#   ES_i  = X̄^E - X̄^C  has SE² = 2 σ² / n_i
#
# Conditions: k=20 studies, n̄ ∈ {20, 40}, σθ² ∈ {0.125, 0.25, 0.5},
# 1000 replicas each.  Output one rep_*.json per replica per
# condition for gc-rust multi-rep mode.

suppressPackageStartupMessages({ library(jsonlite) })

set.seed(20260427L)

K           <- 20L
sigma2_w    <- 10.0
mu_theta    <- 0.0
N_reps      <- 1000L
nbar_grid   <- c(20L, 40L)
sig2theta_g <- c(0.125, 0.25, 0.5)

manifest <- list()
ROOT <- "test/viechtbauer2005"

for (nbar in nbar_grid) {
  for (s2t in sig2theta_g) {
    cond_lab <- sprintf("nbar%d_s2t%g", nbar, s2t)
    dir <- file.path(ROOT, "data", cond_lab)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("  generating %s ... ", cond_lab)); flush.console()
    for (r in 1:N_reps) {
      # heterogeneous per-study sample sizes
      ni <- pmax(4L, round(rnorm(K, mean = nbar, sd = nbar / 3)))
      theta <- rnorm(K, mean = mu_theta, sd = sqrt(s2t))
      arms <- list()
      for (i in 1:K) {
        n   <- ni[i]
        # control arm
        xC  <- rnorm(n, mean = 0, sd = sqrt(sigma2_w))
        # experimental arm
        xE  <- rnorm(n, mean = theta[i], sd = sqrt(sigma2_w))
        # arm-level summaries (gc-rust expects mean, sd, n)
        arms[[length(arms)+1L]] <- list(study = i, treatment = 1L,
                                        mean = mean(xC), sd = sd(xC), n = as.integer(n))
        arms[[length(arms)+1L]] <- list(study = i, treatment = 2L,
                                        mean = mean(xE), sd = sd(xE), n = as.integer(n))
      }
      fn <- file.path(dir, sprintf("rep_%04d.json", r))
      write_json(arms, fn, auto_unbox = TRUE, digits = 10)
    }
    manifest[[length(manifest)+1L]] <- list(
      condition = cond_lab, nbar = nbar, sigma2_theta_true = s2t,
      k = K, n_reps = N_reps,
      data_dir = dir
    )
    cat("done\n")
  }
}
write_json(manifest, file.path(ROOT, "manifest.json"),
           auto_unbox = TRUE, pretty = TRUE)
cat(sprintf("\nGenerated %d replicas across %d conditions\n",
            N_reps * length(nbar_grid) * length(sig2theta_g),
            length(nbar_grid) * length(sig2theta_g)))
