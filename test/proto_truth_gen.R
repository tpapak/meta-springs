#!/usr/bin/env Rscript
# Generate one Gaussian NMA dataset with known τ²_contrast = 0.25.
# T=4 treatments, k=20 studies, 2-arm, ref=1. Per-arm σ²=1/n.
suppressPackageStartupMessages({ library(jsonlite) })
set.seed(20260427)

T  <- 4; k <- 20
tau2_contrast <- 0.25
# Per-arm random-effect variance: τ²_arm = τ²_contrast / 2
tau_arm <- sqrt(tau2_contrast / 2)
d_true  <- c(0, rnorm(T-1, 0, 0.6))

arms_list <- list()
idx <- 1
for (j in 2:T) for (rep in 1:5) {
  if (idx > k) break
  arms_list[[idx]] <- c(1L, j); idx <- idx + 1L
}
while (idx <= k) { arms_list[[idx]] <- sort(sample.int(T, 2)); idx <- idx + 1L }

N <- c(40, 60, 80, 100, 150)
rows <- list()
for (i in seq_len(k)) {
  trts <- arms_list[[i]]
  mu_i <- rnorm(1, 0, 0.5)
  for (j in trts) {
    u  <- rnorm(1, 0, tau_arm)
    n  <- sample(N, 1); se <- 1/sqrt(n)
    y  <- rnorm(1, mu_i + d_true[j] + u, se)
    rows[[length(rows)+1L]] <- data.frame(study=i, treatment=j, mean=y, sd=1, n=n)
  }
}
write_json(do.call(rbind, rows), "/tmp/truth.json", auto_unbox=TRUE)
write_json(list(tau2_contrast_true = tau2_contrast,
                tau2_arm_true      = tau2_contrast/2,
                d_true             = d_true,
                T = T, k = k, n_arms = sum(sapply(arms_list, length))),
           "/tmp/truth_meta.json", auto_unbox=TRUE, digits=6)
cat(sprintf("T=%d, k=%d, A=%d, τ²_contrast=%.4f, τ²_arm=%.4f\n",
            T, k, sum(sapply(arms_list, length)),
            tau2_contrast, tau2_contrast/2))
