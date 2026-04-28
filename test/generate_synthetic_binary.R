#!/usr/bin/env Rscript
# Generate 100 synthetic binary NMA datasets from the binomial-normal model.
# Each dataset is saved as JSON in test/synth_bin/ for both R and Haskell.
library(jsonlite)
set.seed(2024)

dir.create("test/synth_bin", showWarnings = FALSE)

meta <- list()

for (idx in 1:100) {
  # Random network parameters
  k <- sample(5:25, 1)          # number of studies
  t <- sample(3:6, 1)           # number of treatments
  tau <- runif(1, 0, 1.2)       # true tau (SD)
  tau2 <- tau^2

  # True treatment effects (treatment 1 is reference = 0)
  d_true <- c(0, rnorm(t - 1, 0, 0.8))

  # Generate network structure: each study compares 2 (or occasionally 3) treatments
  arms_list <- list()
  # Ensure connectivity: first t-1 studies form a chain
  for (i in 1:(t - 1)) {
    arms_list[[i]] <- c(1, i + 1)  # reference vs treatment i+1
  }
  # Remaining studies: random pairs (ensuring reference appears often)
  for (i in t:k) {
    if (runif(1) < 0.6) {
      # Compare with reference
      other <- sample(2:t, 1)
      arms_list[[i]] <- c(1, other)
    } else {
      # Random pair
      pair <- sort(sample(1:t, 2))
      arms_list[[i]] <- pair
    }
    # Occasionally 3-arm
    if (runif(1) < 0.15 && length(arms_list[[i]]) == 2) {
      extra <- sample(setdiff(1:t, arms_list[[i]]), 1)
      arms_list[[i]] <- sort(c(arms_list[[i]], extra))
    }
  }

  # Generate data
  rows <- list()
  for (i in 1:k) {
    trts <- arms_list[[i]]
    mu_i <- rnorm(1, -2.5, 1)  # baseline log-odds (low event rate)
    for (j in trts) {
      u_ij <- rnorm(1, 0, tau)  # random effect
      logit_p <- mu_i + d_true[j] + u_ij
      p <- 1 / (1 + exp(-logit_p))
      n <- sample(c(30, 50, 80, 100, 150, 200, 300), 1)
      events <- rbinom(1, n, p)
      rows[[length(rows) + 1]] <- data.frame(study = i, treatment = j, events = events, n = n)
    }
  }
  dat <- do.call(rbind, rows)

  # Save
  fn <- sprintf("test/synth_bin/synth_%03d.json", idx)
  write_json(dat, fn, auto_unbox = TRUE)

  meta[[idx]] <- list(
    name = sprintf("synth_%03d", idx),
    k = k, treats = t, tau2_true = tau2,
    d_true = d_true
  )
}

write_json(meta, "test/synth_bin/synth_meta.json", pretty = TRUE, auto_unbox = TRUE)
cat("Generated 100 synthetic datasets in test/synth_bin/\n")
