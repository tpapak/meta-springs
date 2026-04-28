#!/usr/bin/env Rscript
# REML τ² for the same Gaussian multi-arm NMA datasets that
# proto_gc_gaussian_multiarm.hs runs.  Uses netmeta::netmeta (REML)
# as the canonical reference and metafor::rma.mv with a fixed-effect
# study term + treatment-contrast random effects on the contrast scale.
#
# multinma is run only when run_multinma=TRUE because it is slow.
#
# Output: test/gc_compare/gaussian_multiarm_R.json

suppressPackageStartupMessages({
  library(jsonlite)
  library(netmeta)
  library(metafor)
  library(dplyr)
})

datasets <- list(
  list(name = "senn2013",      file = "test/nma_senn2013.json"),
  list(name = "parkinson",     file = "test/nma_parkinson.json"),
  list(name = "franchini2012", file = "test/nma_franchini2012.json"),
  list(name = "stowe2010",     file = "test/nma_stowe2010.json"),
  list(name = "synth_c1",      file = "test/nma_synth_c1.json"),
  list(name = "synth_c2",      file = "test/nma_synth_c2.json"),
  list(name = "synth_c3",      file = "test/nma_synth_c3.json"),
  list(name = "synth_c4",      file = "test/nma_synth_c4.json"),
  list(name = "synth_c5",      file = "test/nma_synth_c5.json"),
  list(name = "synth_c6",      file = "test/nma_synth_c6.json"),
  list(name = "synth_c7",      file = "test/nma_synth_c7.json"),
  list(name = "synth_c8",      file = "test/nma_synth_c8.json"),
  list(name = "synth_c9",      file = "test/nma_synth_c9.json"),
  list(name = "synth_c10",     file = "test/nma_synth_c10.json")
)

run_multinma <- isTRUE(as.logical(Sys.getenv("RUN_MULTINMA", "FALSE")))
if (run_multinma) {
  suppressPackageStartupMessages(library(multinma))
  options(mc.cores = parallel::detectCores())
}

read_dataset <- function(path) {
  d <- fromJSON(path, simplifyDataFrame = TRUE)
  d$study     <- as.character(d$study)
    d$treatment <- as.character(d$treatment)
  d
}

run_netmeta <- function(d) {
  pw <- pairwise(treat   = treatment,
                 n       = n,
                 mean    = mean,
                 sd      = sd,
                 studlab = study,
                 data    = d,
                 sm      = "MD")
  nm <- netmeta(TE, seTE, treat1, treat2, studlab,
                data    = pw,
                sm      = "MD",
                random  = TRUE,
                common  = FALSE,
                method.tau = "REML",
                tol.multiarm = 1e-5)
  list(tau2 = unname(nm$tau2),
       tau  = unname(nm$tau))
}

run_rma_mv <- function(d) {
  ## Build contrast-format data with a single reference treatment per study
  ## (first treatment alphabetically).  rma.mv with random = ~ contrast | study
  ## and struct="UN" gives a treatment-by-treatment heterogeneity, but for τ²
  ## comparable to netmeta we use the standard "consistency" RE: random =
  ## ~factor(treatment) | study with sigma2 = c(0, NA) so that only the
  ## treatment-contrast variance is estimated.  We follow the recommended
  ## approach from Viechtbauer (2017): rma.mv on contrasts with V matrix
  ## from pairwise().
  pw <- pairwise(treat   = treatment,
                 n       = n,
                 mean    = mean,
                 sd      = sd,
                 studlab = study,
                 data    = d,
                 sm      = "MD")
  ## Build the variance-covariance matrix per study honoring multi-arm
  ## correlations: pairwise() returns the V-matrix as attribute "V" only
  ## when called with a different signature; instead we use vcov.netmeta-
  ## style: rma.mv(yi=TE, V=seTE^2, mods=~treat1+treat2 ...).  For a clean
  ## comparison we just rebuild the per-study covariance from arms.
  studies <- unique(pw$studlab)
  V_blocks <- lapply(studies, function(s) {
    sub <- pw[pw$studlab == s, , drop = FALSE]
    k   <- nrow(sub)
    if (k == 1) return(matrix(sub$seTE^2, 1, 1))
    ## Multi-arm: compute the proper covariance from arm-level data.
    armd <- d[d$study == s, , drop = FALSE]
    rownames(armd) <- armd$treatment
    var_arm <- setNames(armd$sd^2 / armd$n, armd$treatment)
    V <- matrix(0, k, k)
    for (i in seq_len(k)) {
      for (j in seq_len(k)) {
        ti1 <- sub$treat1[i]; ti2 <- sub$treat2[i]
        tj1 <- sub$treat1[j]; tj2 <- sub$treat2[j]
        ## Cov((y_a - y_b), (y_c - y_d)) =
        ##   var(a)*[a==c] - var(a)*[a==d] - var(b)*[b==c] + var(b)*[b==d]
        v <- 0
        if (ti1 == tj1) v <- v + var_arm[[ti1]]
        if (ti1 == tj2) v <- v - var_arm[[ti1]]
        if (ti2 == tj1) v <- v - var_arm[[ti2]]
        if (ti2 == tj2) v <- v + var_arm[[ti2]]
        V[i, j] <- v
      }
    }
    V
  })
  V <- as.matrix(Matrix::bdiag(V_blocks))
  pw$contrast <- paste(pw$treat1, pw$treat2, sep = "_vs_")
  pw$row <- seq_len(nrow(pw))
  ## Single τ² across all contrasts (consistency model)
  fit <- tryCatch(
    rma.mv(yi = TE, V = V, mods = ~ contrast - 1,
           random = ~ contrast | studlab, struct = "CS",
           data = pw, method = "REML"),
    error = function(e) NULL)
  if (is.null(fit)) return(list(tau2 = NA_real_, tau = NA_real_))
  ## struct="CS" gives sigma2 = τ²(common) + ρ between contrasts.  τ² is
  ## fit$tau2[1] for "DIAG"-like, but for CS we have sigma2[1] and rho.
  ## We compare against the netmeta τ² which is the consistency-RE variance.
  list(tau2 = unname(fit$tau2[1]), tau = sqrt(unname(fit$tau2[1])))
}

run_multinma_one <- function(d) {
  if (!run_multinma) return(list(tau2_mode = NA_real_, tau2_median = NA_real_))
  d$study     <- factor(d$study)
  d$treatment <- factor(d$treatment)
  net <- set_agd_arm(d, study = study, trt = treatment,
                     y = mean, se = sd / sqrt(n),
                     trt_ref = levels(d$treatment)[1])
  fit <- nma(net, trt_effects = "random",
             likelihood = "normal",
             prior_intercept = normal(scale = 100),
             prior_trt       = normal(scale = 100),
             prior_het       = half_normal(scale = 1),
             iter = 2000, warmup = 1000, chains = 4,
             refresh = 0, seed = 42)
  tau <- as.data.frame(fit, pars = "tau")[[1]]
  tau2 <- tau^2
  dens <- density(tau2, from = 0, to = max(tau2), n = 2048)
  list(tau2_mode   = dens$x[which.max(dens$y)],
       tau2_median = median(tau2),
       tau2_mean   = mean(tau2))
}

results <- lapply(datasets, function(ds) {
  cat(sprintf("\n=== %s ===\n", ds$name)); flush.console()
  d <- read_dataset(ds$file)
  multi <- sum(table(d$study) > 2)
  k <- length(unique(d$study))

  nm <- tryCatch(run_netmeta(d), error = function(e) {
    message("netmeta failed: ", conditionMessage(e))
    list(tau2 = NA_real_, tau = NA_real_)
  })
  rma <- tryCatch(run_rma_mv(d), error = function(e) {
    message("rma.mv failed: ", conditionMessage(e))
    list(tau2 = NA_real_, tau = NA_real_)
  })
  mn <- tryCatch(run_multinma_one(d), error = function(e) {
    message("multinma failed: ", conditionMessage(e))
    list(tau2_mode = NA_real_, tau2_median = NA_real_)
  })

  cat(sprintf("  k=%d multi=%d  netmeta τ²=%.5f  rma.mv τ²=%.5f  mn(mode)=%s\n",
              k, multi, nm$tau2, rma$tau2,
              if (is.na(mn$tau2_mode)) "—" else sprintf("%.5f", mn$tau2_mode)))

  list(name        = ds$name,
       file        = ds$file,
       k_studies   = k,
       multi_arm_k = multi,
       netmeta_tau2 = nm$tau2,
       rma_mv_tau2  = rma$tau2,
       multinma_tau2_mode   = mn$tau2_mode,
       multinma_tau2_median = mn$tau2_median)
})

write_json(list(results = results),
           path = "test/gc_compare/gaussian_multiarm_R.json",
           auto_unbox = TRUE, pretty = TRUE, na = "null")

cat("\nwrote test/gc_compare/gaussian_multiarm_R.json\n")
