#!/usr/bin/env Rscript
# Why does rma.mv disagree with netmeta on multi-arm Gaussian NMA?
#
# Hypothesis: the disagreement is parameterization, not estimation.
# netmeta fits the contrast-based consistency RE model in which the
# random effects on contrasts sharing an arm are forced to correlate
# with ρ = 1/2 — equivalent to one iid arm-level random intercept per
# arm (Lu-Ades / Higgins).  My earlier rma.mv call used
# struct = "CS", which lets ρ float, so it fits a strictly more
# flexible model.  Refit with the four canonical specifications and
# show that the arm-level RE specification matches netmeta exactly.

suppressPackageStartupMessages({
  library(jsonlite); library(netmeta); library(metafor); library(Matrix)
})

datasets <- list(
  list(name = "senn2013",  file = "test/nma_senn2013.json"),
  list(name = "stowe2010", file = "test/nma_stowe2010.json"),
  list(name = "synth_c1",  file = "test/nma_synth_c1.json"),
  list(name = "synth_c2",  file = "test/nma_synth_c2.json"),
  list(name = "synth_c4",  file = "test/nma_synth_c4.json"),
  list(name = "synth_c6",  file = "test/nma_synth_c6.json"),
  list(name = "synth_c10", file = "test/nma_synth_c10.json")
)

read_dataset <- function(path) {
  d <- fromJSON(path, simplifyDataFrame = TRUE)
  d$study <- as.character(d$study); d$treatment <- as.character(d$treatment); d
}

build_V <- function(d, pw) {
  studies <- unique(pw$studlab)
  blocks <- lapply(studies, function(s) {
    sub  <- pw[pw$studlab == s, , drop = FALSE]
    armd <- d[d$study == s, , drop = FALSE]
    var_arm <- setNames(armd$sd^2 / armd$n, armd$treatment)
    k <- nrow(sub); V <- matrix(0, k, k)
    for (i in seq_len(k)) for (j in seq_len(k)) {
      ti1 <- sub$treat1[i]; ti2 <- sub$treat2[i]
      tj1 <- sub$treat1[j]; tj2 <- sub$treat2[j]
      v <- 0
      if (ti1 == tj1) v <- v + var_arm[[ti1]]
      if (ti1 == tj2) v <- v - var_arm[[ti1]]
      if (ti2 == tj1) v <- v - var_arm[[ti2]]
      if (ti2 == tj2) v <- v + var_arm[[ti2]]
      V[i, j] <- v
    }; V
  })
  as.matrix(bdiag(blocks))
}

fit_one <- function(ds) {
  d <- read_dataset(ds$file)
  pw <- pairwise(treat = treatment, n = n, mean = mean, sd = sd,
                 studlab = study, data = d, sm = "MD")
  V <- build_V(d, pw)
  pw$contrast <- paste(pw$treat1, pw$treat2, sep = "_vs_")

  ## (1) netmeta REML — the canonical reference
  nm <- netmeta(TE, seTE, treat1, treat2, studlab,
                data = pw, sm = "MD", random = TRUE, common = FALSE,
                method.tau = "REML", tol.multiarm = 1e-5)

  ## (2) rma.mv with struct="ID": single τ² added to V diagonal,
  ##     ZERO correlation between contrasts in same study.
  ##     Wrong model when there are multi-arm trials.
  fit_id <- tryCatch(rma.mv(TE, V = V, mods = ~ contrast - 1,
                            random = ~ contrast | studlab, struct = "ID",
                            data = pw, method = "REML"),
                     error = function(e) NULL)

  ## (3) rma.mv with struct="CS": single τ² and a free ρ between contrasts
  ##     in same study.  Strictly more flexible than netmeta's model.
  fit_cs <- tryCatch(rma.mv(TE, V = V, mods = ~ contrast - 1,
                            random = ~ contrast | studlab, struct = "CS",
                            data = pw, method = "REML"),
                     error = function(e) NULL)

  ## (4) rma.mv on arm-level data with one iid random effect per arm —
  ##     this IS the Lu-Ades / netmeta model: ρ between arm-sharing
  ##     contrasts is forced to exactly 1/2 and τ²_contrast = 2 τ²_arm.
  d$studyf <- factor(d$study); d$treatf <- factor(d$treatment)
  d$arm_id <- seq_len(nrow(d))
  d$vi <- d$sd^2 / d$n
  fit_arm <- tryCatch(
    rma.mv(yi = mean, V = vi, data = d,
           mods = ~ studyf + treatf - 1,   # FE for each study and treatment
           random = ~ 1 | arm_id, method = "REML"),
    error = function(e) NULL)

  c(name      = ds$name,
    netmeta   = unname(nm$tau2),
    rma_ID    = if (is.null(fit_id)) NA_real_ else unname(fit_id$tau2[1]),
    rma_CS    = if (is.null(fit_cs)) NA_real_ else unname(fit_cs$tau2[1]),
    rma_arm_T2_contrast =
      if (is.null(fit_arm)) NA_real_ else 2 * unname(fit_arm$sigma2[1]),
    rma_CS_rho =
      if (is.null(fit_cs)) NA_real_ else unname(fit_cs$rho))
}

res <- do.call(rbind, lapply(datasets, function(ds) {
  cat(sprintf("== %s ==\n", ds$name)); flush.console()
  r <- fit_one(ds); print(r); r
}))
cat("\nSummary table\n")
print(as.data.frame(res), row.names = FALSE)
write_json(as.data.frame(res), "test/gc_compare/rma_vs_netmeta.json",
           auto_unbox = TRUE, pretty = TRUE, na = "null")
