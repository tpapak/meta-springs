#!/usr/bin/env Rscript
# Run netmeta::netmeta REML on every replica in test/sim_t20/data/.
# Output: test/sim_t20/results/netmeta.json with one row per replica.

suppressPackageStartupMessages({
  library(jsonlite); library(netmeta)
})

manifest <- fromJSON("test/sim_t20/data/manifest.json", simplifyDataFrame = TRUE)
results  <- vector("list", nrow(manifest))

t0 <- Sys.time()
for (i in seq_len(nrow(manifest))) {
  m <- manifest[i, ]
  d <- fromJSON(m$file, simplifyDataFrame = TRUE)
  d$study <- as.character(d$study); d$treatment <- as.character(d$treatment)
  pw <- pairwise(treat = treatment, n = n, mean = mean, sd = sd,
                 studlab = study, data = d, sm = "MD")
  nm <- tryCatch(
    netmeta(TE, seTE, treat1, treat2, studlab,
            data = pw, sm = "MD",
            random = TRUE, common = FALSE, method.tau = "REML",
            tol.multiarm = 1e-5),
    error = function(e) NULL)
  results[[i]] <- list(
    tau2_idx    = m$tau2_idx,
    tau2_true   = m$tau2,
    rep         = m$rep,
    k_studies   = m$k_studies,
    multi_arm_k = m$multi_arm_k,
    netmeta_tau2 = if (is.null(nm)) NA_real_ else unname(nm$tau2)
  )
  if (i %% 10 == 0) {
    el <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("  %d/%d  (%.1fs elapsed)\n", i, nrow(manifest), el))
    flush.console()
  }
}

write_json(list(results = results),
           "test/sim_t20/results/netmeta.json",
           auto_unbox = TRUE, pretty = TRUE, na = "null")
cat(sprintf("\nwrote test/sim_t20/results/netmeta.json (%.1fs)\n",
            as.numeric(difftime(Sys.time(), t0, units = "secs"))))
