#!/usr/bin/env Rscript
# netmeta on test/fallers.csv (380 studies, 139 treatments, 29 multi-arm).
# Uses the contrast-format input directly via netmeta(TE, seTE, t1, t2, studlab).

suppressPackageStartupMessages({library(netmeta); library(jsonlite)})

d <- read.csv("test/fallers.csv", stringsAsFactors = FALSE)
cat(sprintf("input: %d contrasts, %d unique studies, %d treatments\n",
            nrow(d), length(unique(d$id)), length(unique(c(d$t1, d$t2)))))

t0 <- Sys.time()
nm <- netmeta(TE = d$effect, seTE = d$se,
              treat1 = d$t1, treat2 = d$t2, studlab = d$id,
              sm = "OR",                 # log-OR
              random = TRUE, common = FALSE,
              method.tau = "REML",
              tol.multiarm = 1e-3)
elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

cat(sprintf("\nnetmeta wall time: %.2f s\n", elapsed))
cat(sprintf("τ² (REML):  %.5f\n", nm$tau2))
cat(sprintf("τ  (REML):  %.5f\n", nm$tau))
cat(sprintf("Q (random): %.2f on %.0f df, p=%.3g\n",
            nm$Q, as.numeric(nm$df.Q), nm$pval.Q))

# Save numerical summary
out <- list(
  wall_sec = elapsed,
  tau2     = unname(nm$tau2),
  tau      = unname(nm$tau),
  Q        = unname(nm$Q),
  df_Q     = unname(nm$df.Q),
  pval_Q   = unname(nm$pval.Q),
  k_studies = unname(nm$k),
  n_treatments = unname(nm$n),
  n_contrasts  = unname(nm$m)
)
write_json(out, "test/fallers/netmeta_result.json", auto_unbox = TRUE, pretty = TRUE)
cat("\nwrote test/fallers/netmeta_result.json\n")
